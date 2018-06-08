### Rational Choice Energy Model

# for simplicity and comparability, epsilon is taken to be a constant
# however, it is taken as a variable here, that could take input in the future
# if this model is sourced together with such a modification

# we will have an adjacency matrix

# the parameters are set only with default values
stopifnot(packageVersion("SpaDES") >= "1.2.0.9006")

defineModule(sim, list(
  name = "rc_energy_model",
  description = "Simulate bounded confidence opinion dynamics model in Rational Choice framework with utility computation respecting energy levels of agents.",
  keywords = c("opinion dynamics", "hegselmann krause", "rational choice"),
  childModules = character(),
  authors = c(person(c("Malte", "Lars"), "Heckelen", email = "malte.heckelen@ilw.uni-stuttgart.de", role = c("aut", "cre"))),
  version = numeric_version("0.0.1"),
  spatialExtent = raster::extent(rep(NA_real_, 4)),
  timeframe = as.POSIXlt(c(NA, NA)),
  timeunit = "hour",
  citation = list("citation.bib"),
  documentation = list("README.txt", "rc_energy_model.Rmd"),
  reqdPkgs = list("tidyverse", "data.table"),
  parameters = rbind(
    # defineParameter("paramName", "paramClass", value, min, max, "parameter description"),
    defineParameter("epsilon", "numeric", 0.1, NA, NA, "The Bounded Confidence parameter."),
    defineParameter("other_incons_tolerance", "numeric", 0.1, NA, NA, "The parameter controlling the tolerance for the degree with which other agents to have varying opinions over time."),
    defineParameter("self_incons_tolerance", "numeric", 0.1, NA, NA, "The parameter controlling the tolerance for the degree with which the agent himself has varying opinions over time."),
    defineParameter("message_memory_depth", "numeric", 1, NA, NA, "The number of time steps agents remember messages for."),
    defineParameter("opinion_memory_depth", "numeric", 1, NA, NA, "The number of time steps agents remember opinions for."),
    defineParameter("energy_level", "numeric", 100, NA, NA, "The starting energy level of every agent."),
    defineParameter("restoration_factor", "numeric", 20, NA, NA, "The number of energy units that are restored to agents at the end of every round."),
    defineParameter("energy_params_memory_depth", "numeric", 1, NA, NA, "The number of time steps agents remember statistics relevant for overall action selection for."),
    defineParameter("initial_opinion_confidence", "numeric", 1, NA, NA, "The standard deviation of past opinions (centered around the current opinion).")
  ),
  inputObjects = bind_rows(
    expectsInput("environment", "tbl_graph", "The environment for the agents"),
    expectsInput("agent_characteristics", "tbl_df", "The characteristics of each agent.")
  ),
  outputObjects = bind_rows(
    createsOutput("agent_characteristics", "tbl_df", "The characteristics of each agent."),
    createsOutput("data_collection_prompt", "logical", "A prompt to collect data.")
  )
)
)


doEvent.rc_energy_model <- function(sim, eventTime, eventType, debug = FALSE) {

  switch(
    eventType,
    init = {
      ## do stuff for this event
      sim <- rc_energy_modelInit(sim)

      ## schedule future event(s)
      sim <- scheduleEvent(sim, eventTime = start(sim)+1, moduleName = "rc_energy_model", eventType = "step")
    },
    step = {
      ## do stuff for this event
      sim <- rc_energy_modelStep(sim)

      ## schedule future event(s)
      sim <- scheduleEvent(sim, eventTime = time(sim)+1, moduleName = "rc_energy_model", eventType = "step")
    },
    warning(paste("Undefined event type: '", current(sim)[1, "eventType", with = FALSE],
                  "' in module '", current(sim)[1, "moduleName", with = FALSE], "'", sep = ""))
  )

  return(invisible(sim))

}

rc_energy_modelInit <- function(sim) {

  print(time(sim))
  
  ############################################
  #### MODIFY AGENT_CHARACTERISTICS TABLE ####
  ############################################

  sim$agent_characteristics <- sim$agent_characteristics %>%
    data.table() %>%
    .[ , energy := params(sim)$rc_energy_model$energy_level]
  
  ######################################## 
  #### CONSTRUCT CHOSEN_ACTIONS TABLE ####
  ########################################

  sim$chosen_actions <- tibble(

    agent_id = rep(agent_characteristics$agent_id, each=2),
    action_type = rep(c("actions_overall", "actions_send"), sim$no_agents),
    best_action = rep(c("Not assigned"), length(action_type))

  ) %>% data.table() %>%
    .[ , agent_id := as.integer(agent_id) ]
  
  ######################################### 
  #### CONSTRUCT OVERALL_ACTIONS TABLE ####
  #########################################

  sim$actions_overall <- tibble(

    agent_id = rep(agent_characteristics$agent_id, each=4),
    actions = rep(c("Send", "Receive", "Both", "Nothing"), no_agents),
    util_score = rep(0, length(actions))

  ) %>%
    data.table() %>%
    .[ actions == "Both", util_score := 1 ] %>%
    dcast(agent_id ~ actions, value.var = "util_score") %>%
    .[ , agent_id := as.character(agent_id)] %>%
    .[, best_action :=  names(.[ , -c("agent_id")])[apply(.[ , -c("agent_id") ], 1, which.max)]] %>%
    melt( id.vars = c("agent_id", "best_action"),
          measure.vars = c("Send", "Receive", "Both", "Nothing"),
          variable.name = "actions",
          value.name = "util_score" ) %>%
    .[ , agent_id := as.integer(agent_id) ]
  
  #######################################################
  #### ASSIGN BEST OVERALL ACTIONS TO CHOSEN ACTIONS ####
  #######################################################

  sim$chosen_actions <- copy(sim$actions_overall)[ actions == best_action , .(agent_id, best_action) ] %>%
    merge(sim$chosen_actions, by=c("agent_id"), all.x=TRUE, all.y=TRUE) %>%
    .[ action_type == "actions_overall" , best_action := ifelse(!is.na(best_action.x), best_action.x,  "NOT ASSIGNED")] %>% 
    .[ , -c("best_action.x", "best_action.y")]

  #############################################
  #### CONSTRUCT TABLE FOR SENDING ACTIONS ####
  #############################################

  sim$actions_send <- tibble(

    agent_id = rep(agent_characteristics$agent_id, each=2),
    actions = rep(c("Unoptimized", "Optimized"), no_agents),
    util_score = rep(0, length(actions))

  ) %>%  data.table() %>%
    .[ , agent_id := as.integer(agent_id) ]
  
  #################################
  #### CONSTRUCT MESSAGE TABLE ####
  #################################

  # 1.)
  sim$messages <- sim$environment %>%
    activate(edges) %>%
    as_tibble() %>%
    data.table() %>%
    setnames(old = c("from", "to"), new = c("receiver", "sender"))

  # 2.)
  sim$messages <- sim$environment %>%
    activate(edges) %>%
    as_tibble() %>%
    data.table() %>%
    setnames(old = c("from", "to"), new = c("sender", "receiver")) %>%
    rbind(messages) %>%
    .[copy(sim$agent_characteristics)[, .(agent_id, opinion)], nomatch = 0L, on = c("sender" = "agent_id"), allow.cartesian=TRUE] %>%
    setnames("opinion", "opinion_sender")

  ###################################
  #### PRODUCE POSSIBLE MESSAGES ####
  ###################################

  assumption_receiver <- runif(nrow(sim$messages), 0, 1)
  sim$messages <- cbind(sim$messages, assumption_receiver)

  sim$message_matrix <- outer(sim$messages$opinion_sender, sim$messages$assumption_receiver, produce_altered_message) 
  row.names(sim$message_matrix) <- sim$messages[ , sender]
  colnames(sim$message_matrix) <- sim$messages[ , receiver]
  
  sim$messages <- copy(sim$message_matrix) %>%
    data.table() %>%
    .[ , sender := as.numeric(row.names(sim$message_matrix))] %>%
    melt( id.vars = c("sender"),
          measure.vars = as.character(seq(1, sim$no_agents, 1)),
          variable.name = "receiver",
          value.name = "opt_message" ) %>%
    .[ receiver != sender] %>%
    setkey("sender") %>%
    unique() %>%
    .[ , sender := as.integer(sender)] %>%
    .[ , receiver := as.integer(receiver)] %>%
    .[copy(unique(sim$messages)), on=c("sender", "receiver"), nomatch = 0L, allow.cartesian = TRUE] %>%
    .[ , opt_message := median(opt_message), by = .(sender)] 
  
  ##########################################
  #### CONSTRUCT DISCOURSE MEMORY TABLE ####
  ##########################################

  sim$discourse_memory <- copy(sim$messages)[ , -c("receiver", "assumption_receiver") ] %>%
    unique() %>%
    .[ , past_opinions := sapply(opinion_sender, function(x) {list(
		    ifelse(rnorm(params(sim)$rc_energy_model$opinion_memory_depth, x, params(sim)$rc_energy_model$initial_opinion_confidence) > 1, 1,
			    ifelse(rnorm(params(sim)$rc_energy_model$opinion_memory_depth, x, params(sim)$rc_energy_model$initial_opinion_confidence) < 0, 0, rnorm(params(sim)$rc_energy_model$opinion_memory_depth, x, params(sim)$rc_energy_model$initial_opinion_confidence) ) ) )  } )] %>%
    setnames("opinion_sender", "opinion") %>%
    .[ , .(sender, opinion, past_opinions)] %>%
    .[ , past_opinions := as.character(past_opinions) ] %>% # unique() can't handle list columns, so first transform to character
    unique() %>%
    .[ , past_opinions := sapply(past_opinions, function(x) list(eval(parse(text = x))))] %>% # then transform back
    .[ , sender_business := 0 ] %>%
    .[ , receiver_business := 0 ]

   if( length(sim$actions_overall[ best_action %in% c("Both", "Send") , best_action ] > 0 ) ) {

    #######################################
    #### DETERMINE BEST SENDING ACTION ####
    #######################################

    sim$actions_send <- copy(sim$messages) %>%
      .[copy(sim$actions_overall), on = c("sender" = "agent_id"), nomatch = 0L, allow.cartesian = TRUE ] %>%
      .[ best_action == actions ] %>% # filter irrelevant rows with not chosen actions out
      .[ (best_action == "Both" | best_action == "Send") ] %>% # only rows with both or send
      .[ , .(sender , receiver , opt_message , opinion_sender , assumption_receiver) ] %>%
      unique() %>%
      .[copy(sim$actions_send), on = c("sender" = "agent_id"), nomatch = 0L, allow.cartesian = TRUE ] %>%
      .[copy(sim$discourse_memory), on = c("sender"), nomatch = 0L, allow.cartesian = TRUE] %>%
      .[, distance_to_past_opinions := mapply(function(a,b,c,k) {
        switch(k,
               "Unoptimized" = {
                 mean(
                   sapply(a, function(x) {
                     abs(x - c)
                   })
                 )
               },
               "Optimized" = {
                 mean(
                   sapply(a, function(x) {
                     abs(x - b)
                   })
                 )
               }
        )
      }, a=past_opinions, b=opt_message, c=opinion_sender, k=actions)] %>%
      .[, distance_message_opinion := mapply(function(a,b,k) {
        switch(k,
               "Unoptimized" = {
                 abs(a - a)
               },
               "Optimized" = {
                 abs(a - b)
               }
        )
      }, a=opinion_sender, b=opt_message, k=actions)] %>%
      .[, distance_message_assumption := mapply(function(a,b,c,k) {
        switch(k,
               "Unoptimized" = {
                 abs(c - a)
               },
               "Optimized" = {
                 abs(c - b)
               }
        )
      }, a=opinion_sender, b=opt_message, c=assumption_receiver, k=actions)] %>%
      .[ , -c("past_opinions", "receiver")] %>% # include past_messages here in the step function
      .[ , util_score := 0 - distance_to_past_opinions - distance_message_opinion - distance_message_assumption] %>% 
      .[ , agent_id := sender ] %>%
      .[ , .(agent_id, actions, util_score)] %>%
      setkey("agent_id") %>%
      unique() %>%
      .[, util_score := sum(util_score), by=c("agent_id", "actions")] %>%
      unique() %>%
      dcast(agent_id ~ actions, value.var = "util_score") %>%
      .[, best_action :=  ifelse(Optimized > Unoptimized, "Optimized", "Unoptimized")] %>%
      melt( id.vars = c("agent_id", "best_action"),
            measure.vars = c("Optimized", "Unoptimized"),
            variable.name = "actions",
            value.name = "util_score" ) %>%
      .[ , agent_id := as.integer(agent_id) ]

 
    #####################################
    #### UPDATE CHOSEN_ACTIONS TABLE ####
    #####################################

    sim$chosen_actions <- copy(sim$actions_send)[ actions == best_action , .(agent_id, best_action) ] %>%
      merge(sim$chosen_actions, by="agent_id", all.x=TRUE, all.y=TRUE) %>%
      .[ best_action.y == "Receive" , action_type := "actions_overall" ] %>%
      .[ best_action.y == "Nothing" , action_type := "actions_overall" ] %>%
      .[ action_type == "actions_send" , best_action := ifelse(!is.na(best_action.x), best_action.x, "NOT ASSIGNED")] %>%
      .[ action_type == "actions_overall" , best_action := ifelse(!is.na(best_action.y), best_action.y, "NOT ASSIGNED")] %>%
      .[ , -c("best_action.x", "best_action.y")]
    
    #################################
    #### UPDATE DISCOURSE_MEMORY ####
    #################################

    sim$discourse_memory <- copy(sim$actions_send)[ , -c("util_score")] %>%
      .[copy(sim$messages), on = c("agent_id" = "sender"), nomatch = 0L, allow.cartesian = TRUE] %>%
      setnames("agent_id", "sender") %>%
      setkey("sender") %>%
      .[ actions==best_action ] %>%
      .[ , -c("actions")]  %>% 
      unique() %>% 
      .[ , sender_business := .N, by = "sender" ] %>% 
      .[ , receiver_business := .N, by = "receiver" ] %>% 
      merge(sim$discourse_memory[ , -c("sender_business", "receiver_business", "assumption_receiver")], by=c("sender"), all.x=TRUE)%>%
      .[ , sender_business := ifelse(is.na(sender_business), 0, sender_business) ] %>% 
      .[ , receiver_business := ifelse(is.na(receiver_business), 0, receiver_business) ] %>%
      .[ , past_messages := mapply(function(x,y,z) {

	      ifelse(x == "Unoptimized",
		      y,
		      z)

				   }, x=best_action, y=opinion_sender, z=opt_message) ] %>%
      .[ , past_opinions := ifelse(lengths(past_opinions) < params(sim)$rc_energy_model$opinion_memory_depth,
                                   mapply(function(x, y) {
                                     list(c(unlist(x), y))
                                   }, x=past_opinions, y=opinion_sender),
                                   mapply(function(x, y) {
                                     list(c(unlist(x)[1:params(sim)$rc_energy_model$opinion_memory_depth], y))
                                   }, x=past_opinions, y=opinion_sender)
      )] %>%
      setnames("opinion_sender", "opinion") %>%
      setnames("opt_message", "message") %>%
      .[ , .(sender, receiver, opinion, message, past_messages, past_opinions, sender_business, receiver_business)] %>%
      .[ , past_messages := as.character(past_messages) ] %>%
      .[ , past_opinions := as.character(past_opinions) ] %>%
      unique() %>%
      .[ , past_messages := sapply(past_messages, function(x) list(eval(parse(text = x))))] %>%
      .[ , past_opinions := sapply(past_opinions, function(x) list(eval(parse(text = x))))]

    temp <- copy(sim$discourse_memory)[ , .(receiver, receiver_business)] %>% 
	    .[ , receiver_business := max(receiver_business), by=receiver] %>%
	    unique() 

    sim$discourse_memory <- copy( sim$discourse_memory)[ , -c("receiver_business")]  %>%
      merge(temp, by.x = c("sender"), by.y = c("receiver"), all.x = TRUE) %>% 
      .[ , -c("receiver") ] %>%
      .[ , past_receiver_business := ifelse( !is.na(receiver_business), sapply(receiver_business, function(x) list(x) ), 0 ) ] %>%
      .[ , past_sender_business := ifelse( !is.na(sender_business), sapply(sender_business, function(x) list(x) ), 0 ) ] %>%
      .[ , nbh_incohesion := vec_get_nbh_incohesion(sender ) ] %>%
      .[ , past_nbh_incohesion := ifelse( !is.na(nbh_incohesion), sapply(nbh_incohesion, function(x) list(x) ), 0 ) ] %>%
      .[ , self_incohesion := vec_get_self_incohesion( sender ) ] %>%
      .[ , past_self_incohesion := ifelse( !is.na(self_incohesion), sapply(self_incohesion, function(x) list(x) ), 0 ) ] %>%
      .[ , past_messages := as.character(past_messages) ] %>%
      .[ , past_opinions := as.character(past_opinions) ] %>%
      .[ , past_receiver_business := as.character(past_receiver_business) ] %>%
      .[ , past_sender_business := as.character(past_sender_business) ] %>% 
      .[ , past_nbh_incohesion := as.character(past_nbh_incohesion) ] %>%
      .[ , past_self_incohesion := as.character(past_self_incohesion) ] %>%
      unique() %>%
      .[ , past_messages := sapply(past_messages, function(x) list(eval(parse(text = x))))] %>%
      .[ , past_opinions := sapply(past_opinions, function(x) list(eval(parse(text = x))))] %>%
      .[ , past_receiver_business := sapply(past_receiver_business, function(x) list(eval(parse(text = x))))] %>%
      .[ , past_sender_business := sapply(past_sender_business, function(x) list(eval(parse(text = x))))] %>%
      .[ , past_nbh_incohesion := sapply(past_nbh_incohesion, function(x) list(eval(parse(text = x))))] %>%
      .[ , past_self_incohesion := sapply(past_self_incohesion, function(x) list(eval(parse(text = x))))] 

    #########################
    #### "SEND" MESSAGES ####
    #########################

    sim$messages <- copy(sim$messages) %>%
      unique() %>%
      setnames(old = c("sender", "receiver"), new = c("receiver", "sender")) %>%
      .[ , .(receiver, opinion_sender, opt_message)] %>% # opt_message: should that be here?
      .[ , opinion_receiver := opinion_sender] %>%
      .[ , -c("opinion_sender") ] %>%
      .[sim$messages[ , -c("opt_message") ], on="receiver", nomatch=0L, allow.cartesian=TRUE] 

    sim$messages <- copy(sim$messages) %>%
      .[sim$actions_send[ , .(agent_id, actions, best_action)], nomatch=0L, on=c("sender" = "agent_id"), allow.cartesian=TRUE] %>%
      .[ , assumption_sender := ifelse(.[, best_action] == "Unoptimized", opinion_sender, opt_message)] %>%
      setkey("sender") %>%
      unique()

    ###################
    #### RECEIVING ####
    ###################
    
    sim$opinion_updating <- copy(sim$messages)[ , -c("actions", "best_action")] %>%
      unique() %>%
      .[copy(actions_overall), on = c("receiver" = "agent_id"), nomatch = 0L, allow.cartesian = TRUE ] %>% 
      .[ best_action == actions ] %>% 
      .[ (best_action == "Both" | best_action == "Receive") ] %>% 
      .[ , .(sender, receiver, opinion_receiver, assumption_sender, opt_message, actions, best_action)] %>%
      .[copy(sim$discourse_memory)[ , .(sender, past_opinions)] %>% .[ , past_opinions := as.character(past_opinions) ] %>% unique %>% .[ , past_opinions := sapply(past_opinions, function(x) list(eval(parse(text = x))))], on=c("receiver"="sender"), nomatch = 0L, allow.cartesian = TRUE] %>%
      .[ best_action == actions ] %>%
      .[ , distance_to_past_opinions := mapply(function(a,b) {
        mean(
          sapply(a, function(x) {
            abs(x - b)
          })
        )
      }, a=past_opinions, b=assumption_sender)] %>%
      .[ , within_epsilon := abs(opinion_receiver - assumption_sender) < params(sim)$rc_energy_model$epsilon] %>%
      .[ , self_consistent := distance_to_past_opinions < params(sim)$rc_energy_model$self_incons_tolerance] %>%
      .[ within_epsilon == TRUE & self_consistent == TRUE ] %>% 
      .[ , sum_assumptions := sum(assumption_sender), by=receiver] %>%
      .[ , denominator := .N, by=receiver ] %>%
      .[ , reception_energy_loss := .N, by=receiver ] %>%
      .[ , opinion_receiver_new := ifelse( denominator != 0, sum_assumptions / denominator, opinion_receiver )] %>%
      setnames("receiver", "agent_id") %>%
      .[ , .(agent_id, opinion_receiver_new)] %>%
      setkey("agent_id") %>%
      unique() 

    ######################################
    #### UPDATE agent_characteristics ####
    ######################################

    # merge with sim$opinion_updating, assign new opinion if applicable
    sim$agent_characteristics <- merge(sim$agent_characteristics, sim$opinion_updating, by="agent_id", all.x = TRUE) %>%
      .[ , opinion := ifelse(is.na(opinion_receiver_new), opinion, opinion_receiver_new)] %>%
      .[ , -c("opinion_receiver_new")]

    sim$agent_characteristics <- copy(sim$actions_overall)[ , best_axn_overall := best_action][ , .(agent_id, best_axn_overall)] %>%
      unique() %>%
      merge(copy(sim$actions_send)[ , best_axn_send := best_action][ , .(agent_id, best_axn_send)], by="agent_id", allow.cartesian = TRUE) %>%
      unique() %>%
      merge(copy(sim$discourse_memory)[ , .(sender, receiver_business, sender_business)], by.x = "agent_id", by.y = "sender", allow.cartesian = TRUE) %>%
      unique() %>%
      .[ best_axn_overall == "Send" , energy_loss := sender_business ] %>%
      .[ best_axn_overall == "Receive", energy_loss := receiver_business ] %>%
      .[ best_axn_overall == "Both", energy_loss := sender_business + receiver_business ] %>%
      .[ best_axn_overall == "Nothing", energy_loss := 0 ] %>%
      .[ best_axn_send == "Unoptimized", energy_loss := energy_loss ] %>%
      .[ best_axn_send == "Optimized", energy_loss := energy_loss + sender_business ] %>%
      .[ , .(agent_id, energy_loss) ] %>%
      unique() %>%
      .[ , energy_loss := sum(energy_loss), by="agent_id" ] %>%
      unique() %>%
      .[sim$agent_characteristics, on="agent_id"] %>%
      .[ , energy := energy - energy_loss + params(sim)$rc_energy_model$restoration_factor ] %>%
      .[ , -c("energy_loss") ]

    ##################################################
    #### BUSINESS UPDATE FOR sim$discourse_memory ####
    ##################################################

    sim$discourse_memory <- sim$agent_characteristics[ , .(agent_id, opinion) ] %>%
      .[sim$discourse_memory[ , -c("opinion")], on=c("agent_id" = "sender") ] %>%
      setnames("agent_id", "sender") 

  } else {
    
    #################################################
    #### UPDATING OF ENERGY IN CASE OF NO ACTION ####
    #################################################

    sim$discourse_memory <- copy(sim$discourse_memory) %>%
      .[ , receiver_business := 0 ] %>%
      .[ , sender_business := 0 ] %>%
      .[ , past_receiver_business := ifelse(lengths(past_receiver_business) < params(sim)$rc_energy_model$energy_params_memory_depth,
                                            mapply(function(x, y) {
                                              list(c(unlist(x), y))
                                            }, x=past_receiver_business, y=receiver_business),
                                            mapply(function(x, y) {
                                              list(c(unlist(x)[1:params(sim)$rc_energy_model$energy_params_memory_depth], y))
                                            }, x=past_receiver_business, y=receiver_business)
      )] %>%
      .[ , past_sender_business := ifelse(lengths(past_sender_business) < params(sim)$rc_energy_model$energy_params_memory_depth,
                                          mapply(function(x, y) {
                                            list(c(unlist(x), y))
                                          }, x=past_sender_business, y=sender_business),
                                          mapply(function(x, y) {
                                            list(c(unlist(x)[1:params(sim)$rc_energy_model$energy_params_memory_depth], y))
                                          }, x=past_sender_business, y=sender_business)
      )]

    sim$agent_characteristics <- sim$agent_characteristics %>%
      .[ , energy := energy + params(sim)$rc_energy_model$restoration_factor ]

  }

  return(invisible(sim))

}

rc_energy_modelStep <- function(sim) {

  print(time(sim))

  ########################
  #### REBUILD TABLES ####
  ########################

  # tables that are rebuilt at the start of each round: sim$actions_send, sim$actions_overall, sim$messages

  sim$actions_send <- tibble(
    
    agent_id = rep(sim$agent_characteristics$agent_id, each=2),
    actions = rep(c("Unoptimized", "Optimized"), no_agents),
    util_score = rep(0, length(actions))
    
  ) %>%  data.table() %>%
    .[ , agent_id := as.integer(agent_id) ]
  
  sim$actions_overall <- tibble(

    agent_id = rep(sim$agent_characteristics$agent_id, each=4),
    actions = rep(c("Send", "Receive", "Both", "Nothing"), sim$no_agents),
    util_score = rep(0, length(actions))

  ) %>% data.table()


  # 1.)
  sim$messages <- sim$environment %>%
    activate(edges) %>%
    as_tibble() %>%
    data.table() %>%
    setnames(old = c("from", "to"), new = c("receiver", "sender"))

  # 2.)
  sim$messages <- sim$environment %>%
    activate(edges) %>%
    as_tibble() %>%
    data.table() %>%
    setnames(old = c("from", "to"), new = c("sender", "receiver")) %>%
    rbind(sim$messages) %>%
    .[copy(sim$agent_characteristics)[, .(agent_id, opinion)], nomatch = 0L, on = c("sender" = "agent_id"), allow.cartesian=TRUE] %>%
    setnames("opinion", "opinion_sender")

  ######################################## 
  #### CONSTRUCT CHOSEN_ACTIONS TABLE ####
  ########################################

  sim$chosen_actions <- tibble(

    agent_id = rep(agent_characteristics$agent_id, each=2),
    action_type = rep(c("actions_overall", "actions_send"), sim$no_agents),
    best_action = rep(c("Not assigned"), length(action_type))

  ) %>% data.table() %>%
    .[ , agent_id := as.integer(agent_id) ]
  
  #################################################################
  #### BUILD sim$actions_overall AND FIND BEST OVERALL ACTIONS ####
  #################################################################

  sim$actions_overall <- copy(sim$messages)[ , .(sender, receiver)] %>%
    .[ , max_send_energy_loss := .N, by = sender ] %>%
    .[ , max_receive_energy_loss := .N, by = sender ] %>% # this is problematic in directed networks
    .[copy(sim$discourse_memory), on=c("sender") ] %>%
    .[copy(sim$discourse_memory)[, .(sender, message)][ , assumption_sender := message][ , -c("message")] %>% unique, on=c("receiver"="sender"), nomatch=0L] %>%
      .[ , distance_to_past_opinions := mapply(function(a,b) {
        mean(
          sapply(a, function(x) {
            abs(x - b)
          })
        )
      }, a=past_opinions, b=assumption_sender)] %>%
      .[ , past_messages := as.character(past_messages) ] %>%
      .[ , past_opinions := as.character(past_opinions) ] %>%
      .[ , past_receiver_business := as.character(past_receiver_business) ] %>%
      .[ , past_sender_business := as.character(past_sender_business) ] %>% 
      .[ , past_nbh_incohesion := as.character(past_nbh_incohesion) ] %>%
      .[ , past_self_incohesion := as.character(past_self_incohesion) ] %>%
      unique() %>%
      .[ , past_messages := sapply(past_messages, function(x) list(eval(parse(text = x))))] %>%
      .[ , past_opinions := sapply(past_opinions, function(x) list(eval(parse(text = x))))] %>%
      .[ , past_receiver_business := sapply(past_receiver_business, function(x) list(eval(parse(text = x))))] %>%
      .[ , past_sender_business := sapply(past_sender_business, function(x) list(eval(parse(text = x))))] %>%
      .[ , past_nbh_incohesion := sapply(past_nbh_incohesion, function(x) list(eval(parse(text = x))))] %>%
      .[ , past_self_incohesion := sapply(past_self_incohesion, function(x) list(eval(parse(text = x))))] %>%
    .[ , .(distance_to_past_opinions, sender, past_self_incohesion, past_nbh_incohesion, sender_business, receiver_business, past_receiver_business, past_sender_business, nbh_incohesion, self_incohesion, max_send_energy_loss, max_receive_energy_loss)] %>%
      .[ , past_receiver_business := as.character(past_receiver_business) ] %>%
      .[ , past_sender_business := as.character(past_sender_business) ] %>% 
      .[ , past_nbh_incohesion := as.character(past_nbh_incohesion) ] %>%
      .[ , past_self_incohesion := as.character(past_self_incohesion) ] %>%
      unique() %>%
      .[ , past_receiver_business := sapply(past_receiver_business, function(x) list(eval(parse(text = x))))] %>%
      .[ , past_sender_business := sapply(past_sender_business, function(x) list(eval(parse(text = x))))] %>%
      .[ , past_nbh_incohesion := sapply(past_nbh_incohesion, function(x) list(eval(parse(text = x))))] %>%
      .[ , past_self_incohesion := sapply(past_self_incohesion, function(x) list(eval(parse(text = x))))] %>%
    .[ , .(distance_to_past_opinions, sender, past_self_incohesion, past_nbh_incohesion, sender_business, receiver_business, past_receiver_business, past_sender_business, nbh_incohesion, self_incohesion, max_send_energy_loss, max_receive_energy_loss)] %>%
    .[ , control := sum(distance_to_past_opinions) , by=sender ] %>%
    .[(sim$agent_characteristics[ , .(agent_id, energy)] %>% unique()), on=c("sender" = "agent_id") ] %>%
    .[ , rec_business_mean_index := mapply(function(x, y) {

      is_na <- sum(unlist(x)) / (y*length(unlist(x)))
      
      ifelse(is.na(is_na), 0, is_na)

    }, x=past_receiver_business, y=max_receive_energy_loss )] %>%
    .[ , send_business_mean_index := mapply(function(x, y) {

      is_na <- sum(unlist(x)) / (y*length(unlist(x)))
      
      ifelse(is.na(is_na), 0, is_na)

    }, x=past_sender_business, y=max_send_energy_loss )] %>%
    .[ , rec_business_mean := mapply(function(x) {

      series <- rev(unlist(x))
      for (i in 1:length(x)) {
	      series[i] <- series[i] / i
      }
      mean(series)

    }, x=past_receiver_business )] %>%
    .[ , send_business_mean := mapply(function(x) {

      series <- rev(unlist(x))
      for (i in 1:length(x)) {
	      series[i] <- series[i] / i
      }
      mean(series)

    }, x=past_sender_business )] %>%
    .[ , both_business_mean := mapply(function(x, y) {

      series_one <- rev(unlist(x))
      for (i in 1:length(x)) {
	      series_one[i] <- series_one[i] / i
      }

      series_two <- rev(unlist(y))
      for (i in 1:length(y)) {
	      series_two[i] <- series_two[i] / i
      }
	    
      sum( mean(series_one),  mean(series_two))

    }, x=past_sender_business, y=past_receiver_business )] %>%
    .[ , both_business_mean_index := mapply(function(x, y, z, a) {

      numerator <- sum( c(unlist(x), unlist(y)) )

      denominator <- z*length(unlist(y)) + a*length(unlist(x))

      is_na <- numerator / denominator
      
      ifelse(is.na(is_na), 0, is_na)

    }, x=past_sender_business, y=past_receiver_business, z = max_receive_energy_loss, a = max_send_energy_loss )] %>%
    .[sim$actions_overall[ , -c("best_action")], on=c("sender" = "agent_id"), allow.cartesian = TRUE] %>%
    setnames("sender", "agent_id") %>%
    # Concept of these utility functions
    # The first part in brackets computes the fraction of the maximum energy an agent can have the agent will have if it chooses that particular strategy; the more energy the agent will have, the better; this is diminished by the second part in brackets: the higher the past percent business for that strategy is, the worse off is the probable amount of energy
    # the third part in brackets represents the possible self incohesion; this is also diminished by a factor constructed in the same way as above
    # the fourth part in brackets represents the possible neighborhood incohesion; this is also diminished by a factor constructed in the same way as above
    # what sign the third and fourth part have depends on the strategy: Sending does help with neighborhood incohesion, but not self incohesion while Receiving helps with high self incohesion (as the agent doesn't say anything this round), but can't help with high neighborhood incohesion
    # Several Issues: 1. Negative Energy will be actually increased if reduced by the factor, 2. Both is suboptimal, because it will always be bigger; 2 might be solved by solving 1 though
    .[ actions == "Send" , energy_loss := ( energy - sender_business )] %>%
    .[ actions == "Receive" , energy_loss := ( energy - receiver_business )] %>%
    .[ actions == "Both" , energy_loss := ( energy - sender_business - receiver_business )] %>%
    .[ actions == "Send" , projected_energy := ifelse(energy_loss <= 0, 0, energy_loss) / ( energy + params(sim)$rc_energy_model$restoration_factor ) ] %>%
    .[ actions == "Receive" , projected_energy := ifelse(energy_loss <= 0, 0, energy_loss) / ( energy + params(sim)$rc_energy_model$restoration_factor ) ] %>%
    .[ actions == "Both" , projected_energy := ifelse(energy_loss <= 0, 0, energy_loss) / ( energy + params(sim)$rc_energy_model$restoration_factor ) ]  %>%
    .[ , projected_energy := ifelse(is.na(projected_energy), 0, projected_energy) ] %>%
    .[ , dissonance := mapply(function(x, y) {
	    
	    sum(x, y)
	   
    }, x=nbh_incohesion, y=self_incohesion)  ] %>%
    .[ actions == "Send", util_score :=
         ifelse(projected_energy <= 0, 0, projected_energy) + ( 1 - self_incohesion) ] %>%
    .[ actions == "Receive", util_score := 
         ifelse(projected_energy <= 0, 0, projected_energy) + ( 1 - nbh_incohesion ) ] %>%
    .[ actions == "Both", util_score :=  
         ifelse(projected_energy <= 0, 0, projected_energy) + control + ( ( self_incohesion + nbh_incohesion ) * ( 1 - abs( self_incohesion - nbh_incohesion) ) ) ] %>%
    .[ actions == "Nothing", util_score := ifelse( ( energy - sender_business <= 0 | energy - receiver_business <= 0) ,
	    max(util_score)+10000, min(util_score)-10000) ] %>%
    .[ , -c("past_self_incohesion", "past_nbh_incohesion", "past_receiver_business", "past_sender_business") ] %>%
    unique() %>%
    dcast(agent_id ~ actions, value.var = "util_score", fun.aggregate = sum) %>%
    .[ , agent_id := as.character(agent_id)] %>%
    .[, best_action :=  names( .[ , -c("agent_id")] )[ unlist(apply(.[ , -c("agent_id") ], 1, which.max)) ] ] %>%
    melt( id.vars = c("agent_id", "best_action"),
          measure.vars = c("Send", "Receive", "Both", "Nothing"),
          variable.name = "actions",
          value.name = "util_score" ) %>%
    .[ , agent_id := as.integer(agent_id) ]

  ###################################
  #### UPDATE sim$chosen_actions ####
  ###################################

  sim$chosen_actions <- copy(sim$actions_overall)[ actions == best_action , .(agent_id, best_action) ] %>%
    merge(sim$chosen_actions, by=c("agent_id"), all.x=TRUE, all.y=TRUE) %>%
    .[ is.na(action_type) , action_type := "actions_overall" ] %>%
    .[ action_type == "actions_overall" , best_action := ifelse(!is.na(best_action.x), best_action.x,  "NOT ASSIGNED")] %>% 
    .[ , -c("best_action.x", "best_action.y")]

  ##############################################################
  #### UPDATE sim$messages WITH ASSUMPTIONS ABOUT NEIGHBORS ####
  ##############################################################

  sim$messages <-  sim$discourse_memory[ , .(sender, message) ] %>%
    unique() %>%
    setnames("sender", "receiver") %>%
    merge(sim$messages, by = c("receiver") ) %>%
    setnames("message", "assumption_receiver") 

  ####################################
  #### PRODUCE OPTIMIZED MESSAGES ####
  ####################################

  sim$message_matrix <- outer(sim$messages$opinion_sender, sim$messages$assumption_receiver, produce_altered_message) 
  row.names(sim$message_matrix) <- sim$messages[, sender]
  colnames(sim$message_matrix) <- sim$messages[, receiver]

  sim$messages <- copy(sim$message_matrix) %>%
    data.table() %>%
    .[ , sender := as.numeric(row.names(sim$message_matrix))] %>%
    melt( id.vars = c("sender"),
          measure.vars = as.character(seq(1, sim$no_agents, 1)),
          variable.name = "receiver",
          value.name = "opt_message" ) %>%
    .[ receiver != sender] %>%
    setkey("sender") %>%
    unique() %>%
    .[ , sender := as.integer(sender)] %>%
    .[ , receiver := as.integer(receiver)] %>%
    .[copy(unique(sim$messages)), on=c("sender", "receiver"), nomatch = 0L, allow.cartesian = TRUE] %>%
    .[ , opt_message := median(opt_message), by = "sender" ] 
  
  #######################################################
  #### UPDATE sim$discourse_memory WITH NEW OPINIONS ####
  #######################################################

  sim$discourse_memory <- copy(sim$discourse_memory)[ , -c("opinion") ] %>%
    .[sim$agent_characteristics[ , .(agent_id, opinion) ], on=c("sender" = "agent_id")] %>%
    .[ , past_opinions := ifelse(lengths(past_opinions) < params(sim)$rc_energy_model$opinion_memory_depth,
                                 mapply(function(x, y) {
                                   list(c(unlist(x), y))
                                 }, x=past_opinions, y=opinion),
                                 mapply(function(x, y) {
                                   list(c(unlist(x)[1:params(sim)$rc_energy_model$opinion_memory_depth], y))
                                 }, x=past_opinions, y=opinion)
    )] %>%
    .[ , -c("assumption_receiver", "opt_message")]

  if( length(sim$actions_overall[ best_action %in% c("Both", "Send") , best_action ] > 0 ) ) {
   
    ###############################################################
    #### COMPUTE SENDING UTILITIES AND CHOOSE MAX UTIL ACTIONS ####
    ###############################################################

    sim$actions_send <- copy(sim$messages) %>%
      .[copy(sim$actions_overall), on = c("sender" = "agent_id"), nomatch = 0L, allow.cartesian = TRUE ] %>%
      .[ best_action == actions ] %>%
      .[ (best_action == "Both" | best_action == "Send") ] %>%
      .[ , .(sender, receiver , opt_message , opinion_sender , assumption_receiver)] %>%
      .[copy(sim$actions_send), on = c("sender" = "agent_id"), nomatch = 0L, allow.cartesian = TRUE ] %>%
      .[copy(sim$discourse_memory), on = c("sender"), nomatch = 0L, allow.cartesian = TRUE] %>%
      .[, distance_to_past_opinions := mapply(function(a,b,c,k) {
        switch(k,
               "Unoptimized" = {
                 mean(
                   sapply(a, function(x) {
                     abs(x - c)
                   })
                 )
               },
               "Optimized" = {
                 mean(
                   sapply(a, function(x) {
                     abs(x - b)
                   })
                 )
               }
        )
      }, a=past_opinions, b=opt_message, c=opinion_sender, k=actions)] %>%
      .[, distance_to_past_messages := mapply(function(a,b,c,k) {
        switch(k,
               "Unoptimized" = {
                 mean(
                   sapply(a, function(x) {
                     abs(x - c)
                   })
                 )
               },
               "Optimized" = {
                 mean(
                   sapply(a, function(x) {
                     abs(x - b)
                   })
                 )
               }
        )
      }, a=past_messages, b=opt_message, c=opinion_sender, k=actions)] %>%
      .[, distance_message_opinion := mapply(function(a,b,k) {
        switch(k,
               "Unoptimized" = {
                 abs(a - a)
               },
               "Optimized" = {
                 abs(a - b)
               }
        )
      }, a=opinion_sender, b=opt_message, k=actions)] %>%
      .[, distance_message_assumption := mapply(function(a,b,c,k) {
        switch(k,
               "Unoptimized" = {
                 abs(c - a)
               },
               "Optimized" = {
                 abs(c - b)
               }
        )
      }, a=opinion_sender, b=opt_message, c=assumption_receiver, k=actions)] %>%
      .[ , -c("past_opinions", "receiver")] %>%
      .[ , util_score := 0 - distance_to_past_messages - distance_to_past_opinions - distance_message_opinion - distance_message_assumption] %>% # include distance_to_past_messages in step function
      setnames("sender", "agent_id") %>%
      .[, .(agent_id, actions, util_score) ] %>%
      setkey("agent_id") %>%
      unique() %>%
      .[, util_score := sum(util_score), by=c("agent_id", "actions")] %>%
      unique() %>%
      dcast(agent_id ~ actions, value.var = "util_score") %>%
      .[, best_action :=  ifelse(Optimized > Unoptimized, "Optimized", "Unoptimized")] %>%
      melt( id.vars = c("agent_id", "best_action"),
            measure.vars = c("Optimized", "Unoptimized"),
            variable.name = "actions",
            value.name = "util_score" ) %>%
      .[ , agent_id := as.integer(agent_id) ]

    ###################################
    #### UPDATE sim$chosen_actions ####
    ###################################

    sim$chosen_actions <- copy(sim$actions_send)[ actions == best_action , .(agent_id, best_action) ] %>%
      merge(sim$chosen_actions, by="agent_id", all.x=TRUE , all.y=TRUE) %>% 
      .[ best_action.y == "Receive" , action_type := "actions_overall" ] %>%
      .[ best_action.y == "Nothing" , action_type := "actions_overall" ] %>%
      .[ action_type == "actions_send" , best_action := ifelse(!is.na(best_action.x), best_action.x, "NOT ASSIGNED")] %>%
      .[ action_type == "actions_overall" , best_action := ifelse(!is.na(best_action.y), best_action.y, "NOT ASSIGNED")] %>%
      .[ , -c("best_action.x", "best_action.y")]

    print(table(sim$chosen_actions$best_action))

    #####################################
    #### UPDATE sim$discourse_memory ####
    #####################################
print(nrow(sim$discourse_memory))
    sim$discourse_memory <- copy(sim$actions_send)[ , -c("util_score")] %>%
      .[copy(sim$messages), on = c("agent_id" = "sender"), nomatch = 0L, allow.cartesian = TRUE] %>%
      setnames("agent_id", "sender") %>%
      setkey("sender") %>%
      .[ actions==best_action ] %>%
      .[ , -c("actions")]  %>% 
      unique() %>% 
      .[ , sender_business := .N, by = "sender" ] %>% 
      .[ , receiver_business := .N, by = "receiver" ] %>% 
      .[ , -c("opinion_sender") ] %>%
      merge(sim$discourse_memory[ , -c("sender_business", "receiver_business", "assumption_receiver")], by=c("sender"), all.x=TRUE, all.y=TRUE) %>%
      .[ , sender_business := ifelse(is.na(sender_business), 0, sender_business) ] %>% 
      .[ , receiver_business := ifelse(is.na(receiver_business), 0, receiver_business) ] %>% 
      .[ , past_messages := ifelse( is.na(best_action),
            past_messages,
            ifelse(lengths(past_messages) < params(sim)$rc_energy_model$message_memory_depth,
                                               ifelse(.[ , best_action] == "Unoptimized",
                                          mapply(function(x, y) {
                                            list(c(unlist(x), y))
                                          }, x=past_messages, y=opinion),
                                          mapply(function(x, y) {
                                            list(c(unlist(x), y))
                                          }, x=past_messages, y=opt_message)),
                                   ifelse(.[ , best_action] == "Unoptimized",
                                          mapply(function(x, y) {
                                            list(c(unlist(x)[1:params(sim)$rc_energy_model$message_memory_depth], y))
                                          }, x=past_messages, y=opinion),
                                          mapply(function(x, y) {
                                            list(c(unlist(x)[1:params(sim)$rc_energy_model$message_memory_depth], y))
                                          }, x=past_messages, y=opt_message)
                                   )
                                 )
      )] %>%
      .[ , message := ifelse(is.na(opt_message), message, opt_message) ] %>%
      .[ , -c("opt_message") ] %>%
      .[ , .(sender, receiver, opinion, message, past_messages, past_opinions, sender_business, receiver_business, past_sender_business, past_receiver_business, past_nbh_incohesion, past_self_incohesion)] %>%
      # .[copy(sim$messages)[ , -c("opt_message", "assumption_receiver", "opinion_sender")], on = c("sender", "receiver"), allow.cartesian = TRUE] %>%
      .[ , past_messages := as.character(past_messages) ] %>%
      .[ , past_opinions := as.character(past_opinions) ] %>%
      .[ , past_receiver_business := as.character(past_receiver_business) ] %>%
      .[ , past_sender_business := as.character(past_sender_business) ] %>%
      unique() %>%
      .[ , past_messages := sapply(past_messages, function(x) list(eval(parse(text = x))))] %>% 
      .[ , past_opinions := sapply(past_opinions, function(x) list(eval(parse(text = x))))] %>% 
      .[ , past_receiver_business := sapply(past_receiver_business, function(x) list(eval(parse(text = x))))] %>% 
      .[ , past_sender_business := sapply(past_sender_business, function(x) list(eval(parse(text = x))))] 

    temp <- copy(sim$discourse_memory)[ , .(receiver, receiver_business)] %>% 
	    .[ !is.na(receiver) ] %>%
	    .[ , receiver_business := max(receiver_business), by=receiver] %>%
	    unique() 
   
    sim$discourse_memory <-  copy( sim$discourse_memory)[ , -c("receiver_business")]  %>%
      merge(temp, by.x = c("sender"), by.y = c("receiver"), all.x = TRUE, all.y = TRUE) %>% 
      .[ , -c("receiver") ] %>%
      .[ , nbh_incohesion := vec_get_nbh_incohesion(sender) ] %>%
      .[ , self_incohesion := vec_get_self_incohesion( sender ) ] %>%
      .[ , past_sender_business := ifelse(lengths(past_sender_business) < params(sim)$rc_energy_model$energy_params_memory_depth,
                                          mapply(function(x, y) {
                                            list(c(unlist(x), y))
                                          }, x=past_sender_business, y=sender_business),
                                          mapply(function(x, y) {
                                            list(c(unlist(x)[1:params(sim)$rc_energy_model$energy_params_memory_depth], y))
                                          }, x=past_sender_business, y=sender_business)
      )] %>%
      .[ , past_nbh_incohesion := ifelse(lengths(past_nbh_incohesion) < params(sim)$rc_energy_model$energy_params_memory_depth,
                                         mapply(function(x, y) {
                                           list(c(unlist(x), y))
                                         }, x=past_nbh_incohesion, y=nbh_incohesion),
                                         mapply(function(x, y) {
                                           list(c(unlist(x)[1:params(sim)$rc_energy_model$opinion_memory_depth], y))
                                         }, x=past_nbh_incohesion, y=nbh_incohesion)
      )] %>%
      .[ , past_self_incohesion := ifelse(lengths(past_self_incohesion) < params(sim)$rc_energy_model$energy_params_memory_depth,
                                          mapply(function(x, y) {
                                            list(c(unlist(x), y))
                                          }, x=past_self_incohesion, y=self_incohesion),
                                          mapply(function(x, y) {
                                            list(c(unlist(x)[1:params(sim)$rc_energy_model$opinion_memory_depth], y))
                                          }, x=past_self_incohesion, y=self_incohesion)
      )] %>%
      .[ , past_messages := as.character(past_messages) ] %>%
      .[ , past_opinions := as.character(past_opinions) ] %>%
      .[ , past_receiver_business := as.character(past_receiver_business) ] %>%
      .[ , past_sender_business := as.character(past_sender_business) ] %>%
      .[ , past_nbh_incohesion := as.character(past_nbh_incohesion) ] %>%
      .[ , past_self_incohesion := as.character(past_self_incohesion) ] %>%
      unique() %>%
      .[ , past_messages := sapply(past_messages, function(x) list(eval(parse(text = x))))] %>%
      .[ , past_opinions := sapply(past_opinions, function(x) list(eval(parse(text = x))))] %>%
      .[ , past_receiver_business := sapply(past_receiver_business, function(x) list(eval(parse(text = x))))] %>%
      .[ , past_sender_business := sapply(past_sender_business, function(x) list(eval(parse(text = x))))] %>%
      .[ , past_nbh_incohesion := sapply(past_nbh_incohesion, function(x) list(eval(parse(text = x))))] %>%
      .[ , past_self_incohesion := sapply(past_self_incohesion, function(x) list(eval(parse(text = x))))]

    sim$messages <- copy(sim$messages) %>%
      unique() %>%
      setnames(old = c("sender", "receiver"), new = c("receiver", "sender")) %>%
      .[ , .(receiver, opinion_sender, opt_message)] %>%
      .[ , opinion_receiver := opinion_sender] %>%
      .[ , -c("opinion_sender") ] %>%
      .[sim$messages[ , -c("opt_message") ], on="receiver", nomatch=0L, allow.cartesian=TRUE] # WORKS

    sim$messages <- copy(sim$messages) %>%
      .[sim$actions_send[ , .(agent_id, actions, best_action)], nomatch=0L, on=c("sender" = "agent_id"), allow.cartesian=TRUE] %>%
      .[ , assumption_sender := ifelse(.[, best_action] == "Unoptimized", opinion_sender, opt_message)] %>%
      .[ , -c("actions", "best_action")] %>%
      setkey("sender") %>%
      unique()


    # Receiving

if (nrow(sim$actions_overall[ (best_action == "Both" | best_action =="Receive") ]) > 0) {

	print("normal path activates")

    sim$opinion_updating <- copy(sim$messages)[ , -c("actions", "best_action")] %>%
      unique() %>%
      .[copy(actions_overall), on = c("receiver" = "agent_id"), nomatch = 0L, allow.cartesian = TRUE ] %>%
      .[ best_action == actions ] %>% 
      .[ (best_action == "Both" | best_action == "Receive") ] %>%
      .[ , .(sender, receiver, opinion_receiver, assumption_sender, opt_message, actions, best_action)] %>%
      .[copy(sim$discourse_memory)[ , .(sender, past_opinions)] %>% .[ , past_opinions := as.character(past_opinions) ] %>% unique %>% .[ , past_opinions := sapply(past_opinions, function(x) list(eval(parse(text = x))))], on=c("receiver"="sender"), nomatch = 0L, allow.cartesian = TRUE] %>%
      .[copy(sim$discourse_memory)[ , .(sender, past_messages)] %>% .[ , past_messages := as.character(past_messages) ] %>% unique %>% .[ , past_messages := sapply(past_messages, function(x) list(eval(parse(text = x))))], on=c("sender"), nomatch = 0L, allow.cartesian = TRUE] %>%
      .[ best_action == actions ] %>%
      .[ , distance_to_past_opinions := mapply(function(a,b) {
        mean(
          sapply(a, function(x) {
            abs(x - b)
          })
        )
      }, a=past_opinions, b=assumption_sender)] %>%
       .[ , distance_to_past_messages := mapply(function(a,b) {
	     max(
		sapply(a, function(x) {
		      abs(x - b)
		   })
	   )
      }, a=past_messages, b=assumption_sender)] %>%
      .[ , trust := distance_to_past_messages < params(sim)$rc_energy_model$other_incons_tolerance] %>%
      .[ , within_epsilon := abs(opinion_receiver - assumption_sender) < params(sim)$rc_energy_model$epsilon] %>%
      .[ , self_consistent := distance_to_past_opinions < params(sim)$rc_energy_model$self_incons_tolerance] %>%
      .[ within_epsilon == TRUE & self_consistent == TRUE & trust == TRUE] %>% 
      .[ , sum_assumptions := sum(assumption_sender), by=receiver] %>%
      .[ , denominator := .N, by=receiver ] %>%
      .[ , reception_energy_loss := .N, by=receiver ] %>%
      .[ , opinion_receiver_new := ifelse( denominator != 0, sum_assumptions / denominator, opinion_receiver )] %>%
      setnames("receiver", "agent_id") %>%
      .[ , .(agent_id, opinion_receiver_new)] %>%
      setkey("agent_id") %>%
      unique() 

    if( length(sim$actions_overall[ best_action %in% c("Nothing") , best_action ]) > 0  ) {

print("nothing within normal activates")

	  sim$opinion_updating <- copy(sim$actions_overall) %>%
	    .[ best_action == actions ] %>%
	    .[ best_action == "Nothing" ] %>%
            .[sim$discourse_memory, on=c("agent_id"="sender"), nomatch = 0L, allow.cartesian = TRUE] %>%
            .[ best_action == actions ] %>% 
	    .[ , opinion_receiver_new := unlist(sapply(past_opinions, function(x) { median(x) })) ] %>%
	    .[ , .(agent_id, opinion_receiver_new)] %>%
	    setkey("agent_id") %>%
	    unique() %>%
	    rbind(sim$opinion_updating)



    }

    sim$agent_characteristics <- merge(sim$agent_characteristics, sim$opinion_updating, by="agent_id", all.x = TRUE) %>% 
      .[ , opinion := ifelse(is.na(opinion_receiver_new), opinion, opinion_receiver_new)] %>%
      .[ , -c("opinion_receiver_new")]

    sim$agent_characteristics$energy[is.na(sim$agent_characteristics$energy)] <- 0

    sim$agent_characteristics <- copy(sim$actions_overall)[ , best_axn_overall := best_action][ , .(agent_id, best_axn_overall)] %>%
      unique() %>%
      merge(copy(sim$actions_send)[ , best_axn_send := best_action][ , .(agent_id, best_axn_send)], by="agent_id", allow.cartesian = TRUE) %>%
      unique() %>%
      merge(copy(sim$discourse_memory)[ , .(sender, receiver_business, sender_business)], by.x = "agent_id", by.y = "sender", all = TRUE, allow.cartesian = TRUE) %>%
      unique() %>%
      .[ best_axn_overall == "Send" , energy_loss := sender_business ] %>%
      .[ best_axn_overall == "Receive", energy_loss := receiver_business ] %>%
      .[ best_axn_overall == "Both", energy_loss := sender_business + receiver_business ] %>%
      .[ best_axn_overall == "Nothing", energy_loss := 0 ] %>%
      .[ best_axn_send == "Unoptimized", energy_loss := energy_loss ] %>%
      .[ best_axn_send == "Optimized", energy_loss := energy_loss + sender_business ] %>% 
      .[ , .(agent_id, energy_loss) ] %>%
      unique() %>% 
      .[ , energy_loss := sum(energy_loss), by="agent_id" ] %>% 
      unique() %>%
      .[sim$agent_characteristics, on="agent_id"] %>% 
      .[ , energy_loss := ifelse(is.na(energy_loss), 0, energy_loss) ] %>%
      .[ , energy := energy - energy_loss + params(sim)$rc_energy_model$restoration_factor ] %>%
      .[ , -c("energy_loss") ] # works fine until here

    sim$agent_characteristics$energy[is.na(sim$agent_characteristics$energy)] <- 0

    sim$discourse_memory <- copy(sim$messages) %>%
      unique() %>%
      .[ , receiver_business := .N, by="receiver" ] %>%
      .[ , -c("sender") ] %>%
      setnames(old=c("receiver","receiver_business"), new=c( "sender", "receiver_business_y")) %>%
      .[ , .(sender, receiver_business_y) ] %>%
      unique() %>%
      .[copy(sim$discourse_memory), on=c("sender")] %>%
      .[ , receiver_business := ifelse(is.na(receiver_business_y), receiver_business, receiver_business_y) ] %>%
      .[ , past_receiver_business := ifelse(lengths(past_receiver_business) < params(sim)$rc_energy_model$energy_params_memory_depth,
                                            mapply(function(x, y) {
                                              list(c(unlist(x), y))
                                            }, x=past_receiver_business, y=receiver_business),
                                            mapply(function(x, y) {
                                              list(c(unlist(x)[1:params(sim)$rc_energy_model$energy_params_memory_depth], y))
                                            }, x=past_receiver_business, y=receiver_business)
      )] 

    sim$discourse_memory <- sim$agent_characteristics[ , .(agent_id, opinion) ] %>%
      .[sim$discourse_memory[ , -c("opinion")], on=c("agent_id" = "sender") ] %>%
      setnames("agent_id", "sender") 

      } else {

	      print("send but not receive activates")

    sim$discourse_memory <- copy(sim$messages) %>%
      .[ , receiver_business := .N, by=receiver ] %>%
      .[ , -c("sender") ] %>%
      setnames("receiver", "sender") %>%
      .[ , .(sender, receiver_business) ] %>%
      unique() %>%
      .[copy(sim$discourse_memory), on=c("sender")] %>%
      .[ , past_receiver_business := ifelse(lengths(past_receiver_business) < params(sim)$rc_energy_model$energy_params_memory_depth,
                                            mapply(function(x, y) {
                                              list(c(unlist(x), y))
                                            }, x=past_receiver_business, y=receiver_business),
                                            mapply(function(x, y) {
                                              list(c(unlist(x)[1:params(sim)$rc_energy_model$energy_params_memory_depth], y))
                                            }, x=past_receiver_business, y=receiver_business)
      )] 


	  sim$opinion_updating <- copy(sim$actions_overall) %>%
	    .[ best_action == actions ] %>%
	    .[ (best_action == "Nothing" | best_action == "Send") ] %>%
            .[copy(sim$agent_characteristics), on=c("agent_id"), nomatch = 0L, allow.cartesian = TRUE] %>%
            .[ best_action == actions ] %>% 
	    .[ , opinion_receiver_new := opinion ] %>%
	    .[ , .(agent_id, opinion_receiver_new)] %>%
	    setkey("agent_id") %>%
	    unique() 

   sim$agent_characteristics <- merge(sim$agent_characteristics, sim$opinion_updating, by="agent_id", all.x = TRUE) %>%
      .[ , opinion := ifelse(is.na(opinion_receiver_new), opinion, opinion_receiver_new)] %>%
      .[ , -c("opinion_receiver_new")]

    sim$agent_characteristics <- copy(sim$actions_overall)[ , best_axn_overall := best_action][ , .(agent_id, best_axn_overall)] %>%
      unique() %>%
      merge(copy(sim$actions_send)[ , best_axn_send := best_action][ , .(agent_id, best_axn_send)], by="agent_id", allow.cartesian = TRUE) %>%
      unique() %>%
      merge(copy(sim$discourse_memory)[ , .(sender, receiver_business, sender_business)], by.x = "agent_id", by.y = "sender", all = TRUE, allow.cartesian = TRUE) %>%
      unique() %>%
      .[ best_axn_overall == "Send" , energy_loss := sender_business ] %>%
      .[ best_axn_overall == "Receive", energy_loss := receiver_business ] %>%
      .[ best_axn_overall == "Both", energy_loss := sender_business + receiver_business ] %>%
      .[ best_axn_overall == "Nothing", energy_loss := 0 ] %>%
      .[ best_axn_send == "Unoptimized", energy_loss := energy_loss ] %>%
      .[ best_axn_send == "Optimized", energy_loss := energy_loss + sender_business ] %>% 
      .[ , .(agent_id, energy_loss) ] %>%
      unique() %>% 
      .[ , energy_loss := sum(energy_loss), by="agent_id" ] %>% 
      unique() %>%
      .[sim$agent_characteristics, on="agent_id"] %>% 
      .[ , energy_loss := ifelse(is.na(energy_loss), 0, energy_loss) ] %>%
      .[ , energy := energy - energy_loss + params(sim)$rc_energy_model$restoration_factor ] %>%
      .[ , -c("energy_loss") ]

      }
    
  } else {

print("noone does anything activates")

    sim$discourse_memory <- copy(sim$discourse_memory) %>%
      .[ , receiver_business := 0 ] %>%
      .[ , sender_business := 0 ] %>%
      .[ , past_receiver_business := ifelse(lengths(past_receiver_business) < params(sim)$rc_energy_model$energy_params_memory_depth,
                                            mapply(function(x, y) {
                                              list(c(unlist(x), y))
                                            }, x=past_receiver_business, y=receiver_business),
                                            mapply(function(x, y) {
                                              list(c(unlist(x)[1:params(sim)$rc_energy_model$energy_params_memory_depth], y))
                                            }, x=past_receiver_business, y=receiver_business)
      )] %>%
      .[ , past_sender_business := ifelse(lengths(past_sender_business) < params(sim)$rc_energy_model$energy_params_memory_depth,
                                          mapply(function(x, y) {
                                            list(c(unlist(x), y))
                                          }, x=past_sender_business, y=sender_business),
                                          mapply(function(x, y) {
                                            list(c(unlist(x)[1:params(sim)$rc_energy_model$energy_params_memory_depth], y))
                                          }, x=past_sender_business, y=sender_business)
      )]

sim$opinion_updating <- copy(sim$discourse_memory) %>%
    .[ , opinion_receiver_new := unlist(sapply(past_opinions, function(x) { median(x) })) ] %>%
    setnames("sender", "agent_id") %>%
    .[ , .(agent_id, opinion_receiver_new)] %>%
    setkey("agent_id") %>%
    unique()

   sim$agent_characteristics <- merge(sim$agent_characteristics, sim$opinion_updating, by="agent_id", all.x = TRUE) %>% 
      .[ , opinion := ifelse(is.na(opinion_receiver_new), opinion, opinion_receiver_new)] %>%
      .[ , -c("opinion_receiver_new")]

    sim$agent_characteristics <- copy(sim$actions_overall)[ , best_axn_overall := best_action][ , .(agent_id, best_axn_overall)] %>%
      unique() %>%
      merge(copy(sim$discourse_memory)[ , .(sender, receiver_business, sender_business)], by.x = "agent_id", by.y = "sender", all = TRUE, allow.cartesian = TRUE) %>%
      unique() %>%
      .[ best_axn_overall == "Send" , energy_loss := sender_business ] %>%
      .[ best_axn_overall == "Receive", energy_loss := receiver_business ] %>%
      .[ best_axn_overall == "Both", energy_loss := sender_business + receiver_business ] %>%
      .[ best_axn_overall == "Nothing", energy_loss := 0 ] %>%
      .[ , .(agent_id, energy_loss) ] %>%
      unique() %>% 
      .[ , energy_loss := sum(energy_loss), by="agent_id" ] %>% 
      unique() %>%
      .[sim$agent_characteristics, on="agent_id"] %>% 
      .[ , energy_loss := ifelse(is.na(energy_loss), 0, energy_loss) ] %>%
      .[ , energy := energy - energy_loss + params(sim)$rc_energy_model$restoration_factor ] %>%
      .[ , -c("energy_loss") ]
  }

  return(invisible(sim))

}

# FUNCTIONS


### produce_altered_message

produce_altered_message <- function(opinion_send, message_receive) {

  # produce altered message without epsilon bound
  altered_message <- ifelse(opinion_send < message_receive,
                            opinion_send + abs(opinion_send - message_receive) / 2,
                            opinion_send - abs(opinion_send - message_receive) / 2)

  return(altered_message)

}

### get_nbh_incohesion

get_nbh_incohesion <- function( id ) {

  nbh_indices <- sim$agent_characteristics[ agent_id == id , neighborhood ] %>% unlist()

  nbh_assumptions <- sim$messages[ , .(sender, receiver, assumption_receiver) ] %>%
    .[ sender == id & receiver %in% nbh_indices , assumption_receiver ]

  mean_deviations <- outer(nbh_assumptions, nbh_assumptions, "-") %>%
    abs() %>%
    as.vector() %>%
    sum() %>%
    sapply( "/", ((length(nbh_assumptions)*length(nbh_assumptions)) - length(nbh_assumptions)) ) # mean not appropriate, 0 values of diagonal factor in denominator

  return(mean_deviations)

}

vec_get_nbh_incohesion <- Vectorize(get_nbh_incohesion)

### get_self_incohesion

get_self_incohesion <- function( id ) {

  past <- sim$discourse_memory[ sender == id, past_opinions ] %>% unlist()

  mean_deviations <- outer(past, past, "-") %>%
    abs() %>%
    as.vector() %>%
    sum() %>%
    sapply( "/", ((length(past)*length(past)) - length(past)) )

  return(mean_deviations)

}

vec_get_self_incohesion <- Vectorize(get_self_incohesion)

### reverse_sign

reverse_sign <- function ( x ) {

  if (x < 0) {
    return(-x)
  }
  if (x > 0) {
    return(x)
  }

}

### vectorized minimum

vec_min <- Vectorize(min)

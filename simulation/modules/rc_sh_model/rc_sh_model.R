### Rational Choice Systematic Heuristic Model

# for simplicity and comparability, epsilon is taken to be a constant
# however, it is taken as a variable here, that could take input in the future
# if this model is sourced together with such a modification

# we will have an adjacency matrix

# the parameters are set only with default values
stopifnot(packageVersion("SpaDES") >= "1.2.0.9006")

defineModule(sim, list(
  name = "rc_sh_model",
  description = "Simulate bounded confidence opinion dynamics model in Rational Choice framework with utility computation respecting energy levels of agents.",
  keywords = c("opinion dynamics", "hegselmann krause", "rational choice"),
  childModules = character(),
  authors = c(person(c("Malte", "Lars"), "Heckelen", email = "malte.heckelen@ilw.uni-stuttgart.de", role = c("aut", "cre"))),
  version = numeric_version("0.0.1"),
  spatialExtent = raster::extent(rep(NA_real_, 4)),
  timeframe = as.POSIXlt(c(NA, NA)),
  timeunit = "hour",
  citation = list("citation.bib"),
  documentation = list("README.txt", "rc_sh_model.Rmd"),
  reqdPkgs = list("tidyverse", "data.table", "logitnorm"),
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
    defineParameter("no_groups", "numeric", 1, NA, NA, "The number social groups."),
    defineParameter("expert_percentage", "numeric", 0, NA, NA, "The percentage of declared experts."),
    defineParameter("sigma_complexity", "numeric", 1, NA, NA, "The percentage of declared experts."),
    defineParameter("initial_opinion_confidence", "numeric", 1, NA, NA, "The standard deviation of past opinions (centered around the current opinion)."),
    defineParameter("argumentation_memory_depth", "numeric", 1, NA, NA, "The number of time steps agents remember argumentations of various complexities for.")
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


doEvent.rc_sh_model <- function(sim, eventTime, eventType, debug = FALSE) {

  switch(
    eventType,
    init = {
      ## do stuff for this event
      sim <- rc_sh_modelInit(sim)

      ## schedule future event(s)
      sim <- scheduleEvent(sim, eventTime = start(sim)+1, moduleName = "rc_sh_model", eventType = "step")
    },
    step = {
      ## do stuff for this event
      sim <- rc_sh_modelStep(sim)

      ## schedule future event(s)
      sim <- scheduleEvent(sim, eventTime = time(sim)+1, moduleName = "rc_sh_model", eventType = "step")
    },
    warning(paste("Undefined event type: '", current(sim)[1, "eventType", with = FALSE],
                  "' in module '", current(sim)[1, "moduleName", with = FALSE], "'", sep = ""))
  )

  return(invisible(sim))

}

rc_sh_modelInit <- function(sim) {

  print(time(sim))
  
  ############################################
  #### MODIFY AGENT_CHARACTERISTICS TABLE ####
  ############################################

  sim$agent_characteristics <- sim$agent_characteristics %>%
    data.table() %>%
    .[ , energy := params(sim)$rc_sh_model$energy_level] %>%
    .[ , group := sample(c(1:params(sim)$rc_sh_model$no_groups), size=sim$no_agents, replace=TRUE) ]  %>%
    .[ , expert := sample(c(0,1), size=params(sim)$basic_setup$no_agents, replace=TRUE, prob=c((1-params(sim)$rc_sh_model$expert_percentage), params(sim)$rc_sh_model$expert_percentage)) ] %>%
    .[ , op_compl := rlogitnorm(0, params(sim)$rc_sh_model$sigma_complexity, params(sim)$basic_setup$no_agents) ]
  
  ######################################## 
  #### CONSTRUCT CHOSEN_ACTIONS TABLE ####
  ########################################

  sim$chosen_actions <- tibble(

    agent_id = rep(agent_characteristics$agent_id, each=3),
    action_type = rep(c("actions_overall", "actions_send", "actions_receive"), sim$no_agents),
    best_action = rep(c("Not assigned"), length(action_type))

  ) %>% data.table() %>%
    .[ , agent_id := as.integer(agent_id) ]
  
  ######################################### 
  #### CONSTRUCT OVERALL_ACTIONS TABLE ####
  #########################################

  sim$actions_overall <- tibble(

    agent_id = rep(agent_characteristics$agent_id, each=4),
    actions = rep(c("Send", "Receive", "Both", "Nothing"), sim$no_agents),
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
  
  ########################################### 
  #### CONSTRUCT RECEIVING_ACTIONS TABLE ####
  ###########################################

  sim$actions_receive <- tibble(

    agent_id = rep(sim$agent_characteristics$agent_id, each=2),
    actions = rep(c("Systematic", "Heuristic"), sim$no_agents),
    util_score = rep(0, length(actions))

  ) %>%
  data.table()

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

    agent_id = rep(agent_characteristics$agent_id, each=4),
    actions = rep(c("Unoptimized", "Optimized", "Unoptimized_appeal", "Optimized_appeal"), sim$no_agents),
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
    .[ receiver != sender ] %>%
    setkey("sender") %>%
    unique() %>%
    .[ , sender := as.integer(sender)] %>%
    .[ , receiver := as.integer(receiver)] %>%
    .[copy(unique(sim$messages)), on=c("sender", "receiver"), nomatch = 0L, allow.cartesian = TRUE] %>%
    .[ , opt_message := median(opt_message), by = .(sender)] %>%
    merge(copy(sim$agent_characteristics)[ , .(agent_id, op_compl) ], by.x=c("sender"), by.y=c("agent_id"), allow.cartesian = TRUE) %>% # at this point every agent can only argue for optimized messages with the complexity of his own internal argumentation for his own opinion 
    .[ , msg_compl := op_compl ]

  ##########################################
  #### CONSTRUCT DISCOURSE MEMORY TABLE ####
  ##########################################

  sim$discourse_memory <- copy(sim$messages)[ , -c("receiver", "assumption_receiver") ] %>%
    unique() %>%
    .[ , past_opinions := sapply(opinion_sender, function(x) {list(
		    ifelse(rnorm(params(sim)$rc_sh_model$opinion_memory_depth, x, params(sim)$rc_sh_model$initial_opinion_confidence) > 1, 1,
			    ifelse(rnorm(params(sim)$rc_sh_model$opinion_memory_depth, x, params(sim)$rc_sh_model$initial_opinion_confidence) < 0, 0, rnorm(params(sim)$rc_sh_model$opinion_memory_depth, x, params(sim)$rc_sh_model$initial_opinion_confidence) ) ) )  } )] %>%
    setnames("opinion_sender", "opinion") %>%
    .[ , .(sender, opinion, past_opinions, op_compl)] %>%
    .[ , past_op_compls := sapply(op_compl, function(x) {list(x)} ) ] %>%
    .[ , past_opinions := as.character(past_opinions) ] %>% # unique() can't handle list columns, so first transform to character
    .[ , past_op_compls := as.character(past_op_compls) ] %>% # unique() can't handle list columns, so first transform to character
    unique() %>%
    .[ , past_opinions := sapply(past_opinions, function(x) list(eval(parse(text = x))))] %>% # then transform back
    .[ , past_op_compls := sapply(past_op_compls, function(x) list(eval(parse(text = x))))] %>% # then transform back
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
      .[, involvement := sapply( past_op_compls, function(x) mean(x) )  ] %>%
      .[, msg_compl := mapply(function(a, b, k) {
        switch(k,
               "Unoptimized" = {
                 b + ifelse(mean(c(unlist(a)) - b) > 0, mean(c(unlist(a)) - b), 0) # new opinion can be argued for with the mean deviation complexity of own opinion complexity 
	       },
               "Optimized" = {
                 b + ifelse(mean(c(unlist(a)) - b) > 0, mean(c(unlist(a)) - b), 0) 
	       },
               "Unoptimized_appeal" = {
                 0
	       },
               "Optimized_appeal" = {
                 0 
	       }
        )
      }, a=past_op_compls, b=op_compl, k=actions)] %>% # here, later also past_msg_compls
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
               },
               "Unoptimized_appeal" = {
                 mean(
                   sapply(a, function(x) {
                     abs(x - c)
                   })
                 )
               },
               "Optimized_appeal" = {
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
               },
               "Unoptimized_appeal" = {
                 abs(a - a)
               },
               "Optimized_appeal" = {
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
               },
               "Unoptimized_appeal" = {
                 abs(c - a)
               },
               "Optimized_appeal" = {
                 abs(c - b)
               }
        )
      }, a=opinion_sender, b=opt_message, c=assumption_receiver, k=actions)] %>%
      .[ , -c("past_opinions", "receiver")] %>% # include past_messages here in the step function
      .[ actions == "Unoptimized" , util_score := 0 - distance_to_past_opinions - distance_message_opinion - distance_message_assumption - msg_compl + involvement] %>% 
      .[ actions == "Unoptimized_appeal", util_score := 0 - distance_to_past_opinions - distance_message_opinion - distance_message_assumption + msg_compl - involvement] %>% 
      .[ actions == "Optimized", util_score := 0 - distance_to_past_opinions - distance_message_opinion - distance_message_assumption - msg_compl + involvement] %>% 
      .[ actions == "Optimized_appeal", util_score := 0 - distance_to_past_opinions - distance_message_opinion - distance_message_assumption + msg_compl - involvement] %>% 
      .[ , agent_id := sender ] %>%
      .[ , .(agent_id, actions, util_score)] %>%
      setkey("agent_id") %>%
      unique() %>%
      .[, util_score := sum(util_score), by=c("agent_id", "actions")] %>%
      unique() %>%
      dcast(agent_id ~ actions, value.var = "util_score") %>%
      .[, best_action :=  names(.[ , -c("agent_id")])[apply(.[ , -c("agent_id") ], 1, which.max)]] %>%
      melt( id.vars = c("agent_id", "best_action"),
            measure.vars = c("Optimized", "Unoptimized", "Unoptimized_appeal", "Optimized_appeal"),
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
      .[ , sender_business := .N , by=sender ] %>%
      .[ , sender_business := ifelse(is.na(sender_business), 0, sender_business) ] %>% # if not a sender, 0 sender business
      .[ , receiver_business := .N, by = receiver ] %>% # generate business
      merge(sim$discourse_memory[ , -c("sender_business", "receiver_business", "assumption_receiver", "op_compl")], by=c("sender"), all.x=TRUE) %>%
      .[ , sender_business := mapply(function(a, b, c, k) {
        switch(k,
               "Unoptimized" = {
                 c + b + ifelse(mean(c(unlist(a)) - b) > 0, mean(c(unlist(a)) - b), 0) # number of sent messages plus complexity of that message
	       },
               "Optimized" = {
                 c + b + ifelse(mean(c(unlist(a)) - b) > 0, mean(c(unlist(a)) - b), 0) 
	       },
               "Unoptimized_appeal" = {
                c 
	       },
               "Optimized_appeal" = {
                 c 
	       }
        )
      }, a=past_op_compls, b=op_compl, c= sender_business, k=best_action)] %>% # here, later also past_msg_compls
      .[ , sender_business := ifelse(is.na(sender_business), 0, sender_business) ] %>% 
      .[ , receiver_business := ifelse(is.na(receiver_business), 0, receiver_business) ] %>%
      .[ , past_messages := mapply(function(x,y,z) {

	      ifelse(x == "Unoptimized",
		      y,
		      z)

				   }, x=best_action, y=opinion_sender, z=opt_message) ] %>%
      .[ , past_msg_compls := sapply(op_compl, function(x) { list(x) } ) ] %>%
      .[ , past_opinions := ifelse(lengths(past_opinions) < params(sim)$rc_sh_model$opinion_memory_depth,
                                   mapply(function(x, y) {
                                     list(c(unlist(x), y))
                                   }, x=past_opinions, y=opinion_sender),
                                   mapply(function(x, y) {
                                     list(c(unlist(x)[1:params(sim)$rc_sh_model$opinion_memory_depth], y))
                                   }, x=past_opinions, y=opinion_sender)
      )] %>%
      setnames("opinion_sender", "opinion") %>%
      setnames("opt_message", "message") %>%
      .[ , .(sender, receiver, opinion, message, op_compl, msg_compl, past_messages, past_opinions, sender_business, receiver_business, past_msg_compls, past_op_compls)] %>%
      .[ , past_messages := as.character(past_messages) ] %>%
      .[ , past_opinions := as.character(past_opinions) ] %>%
      .[ , past_msg_compls := as.character(past_msg_compls) ] %>%
      .[ , past_op_compls := as.character(past_op_compls) ] %>%
      unique() %>%
      .[ , past_messages := sapply(past_messages, function(x) list(eval(parse(text = x))))] %>%
      .[ , past_opinions := sapply(past_opinions, function(x) list(eval(parse(text = x))))] %>%
      .[ , past_msg_compls := sapply(past_msg_compls, function(x) list(eval(parse(text = x))))] %>%
      .[ , past_op_compls := sapply(past_op_compls, function(x) list(eval(parse(text = x))))]

    temp <- copy(sim$discourse_memory)[ , .(receiver, receiver_business)] %>%
      .[ , receiver_business := max(receiver_business), by=receiver] %>%
      unique()

    sim$discourse_memory <- copy( sim$discourse_memory)[ , -c("receiver_business")]  %>%
      merge(temp, by.x = c("sender"), by.y = c("receiver"), all.x = TRUE) %>% 
      .[ , -c("receiver") ] %>%
      .[ , past_receiver_business := ifelse( !is.na(receiver_business), sapply(receiver_business, function(x) list(x) ), 0 ) ] %>%
      .[ , past_sender_business := ifelse( !is.na(sender_business), sapply(sender_business, function(x) list(x) ), 0 ) ] %>%
      .[ , nbh_incohesion := vec_get_nbh_incohesion(sender) ] %>%
      .[ , past_nbh_incohesion := ifelse( !is.na(nbh_incohesion), sapply(nbh_incohesion, function(x) list(x) ), 0 ) ] %>%
      .[ , self_incohesion := vec_get_self_incohesion( sender ) ] %>%
      .[ , past_self_incohesion := ifelse( !is.na(self_incohesion), sapply(self_incohesion, function(x) list(x) ), 0 ) ] %>%
      .[ , past_messages := as.character(past_messages) ] %>%
      .[ , past_opinions := as.character(past_opinions) ] %>%
      .[ , past_receiver_business := as.character(past_receiver_business) ] %>%
      .[ , past_sender_business := as.character(past_sender_business) ] %>% 
      .[ , past_nbh_incohesion := as.character(past_nbh_incohesion) ] %>%
      .[ , past_self_incohesion := as.character(past_self_incohesion) ] %>%
      .[ , past_msg_compls := as.character(past_msg_compls) ] %>%
      .[ , past_op_compls := as.character(past_op_compls) ] %>%
      unique() %>%
      .[ , past_messages := sapply(past_messages, function(x) list(eval(parse(text = x))))] %>%
      .[ , past_opinions := sapply(past_opinions, function(x) list(eval(parse(text = x))))] %>%
      .[ , past_receiver_business := sapply(past_receiver_business, function(x) list(eval(parse(text = x))))] %>%
      .[ , past_sender_business := sapply(past_sender_business, function(x) list(eval(parse(text = x))))] %>%
      .[ , past_nbh_incohesion := sapply(past_nbh_incohesion, function(x) list(eval(parse(text = x))))] %>%
      .[ , past_self_incohesion := sapply(past_self_incohesion, function(x) list(eval(parse(text = x))))] %>%
      .[ , past_msg_compls := sapply(past_msg_compls, function(x) list(eval(parse(text = x))))] %>%
      .[ , past_op_compls := sapply(past_op_compls, function(x) list(eval(parse(text = x))))]
    
    print("no of rows discmem")
    print(nrow(sim$discourse_memory))
    #########################
    #### "SEND" MESSAGES ####
    #########################

    sim$messages <- copy(sim$messages) %>%
      unique() %>%
      setnames(old = c("sender", "receiver"), new = c("receiver", "sender")) %>%
      .[ , .(receiver, opinion_sender, op_compl)] %>%
      setnames(old=c("opinion_sender", "op_compl"), new=c("opinion_receiver", "op_compl_receiver")) %>%
      .[sim$messages, on="receiver", nomatch=0L, allow.cartesian=TRUE] 

    sim$messages <- copy(sim$messages) %>%
      .[sim$actions_send[ best_action == actions , .(agent_id, actions, best_action)], nomatch=0L, on=c("sender" = "agent_id"), allow.cartesian=TRUE] %>%
      unique() %>%
      merge(sim$discourse_memory[ , .(sender, past_msg_compls, past_op_compls)], nomatch=0L, by=c("sender"), allow.cartesian=TRUE) %>% 
      .[ , past_msg_compls := as.character(past_msg_compls) ] %>%
      .[ , past_op_compls := as.character(past_op_compls) ] %>%
      unique() %>% 
      .[ , past_msg_compls := sapply(past_msg_compls, function(x) list(eval(parse(text = x))))] %>%
      .[ , past_op_compls := sapply(past_op_compls, function(x) list(eval(parse(text = x))))] %>%
      .[, msg_compl := mapply(function(a, b, k) {
        switch(k,
               "Unoptimized" = {
                 b + ifelse(mean(c(unlist(a)) - b) > 0, mean(c(unlist(a)) - b), 0) # new opinion can be argued for with the mean deviation complexity of own opinion complexity 
	       },
               "Optimized" = {
                 b + ifelse(mean(c(unlist(a)) - b) > 0, mean(c(unlist(a)) - b), 0) 
	       },
               "Unoptimized_appeal" = {
                 0
	       },
               "Optimized_appeal" = {
                 0 
	       }
        )
      }, a=past_op_compls, b=op_compl, k=actions)] %>% # here, later also past_msg_compls
      .[ , assumption_sender := ifelse( (.[, best_action] == "Unoptimized" | .[, best_action] == "Unoptimized_appeal") , opinion_sender, opt_message)] %>%
      .[ , -c("past_msg_compls", "past_op_compls", "actions", "best_action")] %>%
      setkey("sender") %>%
      unique()

    ###################
    #### RECEIVING ####
    ###################
   
    ###################################################
    #### DETERMINE UTILITIES FOR RECEIVING ACTIONS ####
    ###################################################

    sim$actions_receive <- copy(sim$messages)[, agent_id := receiver][ , -c("sender", "actions", "best_action")] %>%
      unique() %>%
      .[copy(sim$actions_overall)[ , .(agent_id, best_action, actions)], on = c("agent_id"), nomatch = 0L, allow.cartesian = TRUE ] %>% # table is number of edges*8 x 8
      .[ best_action == actions ] %>% 
      .[ (best_action == "Both" | best_action == "Receive") ] %>% 
      .[ , -c("best_action", "actions") ] %>%
      .[copy(sim$actions_receive), on = c("agent_id"), nomatch = 0L, allow.cartesian = TRUE] %>%
      .[ , complexity := mean(ifelse(msg_compl > op_compl_receiver, 1, 0)) , by = agent_id ] %>% # sum is better than mean, because in case of Systematic, the agent would actually have to process ALL messages
      .[copy(sim$discourse_memory)[ , .(sender, past_op_compls, past_msg_compls) ], on = c("agent_id" =  "sender"), nomatch = 0L, allow.cartesian = TRUE ] %>%
      .[ , involvement := mean(c(c(unlist(past_op_compls)), c(unlist(past_msg_compls)))), by=agent_id ] %>%
      .[ actions == "Systematic" , util_score := involvement , by = agent_id ] %>%
      .[ actions == "Heuristic" , util_score := complexity , by = agent_id ] %>%
      .[ , .(agent_id, actions, util_score)] %>%
      setkey("agent_id") %>%
      .[ , -c("past_op_compls", "past_msg_compls") ] %>%
      unique() %>%
      .[, util_score := sum(util_score), by=c("agent_id", "actions")] %>%
      unique() %>%
      dcast(agent_id ~ actions, value.var = "util_score") %>%
      .[, best_action :=  ifelse(Systematic > Heuristic, "Systematic", "Heuristic")] %>%
      melt( id.vars = c("agent_id", "best_action"),
            measure.vars = c("Systematic", "Heuristic"),
            variable.name = "actions",
            value.name = "util_score" ) %>%
      .[ , agent_id := as.integer(agent_id) ]
   
    # Heuristic Mode
    
    if (nrow(sim$actions_receive[ best_action == "Heuristic" ]) > 0) {

	    sim$opinion_updating_h <- copy(sim$messages) %>%
	      unique() %>%
	      .[copy(sim$actions_receive), on = c("receiver" = "agent_id"), nomatch = 0L, allow.cartesian = TRUE ] %>% 
	      .[ best_action == actions ] %>% # table is no_agents*2 x 8 with sim$no_agents values in variable "from"
	      .[ best_action == "Heuristic"] %>% # table is 1000 x 8 (condition 1 is true for all agents in init)
	      .[ , .(sender, receiver, opinion_receiver, assumption_sender, opt_message, op_compl_receiver, msg_compl, actions, best_action)] %>%
	      .[ , message_popularity := mapply(function(a,b) {
		sum(
		  sapply(a, function(x) {
		    sum( abs(x - b) ) / .N
		  })
		)
	      }, a=assumption_sender, b=assumption_sender), by = sender] %>%
	      .[copy(sim$agent_characteristics)[ , .(agent_id, group, expert) ], on=c("sender"="agent_id"), nomatch=0L, allow.cartesian=TRUE] %>%
	      .[ , acceptance_util := message_popularity + group + expert ] %>%
	      .[ , max_util := max(acceptance_util), by=receiver ] %>%
	      .[ , accepted := acceptance_util >= max_util ] %>%
	      .[ accepted == TRUE ]  %>%
	      .[ , msg_compl := 0 ]

	    seen <- vector()
	    sim$opinion_updating_h[ , unique_within_sender := TRUE ] 
	    for (i in 1:length(sim$opinion_updating_h$sender)) {
	      if (!(sim$opinion_updating_h$receiver[i] %in% seen)) {
		      
		sim$opinion_updating_h$unique_within_sender[i] <- FALSE } else {
		sim$opinion_updating_h$unique_within_sender[i] <- TRUE

	       }

	    seen <- c(seen, sim$opinion_updating_h$sender[i])

	    }

	sim$opinion_updating_h <- sim$opinion_updating_h[ unique_within_sender == TRUE ] %>%
	      .[ , opinion_receiver_new := sum(assumption_sender) / .N, by="receiver" ] %>%
	      setnames("receiver", "agent_id") %>%
	      .[ , new_op_compl := 0 ] %>%
	      .[ , .(agent_id, opinion_receiver_new, new_op_compl)] %>%
	      setkey("agent_id") %>%
	      unique() %>%
	      .[ , receiver_business := .N, by=agent_id ]

	    }
       
    # Systematic Mode

    if (nrow(sim$actions_receive[ best_action == "Systematic" ]) > 0) {
	   
	    sim$opinion_updating_s <- copy(sim$messages) %>%
	      unique() %>%
	      .[copy(sim$actions_receive), on = c("receiver" = "agent_id"), nomatch = 0L, allow.cartesian = TRUE ] %>% 
	      .[ best_action == actions ] %>% # table is no_agents*2 x 8 with sim$no_agents values in variable "from"
	      .[ best_action == "Systematic"] %>% # table is 1000 x 8 (condition 1 is true for all agents in init)
	      .[ , .(sender, receiver, opinion_receiver, assumption_sender, opt_message, op_compl_receiver, msg_compl, actions, best_action)] %>%
              .[copy(sim$discourse_memory)[ , .(sender, past_opinions)] %>% .[ , past_opinions := as.character(past_opinions) ] %>% unique %>% .[ , past_opinions := sapply(past_opinions, function(x) list(eval(parse(text = x))))], on=c("receiver"="sender"), nomatch = 0L, allow.cartesian = TRUE] %>%
	      .[ , distance_to_past_opinions := mapply(function(a,b) {
		mean(
		  sapply(a, function(x) {
		    abs(x - b)
		  })
		)
	      }, a=past_opinions, b=assumption_sender)] %>%
	      .[ , sum_msg_compls := sum(msg_compl) , by=receiver ] %>% # problem: only the persuaded agents get an update for the complexity business
	      .[ , within_epsilon := abs(opinion_receiver - assumption_sender) < params(sim)$rc_sh_model$epsilon] %>%
	      .[ , self_consistent := distance_to_past_opinions < params(sim)$rc_sh_model$self_incons_tolerance] %>%
	      .[ , well_argumented := msg_compl > op_compl_receiver ] 

           sim$s_mode_for_energy <- copy(sim$opinion_updating_s) %>%
		   .[ , receiver_business := .N + sum_msg_compls, by = receiver ] %>%
		   setnames("receiver", "agent_id")

           sim$opinion_updating_s <- copy(sim$opinion_updating_s) %>%
	      .[ within_epsilon == TRUE & self_consistent == TRUE & well_argumented == TRUE ] %>%
              .[ , .(opinion_receiver, assumption_sender, receiver, within_epsilon, self_consistent, well_argumented, msg_compl) ] %>%
              unique %>%
	      .[ , sum_assumptions := sum(assumption_sender), by=receiver] %>%
	      .[ , denominator := .N, by=receiver ] %>%
	      .[ , opinion_receiver_new := ifelse( denominator != 0, sum_assumptions / denominator, opinion_receiver )] %>%
	      .[ , new_op_compl := sum(msg_compl) / .N , by=receiver ] %>%
	      setnames("receiver", "agent_id") %>%
	      .[ , .(agent_id, opinion_receiver_new, new_op_compl)] %>%
	      unique

    } 
    
    #################################
    #### UPDATE DISCOURSE_MEMORY ####
    #################################

   if ( (nrow(sim$actions_receive[ best_action == "Heuristic" ]) > 0 && nrow(sim$actions_receive[ best_action == "Systematic" ] > 0 ) ) )  {

	sim$discourse_memory <- copy(sim$s_mode_for_energy)[ , .(agent_id, receiver_business) ] %>%
	      rbind(copy(sim$opinion_updating_h)[ , .(agent_id, receiver_business) ]) %>%
	      setkey("agent_id") %>%
	      unique() %>%
	      .[copy(sim$discourse_memory)[ , -c("receiver_business") ], on=c("agent_id"="sender") ] %>%
	      setnames("agent_id", "sender") %>%
	      .[ , receiver_business := ifelse(is.na(receiver_business), 0, receiver_business ) ]  

    sim$opinion_updating <- sim$opinion_updating_s[ , .(agent_id, opinion_receiver_new, new_op_compl) ] %>%
      rbind(sim$opinion_updating_h[ , .(agent_id, opinion_receiver_new, new_op_compl) ] ) %>%
      setkey("agent_id") %>%
      unique() 

      } else {
	   if ( (nrow(sim$actions_receive[ best_action == "Heuristic" ]) > 0 ) ) {

	sim$discourse_memory <- copy(sim$opinion_updating_h)[ , .(agent_id, receiver_business) ] %>%
	      setkey("agent_id") %>%
	      unique() %>%
	      .[copy(sim$discourse_memory)[ , -c("receiver_business") ], on=c("agent_id"="sender") ] %>%
	      setnames("agent_id", "sender") %>%
	      .[ , receiver_business := ifelse(is.na(receiver_business), 0, receiver_business ) ]  

    sim$opinion_updating <- sim$opinion_updating_h[ , .(agent_id, opinion_receiver_new, new_op_compl) ] %>%
      setkey("agent_id") %>%
      unique() 

	   } else {

	sim$discourse_memory <- copy(sim$s_mode_for_energy)[ , .(agent_id, receiver_business) ] %>%
	      setkey("agent_id") %>%
	      unique() %>%
	      .[copy(sim$discourse_memory)[ , -c("receiver_business") ], on=c("agent_id"="sender") ] %>%
	      setnames("agent_id", "sender") %>%
	      .[ , receiver_business := ifelse(is.na(receiver_business), 0, receiver_business ) ]  

    sim$opinion_updating <- sim$opinion_updating_s[ , .(agent_id, opinion_receiver_new, new_op_compl) ] %>%
      setkey("agent_id") %>%
      unique() 

	   }

	      }
   
    ######################################
    #### UPDATE agent_characteristics ####
    ######################################

    sim$agent_characteristics <- merge(sim$agent_characteristics, sim$opinion_updating, by="agent_id", all.x = TRUE) %>% # produces NAs for not repoduced rows
      .[ , opinion := ifelse(is.na(opinion_receiver_new), opinion, opinion_receiver_new)] %>%
      .[ , op_compl := ifelse(is.na(opinion_receiver_new), op_compl, new_op_compl) ] %>%
      .[ , -c("opinion_receiver_new", "new_op_compl")]

    sim$agent_characteristics <- copy(sim$actions_overall)[ , best_axn_overall := best_action][ , .(agent_id, best_axn_overall)] %>%
      unique() %>%
      merge(copy(sim$discourse_memory)[ , .(sender, receiver_business, sender_business)], by.x = "agent_id", by.y = "sender", allow.cartesian = TRUE) %>%
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
      .[ , energy := energy - energy_loss + params(sim)$rc_sh_model$restoration_factor ] %>%
      .[ , -c("energy_loss") ]
 
    ##################################################
    #### BUSINESS UPDATE FOR sim$discourse_memory ####
    ##################################################

    sim$discourse_memory <- sim$agent_characteristics[ , .(agent_id, opinion) ] %>%
      .[sim$discourse_memory[ , -c("opinion")], on=c("agent_id" = "sender") ] %>%
      setnames("agent_id", "sender") %>%
      .[ , past_receiver_business := ifelse(lengths(past_receiver_business) < params(sim)$rc_sh_model$energy_params_memory_depth,
                                            mapply(function(x, y) {
                                              list(c(unlist(x), y))
                                            }, x=past_receiver_business, y=receiver_business),
                                            mapply(function(x, y) {
                                              list(c(unlist(x)[1:params(sim)$rc_sh_model$opinion_memory_depth], y))
                                            }, x=past_receiver_business, y=receiver_business)
      )] %>%
      .[ , past_sender_business := ifelse(lengths(past_sender_business) < params(sim)$rc_sh_model$energy_params_memory_depth,
                                          mapply(function(x, y) {
                                            list(c(unlist(x), y))
                                          }, x=past_sender_business, y=sender_business),
                                          mapply(function(x, y) {
                                            list(c(unlist(x)[1:params(sim)$rc_sh_model$opinion_memory_depth], y))
                                          }, x=past_sender_business, y=sender_business)
      )] 

    #### sim$discourse_memory table specs at this point:
    # rowlength: ( sim$environment %>% activate(edges) %>% as_tibble %>% nrow )*2 (in case of undirected)
    # columns: from (integer), to (integer), receiver_business (numeric), opinion (numeric), message (numeric), past_messages, past_opinions, distance_to_past_opinions, sender_business, past_receiver_business, past_sender_business, nbh_incohesion, past_nbh_incohesion, self_incohesion, past_self_incohesion (all numeric)

  } else {
    
    #################################################
    #### UPDATING OF ENERGY IN CASE OF NO ACTION ####
    #################################################

    # the past_receiver_business and past_sender_business needs to be updated with 0 business for this round
    sim$discourse_memory <- copy(sim$discourse_memory) %>%
      .[ , receiver_business := 0 ] %>%
      .[ , sender_business := 0 ] %>%
      .[ , past_receiver_business := ifelse(lengths(past_receiver_business) < params(sim)$rc_sh_model$energy_params_memory_depth,
                                            mapply(function(x, y) {
                                              list(c(unlist(x), y))
                                            }, x=past_receiver_business, y=receiver_business),
                                            mapply(function(x, y) {
                                              list(c(unlist(x)[1:params(sim)$rc_sh_model$energy_params_memory_depth], y))
                                            }, x=past_receiver_business, y=receiver_business)
      )] %>%
      .[ , past_sender_business := ifelse(lengths(past_sender_business) < params(sim)$rc_sh_model$energy_params_memory_depth,
                                          mapply(function(x, y) {
                                            list(c(unlist(x), y))
                                          }, x=past_sender_business, y=sender_business),
                                          mapply(function(x, y) {
                                            list(c(unlist(x)[1:params(sim)$rc_sh_model$energy_params_memory_depth], y))
                                          }, x=past_sender_business, y=sender_business)
      )]


  sim$opinion_updating <- copy(sim$discourse_memory) %>%
    .[ , opinion_receiver_new := sapply(past_opinions, function(x) { median(x) }) ] %>%
    .[, new_op_compl := op_compl + ifelse(mean(c(unlist(past_op_compls)) - op_compl) > 0, mean(c(unlist(past_op_compls)) - op_compl), 0) ]
    setnames("sender", "agent_id") %>%
    .[ , .(agent_id, opinion_receiver_new, new_op_compl)] %>%
    setkey("agent_id") %>%
    unique() %>%
    rbind(sim$opinion_updating)

   sim$agent_characteristics <- merge(sim$agent_characteristics, sim$opinion_updating, by="agent_id", all.x = TRUE) %>% # produces NAs for not repoduced rows
      .[ , opinion := ifelse(is.na(opinion_receiver_new), opinion, opinion_receiver_new)] %>%
      .[ , op_compl := ifelse(is.na(opinion_receiver_new), op_compl, new_op_compl) ] %>%
      .[ , -c("opinion_receiver_new", "new_op_compl")]

    # sim$agent_characteristics$energy is not diminished, but the amount of the restoration factor is added
    sim$agent_characteristics <- sim$agent_characteristics %>%
      .[ , energy := energy + params(sim)$rc_sh_model$restoration_factor ]

  }

  return(invisible(sim))

}

rc_sh_modelStep <- function(sim) {

  print(time(sim))

  ########################
  #### REBUILD TABLES ####
  ########################

  # tables that are rebuilt at the start of each round: sim$actions_send, sim$actions_overall, sim$messages

  sim$actions_send <- tibble(
    
    agent_id = rep(agent_characteristics$agent_id, each=4),
    actions = rep(c("Unoptimized", "Optimized", "Unoptimized_appeal", "Optimized_appeal"), sim$no_agents),
    util_score = rep(0, length(actions))
    
  ) %>%  data.table() %>%
    .[ , agent_id := as.integer(agent_id) ]
  
  sim$actions_overall <- tibble(

    agent_id = rep(sim$agent_characteristics$agent_id, each=4),
    actions = rep(c("Send", "Receive", "Both", "Nothing"), sim$no_agents),
    util_score = rep(0, length(actions))

  ) %>% data.table()

  sim$actions_receive <- tibble(

    agent_id = rep(sim$agent_characteristics$agent_id, each=2),
    actions = rep(c("Systematic", "Heuristic"), sim$no_agents),
    util_score = rep(0, length(actions))

  ) %>%
  data.table()

  sim$messages <- sim$environment %>%
    activate(edges) %>%
    as_tibble() %>%
    data.table() %>%
    setnames(old = c("from", "to"), new = c("receiver", "sender"))

  sim$messages <- sim$environment %>%
    activate(edges) %>%
    as_tibble() %>%
    data.table() %>%
    setnames(old = c("from", "to"), new = c("sender", "receiver")) %>%
    rbind(sim$messages) %>%
    .[copy(sim$agent_characteristics)[, .(agent_id, opinion, op_compl)], nomatch = 0L, on = c("sender" = "agent_id"), allow.cartesian=TRUE] %>%
    setnames("opinion", "opinion_sender")

  ######################################## 
  #### CONSTRUCT CHOSEN_ACTIONS TABLE ####
  ########################################

  sim$chosen_actions <- tibble(

    agent_id = rep(agent_characteristics$agent_id, each=3),
    action_type = rep(c("actions_overall", "actions_send", "actions_receive"), sim$no_agents),
    best_action = rep(c("Not assigned"), length(action_type))

  ) %>% data.table() %>%
    .[ , agent_id := as.integer(agent_id) ]


  #################################################################
  #### BUILD sim$actions_overall AND FIND BEST OVERALL ACTIONS ####
  #################################################################

  sim$actions_overall <- copy(sim$messages)[ , .(sender, receiver)] %>%
    .[ , max_send_energy_loss := .N*2, by = sender ] %>%
    .[ , max_receive_energy_loss := .N*2, by = receiver] %>%
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
    .[ actions == "Send" , energy_loss := ( energy - sender_business )] %>%
    .[ actions == "Receive" , energy_loss := ( energy - receiver_business )] %>%
    .[ actions == "Both" , energy_loss := ( energy - sender_business - receiver_business )] %>%
    .[ actions == "Send" , projected_energy := ifelse(energy_loss <= 0, 0, energy_loss) / ( energy + params(sim)$rc_sh_model$restoration_factor ) ] %>%
    .[ actions == "Receive" , projected_energy := ifelse(energy_loss <= 0, 0, energy_loss) / ( energy + params(sim)$rc_sh_model$restoration_factor ) ] %>%
    .[ actions == "Both" , projected_energy := ifelse(energy_loss <= 0, 0, energy_loss) / ( energy + params(sim)$rc_sh_model$restoration_factor ) ]  %>%
    .[ , projected_energy := ifelse(is.na(projected_energy), 0, projected_energy) ] %>%
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
    setnames("message", "assumption_receiver") # works like a charm until here

  ####################################
  #### PRODUCE OPTIMIZED MESSAGES ####
  ####################################

  sim$message_matrix <- outer(sim$messages$opinion_sender, sim$messages$assumption_receiver, produce_altered_message) # works
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

  sim$discourse_memory <- copy(sim$discourse_memory)[ , -c("opinion", "op_compl") ] %>%
    .[sim$agent_characteristics[ , .(agent_id, opinion, op_compl) ], on=c("sender" = "agent_id")] %>%
    .[ , past_opinions := ifelse(lengths(past_opinions) < params(sim)$rc_sh_model$opinion_memory_depth,
                                 mapply(function(x, y) {
                                   list(c(unlist(x), y))
                                 }, x=past_opinions, y=opinion),
                                 mapply(function(x, y) {
                                   list(c(unlist(x)[1:params(sim)$rc_sh_model$opinion_memory_depth], y))
                                 }, x=past_opinions, y=opinion)
    )] %>%
    .[ ,  past_op_compls := ifelse(lengths(past_op_compls) < params(sim)$rc_sh_model$argumentation_memory_depth,
                                 mapply(function(x, y) {
                                   list(c(unlist(x), y))
                                 }, x=past_op_compls, y=op_compl),
                                 mapply(function(x, y) {
                                   list(c(unlist(x)[1:params(sim)$rc_sh_model$argumentation_memory_depth], y))
                                 }, x=past_op_compls, y=op_compl)
    )] %>%
    .[ , -c("assumption_receiver", "opt_message")]
  

    ###############################################################
    #### COMPUTE SENDING UTILITIES AND CHOOSE MAX UTIL ACTIONS ####
    ###############################################################

   if( length(sim$actions_overall[ best_action %in% c("Both", "Send") , best_action ] > 0 ) ) {

    sim$actions_send <- copy(sim$messages) %>%
      .[copy(sim$actions_overall), on = c("sender" = "agent_id"), nomatch = 0L, allow.cartesian = TRUE ] %>%
      .[ best_action == actions ] %>%
      .[ (best_action == "Both" | best_action == "Send") ] %>%
      .[ , .(sender , receiver , opt_message , opinion_sender , assumption_receiver)] %>%
      .[copy(sim$actions_send), on = c("sender" = "agent_id"), nomatch = 0L, allow.cartesian = TRUE ] %>%
      .[copy(sim$discourse_memory), on = c("sender"), nomatch = 0L, allow.cartesian = TRUE] %>%
      .[, involvement := sapply( past_op_compls, function(x) mean(unlist(x)) )  ] %>%
      .[, msg_compl := mapply(function(a, b, c, k) {
        switch(k,
               "Unoptimized" = {
                 b + ifelse(mean(c(c(unlist(a)), c(unlist(c))) - b) > 0, mean(c(c(unlist(a)), c(unlist(c)))), 0) # new opinion can be argued for with the mean deviation complexity of own opinion complexity 
	       },
               "Optimized" = {
                 b + ifelse(mean(c(c(unlist(a)), c(unlist(c))) - b) > 0, mean(c(c(unlist(a)), c(unlist(c)))), 0) # new opinion can be argued for with the mean deviation complexity of own opinion complexity 
	       },
               "Unoptimized_appeal" = {
                 0
	       },
               "Optimized_appeal" = {
                 0 
	       }
        )
      }, a=past_op_compls, b=op_compl, c=past_msg_compls, k=actions)] %>% # here, later also past_msg_compls
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
               },
               "Unoptimized_appeal" = {
                 mean(
                   sapply(a, function(x) {
                     abs(x - c)
                   })
                 )
               },
               "Optimized_appeal" = {
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
               },
               "Unoptimized_appeal" = {
                 mean(
                   sapply(a, function(x) {
                     abs(x - c)
                   })
                 )
               },
               "Optimized_appeal" = {
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
               },
               "Unoptimized_appeal" = {
                 abs(a - a)
               },
               "Optimized_appeal" = {
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
               },
               "Unoptimized_appeal" = {
                 abs(c - a)
               },
               "Optimized_appeal" = {
                 abs(c - b)
               }
        )
      }, a=opinion_sender, b=opt_message, c=assumption_receiver, k=actions)] %>%
      .[ , -c("past_opinions", "receiver")] %>% # include past_messages here in the step function
      .[ actions == "Unoptimized" , util_score := 0 - distance_to_past_opinions - distance_message_opinion - distance_message_assumption - msg_compl + involvement] %>% 
      .[ actions == "Unoptimized_appeal", util_score := 0 - distance_to_past_opinions - distance_message_opinion - distance_message_assumption + msg_compl - involvement] %>% 
      .[ actions == "Optimized", util_score := 0 - distance_to_past_opinions - distance_message_opinion - distance_message_assumption - msg_compl + involvement] %>% 
      .[ actions == "Optimized_appeal", util_score := 0 - distance_to_past_opinions - distance_message_opinion - distance_message_assumption + msg_compl - involvement] %>% 
      .[ , agent_id := sender ] %>%
      .[ , .(agent_id, actions, util_score)] %>%
      setkey("agent_id") %>%
      unique() %>%
      .[, util_score := sum(util_score), by=c("agent_id", "actions")] %>%
      unique() %>%
      dcast(agent_id ~ actions, value.var = "util_score") %>%
      .[, best_action :=  names(.[ , -c("agent_id")])[apply(.[ , -c("agent_id") ], 1, which.max)]] %>%
      melt( id.vars = c("agent_id", "best_action"),
            measure.vars = c("Optimized", "Unoptimized", "Unoptimized_appeal", "Optimized_appeal"),
            variable.name = "actions",
            value.name = "util_score" ) %>%
      .[ , agent_id := as.integer(agent_id) ]

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

    sim$send_busy <- copy(sim$actions_send)[ , -c("util_score")] %>%
      .[copy(sim$messages)[ , .(sender, receiver, opt_message) ], on = c("agent_id" = "sender"), nomatch = 0L, allow.cartesian = TRUE] %>%
      setnames("agent_id", "sender") %>%
      setkey("sender") %>%
      .[ actions==best_action ] %>%
      .[ , -c("actions")]  %>% 
      unique() %>% 
      .[ , sender_business := .N, by = "sender" ] %>% 
      .[ , receiver_business := .N, by = "receiver" ] %>% 
      .[ , -c("opinion_sender") ] 

    sim$receive_busy <- copy(sim$send_busy)[ , .(receiver, receiver_business)] %>% 
	    .[ !is.na(receiver) ] %>%
	    .[ , receiver_business := max(receiver_business), by=receiver] %>%
	    unique() 

    sim$discourse_memory <- copy(sim$discourse_memory)[ , -c("sender_business", "receiver_business")] %>%
      merge(sim$send_busy[ , -c("receiver_business") ], by=c("sender"), all.x=TRUE, all.y=TRUE) %>% # includes best_action
      .[sim$receive_busy, on=c("sender"="receiver") ] %>%
      .[ , receiver_business := ifelse(is.na(receiver_business), 0, receiver_business) ] 

sim$discourse_memory <- copy(sim$discourse_memory) %>% 
      .[ !is.na(best_action) , msg_compl := mapply(function(a, b, c, k) {
        switch(k,
               "Unoptimized" = {
                 b + ifelse(mean(c(unlist(a)) - b) > 0 | mean(c(unlist(a)) - c) > 0, max(c(mean(c(unlist(a)) - b), mean(c(unlist(a)) - c))), 0) # new opinion can be argued for with the mean deviation complexity of own opinion complexity 
	       },
               "Optimized" = {
                 b + ifelse(mean(c(unlist(a)) - b) > 0 | mean(c(unlist(a)) - c) > 0, max(c(mean(c(unlist(a)) - b), mean(c(unlist(a)) - c))), 0) # new opinion can be argued for with the mean deviation complexity of own opinion complexity 
	       },
               "Unoptimized_appeal" = {
                 0
	       },
               "Optimized_appeal" = {
                 0 
	       }
        )
      }, a=past_op_compls, b=op_compl, c=past_msg_compls, k=best_action)] %>%
      .[ !is.na(best_action), sender_business := mapply(function(a, b, c, k) {
        switch(k,
               "Unoptimized" = {
                 c + b + a 
	       },
               "Optimized" = {
                 c + b + a 
	       },
               "Unoptimized_appeal" = {
                c 
	       },
               "Optimized_appeal" = {
                c 
	       }
        )
      }, a=msg_compl , b=op_compl, c= sender_business, k=best_action)] %>% # here, later also past_msg_compls
      .[ , sender_business := ifelse(is.na(sender_business), 0, sender_business) ] %>% 
      .[ , past_messages := ifelse( is.na(best_action),
            past_messages,
            ifelse(lengths(past_messages) < params(sim)$rc_sh_model$message_memory_depth,
                                               ifelse(.[ , best_action] == "Unoptimized",
                                          mapply(function(x, y) {
                                            list(c(unlist(x), y))
                                          }, x=past_messages, y=opinion),
                                          mapply(function(x, y) {
                                            list(c(unlist(x), y))
                                          }, x=past_messages, y=opt_message)),
                                   ifelse(.[ , best_action] == "Unoptimized",
                                          mapply(function(x, y) {
                                            list(c(unlist(x)[1:params(sim)$rc_sh_model$message_memory_depth], y))
                                          }, x=past_messages, y=opinion),
                                          mapply(function(x, y) {
                                            list(c(unlist(x)[1:params(sim)$rc_sh_model$message_memory_depth], y))
                                          }, x=past_messages, y=opt_message)
                                   )
                                 )
      )] %>%
      .[ , message := ifelse(is.na(opt_message), message, opt_message) ] %>%
      .[ , -c("opt_message") ] %>%
      .[ , .(sender, receiver, opinion, message, op_compl, msg_compl, past_messages, past_opinions, past_op_compls, past_msg_compls, sender_business, receiver_business, past_sender_business, past_receiver_business, past_nbh_incohesion, past_self_incohesion)] %>%
      .[ , past_messages := as.character(past_messages) ] %>%
      .[ , past_opinions := as.character(past_opinions) ] %>%
      .[ , past_receiver_business := as.character(past_receiver_business) ] %>%
      .[ , past_sender_business := as.character(past_sender_business) ] %>%
      .[ , past_nbh_incohesion := as.character(past_nbh_incohesion) ] %>%
      .[ , past_self_incohesion := as.character(past_self_incohesion) ] %>%
      .[ , past_msg_compls := as.character(past_msg_compls) ] %>%
      .[ , past_op_compls := as.character(past_op_compls) ] %>%
      .[ , -c("receiver") ] %>%
      unique() %>%
      .[ , past_messages := sapply(past_messages, function(x) list(eval(parse(text = x))))] %>% 
      .[ , past_opinions := sapply(past_opinions, function(x) list(eval(parse(text = x))))] %>% 
      .[ , past_receiver_business := sapply(past_receiver_business, function(x) list(eval(parse(text = x))))] %>% 
      .[ , past_sender_business := sapply(past_sender_business, function(x) list(eval(parse(text = x))))] %>%
      .[ , past_nbh_incohesion := sapply(past_nbh_incohesion, function(x) list(eval(parse(text = x))))] %>%
      .[ , past_self_incohesion := sapply(past_self_incohesion, function(x) list(eval(parse(text = x))))] %>%
      .[ , past_msg_compls := sapply(past_msg_compls, function(x) list(eval(parse(text = x))))] %>%
      .[ , past_op_compls := sapply(past_op_compls, function(x) list(eval(parse(text = x))))] %>%
      .[ , nbh_incohesion := vec_get_nbh_incohesion(sender) ] %>%
      .[ , self_incohesion := vec_get_self_incohesion( sender ) ] %>%
      .[ , past_sender_business := ifelse(lengths(past_sender_business) < params(sim)$rc_sh_model$energy_params_memory_depth,
                                          mapply(function(x, y) {
                                            list(c(unlist(x), y))
                                          }, x=past_sender_business, y=sender_business),
                                          mapply(function(x, y) {
                                            list(c(unlist(x)[1:params(sim)$rc_sh_model$energy_params_memory_depth], y))
                                          }, x=past_sender_business, y=sender_business)
      )] %>%
      .[ , past_nbh_incohesion := ifelse(lengths(past_nbh_incohesion) < params(sim)$rc_sh_model$energy_params_memory_depth,
                                         mapply(function(x, y) {
                                           list(c(unlist(x), y))
                                         }, x=past_nbh_incohesion, y=nbh_incohesion),
                                         mapply(function(x, y) {
                                           list(c(unlist(x)[1:params(sim)$rc_sh_model$opinion_memory_depth], y))
                                         }, x=past_nbh_incohesion, y=nbh_incohesion)
      )] %>%
      .[ , past_self_incohesion := ifelse(lengths(past_self_incohesion) < params(sim)$rc_sh_model$energy_params_memory_depth,
                                          mapply(function(x, y) {
                                            list(c(unlist(x), y))
                                          }, x=past_self_incohesion, y=self_incohesion),
                                          mapply(function(x, y) {
                                            list(c(unlist(x)[1:params(sim)$rc_sh_model$opinion_memory_depth], y))
                                          }, x=past_self_incohesion, y=self_incohesion)
      )] %>%
      .[ , past_messages := as.character(past_messages) ] %>%
      .[ , past_opinions := as.character(past_opinions) ] %>%
      .[ , past_receiver_business := as.character(past_receiver_business) ] %>%
      .[ , past_sender_business := as.character(past_sender_business) ] %>%
      .[ , past_nbh_incohesion := as.character(past_nbh_incohesion) ] %>%
      .[ , past_self_incohesion := as.character(past_self_incohesion) ] %>%
      .[ , past_msg_compls := as.character(past_msg_compls) ] %>%
      .[ , past_op_compls := as.character(past_op_compls) ] %>%
      unique() %>%
      .[ , past_messages := sapply(past_messages, function(x) list(eval(parse(text = x))))] %>%
      .[ , past_opinions := sapply(past_opinions, function(x) list(eval(parse(text = x))))] %>%
      .[ , past_receiver_business := sapply(past_receiver_business, function(x) list(eval(parse(text = x))))] %>%
      .[ , past_sender_business := sapply(past_sender_business, function(x) list(eval(parse(text = x))))] %>%
      .[ , past_nbh_incohesion := sapply(past_nbh_incohesion, function(x) list(eval(parse(text = x))))] %>%
      .[ , past_self_incohesion := sapply(past_self_incohesion, function(x) list(eval(parse(text = x))))]%>%
      .[ , past_msg_compls := sapply(past_msg_compls, function(x) list(eval(parse(text = x))))] %>%
      .[ , past_op_compls := sapply(past_op_compls, function(x) list(eval(parse(text = x))))]

     sim$messages <- copy(sim$messages) %>%
      unique() %>%
      setnames(old = c("sender", "receiver"), new = c("receiver", "sender")) %>%
      .[ , .(receiver, opinion_sender, op_compl)] %>%
      setnames(old=c("opinion_sender", "op_compl"), new=c("opinion_receiver", "op_compl_receiver")) %>%
      .[sim$messages, on="receiver", nomatch=0L, allow.cartesian=TRUE] 

    sim$messages <- copy(sim$messages) %>%
      .[sim$actions_send[ best_action == actions , .(agent_id, actions, best_action)], nomatch=0L, on=c("sender" = "agent_id"), allow.cartesian=TRUE] %>%
      unique() %>%
      merge(sim$discourse_memory[ , .(sender, past_msg_compls, past_op_compls)], nomatch=0L, by=c("sender"), allow.cartesian=TRUE) %>% 
      .[ , past_msg_compls := as.character(past_msg_compls) ] %>%
      .[ , past_op_compls := as.character(past_op_compls) ] %>%
      unique() %>% 
      .[ , past_msg_compls := sapply(past_msg_compls, function(x) list(eval(parse(text = x))))] %>%
      .[ , past_op_compls := sapply(past_op_compls, function(x) list(eval(parse(text = x))))] %>%
      .[, msg_compl := mapply(function(a, b, k) {
        switch(k,
               "Unoptimized" = {
                 b + ifelse(mean(c(unlist(a)) - b) > 0, mean(c(unlist(a)) - b), 0) # new opinion can be argued for with the mean deviation complexity of own opinion complexity 
	       },
               "Optimized" = {
                 b + ifelse(mean(c(unlist(a)) - b) > 0, mean(c(unlist(a)) - b), 0) 
	       },
               "Unoptimized_appeal" = {
                 0
	       },
               "Optimized_appeal" = {
                 0 
	       }
        )
      }, a=past_op_compls, b=op_compl, k=actions)] %>% # here, later also past_msg_compls
      .[ , assumption_sender := ifelse( (.[, best_action] == "Unoptimized" | .[, best_action] == "Unoptimized_appeal") , opinion_sender, opt_message)] %>%
      .[ , -c("past_msg_compls", "past_op_compls", "actions", "best_action")] %>%
      setkey("sender") %>%
      unique()
 
    ###################
    #### RECEIVING ####
    ###################
   
    ###################################################
    #### DETERMINE UTILITIES FOR RECEIVING ACTIONS ####
    ###################################################

    sim$actions_receive <- copy(sim$messages)[, agent_id := receiver][ , -c("sender", "actions", "best_action")] %>%
      unique() %>%
      .[copy(sim$actions_overall)[ , .(agent_id, best_action, actions)], on = c("agent_id"), nomatch = 0L, allow.cartesian = TRUE ] %>% # table is number of edges*8 x 8
      .[ best_action == actions ] %>% 
      .[ (best_action == "Both" | best_action == "Receive") ] %>% 
      .[ , -c("best_action", "actions") ] %>%
      .[copy(sim$actions_receive), on = c("agent_id"), nomatch = 0L, allow.cartesian = TRUE] %>%
      .[ , complexity := mean(ifelse(msg_compl > op_compl_receiver, 1, 0)) , by = agent_id ] %>% # sum is better than mean, because in case of Systematic, the agent would actually have to process ALL messages
      .[copy(sim$discourse_memory)[ , .(sender, past_op_compls, past_msg_compls) ], on = c("agent_id" =  "sender"), nomatch = 0L, allow.cartesian = TRUE ] %>%
      .[ , involvement := mean(c(c(unlist(past_op_compls)), c(unlist(past_msg_compls)))), by=agent_id ] %>%
      .[ actions == "Systematic" , util_score := involvement , by = agent_id ] %>%
      .[ actions == "Heuristic" , util_score := complexity , by = agent_id ] %>%
      .[ , .(agent_id, actions, util_score)] %>%
      setkey("agent_id") %>%
      .[ , -c("past_op_compls", "past_msg_compls") ] %>%
      unique() %>%
      .[, util_score := sum(util_score), by=c("agent_id", "actions")] %>%
      unique() %>%
      dcast(agent_id ~ actions, value.var = "util_score") %>%
      .[, best_action :=  ifelse(Systematic > Heuristic, "Systematic", "Heuristic")] %>%
      melt( id.vars = c("agent_id", "best_action"),
            measure.vars = c("Systematic", "Heuristic"),
            variable.name = "actions",
            value.name = "util_score" ) %>%
      .[ , agent_id := as.integer(agent_id) ]

	sim$chosen_actions <- copy(sim$actions_receive)[ actions == best_action , .(agent_id, best_action) ] %>%
	      merge(sim$chosen_actions, by="agent_id", all.x=TRUE , all.y=TRUE) %>% 
	      .[ best_action.y == "Receive" , action_type := "actions_overall" ] %>%
	      .[ best_action.y == "Nothing" , action_type := "actions_overall" ] %>%
	      .[ action_type == "actions_send" , best_action := ifelse(!is.na(best_action.y), best_action.y, "NOT ASSIGNED")] %>%
	      .[ action_type == "actions_overall" , best_action := ifelse(!is.na(best_action.y), best_action.y, "NOT ASSIGNED")] %>%
	      .[ action_type == "actions_receive" , best_action := ifelse(!is.na(best_action.x), best_action.x, "NOT ASSIGNED")] %>%
	      .[ , -c("best_action.x", "best_action.y")]

      print(table(sim$chosen_actions$best_action))

    # Heuristic Mode
    
    if (nrow(sim$actions_receive[ best_action == "Heuristic" ]) > 0) {

	    sim$opinion_updating_h <- copy(sim$messages) %>%
	      unique() %>%
	      .[copy(sim$actions_receive), on = c("receiver" = "agent_id"), nomatch = 0L, allow.cartesian = TRUE ] %>% 
	      .[ best_action == actions ] %>% # table is no_agents*2 x 8 with sim$no_agents values in variable "from"
	      .[ best_action == "Heuristic"] %>% # table is 1000 x 8 (condition 1 is true for all agents in init)
	      .[ , .(sender, receiver, opinion_receiver, assumption_sender, opt_message, op_compl_receiver, msg_compl, actions, best_action)] %>%
	      .[ , message_popularity := mapply(function(a,b) {
		sum(
		  sapply(a, function(x) {
		    sum( abs(x - b) ) / .N
		  })
		)
	      }, a=assumption_sender, b=assumption_sender), by = sender] %>%
	      .[copy(sim$agent_characteristics)[ , .(agent_id, group, expert) ], on=c("sender"="agent_id"), nomatch=0L, allow.cartesian=TRUE] %>%
	      .[ , acceptance_util := message_popularity + group + expert ] %>%
	      .[ , max_util := max(acceptance_util), by=receiver ] %>%
	      .[ , accepted := acceptance_util >= max_util ] %>%
	      .[ accepted == TRUE ]  %>%
	      .[ , msg_compl := 0 ]

	    seen <- vector()
	    sim$opinion_updating_h[ , unique_within_sender := TRUE ] 
	    for (i in 1:length(sim$opinion_updating_h$sender)) {
	      if (!(sim$opinion_updating_h$receiver[i] %in% seen)) {
		      
		sim$opinion_updating_h$unique_within_sender[i] <- FALSE } else {
		sim$opinion_updating_h$unique_within_sender[i] <- TRUE

	       }

	    seen <- c(seen, sim$opinion_updating_h$sender[i])

	    }

	sim$opinion_updating_h <- sim$opinion_updating_h[ unique_within_sender == TRUE ] %>%
	      .[ , opinion_receiver_new := sum(assumption_sender) / .N, by="receiver" ] %>%
	      setnames("receiver", "agent_id") %>%
	      .[ , new_op_compl := 0 ] %>%
	      .[ , .(agent_id, opinion_receiver_new, new_op_compl)] %>%
	      setkey("agent_id") %>%
	      unique()

	    }
       
    # Systematic Mode

    if (nrow(sim$actions_receive[ best_action == "Systematic" ]) > 0) {
	   
	    sim$opinion_updating_s <- copy(sim$messages) %>%
	      unique() %>%
	      .[copy(sim$actions_receive), on = c("receiver" = "agent_id"), nomatch = 0L, allow.cartesian = TRUE ] %>% 
	      .[ best_action == actions ] %>% # table is no_agents*2 x 8 with sim$no_agents values in variable "from"
	      .[ best_action == "Systematic"] %>% # table is 1000 x 8 (condition 1 is true for all agents in init)
	      .[ , .(sender, receiver, opinion_receiver, assumption_sender, opt_message, op_compl_receiver, msg_compl, actions, best_action)] %>%
              .[copy(sim$discourse_memory)[ , .(sender, past_opinions)] %>% .[ , past_opinions := as.character(past_opinions) ] %>% unique %>% .[ , past_opinions := sapply(past_opinions, function(x) list(eval(parse(text = x))))], on=c("receiver"="sender"), nomatch = 0L, allow.cartesian = TRUE] %>%
	      .[ , distance_to_past_opinions := mapply(function(a,b) {
		mean(
		  sapply(a, function(x) {
		    abs(x - b)
		  })
		)
	      }, a=past_opinions, b=assumption_sender)] %>%
	      .[ , sum_msg_compls := sum(msg_compl) , by=receiver ] %>% # problem: only the persuaded agents get an update for the complexity business
	      .[ , within_epsilon := abs(opinion_receiver - assumption_sender) < params(sim)$rc_sh_model$epsilon] %>%
	      .[ , self_consistent := distance_to_past_opinions < params(sim)$rc_sh_model$self_incons_tolerance] %>%
	      .[ , well_argumented := msg_compl > op_compl_receiver ] 

           s_mode_for_energy <- copy(sim$opinion_updating_s) %>%
		   .[ , receiver_business := .N + sum_msg_compls, by = receiver ] %>%
		   setnames("receiver", "agent_id") %>%
	           .[ , .(agent_id, receiver_business) ] %>%
		   unique()

           sim$opinion_updating_s <- copy(sim$opinion_updating_s) %>%
	      .[ within_epsilon == TRUE & self_consistent == TRUE & well_argumented == TRUE ] %>%
              .[ , .(opinion_receiver, assumption_sender, receiver, within_epsilon, self_consistent, well_argumented, msg_compl) ] %>%
              unique %>%
	      .[ , sum_assumptions := sum(assumption_sender), by=receiver] %>%
	      .[ , denominator := .N, by=receiver ] %>%
	      .[ , opinion_receiver_new := ifelse( denominator != 0, sum_assumptions / denominator, opinion_receiver )] %>%
	      .[ , new_op_compl := sum(msg_compl) / .N , by=receiver ] %>%
	      setnames("receiver", "agent_id") %>%
	      .[ , .(agent_id, opinion_receiver_new, new_op_compl)] %>%
	      unique

    } 
    
    #################################
    #### UPDATE DISCOURSE_MEMORY ####
    #################################

   if ( (nrow(sim$actions_receive[ best_action == "Heuristic" ]) > 0 && nrow(sim$actions_receive[ best_action == "Systematic" ]) > 0 ) )  {

	sim$discourse_memory <- copy(sim$s_mode_for_energy)[ , .(agent_id, receiver_business) ] %>%
              setnames("receiver_business", "receiver_business_y") %>%
	      setkey("agent_id") %>%
	      unique() %>%
	      .[copy(sim$discourse_memory), on=c("agent_id"="sender") ] %>%
	      setnames("agent_id", "sender") %>%
	      .[ , receiver_business := ifelse(is.na(receiver_business_y), receiver_business, receiver_business_y ) ] %>%
	      .[ , -c("receiver_business_y") ]

    sim$opinion_updating <- sim$opinion_updating_s[ , .(agent_id, opinion_receiver_new, new_op_compl) ] %>%
      rbind(sim$opinion_updating_h[ , .(agent_id, opinion_receiver_new, new_op_compl) ] ) %>%
      setkey("agent_id") %>%
      unique() 

      } else {
	   if ( (nrow(sim$actions_receive[ best_action == "Heuristic" ]) > 0 ) ) {

    sim$opinion_updating <- sim$opinion_updating_h[ , .(agent_id, opinion_receiver_new, new_op_compl) ] %>%
      setkey("agent_id") %>%
      unique() 

	   } else {

	sim$discourse_memory <- copy(sim$s_mode_for_energy)[ , .(agent_id, receiver_business) ] %>%
              setnames("receiver_business", "receiver_business_y") %>%
	      setkey("agent_id") %>%
	      unique() %>%
	      .[copy(sim$discourse_memory), on=c("agent_id"="sender") ] %>%
	      setnames("agent_id", "sender") %>%
	      .[ , receiver_business := ifelse(is.na(receiver_business_y), receiver_business, receiver_business_y ) ] %>%
	      .[ , -c("receiver_business_y") ]

    sim$opinion_updating <- sim$opinion_updating_s[ , .(agent_id, opinion_receiver_new, new_op_compl) ] %>%
      setkey("agent_id") %>%
      unique() 

	   }

	      }
   
    ######################################
    #### UPDATE agent_characteristics ####
    ######################################

    sim$agent_characteristics <- merge(sim$agent_characteristics, sim$opinion_updating, by="agent_id", all.x = TRUE) %>% # produces NAs for not repoduced rows
      .[ , opinion := ifelse(is.na(opinion_receiver_new), opinion, opinion_receiver_new)] %>%
      .[ , op_compl := ifelse(is.na(opinion_receiver_new), op_compl, new_op_compl) ] %>%
      .[ , -c("opinion_receiver_new", "new_op_compl")]

    sim$agent_characteristics <- copy(sim$actions_overall)[ , best_axn_overall := best_action][ , .(agent_id, best_axn_overall)] %>%
      unique() %>%
      merge(copy(sim$discourse_memory)[ , .(sender, receiver_business, sender_business)], by.x = "agent_id", by.y = "sender", allow.cartesian = TRUE) %>%
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
      .[ , energy := energy - energy_loss + params(sim)$rc_sh_model$restoration_factor ] %>%
      .[ , -c("energy_loss") ]
 
    ##################################################
    #### BUSINESS UPDATE FOR sim$discourse_memory ####
    ##################################################

    sim$discourse_memory <- sim$agent_characteristics[ , .(agent_id, opinion) ] %>%
      .[sim$discourse_memory[ , -c("opinion")], on=c("agent_id" = "sender") ] %>%
      setnames("agent_id", "sender") %>%
      .[ , past_receiver_business := ifelse(lengths(past_receiver_business) < params(sim)$rc_sh_model$energy_params_memory_depth,
                                            mapply(function(x, y) {
                                              list(c(unlist(x), y))
                                            }, x=past_receiver_business, y=receiver_business),
                                            mapply(function(x, y) {
                                              list(c(unlist(x)[1:params(sim)$rc_sh_model$opinion_memory_depth], y))
                                            }, x=past_receiver_business, y=receiver_business)
      )] %>%
      .[ , past_sender_business := ifelse(lengths(past_sender_business) < params(sim)$rc_sh_model$energy_params_memory_depth,
                                          mapply(function(x, y) {
                                            list(c(unlist(x), y))
                                          }, x=past_sender_business, y=sender_business),
                                          mapply(function(x, y) {
                                            list(c(unlist(x)[1:params(sim)$rc_sh_model$opinion_memory_depth], y))
                                          }, x=past_sender_business, y=sender_business)
      )] 

  } else {
    
    #################################################
    #### UPDATING OF ENERGY IN CASE OF NO ACTION ####
    #################################################

    # the past_receiver_business and past_sender_business needs to be updated with 0 business for this round
    sim$discourse_memory <- copy(sim$discourse_memory) %>%
      .[ , receiver_business := 0 ] %>%
      .[ , sender_business := 0 ] %>%
      .[ , past_receiver_business := ifelse(lengths(past_receiver_business) < params(sim)$rc_sh_model$energy_params_memory_depth,
                                            mapply(function(x, y) {
                                              list(c(unlist(x), y))
                                            }, x=past_receiver_business, y=receiver_business),
                                            mapply(function(x, y) {
                                              list(c(unlist(x)[1:params(sim)$rc_sh_model$energy_params_memory_depth], y))
                                            }, x=past_receiver_business, y=receiver_business)
      )] %>%
      .[ , past_sender_business := ifelse(lengths(past_sender_business) < params(sim)$rc_sh_model$energy_params_memory_depth,
                                          mapply(function(x, y) {
                                            list(c(unlist(x), y))
                                          }, x=past_sender_business, y=sender_business),
                                          mapply(function(x, y) {
                                            list(c(unlist(x)[1:params(sim)$rc_sh_model$energy_params_memory_depth], y))
                                          }, x=past_sender_business, y=sender_business)
      )]


  sim$opinion_updating <- copy(sim$discourse_memory) %>%
    .[ , opinion_receiver_new := sapply(past_opinions, function(x) { median(x) }) ] %>%
    .[, new_op_compl := op_compl + ifelse(mean(c(unlist(past_op_compls)) - op_compl) > 0, mean(c(unlist(past_op_compls)) - op_compl), 0) ]
    setnames("sender", "agent_id") %>%
    .[ , .(agent_id, opinion_receiver_new, new_op_compl)] %>%
    setkey("agent_id") %>%
    unique() %>%
    rbind(sim$opinion_updating)

   sim$agent_characteristics <- merge(sim$agent_characteristics, sim$opinion_updating, by="agent_id", all.x = TRUE) %>% # produces NAs for not repoduced rows
      .[ , opinion := ifelse(is.na(opinion_receiver_new), opinion, opinion_receiver_new)] %>%
      .[ , op_compl := ifelse(is.na(opinion_receiver_new), op_compl, new_op_compl) ] %>%
      .[ , -c("opinion_receiver_new", "new_op_compl")]

    # sim$agent_characteristics$energy is not diminished, but the amount of the restoration factor is added
    sim$agent_characteristics <- sim$agent_characteristics %>%
      .[ , energy := energy + params(sim)$rc_sh_model$restoration_factor ]

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

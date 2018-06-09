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
    data.table() 
  sim$agent_characteristics[ , energy := params(sim)$rc_energy_model$energy_level]
  
  ######################################## 
  #### CONSTRUCT CHOSEN_ACTIONS TABLE ####
  ########################################

  sim$chosen_actions <- tibble(

    agent_id = rep(agent_characteristics$agent_id, each=2),
    action_type = rep(c("actions_overall", "actions_send"), sim$no_agents),
    best_action = rep(c("Not assigned"), length(sim$no_agents*2))

  ) %>% data.table()
  sim$chosen_actions[ , agent_id := as.integer(agent_id) ]
  
  ######################################### 
  #### CONSTRUCT OVERALL_ACTIONS TABLE ####
  #########################################

  sim$actions_overall <- tibble(

    agent_id = rep(agent_characteristics$agent_id, each=4),
    actions = rep(c("Send", "Receive", "Both", "Nothing"), sim$no_agents),
    util_score = rep(0, length(sim$no_agents*4))

  ) %>%
    data.table()

  sim$actions_overall[ actions == "Both", util_score := 1 ] 
  sim$actions_overall <- dcast(sim$actions_overall, 
	  agent_id ~ actions, 
	  value.var = "util_score")
  sim$actions_overall[ , agent_id := as.character(agent_id)] 
  sim$actions_overall[, best_action :=  names(sim$actions_overall[ , -c("agent_id")])[apply(sim$actions_overall[ , -c("agent_id") ], 1, which.max)]] 
  sim$actions_overall <- melt( sim$actions_overall,
	  id.vars = c("agent_id", "best_action"),
          measure.vars = c("Send", "Receive", "Both", "Nothing"),
          variable.name = "actions",
          value.name = "util_score" )
  sim$actions_overall[ , agent_id := as.integer(agent_id) ]
  
  #######################################################
  #### ASSIGN BEST OVERALL ACTIONS TO CHOSEN ACTIONS ####
  #######################################################

  sim$chosen_actions <-  merge(copy(sim$actions_overall)[ actions == best_action , .(agent_id, best_action) ], sim$chosen_actions, by=c("agent_id"), all.x=TRUE, all.y=TRUE) 
  sim$chosen_actions[ action_type == "actions_overall" , best_action := ifelse(!is.na(best_action.x), best_action.x,  "NOT ASSIGNED")]
  sim$chosen_actions[ , -c("best_action.x", "best_action.y")]

  #############################################
  #### CONSTRUCT TABLE FOR SENDING ACTIONS ####
  #############################################

  sim$actions_send <- tibble(

    agent_id = rep(agent_characteristics$agent_id, each=2),
    actions = rep(c("Unoptimized", "Optimized"), no_agents),
    util_score = rep(0, length(actions))

  ) %>%  data.table() 
 sim$actions_send[ , agent_id := as.integer(agent_id) ]
  
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
  sim$messages_temp <- sim$environment %>%
    activate(edges) %>%
    as_tibble() %>%
    data.table()
  setnames(sim$messages_temp, old = c("from", "to"), new = c("sender", "receiver"))
  sim$messages <- rbind(sim$messages_temp, sim$messages)
  sim$messages <- sim$messages[copy(sim$agent_characteristics)[, .(agent_id, opinion)], nomatch = 0L, on = c("sender" = "agent_id")]
  setnames(sim$messages, "opinion", "opinion_sender")

  ###################################
  #### PRODUCE POSSIBLE MESSAGES ####
  ###################################

  assumption_receiver <- runif(nrow(sim$messages), 0, 1)
  sim$messages <- cbind(sim$messages, assumption_receiver)

  sim$message_matrix <- outer(sim$messages$opinion_sender, sim$messages$assumption_receiver, produce_altered_message) 
  row.names(sim$message_matrix) <- sim$messages[ , sender]
  colnames(sim$message_matrix) <- sim$messages[ , receiver]
  
  sim$messages_temp <- data.table(copy(sim$message_matrix)) 
  sim$messages_temp[ , sender := as.numeric(row.names(sim$message_matrix))] 
  sim$messages_temp <- melt( sim$messages_temp,
	  id.vars = c("sender"),
          measure.vars = as.character(seq(1, sim$no_agents, 1)),
          variable.name = "receiver",
          value.name = "opt_message" )
  sim$messages_temp[ receiver != sender]
  setkey(sim$messages_temp, "sender") 
  sim$messages_temp <- unique(sim$messages_temp) 
  sim$messages_temp[ , sender := as.integer(sender)] 
  sim$messages_temp[ , receiver := as.integer(receiver)] 
  sim$messages <- sim$messages_temp[copy(unique(sim$messages)), on=c("sender", "receiver"), nomatch = 0L] 
  sim$messages[ , opt_message := median(opt_message), by = .(sender)] 

  ##########################################
  #### CONSTRUCT DISCOURSE MEMORY TABLE ####
  ##########################################

  sim$discourse_memory <- copy(sim$messages)[ , -c("receiver", "assumption_receiver") ] 
  sim$discourse_memory <- unique(sim$discourse_memory) 
  sim$discourse_memory[ , past_opinions := mapply(function(x) {list(
		    ifelse(rnorm(params(sim)$rc_energy_model$opinion_memory_depth, x, params(sim)$rc_energy_model$initial_opinion_confidence) > 1, 1,
			    ifelse(rnorm(params(sim)$rc_energy_model$opinion_memory_depth, x, params(sim)$rc_energy_model$initial_opinion_confidence) < 0, 0, rnorm(params(sim)$rc_energy_model$opinion_memory_depth, x, params(sim)$rc_energy_model$initial_opinion_confidence) ) ) )  }, x = opinion_sender )] 
  setnames(sim$discourse_memory, "opinion_sender", "opinion") 
  sim$discourse_memory <- sim$discourse_memory[ , .(sender, opinion, past_opinions)] 
  sim$discourse_memory[ , past_opinions := as.character(past_opinions) ]
  sim$discourse_memory <- unique(sim$discourse_memory)
  sim$discourse_memory[ , past_opinions := mapply(function(x) list(eval(parse(text = x))), x=past_opinions )] 
  sim$discourse_memory[ , sender_business := 0 ] 
  sim$discourse_memory[ , receiver_business := 0 ]
  
   if( length(sim$actions_overall[ best_action %in% c("Both", "Send") , best_action ] > 0 ) ) {

    #######################################
    #### DETERMINE BEST SENDING ACTION ####
    #######################################

    sim$actions_send_temp <- copy(sim$messages)[copy(sim$actions_overall)[best_action==actions], on = c("sender" = "agent_id"), nomatch = 0L ] 
    sim$actions_send_temp <- sim$actions_send_temp[ (best_action == "Both" | best_action == "Send") ]
    sim$actions_send_temp <- sim$actions_send_temp[ , .(sender , receiver , opt_message , opinion_sender , assumption_receiver) ]
    sim$actions_send_temp <- unique(sim$actions_send_temp)
    sim$actions_send <- sim$actions_send_temp[copy(sim$actions_send), on = c("sender" = "agent_id"), nomatch = 0L, allow.cartesian = TRUE ] 
    sim$actions_send <- sim$actions_send[copy(sim$discourse_memory), on = c("sender"), nomatch = 0L] 
    sim$actions_send[, distance_to_past_opinions := mapply(function(a,b,c,k) {
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
      }, a=past_opinions, b=opt_message, c=opinion_sender, k=actions)] 
    sim$actions_send[, distance_message_opinion := mapply(function(a,b,k) {
        switch(k,
               "Unoptimized" = {
                 abs(a - a)
               },
               "Optimized" = {
                 abs(a - b)
               }
        )
      }, a=opinion_sender, b=opt_message, k=actions)] 
    sim$actions_send[, distance_message_assumption := mapply(function(a,b,c,k) {
        switch(k,
               "Unoptimized" = {
                 abs(c - a)
               },
               "Optimized" = {
                 abs(c - b)
               }
        )
      }, a=opinion_sender, b=opt_message, c=assumption_receiver, k=actions)]
    sim$actions_send[ , past_opinions := NULL ][ , receiver := NULL ] 
    sim$actions_send[ , util_score := 0 - distance_to_past_opinions - distance_message_opinion - distance_message_assumption] 
    sim$actions_send[ , agent_id := sender ] 
    sim$actions_send[ , .(agent_id, actions, util_score)]
    setkey(sim$actions_send, "agent_id") 
    sim$actions_send <- unique(sim$actions_send)
    sim$actions_send[, util_score := sum(util_score), by=c("agent_id", "actions")] 
    sim$actions_send <- unique(sim$actions_send) 
    sim$actions_send <- dcast(sim$actions_send,
	    agent_id ~ actions, value.var = "util_score") 
    sim$actions_send[, best_action :=  ifelse(Optimized > Unoptimized, "Optimized", "Unoptimized")] 
    sim$actions_send <- melt( sim$actions_send,
	    id.vars = c("agent_id", "best_action"),
            measure.vars = c("Optimized", "Unoptimized"),
            variable.name = "actions",
            value.name = "util_score" )
    sim$actions_send[ , agent_id := as.integer(agent_id) ]

    #####################################
    #### UPDATE CHOSEN_ACTIONS TABLE ####
    #####################################

    sim$chosen_actions <- merge(copy(sim$actions_send)[ actions == best_action , .(agent_id, best_action) ], sim$chosen_actions, by="agent_id", all.x=TRUE, all.y=TRUE) 
    sim$chosen_actions[ best_action.y == "Receive" , action_type := "actions_overall" ] 
    sim$chosen_actions[ best_action.y == "Nothing" , action_type := "actions_overall" ] 
    sim$chosen_actions[ action_type == "actions_send" , best_action := ifelse(!is.na(best_action.x), best_action.x, "NOT ASSIGNED")] 
    sim$chosen_actions[ action_type == "actions_overall" , best_action := ifelse(!is.na(best_action.y), best_action.y, "NOT ASSIGNED")]
    sim$chosen_actions[ , -c("best_action.x", "best_action.y")]
    
    #################################
    #### UPDATE DISCOURSE_MEMORY ####
    #################################

    sim$discourse_memory_temp <- copy(sim$actions_send)[ , -c("util_score")][ actions==best_action ][copy(sim$messages), on = c("agent_id" = "sender"), nomatch = 0L]
    setnames(sim$discourse_memory_temp,"agent_id", "sender")
    setkey(sim$discourse_memory_temp, "sender")
    sim$discourse_memory_temp[ , actions := NULL ] 
    sim$discourse_memory_temp <- unique(sim$discourse_memory_temp) 
    sim$discourse_memory_temp[ , sender_business := .N, by = "sender" ]
    sim$discourse_memory_temp[ , receiver_business := .N, by = "receiver" ]
    sim$discourse_memory <- merge(sim$discourse_memory_temp, sim$discourse_memory[ , -c("sender_business", "receiver_business", "assumption_receiver")], by=c("sender"), all.x=TRUE)
    sim$discourse_memory[ , sender_business := ifelse(is.na(sender_business), 0, sender_business) ] 
    sim$discourse_memory[ , receiver_business := ifelse(is.na(receiver_business), 0, receiver_business) ] 
    sim$discourse_memory[ , past_messages := mapply(function(x,y,z) {

	      ifelse(x == "Unoptimized",
		      y,
		      z)

				   }, x=best_action, y=opinion_sender, z=opt_message) ]
    sim$discourse_memory[ , past_opinions := ifelse(lengths(past_opinions) < params(sim)$rc_energy_model$opinion_memory_depth,
                                   mapply(function(x, y) {
                                     list(c(unlist(x), y))
                                   }, x=past_opinions, y=opinion_sender),
                                   mapply(function(x, y) {
                                     list(c(unlist(x)[1:params(sim)$rc_energy_model$opinion_memory_depth], y))
                                   }, x=past_opinions, y=opinion_sender)
      )] 
    setnames(sim$discourse_memory, "opinion_sender", "opinion")
    setnames(sim$discourse_memory, "opt_message", "message") 
    sim$discourse_memory <- sim$discourse_memory[ , .(sender, receiver, opinion, message, past_messages, past_opinions, sender_business, receiver_business)]
    sim$discourse_memory[ , past_messages := as.character(past_messages) ]
    sim$discourse_memory[ , past_opinions := as.character(past_opinions) ] 
    sim$discourse_memory <- unique(sim$discourse_memory) 
    sim$discourse_memory[ , past_messages := mapply(function(x) list(eval(parse(text = x))), x=past_messages)] 
    sim$discourse_memory[ , past_opinions := mapply(function(x) list(eval(parse(text = x))), x=past_opinions)]

    temp <- copy(sim$discourse_memory)[ , .(receiver, receiver_business)] 
    temp[ , receiver_business := max(receiver_business), by=receiver]
    temp <- unique(temp) 

    sim$discourse_memory <- merge(copy( sim$discourse_memory)[ , -c("receiver_business")], temp, by.x = c("sender"), by.y = c("receiver"), all.x = TRUE) 
    sim$discourse_memory[ , receiver := NULL ] 
    sim$discourse_memory[ , past_receiver_business := ifelse( !is.na(receiver_business), mapply(function(x) list(x), x=receiver_business ), 0 ) ]
    sim$discourse_memory[ , past_sender_business := ifelse( !is.na(sender_business), mapply(function(x) list(x), x=sender_business ), 0 ) ] 
    sim$discourse_memory[ , nbh_incohesion := vec_get_nbh_incohesion(sender ) ] 
    sim$discourse_memory[ , past_nbh_incohesion := ifelse( !is.na(nbh_incohesion), sapply(nbh_incohesion, function(x) list(x) ), 0 ) ]
    sim$discourse_memory[ , self_incohesion := vec_get_self_incohesion( sender ) ]
    sim$discourse_memory[ , past_self_incohesion := ifelse( !is.na(self_incohesion), sapply(self_incohesion, function(x) list(x) ), 0 ) ] 
    sim$discourse_memory[ , past_messages := as.character(past_messages) ] 
    sim$discourse_memory[ , past_opinions := as.character(past_opinions) ] 
    sim$discourse_memory[ , past_receiver_business := as.character(past_receiver_business) ]
    sim$discourse_memory[ , past_sender_business := as.character(past_sender_business) ] 
    sim$discourse_memory[ , past_nbh_incohesion := as.character(past_nbh_incohesion) ] 
    sim$discourse_memory[ , past_self_incohesion := as.character(past_self_incohesion) ] 
    sim$discourse_memory <- unique(sim$discourse_memory) 
    sim$discourse_memory[ , past_messages := mapply(function(x) list(eval(parse(text = x))), x=past_messages)] 
    sim$discourse_memory[ , past_opinions := mapply(function(x) list(eval(parse(text = x))), x=past_opinions  )] 
    sim$discourse_memory[ , past_receiver_business := mapply(function(x) list(eval(parse(text = x))), x=past_receiver_business)]
    sim$discourse_memory[ , past_sender_business := mapply(function(x) list(eval(parse(text = x))), x=past_sender_business)] 
    sim$discourse_memory[ , past_nbh_incohesion := mapply(function(x) list(eval(parse(text = x))), x=past_nbh_incohesion)] 
    sim$discourse_memory[ , past_self_incohesion := mapply(function(x) list(eval(parse(text = x))), x=past_self_incohesion)] 
    
    #########################
    #### "SEND" MESSAGES ####
    #########################

    sim$messages_temp <- unique(copy(sim$messages)) 
    setnames(sim$messages_temp, old = c("sender", "receiver"), new = c("receiver", "sender")) 
    sim$messages_temp <- sim$messages_temp[ , .(receiver, opinion_sender, opt_message)] 
    sim$messages_temp[ , opinion_receiver := opinion_sender]
    sim$messages_temp[ , opinion_sender := NULL ] 
    sim$messages <- unique(sim$messages_temp)[sim$messages[ , -c("opt_message") ], on="receiver", nomatch=0L] 

    sim$messages <- copy(sim$messages)[sim$actions_send[ , .(agent_id, actions, best_action)][ best_action == actions ], nomatch=0L, on=c("sender" = "agent_id")] 
    sim$messages[ , assumption_sender := ifelse( best_action == "Unoptimized", opinion_sender, opt_message)] 
    sim$messages[ , actions := NULL ][ , best_action := NULL ]
    setkey(sim$messages, "sender")
    sim$messages <- unique(sim$messages)

    ###################
    #### RECEIVING ####
    ###################
    
 merge_receiver_opinions <- copy(sim$discourse_memory)[ , .(sender, past_opinions)]
 merge_receiver_opinions[ , past_opinions := as.character(past_opinions) ]
 merge_receiver_opinions <- unique(merge_receiver_opinions)
 merge_receiver_opinions[ , past_opinions := mapply(function(x) list(eval(parse(text = x))), x=past_opinions ) ]

 merge_sender_messages <- copy(sim$discourse_memory)[ , .(sender, past_messages)]
 merge_sender_messages[ , past_messages := as.character(past_messages) ]
 merge_sender_messages <- unique(merge_sender_messages)
 merge_sender_messages[ , past_messages := mapply(function(x) list(eval(parse(text = x))), x=past_messages ) ]
 
 sim$opinion_updating <- unique(copy(sim$messages)[ , -c("actions", "best_action")])[copy(actions_overall)[ best_action == actions ], on = c("receiver" = "agent_id"), nomatch = 0L ] 
 sim$opinion_updating <- sim$opinion_updating[ best_action == actions ] 
 sim$opinion_updating <- sim$opinion_updating[ (best_action == "Both" | best_action == "Receive") ]
 sim$opinion_updating <- sim$opinion_updating[ , .(sender, receiver, opinion_receiver, assumption_sender, opt_message, actions, best_action)]
 sim$opinion_updating <- sim$opinion_updating[merge_receiver_opinions, on=c("receiver"="sender"), nomatch = 0L] 
 sim$opinion_updating <- sim$opinion_updating[ best_action == actions ] 
 sim$opinion_updating[ , distance_to_past_opinions := mapply(function(a,b) {
        mean(
          sapply(a, function(x) {
            abs(x - b)
          })
        )
      }, a=past_opinions, b=assumption_sender)] 
 sim$opinion_updating[ , within_epsilon := abs(opinion_receiver - assumption_sender) < params(sim)$rc_energy_model$epsilon]
 sim$opinion_updating[ , self_consistent := distance_to_past_opinions < params(sim)$rc_energy_model$self_incons_tolerance]
 sim$opinion_updating <- sim$opinion_updating[ within_epsilon == TRUE & self_consistent == TRUE ] 
 sim$opinion_updating[ , sum_assumptions := sum(assumption_sender), by=receiver] 
 sim$opinion_updating[ , denominator := .N, by=receiver ]
 sim$opinion_updating[ , reception_energy_loss := .N, by=receiver ] 
 sim$opinion_updating[ , opinion_receiver_new := ifelse( denominator != 0, sum_assumptions / denominator, opinion_receiver )]
 setnames(sim$opinion_updating, "receiver", "agent_id") 
 sim$opinion_updating <- sim$opinion_updating[ , .(agent_id, opinion_receiver_new)]
 setkey(sim$opinion_updating, "agent_id")
 sim$opinion_updating <-unique(sim$opinion_updating) 

 ######################################
 #### UPDATE agent_characteristics ####
 ######################################

 # merge with sim$opinion_updating, assign new opinion if applicable
 sim$agent_characteristics <- merge(sim$agent_characteristics, sim$opinion_updating, by="agent_id", all.x = TRUE) 
 sim$agent_characteristics[ , opinion := ifelse(is.na(opinion_receiver_new), opinion, opinion_receiver_new)] 
 sim$agent_characteristics[ , opinion_receiver_new := NULL ]
 print(names(sim$agent_characteristics))
 sim$agent_characteristics_temp <- copy(sim$actions_overall)[ , best_axn_overall := best_action][ , .(agent_id, best_axn_overall)] 
 sim$agent_characteristics_temp <- unique(sim$agent_characteristics_temp) 
 sim$agent_characteristics_temp <- merge(sim$agent_characteristics_temp, copy(sim$actions_send)[ , best_axn_send := best_action][ , .(agent_id, best_axn_send)], by="agent_id")
 sim$agent_characteristics_temp <- unique(sim$agent_characteristics_temp) 
 sim$agent_characteristics_temp <- merge(sim$agent_characteristics_temp, copy(sim$discourse_memory)[ , .(sender, receiver_business, sender_business)], by.x = "agent_id", by.y = "sender") 
 sim$agent_characteristics_temp <- unique(sim$agent_characteristics_temp)
 sim$agent_characteristics_temp[ best_axn_overall == "Send" , energy_loss := sender_business ]
 sim$agent_characteristics_temp[ best_axn_overall == "Receive", energy_loss := receiver_business ]
 sim$agent_characteristics_temp[ best_axn_overall == "Both", energy_loss := sender_business + receiver_business ]
 sim$agent_characteristics_temp[ best_axn_overall == "Nothing", energy_loss := 0 ]
 sim$agent_characteristics_temp[ best_axn_send == "Unoptimized", energy_loss := energy_loss ] 
 sim$agent_characteristics_temp[ best_axn_send == "Optimized", energy_loss := energy_loss + sender_business ] 
 sim$agent_characteristics_temp <- sim$agent_characteristics_temp[ , .(agent_id, energy_loss) ]
 sim$agent_characteristics_temp <- unique(sim$agent_characteristics_temp) 
 sim$agent_characteristics_temp[ , energy_loss := sum(energy_loss), by="agent_id" ] 
 sim$agent_characteristics_temp <- unique(sim$agent_characteristics_temp) 
 print(names(sim$agent_characteristics))
 sim$agent_characteristics <- sim$agent_characteristics_temp[sim$agent_characteristics, on="agent_id"]
 sim$agent_characteristics[ , energy := energy - energy_loss + params(sim)$rc_energy_model$restoration_factor ]
 sim$agent_characteristics[ , energy_loss := NULL ]

 ##################################################
 #### BUSINESS UPDATE FOR sim$discourse_memory ####
 ##################################################

 sim$discourse_memory <- sim$agent_characteristics[ , .(agent_id, opinion) ][sim$discourse_memory[ , -c("opinion")], on=c("agent_id" = "sender") ]  
 setnames(sim$discourse_memory, "agent_id", "sender") 

  } else {
    
    #################################################
    #### UPDATING OF ENERGY IN CASE OF NO ACTION ####
    #################################################

  sim$discourse_memory[ , receiver_business := 0 ] 
  sim$discourse_memory[ , sender_business := 0 ] 
  sim$discourse_memory[ , past_receiver_business := ifelse(lengths(past_receiver_business) < params(sim)$rc_energy_model$energy_params_memory_depth,
                                            mapply(function(x, y) {
                                              list(c(unlist(x), y))
                                            }, x=past_receiver_business, y=receiver_business),
                                            mapply(function(x, y) {
                                              list(c(unlist(x)[1:params(sim)$rc_energy_model$energy_params_memory_depth], y))
                                            }, x=past_receiver_business, y=receiver_business)
      )] 
  sim$discourse_memory[ , past_sender_business := ifelse(lengths(past_sender_business) < params(sim)$rc_energy_model$energy_params_memory_depth,
                                          mapply(function(x, y) {
                                            list(c(unlist(x), y))
                                          }, x=past_sender_business, y=sender_business),
                                          mapply(function(x, y) {
                                            list(c(unlist(x)[1:params(sim)$rc_energy_model$energy_params_memory_depth], y))
                                          }, x=past_sender_business, y=sender_business)
      )]

  sim$agent_characteristics[ , energy := energy + params(sim)$rc_energy_model$restoration_factor ]
      

  }

  return(invisible(sim))

}

rc_energy_modelStep <- function(sim) {

  print(time(sim))

  ########################
  #### REBUILD TABLES ####
  ########################

  # tables that are rebuilt at the start of each round: sim$actions_send, sim$actions_overall, sim$messages

  sim$actions_send <- data.table(
    
    agent_id = rep(sim$agent_characteristics$agent_id, each=2),
    actions = rep(c("Unoptimized", "Optimized"), sim$no_agents),
    util_score = rep(0, length(sim$no_agents*2))
    
  ) 
  sim$actions_send[ , agent_id := as.integer(agent_id) ]
  
  sim$actions_overall <- data.table(

    agent_id = rep(sim$agent_characteristics$agent_id, each=4),
    actions = rep(c("Send", "Receive", "Both", "Nothing"), sim$no_agents),
    util_score = rep(0, length(sim$no_agents*4))

  ) 


  # 1.)
  sim$messages_temp <- sim$environment %>%
    activate(edges) %>%
    as_tibble() %>%
    data.table() 
  setnames(sim$messages_temp, old = c("from", "to"), new = c("receiver", "sender"))

  # 2.)
  sim$messages <- sim$environment %>%
    activate(edges) %>%
    as_tibble() %>%
    data.table() 
  setnames(sim$messages, old = c("from", "to"), new = c("sender", "receiver")) 
  sim$messages <- rbind(sim$messages_temp, sim$messages) 
  sim$messages <- sim$messages[copy(sim$agent_characteristics)[, .(agent_id, opinion)], nomatch = 0L, on = c("sender" = "agent_id") ]
  setnames(sim$messages, "opinion", "opinion_sender")
print(nrow(sim$messages))
  ######################################## 
  #### CONSTRUCT CHOSEN_ACTIONS TABLE ####
  ########################################

  sim$chosen_actions <- data.table(

    agent_id = rep(agent_characteristics$agent_id, each=2),
    action_type = rep(c("actions_overall", "actions_send"), sim$no_agents),
    best_action = rep(c("Not assigned"), length(sim$no_agents*2))

  ) 
  sim$chosen_actions[ , agent_id := as.integer(agent_id) ]
  
  #################################################################
  #### BUILD sim$actions_overall AND FIND BEST OVERALL ACTIONS ####
  #################################################################

  sim$actions_overall_temp <- copy(sim$messages)[ , .(sender, receiver)]
  sim$actions_overall_temp[ , max_send_energy_loss := .N, by = sender ] 
  sim$actions_overall_temp[ , max_receive_energy_loss := .N, by = sender ] 
  sim$actions_overall_temp <- sim$actions_overall_temp[copy(sim$discourse_memory), on=c("sender") ] 
  sim$actions_overall_temp <- sim$actions_overall_temp[unique(copy(sim$discourse_memory)[, .(sender, message)][ , assumption_receiver := message][ , -c("message")]), on=c("receiver"="sender"), nomatch=0L] 
  sim$actions_overall_temp[ , distance_to_past_opinions := mapply(function(a,b) {
        mean(
          sapply(a, function(x) {
            abs(x - b)
          })
        )
      }, a=past_opinions, b=assumption_receiver)] 
  sim$actions_overall_temp[ , past_messages := as.character(past_messages) ] 
  sim$actions_overall_temp[ , past_opinions := as.character(past_opinions) ] 
  sim$actions_overall_temp[ , past_receiver_business := as.character(past_receiver_business) ] 
  sim$actions_overall_temp[ , past_sender_business := as.character(past_sender_business) ]
  sim$actions_overall_temp[ , past_nbh_incohesion := as.character(past_nbh_incohesion) ] 
  sim$actions_overall_temp[ , past_self_incohesion := as.character(past_self_incohesion) ] 
  sim$actions_overall_temp <- unique(sim$actions_overall_temp)
  sim$actions_overall_temp[ , past_messages := mapply(function(x) list(eval(parse(text = x))), x=past_messages)] 
  sim$actions_overall_temp[ , past_opinions := mapply(function(x) list(eval(parse(text = x))), x=past_opinions )] 
  sim$actions_overall_temp[ , past_receiver_business := mapply(function(x) list(eval(parse(text = x))), x=past_sender_business)] 
  sim$actions_overall_temp[ , past_sender_business := mapply(function(x) list(eval(parse(text = x))), x=past_receiver_business)] 
  sim$actions_overall_temp[ , past_nbh_incohesion := mapply(function(x) list(eval(parse(text = x))), x=past_nbh_incohesion)] 
  sim$actions_overall_temp[ , past_self_incohesion := mapply(function(x) list(eval(parse(text = x))), x=past_self_incohesion)] 
  sim$actions_overall_temp <- sim$actions_overall_temp[ , .(distance_to_past_opinions, sender, past_self_incohesion, past_nbh_incohesion, sender_business, receiver_business, past_receiver_business, past_sender_business, nbh_incohesion, self_incohesion, max_send_energy_loss, max_receive_energy_loss)] 
  sim$actions_overall_temp[ , past_receiver_business := as.character(past_receiver_business) ] 
  sim$actions_overall_temp[ , past_sender_business := as.character(past_sender_business) ] 
  sim$actions_overall_temp[ , past_nbh_incohesion := as.character(past_nbh_incohesion) ]
  sim$actions_overall_temp[ , past_self_incohesion := as.character(past_self_incohesion) ]
  sim$actions_overall_temp <- unique(sim$actions_overall_temp) 
  sim$actions_overall_temp[ , past_receiver_business := mapply(function(x) list(eval(parse(text = x))), x=past_receiver_business)]
  sim$actions_overall_temp[ , past_sender_business := mapply(function(x) list(eval(parse(text = x))), x=past_sender_business)] 
  sim$actions_overall_temp[ , past_nbh_incohesion := mapply(function(x) list(eval(parse(text = x))), x=past_nbh_incohesion)] 
  sim$actions_overall_temp[ , past_self_incohesion := mapply(function(x) list(eval(parse(text = x))), x=past_self_incohesion)] 
  sim$actions_overall_temp <- sim$actions_overall_temp[ , .(distance_to_past_opinions, sender, past_self_incohesion, past_nbh_incohesion, sender_business, receiver_business, past_receiver_business, past_sender_business, nbh_incohesion, self_incohesion, max_send_energy_loss, max_receive_energy_loss)] 
  sim$actions_overall_temp[ , control := mean(distance_to_past_opinions) , by=sender ] 
  sim$actions_overall_temp <- sim$actions_overall_temp[(sim$agent_characteristics[ , .(agent_id, energy)] %>% unique()), on=c("sender" = "agent_id") ] 
  sim$actions_overall_temp[ , rec_business_mean_index := mapply(function(x, y) {

      is_na <- sum(unlist(x)) / (y*length(unlist(x)))
      
      ifelse(is.na(is_na), 0, is_na)

    }, x=past_receiver_business, y=max_receive_energy_loss )] 
  sim$actions_overall_temp[ , send_business_mean_index := mapply(function(x, y) {

      is_na <- sum(unlist(x)) / (y*length(unlist(x)))
      
      ifelse(is.na(is_na), 0, is_na)

    }, x=past_sender_business, y=max_send_energy_loss )] 
  sim$actions_overall_temp[ , rec_business_mean := mapply(function(x, y) {
     
series <- rev(unlist(x))
new_series <- vector()

      for (i in 1:length(x)) {

	      if (runif(1, 0, 1) > i**2 / params(sim)$rc_energy_model$opinion_memory_depth) {
	      new_series <- c(new_series, series[i])
      }
      }
      ifelse(is.na(mean(new_series)), y, new_series)

    }, x=past_receiver_business, y=receiver_business )] 
  sim$actions_overall_temp[ , send_business_mean := mapply(function(x, y) {

series <- rev(unlist(x))
new_series <- vector()

      for (i in 1:length(x)) {

	      if (runif(1, 0, 1) > i**2 / params(sim)$rc_energy_model$opinion_memory_depth) {
	      new_series <- c(new_series, series[i])
      }
      }
      ifelse(is.na(mean(new_series)), y, new_series)

    }, x=past_sender_business, y=sender_business )] 
  sim$actions_overall_temp[ , both_business_mean := mapply(function(x, y, a, b) {

series <- rev(unlist(x))
new_series_one <- vector()

      for (i in 1:length(x)) {

	      if (runif(1, 0, 1) > i**2 / params(sim)$rc_energy_model$opinion_memory_depth) {
	      new_series_one <- c(new_series_one, series[i])
      }
      }

      mean_one <- ifelse(is.na(mean(new_series_one)), a, new_series_one)

series <- rev(unlist(y))
new_series_two <- vector()

      for (i in 1:length(y)) {

	      if (runif(1, 0, 1) > i**2 / params(sim)$rc_energy_model$opinion_memory_depth) {
	      new_series_two <- c(new_series_two, series[i])
      }
      }
      mean_two <- ifelse(is.na(mean(new_series_two)), b, new_series_two)

      mean( mean_one, mean_two )

    }, x=past_sender_business, y=past_receiver_business, a=sender_business, b=receiver_business )] 
  sim$actions_overall_temp[ , both_business_mean_index := mapply(function(x, y, z, a) {

      numerator <- sum( c(unlist(x), unlist(y)) )

      denominator <- z*length(unlist(y)) + a*length(unlist(x))

      is_na <- numerator / denominator
      
      ifelse(is.na(is_na), 0, is_na)

    }, x=past_sender_business, y=past_receiver_business, z = max_receive_energy_loss, a = max_send_energy_loss )] 
  sim$actions_overall <- sim$actions_overall_temp[sim$actions_overall[ , -c("best_action")], on=c("sender" = "agent_id"), allow.cartesian = TRUE] 
  setnames(sim$actions_overall, "sender", "agent_id") 
  sim$actions_overall[ actions == "Send" , energy_loss := ( energy - sender_business )] 
  sim$actions_overall[ actions == "Receive" , energy_loss := ( energy - receiver_business )] 
  sim$actions_overall[ actions == "Both" , energy_loss := ( energy - sender_business - receiver_business )] 
  sim$actions_overall[ actions == "Send" , projected_energy := ifelse(energy_loss <= 0, 0, energy_loss) / ( energy + params(sim)$rc_energy_model$restoration_factor ) ] 
  sim$actions_overall[ actions == "Receive" , projected_energy := ifelse(energy_loss <= 0, 0, energy_loss) / ( energy + params(sim)$rc_energy_model$restoration_factor ) ] 
  sim$actions_overall[ actions == "Both" , projected_energy := ifelse(energy_loss <= 0, 0, energy_loss) / ( energy + params(sim)$rc_energy_model$restoration_factor ) ]  
  sim$actions_overall[ , projected_energy := ifelse(is.na(projected_energy), 0, projected_energy) ] 
  sim$actions_overall[ , dissonance := mapply(function(x, y) {
	    
	    sum(x, y)
	   
    }, x=nbh_incohesion, y=self_incohesion)  ] 
  sim$actions_overall[ actions == "Send", util_score :=
         ifelse(projected_energy <= 0, 0, projected_energy) + ( 1 - self_incohesion) ] 
  sim$actions_overall[ actions == "Receive", util_score := 
         ifelse(projected_energy <= 0, 0, projected_energy) + ( 1 - nbh_incohesion ) ] 
  sim$actions_overall[ actions == "Both", util_score :=  
         ifelse(projected_energy <= 0, 0, projected_energy) + control + ( ( self_incohesion + nbh_incohesion ) * ( 1 - abs( self_incohesion - nbh_incohesion) ) ) ] 
  sim$actions_overall[ actions == "Nothing", util_score := ifelse( ( energy - sender_business <= 0 | energy - receiver_business <= 0) ,
	    max(util_score)+10000, min(util_score)-10000) ] 
  sim$actions_overall[ , past_self_incohesion := NULL ][ , past_nbh_incohesion := NULL ][ , past_receiver_business := NULL ][, past_sender_business := NULL ] 
  sim$actions_overall <- unique(sim$actions_overall) 
  sim$actions_overall <- dcast(sim$actions_overall,
	  agent_id ~ actions, 
	  value.var = "util_score", 
	  fun.aggregate = sum) 
  sim$actions_overall[ , agent_id := as.character(agent_id)] 
  sim$actions_overall[, best_action :=  names( sim$actions_overall[ , -c("agent_id")] )[ unlist(apply(sim$actions_overall[ , -c("agent_id") ], 1, which.max)) ] ] 
  sim$actions_overall <- melt( sim$actions_overall,
	  id.vars = c("agent_id", "best_action"),
          measure.vars = c("Send", "Receive", "Both", "Nothing"),
          variable.name = "actions",
          value.name = "util_score" ) 
  sim$actions_overall[ , agent_id := as.integer(agent_id) ]

  # define lists

  sim$bothers <- unique(sim$actions_overall[ best_action == "Both" ]$agent_id)
  sim$senders <- unique(sim$actions_overall[ best_action == "Send" ]$agent_id)
  sim$receivers <- unique(sim$actions_overall[ best_action == "Receive" ]$agent_id)
  sim$nothingers <- unique(sim$actions_overall[ best_action == "Nothing" ]$agent_id)

  ###################################
  #### UPDATE sim$chosen_actions ####
  ###################################

  sim$chosen_actions <- merge(copy(sim$actions_overall)[ actions == best_action , .(agent_id, best_action) ], sim$chosen_actions, by=c("agent_id"), all.x=TRUE, all.y=TRUE) 
  sim$chosen_actions[ is.na(action_type) , action_type := "actions_overall" ] 
  sim$chosen_actions[ action_type == "actions_overall" , best_action := ifelse(!is.na(best_action.x), best_action.x,  "NOT ASSIGNED")] 
  sim$chosen_actions[ , best_action.x := NULL ][ , best_action.y := NULL ]

  ##############################################################
  #### UPDATE sim$messages WITH ASSUMPTIONS ABOUT NEIGHBORS ####
  ##############################################################

  sim$messages <- merge(setnames(unique(sim$discourse_memory[ , .(sender, message) ]), "sender", "receiver"), unique(sim$messages), by = c("receiver") )
  setnames(sim$messages, "message", "assumption_receiver") 

  ####################################
  #### PRODUCE OPTIMIZED MESSAGES ####
  ####################################

  sim$message_matrix <- outer(sim$messages$opinion_sender, sim$messages$assumption_receiver, produce_altered_message) 
  row.names(sim$message_matrix) <- sim$messages[, sender]
  colnames(sim$message_matrix) <- sim$messages[, receiver]

  sim$messages_temp <- data.table(copy(sim$message_matrix)) 
  sim$messages_temp[ , sender := as.numeric(row.names(sim$message_matrix))] 
  sim$messages_temp <- melt( sim$messages_temp,
	  id.vars = c("sender"),
          measure.vars = as.character(seq(1, sim$no_agents, 1)),
          variable.name = "receiver",
          value.name = "opt_message" ) 
  sim$messages_temp <- sim$messages_temp[ receiver != sender] 
  setkey(sim$messages_temp, "sender") 
  sim$messages_temp <- unique(sim$messages_temp)
  sim$messages_temp[ , sender := as.integer(sender)] 
  sim$messages_temp[ , receiver := as.integer(receiver)] 
  sim$messages <- sim$messages_temp[copy(unique(sim$messages)), on=c("sender", "receiver"), nomatch = 0L] 
  sim$messages[ , opt_message := median(opt_message), by = "sender" ] 
  
  #######################################################
  #### UPDATE sim$discourse_memory WITH NEW OPINIONS ####
  #######################################################

  sim$discourse_memory <- copy(sim$discourse_memory)[ , opinion := NULL ][sim$agent_characteristics[ , .(agent_id, opinion) ], on=c("sender" = "agent_id")] 
  sim$discourse_memory[ , past_opinions := ifelse(lengths(past_opinions) < params(sim)$rc_energy_model$opinion_memory_depth,
                                 mapply(function(x, y) {
                                   list(c(unlist(x), y))
                                 }, x=past_opinions, y=opinion),
                                 mapply(function(x, y) {
                                   list(c(unlist(x)[1:params(sim)$rc_energy_model$opinion_memory_depth], y))
                                 }, x=past_opinions, y=opinion)
    )] 
  sim$discourse_memory[ ,assumption_receiver := NULL ][ , opt_message := NULL ]

  if( length(c(sim$bothers, sim$senders)) > 0 )  {
   
    ###############################################################
    #### COMPUTE SENDING UTILITIES AND CHOOSE MAX UTIL ACTIONS ####
    ###############################################################

    sim$actions_send_temp <- copy(sim$messages)[ ( sender %in% sim$senders | sender %in% sim$bothers ) ]
    sim$actions_send_temp <- sim$actions_send_temp[ , .(sender, receiver , opt_message , opinion_sender , assumption_receiver)] 
    sim$actions_send <- sim$actions_send_temp[copy(sim$actions_send), on = c("sender" = "agent_id"), nomatch = 0L, allow.cartesian = TRUE ] 
    sim$actions_send <- sim$actions_send[copy(sim$discourse_memory), on = c("sender"), nomatch = 0L ] 
    sim$actions_send[, distance_to_past_opinions := mapply(function(a,b,c,k) {
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
      }, a=past_opinions, b=opt_message, c=opinion_sender, k=actions)] 
    sim$actions_send[, distance_to_past_messages := mapply(function(a,b,c,k) {
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
      }, a=past_messages, b=opt_message, c=opinion_sender, k=actions)] 
    sim$actions_send[, distance_message_opinion := mapply(function(a,b,k) {
        switch(k,
               "Unoptimized" = {
                 abs(a - a)
               },
               "Optimized" = {
                 abs(a - b)
               }
        )
      }, a=opinion_sender, b=opt_message, k=actions)] 
    sim$actions_send[, distance_message_assumption := mapply(function(a,b,c,k) {
        switch(k,
               "Unoptimized" = {
                 abs(c - a)
               },
               "Optimized" = {
                 abs(c - b)
               }
        )
      }, a=opinion_sender, b=opt_message, c=assumption_receiver, k=actions)] 
    sim$actions_send[ , past_opinions := NULL ][ , receiver := NULL ] 
    sim$actions_send[ , util_score := 0 - distance_to_past_messages - distance_to_past_opinions - distance_message_opinion - distance_message_assumption] 
    setnames(sim$actions_send, "sender", "agent_id")
    sim$actions_send <- sim$actions_send[, .(agent_id, actions, util_score) ] 
    setkey(sim$actions_send, "agent_id") 
    sim$actions_send <- unique(sim$actions_send) 
    sim$actions_send[, util_score := sum(util_score), by=c("agent_id", "actions")] 
    sim$actions_send <- unique(sim$actions_send) 
    sim$actions_send <- dcast(sim$actions_send,
	    agent_id ~ actions, value.var = "util_score") 
    sim$actions_send[, best_action :=  ifelse(Optimized > Unoptimized, "Optimized", "Unoptimized")] 
    sim$actions_send <- melt( sim$actions_send,
	    id.vars = c("agent_id", "best_action"),
            measure.vars = c("Optimized", "Unoptimized"),
            variable.name = "actions",
            value.name = "util_score" ) 
    sim$actions_send[ , agent_id := as.integer(agent_id) ]

    ###################################
    #### UPDATE sim$chosen_actions ####
    ###################################

    sim$chosen_actions <- merge(copy(sim$actions_send)[ actions == best_action , .(agent_id, best_action) ], sim$chosen_actions, by="agent_id", all.x=TRUE , all.y=TRUE) 
    sim$chosen_actions[ best_action.y == "Receive" , action_type := "actions_overall" ] 
    sim$chosen_actions[ best_action.y == "Nothing" , action_type := "actions_overall" ] 
    sim$chosen_actions[ action_type == "actions_send" , best_action := ifelse(!is.na(best_action.x), best_action.x, "NOT ASSIGNED")] 
    sim$chosen_actions[ action_type == "actions_overall" , best_action := ifelse(!is.na(best_action.y), best_action.y, "NOT ASSIGNED")] 
    sim$chosen_actions[ , best_action.x := NULL ][ , best_action.y := NULL ]

    print(table(sim$chosen_actions$best_action))

    #####################################
    #### UPDATE sim$discourse_memory ####
    #####################################

    sim$discourse_memory_temp <- copy(sim$actions_send)[best_action == actions ][ , -c("util_score")][copy(sim$messages), on = c("agent_id" = "sender"), nomatch = 0L]  
    setnames(sim$discourse_memory_temp, "agent_id", "sender") 
    setkey(sim$discourse_memory_temp, "sender") 
    sim$discourse_memory_temp <- sim$discourse_memory_temp[ actions==best_action ] 
    sim$discourse_memory_temp[ , actions := NULL ]  
    sim$discourse_memory_temp <-  unique(sim$discourse_memory_temp)  
    sim$discourse_memory_temp[ , sender_business := .N, by = "sender" ] 
    sim$discourse_memory_temp[ , receiver_business := .N, by = "receiver" ] 
    sim$discourse_memory_temp[ , opinion_sender := NULL ] 
    sim$discourse_memory <- merge(sim$discourse_memory_temp, sim$discourse_memory[ , -c("sender_business", "receiver_business", "assumption_receiver")], by=c("sender"), all.x=TRUE, all.y=TRUE) 
    sim$discourse_memory[ , sender_business := ifelse(is.na(sender_business), 0, sender_business) ] 
    sim$discourse_memory[ , receiver_business := ifelse(is.na(receiver_business), 0, receiver_business) ] 
    sim$discourse_memory[ , past_messages := ifelse( is.na(best_action),
            past_messages,
            ifelse(lengths(past_messages) < params(sim)$rc_energy_model$message_memory_depth,
                                               ifelse( best_action == "Unoptimized",
                                          mapply(function(x, y) {
                                            list(c(unlist(x), y))
                                          }, x=past_messages, y=opinion),
                                          mapply(function(x, y) {
                                            list(c(unlist(x), y))
                                          }, x=past_messages, y=opt_message)),
                                   ifelse( best_action == "Unoptimized",
                                          mapply(function(x, y) {
                                            list(c(unlist(x)[1:params(sim)$rc_energy_model$message_memory_depth], y))
                                          }, x=past_messages, y=opinion),
                                          mapply(function(x, y) {
                                            list(c(unlist(x)[1:params(sim)$rc_energy_model$message_memory_depth], y))
                                          }, x=past_messages, y=opt_message)
                                   )
                                 )
      )] 
    sim$discourse_memory[ , message := ifelse(is.na(opt_message), message, opt_message) ] 
    sim$discourse_memory[ , opt_message := NULL ]
    sim$discourse_memory <- sim$discourse_memory[ , .(sender, receiver, opinion, message, past_messages, past_opinions, sender_business, receiver_business, past_sender_business, past_receiver_business, past_nbh_incohesion, past_self_incohesion)] 
    sim$discourse_memory[ , past_messages := as.character(past_messages) ] 
    sim$discourse_memory[ , past_opinions := as.character(past_opinions) ] 
    sim$discourse_memory[ , past_receiver_business := as.character(past_receiver_business) ] 
    sim$discourse_memory[ , past_sender_business := as.character(past_sender_business) ] 
    sim$discourse_memory <- unique(sim$discourse_memory) 
    sim$discourse_memory[ , past_messages := sapply(past_messages, function(x) list(eval(parse(text = x))))] 
    sim$discourse_memory[ , past_opinions := sapply(past_opinions, function(x) list(eval(parse(text = x))))] 
    sim$discourse_memory[ , past_receiver_business := sapply(past_receiver_business, function(x) list(eval(parse(text = x))))]
    sim$discourse_memory[ , past_sender_business := sapply(past_sender_business, function(x) list(eval(parse(text = x))))] 

    temp <- copy(sim$discourse_memory)[ , .(receiver, receiver_business)] 
    temp <- temp[ !is.na(receiver) ] 
    temp[ , receiver_business := max(receiver_business), by=receiver] 
    temp <- unique(temp) 
   
    sim$discourse_memory <- merge(copy( sim$discourse_memory)[ , -c("receiver_business")], temp, by.x = c("sender"), by.y = c("receiver"), all.x = TRUE, all.y = TRUE)   
    sim$discourse_memory[ , -c("receiver") ] 
    sim$discourse_memory[ , nbh_incohesion := vec_get_nbh_incohesion(sender) ] 
    sim$discourse_memory[ , self_incohesion := vec_get_self_incohesion( sender ) ] 
    sim$discourse_memory[ , past_sender_business := ifelse(lengths(past_sender_business) < params(sim)$rc_energy_model$energy_params_memory_depth,
                                          mapply(function(x, y) {
                                            list(c(unlist(x), y))
                                          }, x=past_sender_business, y=sender_business),
                                          mapply(function(x, y) {
                                            list(c(unlist(x)[1:params(sim)$rc_energy_model$energy_params_memory_depth], y))
                                          }, x=past_sender_business, y=sender_business)
      )] 
    sim$discourse_memory[ , past_nbh_incohesion := ifelse(lengths(past_nbh_incohesion) < params(sim)$rc_energy_model$energy_params_memory_depth,
                                         mapply(function(x, y) {
                                           list(c(unlist(x), y))
                                         }, x=past_nbh_incohesion, y=nbh_incohesion),
                                         mapply(function(x, y) {
                                           list(c(unlist(x)[1:params(sim)$rc_energy_model$opinion_memory_depth], y))
                                         }, x=past_nbh_incohesion, y=nbh_incohesion)
      )] 
    sim$discourse_memory[ , past_self_incohesion := ifelse(lengths(past_self_incohesion) < params(sim)$rc_energy_model$energy_params_memory_depth,
                                          mapply(function(x, y) {
                                            list(c(unlist(x), y))
                                          }, x=past_self_incohesion, y=self_incohesion),
                                          mapply(function(x, y) {
                                            list(c(unlist(x)[1:params(sim)$rc_energy_model$opinion_memory_depth], y))
                                          }, x=past_self_incohesion, y=self_incohesion)
      )] 
    sim$discourse_memory[ , receiver := NULL ]
    sim$discourse_memory[ , past_messages := as.character(past_messages) ] 
    sim$discourse_memory[ , past_opinions := as.character(past_opinions) ] 
    sim$discourse_memory[ , past_receiver_business := as.character(past_receiver_business) ] 
    sim$discourse_memory[ , past_sender_business := as.character(past_sender_business) ] 
    sim$discourse_memory[ , past_nbh_incohesion := as.character(past_nbh_incohesion) ] 
    sim$discourse_memory[ , past_self_incohesion := as.character(past_self_incohesion) ] 
    sim$discourse_memory <- unique(sim$discourse_memory)
    sim$discourse_memory[ , past_messages := mapply(function(x) list(eval(parse(text = x))), x=past_messages)] 
    sim$discourse_memory[ , past_opinions := mapply(function(x) list(eval(parse(text = x))), x= past_opinions)] 
    sim$discourse_memory[ , past_receiver_business := mapply(function(x) list(eval(parse(text = x))), x = past_receiver_business)] 
    sim$discourse_memory[ , past_sender_business := mapply(function(x) list(eval(parse(text = x))), x=past_sender_business)] 
    sim$discourse_memory[ , past_nbh_incohesion := mapply(function(x) list(eval(parse(text = x))), x=past_nbh_incohesion)] 
    sim$discourse_memory[ , past_self_incohesion := mapply(function(x) list(eval(parse(text = x))), x=past_self_incohesion)]

    sim$messages_temp <- unique(copy(sim$messages)) 
    setnames(sim$messages_temp, old = c("sender", "receiver"), new = c("receiver", "sender")) 
    sim$messages_temp <- sim$messages_temp[ , .(receiver, opinion_sender, opt_message)] 
    sim$messages_temp[ , opinion_receiver := opinion_sender] 
    sim$messages_temp[ , opinion_sender := NULL ] 
    sim$messages <- unique(sim$messages_temp)[sim$messages[ , -c("opt_message") ], on="receiver", nomatch=0L] 

    sim$messages <- copy(sim$messages)[sim$actions_send[ , .(agent_id, actions, best_action)][ best_action == actions ], nomatch=0L, on=c("sender" = "agent_id")]  
    sim$messages[ , assumption_sender := ifelse( best_action == "Unoptimized", opinion_sender, opt_message)] 
    sim$messages[ , actions := NULL ][ , best_action := NULL ] 
    setkey(sim$messages, "sender") 
    sim$messages <- unique(sim$messages)


    # Receiving

if ( length(c(sim$bothers, sim$receivers)) > 0) {

    merge_receiver_opinions <- copy(sim$discourse_memory)[ , .(sender, past_opinions)]
    merge_receiver_opinions[ , past_opinions := as.character(past_opinions) ]
    merge_receiver_opinions <- unique(merge_receiver_opinions)
    merge_receiver_opinions[ , past_opinions := mapply(function(x) list(eval(parse(text = x))), x=past_opinions ) ]

    merge_sender_messages <- copy(sim$discourse_memory)[ , .(sender, past_messages)]
    merge_sender_messages[ , past_messages := as.character(past_messages) ]
    merge_sender_messages <- unique(merge_sender_messages)
    merge_sender_messages[ , past_messages := mapply(function(x) list(eval(parse(text = x))), x=past_messages ) ]

    sim$opinion_updating <- copy(sim$messages)[ ( receiver %in% sim$bothers | receiver %in% sim$senders | !( receiver %in% sim$nothingers) )  ]
    sim$opinion_updating <- unique(sim$opinion_updating) 
    sim$opinion_updating <- sim$opinion_updating[ , .(sender, receiver, opinion_receiver, assumption_sender, opt_message )] 
    sim$opinion_updating <- sim$opinion_updating[merge_receiver_opinions, on=c("receiver"="sender"), nomatch = 0L] 
    sim$opinion_updating <- sim$opinion_updating[merge_sender_messages, on=c("sender"), nomatch = 0L] 
    sim$opinion_updating[ , distance_to_past_opinions := mapply(function(a,b) {
        mean(
          sapply(a, function(x) {
            abs(x - b)
          })
        )
      }, a=past_opinions, b=assumption_sender)] 
    sim$opinion_updating[ , distance_to_past_messages := mapply(function(a,b) {
	     max(
		sapply(a, function(x) {
		      abs(x - b)
		   })
	   )
      }, a=past_messages, b=assumption_sender)] 
    sim$opinion_updating[ , trust := distance_to_past_messages < params(sim)$rc_energy_model$other_incons_tolerance] 
    sim$opinion_updating[ , within_epsilon := abs(opinion_receiver - assumption_sender) < params(sim)$rc_energy_model$epsilon] 
    sim$opinion_updating[ , self_consistent := distance_to_past_opinions < params(sim)$rc_energy_model$self_incons_tolerance] 
    sim$opinion_updating <- sim$opinion_updating[ within_epsilon == TRUE & self_consistent == TRUE & trust == TRUE] 
    sim$opinion_updating[ , sum_assumptions := sum(assumption_sender), by=receiver] 
    sim$opinion_updating[ , denominator := .N, by=receiver ] 
    sim$opinion_updating[ , reception_energy_loss := .N, by=receiver ] 
    sim$opinion_updating[ , opinion_receiver_new := ifelse( denominator != 0, sum_assumptions / denominator, opinion_receiver )] 
    setnames(sim$opinion_updating, "receiver", "agent_id") 
    sim$opinion_updating <- sim$opinion_updating[ , .(agent_id, opinion_receiver_new)] 
    setkey(sim$opinion_updating, "agent_id") 
    sim$opinion_updating <- unique(sim$opinion_updating) 

    if( length(sim$nothingers) > 0  ) {

          sim$opinion_updating_n <- copy(sim$discourse_memory)[ sender %in% sim$nothingers ] 
	  sim$opinion_updating_n[ , opinion_receiver_new := unlist(sapply(past_opinions, function(x) { median(x) })) ] 
	  sim$opinion_updating_n <- sim$opinion_updating_n[ , .(sender, opinion_receiver_new)] 
	  setnames(sim$opinion_updating_n, "sender", "agent_id")
	  setkey(sim$opinion_updating_n, "agent_id") 
	  sim$opinion_updating_n <- unique(sim$opinion_updating_n) 
	  sim$opinion_updating <- rbind(sim$opinion_updating_n, sim$opinion_updating)

    }

    sim$agent_characteristics <- merge(sim$agent_characteristics, sim$opinion_updating, by="agent_id", all.x = TRUE) 
    sim$agent_characteristics[ , opinion := ifelse(is.na(opinion_receiver_new), opinion, opinion_receiver_new)] 
    sim$agent_characteristics[ , opinion_receiver_new := NULL ]

    sim$agent_characteristics$energy[is.na(sim$agent_characteristics$energy)] <- 0

    sim$agent_characteristics_temp <- unique(copy(sim$actions_overall)[ , best_axn_overall := best_action][ , .(agent_id, best_axn_overall)]) 
    sim$agent_characteristics_temp <- merge(sim$agent_characteristics_temp, copy(sim$actions_send)[ , best_axn_send := best_action][ , .(agent_id, best_axn_send)], by="agent_id") 
    sim$agent_characteristics_temp <- unique(sim$agent_characteristics_temp) 
    sim$agent_characteristics_temp <- merge(sim$agent_characteristics_temp, copy(sim$discourse_memory)[ , .(sender, receiver_business, sender_business)], by.x = "agent_id", by.y = "sender", all = TRUE) 
    sim$agent_characteristics_temp <- unique(sim$agent_characteristics_temp) 
    sim$agent_characteristics_temp[ best_axn_overall == "Send" , energy_loss := sender_business ] 
    sim$agent_characteristics_temp[ best_axn_overall == "Receive", energy_loss := receiver_business ] 
    sim$agent_characteristics_temp[ best_axn_overall == "Both", energy_loss := sender_business + receiver_business ] 
    sim$agent_characteristics_temp[ best_axn_overall == "Nothing", energy_loss := 0 ] 
    sim$agent_characteristics_temp[ best_axn_send == "Unoptimized", energy_loss := energy_loss ] 
    sim$agent_characteristics_temp[ best_axn_send == "Optimized", energy_loss := energy_loss + sender_business ] 
    sim$agent_characteristics_temp <- sim$agent_characteristics_temp[ , .(agent_id, energy_loss) ] 
    sim$agent_characteristics_temp <- unique(sim$agent_characteristics_temp) 
    sim$agent_characteristics_temp[ , energy_loss := sum(energy_loss), by="agent_id" ] 
    sim$agent_characteristics_temp <- unique(sim$agent_characteristics_temp) 
    sim$agent_characteristics <- sim$agent_characteristics_temp[sim$agent_characteristics, on="agent_id"] 
    sim$agent_characteristics[ , energy_loss := ifelse(is.na(energy_loss), 0, energy_loss) ] 
    sim$agent_characteristics[ , energy := energy - energy_loss + params(sim)$rc_energy_model$restoration_factor ] 
    sim$agent_characteristics[ , energy_loss := NULL ] 

    sim$agent_characteristics$energy[is.na(sim$agent_characteristics$energy)] <- 0

    sim$discourse_memory[ , receiver_business := ifelse(is.na(receiver_business), 0, receiver_business) ]
    sim$discourse_memory[ , past_receiver_business := ifelse(lengths(past_receiver_business) < params(sim)$rc_energy_model$energy_params_memory_depth,
                                            mapply(function(x, y) {
                                              list(c(unlist(x), y))
                                            }, x=past_receiver_business, y=receiver_business),
                                            mapply(function(x, y) {
                                              list(c(unlist(x)[1:params(sim)$rc_energy_model$energy_params_memory_depth], y))
                                            }, x=past_receiver_business, y=receiver_business)
      )] 
    sim$discourse_memory[ , past_messages := as.character(past_messages) ] 
    sim$discourse_memory[ , past_opinions := as.character(past_opinions) ] 
    sim$discourse_memory[ , past_receiver_business := as.character(past_receiver_business) ] 
    sim$discourse_memory[ , past_sender_business := as.character(past_sender_business) ] 
    sim$discourse_memory[ , past_nbh_incohesion := as.character(past_nbh_incohesion) ] 
    sim$discourse_memory[ , past_self_incohesion := as.character(past_self_incohesion) ] 
    sim$discourse_memory <- unique(sim$discourse_memory)
    sim$discourse_memory[ , past_messages := mapply(function(x) list(eval(parse(text = x))), x=past_messages)] 
    sim$discourse_memory[ , past_opinions := mapply(function(x) list(eval(parse(text = x))), x= past_opinions)] 
    sim$discourse_memory[ , past_receiver_business := mapply(function(x) list(eval(parse(text = x))), x = past_receiver_business)] 
    sim$discourse_memory[ , past_sender_business := mapply(function(x) list(eval(parse(text = x))), x=past_sender_business)] 
    sim$discourse_memory[ , past_nbh_incohesion := mapply(function(x) list(eval(parse(text = x))), x=past_nbh_incohesion)] 
    sim$discourse_memory[ , past_self_incohesion := mapply(function(x) list(eval(parse(text = x))), x=past_self_incohesion)]

    sim$discourse_memory <- sim$agent_characteristics[ , .(agent_id, opinion) ][sim$discourse_memory[ , -c("opinion")], on=c("agent_id" = "sender") ] 
    setnames(sim$discourse_memory, "agent_id", "sender") 

      } else {

    sim$discourse_memory_temp <- copy(sim$messages) 
    sim$discourse_memory_temp[ , receiver_business := .N, by=receiver ] 
    sim$discourse_memory_temp[ , sender := NULL ] 
    setnames(sim$discourse_memory_temp, "receiver", "sender") 
    sim$discourse_memory_temp <- sim$discourse_memory_temp[ , .(sender, receiver_business) ] 
    sim$discourse_memory_temp <- unique(sim$discourse_memory_temp) 
    sim$discourse_memory <- sim$discourse_memory_temp[copy(sim$discourse_memory), on=c("sender")] 
    sim$discourse_memory[ , past_receiver_business := ifelse(lengths(past_receiver_business) < params(sim)$rc_energy_model$energy_params_memory_depth,
                                            mapply(function(x, y) {
                                              list(c(unlist(x), y))
                                            }, x=past_receiver_business, y=receiver_business),
                                            mapply(function(x, y) {
                                              list(c(unlist(x)[1:params(sim)$rc_energy_model$energy_params_memory_depth], y))
                                            }, x=past_receiver_business, y=receiver_business)
      )] 


	  sim$opinion_updating <- copy(sim$actions_overall) 
	  sim$opinion_updating <- sim$opinion_updating[ best_action == actions ] 
	  sim$opinion_updating <- sim$opinion_updating[ (best_action == "Nothing" | best_action == "Send") ] 
          sim$opinion_updating <- sim$opinion_updating[copy(sim$agent_characteristics), on=c("agent_id"), nomatch = 0L] 
          sim$opinion_updating <- sim$opinion_updating[ best_action == actions ] 
	  sim$opinion_updating[ , opinion_receiver_new := opinion ] 
	  sim$opinion_updating <- sim$opinion_updating[ , .(agent_id, opinion_receiver_new)] 
	  setkey(sim$opinion_updating, "agent_id") 
	  unique(sim$opinion_updating) 

   sim$agent_characteristics <- merge(sim$agent_characteristics, sim$opinion_updating, by="agent_id", all.x = TRUE) 
   sim$agent_characteristics[ , opinion := ifelse(is.na(opinion_receiver_new), opinion, opinion_receiver_new)] 
   sim$agent_characteristics[ , opinion_receiver_new := NULL ]

    sim$agent_characteristics_temp <- unique(copy(sim$actions_overall)[ , best_axn_overall := best_action][ , .(agent_id, best_axn_overall)]) 
    sim$agent_characteristics_temp <- merge(sim$agent_characteristics_temp, copy(sim$actions_send)[ , best_axn_send := best_action][ , .(agent_id, best_axn_send)], by="agent_id") 
    sim$agent_characteristics_temp <- unique(sim$agent_characteristics_temp) 
    sim$agent_characteristics_temp <- merge(sim$agent_characteristics_temp, copy(sim$discourse_memory)[ , .(sender, receiver_business, sender_business)], by.x = "agent_id", by.y = "sender", all = TRUE) 
    sim$agent_characteristics_temp <- unique(sim$agent_characteristics_temp) 
    sim$agent_characteristics_temp[ best_axn_overall == "Send" , energy_loss := sender_business ] 
    sim$agent_characteristics_temp[ best_axn_overall == "Receive", energy_loss := receiver_business ] 
    sim$agent_characteristics_temp[ best_axn_overall == "Both", energy_loss := sender_business + receiver_business ] 
    sim$agent_characteristics_temp[ best_axn_overall == "Nothing", energy_loss := 0 ] 
    sim$agent_characteristics_temp[ best_axn_send == "Unoptimized", energy_loss := energy_loss ] 
    sim$agent_characteristics_temp[ best_axn_send == "Optimized", energy_loss := energy_loss + sender_business ] 
    sim$agent_characteristics_temp <- sim$agent_characteristics_temp[ , .(agent_id, energy_loss) ] 
    sim$agent_characteristics_temp <- unique(sim$agent_characteristics_temp) 
    sim$agent_characteristics_temp[ , energy_loss := sum(energy_loss), by="agent_id" ] 
    sim$agent_characteristics_temp <- unique(sim$agent_characteristics_temp) 
    sim$agent_characteristics <- sim$agent_characteristics_temp[sim$agent_characteristics, on="agent_id"] 
    sim$agent_characteristics[ , energy_loss := ifelse(is.na(energy_loss), 0, energy_loss) ] 
    sim$agent_characteristics[ , energy := energy - energy_loss + params(sim)$rc_energy_model$restoration_factor ] 
    sim$agent_characteristics[ , energy_loss := NULL ]

      }
    
  } else {
print("this activates")
    sim$discourse_memory[ , receiver_business := 0 ] 
    sim$discourse_memory[ , sender_business := 0 ] 
    sim$discourse_memory[ , past_receiver_business := ifelse(lengths(past_receiver_business) < params(sim)$rc_energy_model$energy_params_memory_depth,
                                            mapply(function(x, y) {
                                              list(c(unlist(x), y))
                                            }, x=past_receiver_business, y=receiver_business),
                                            mapply(function(x, y) {
                                              list(c(unlist(x)[1:params(sim)$rc_energy_model$energy_params_memory_depth], y))
                                            }, x=past_receiver_business, y=receiver_business)
      )] 
    sim$discourse_memory[ , past_sender_business := ifelse(lengths(past_sender_business) < params(sim)$rc_energy_model$energy_params_memory_depth,
                                          mapply(function(x, y) {
                                            list(c(unlist(x), y))
                                          }, x=past_sender_business, y=sender_business),
                                          mapply(function(x, y) {
                                            list(c(unlist(x)[1:params(sim)$rc_energy_model$energy_params_memory_depth], y))
                                          }, x=past_sender_business, y=sender_business)
      )]

sim$opinion_updating <- copy(sim$discourse_memory) 
sim$opinion_updating[ , opinion_receiver_new := mapply(function(x, y) { 
     
series <- rev(unlist(x))
new_series <- vector()

      for (i in 1:length(x)) {

	      if (runif(1, 0, 1) > i**2 / params(sim)$rc_energy_model$opinion_memory_depth) {
	      new_series <- c(new_series, series[i])
      }
      }
      ifelse(is.na(median(new_series)), y, new_series)

       }, x=past_opinions, y=opinion ) ] 
setnames(sim$opinion_updating, "sender", "agent_id") 
sim$opinion_updating <- sim$opinion_updating[ , .(agent_id, opinion_receiver_new)] 
setkey(sim$opinion_updating, "agent_id") 
sim$opinion_updating <- unique(sim$opinion_updating)

   sim$agent_characteristics <- merge(sim$agent_characteristics, sim$opinion_updating, by="agent_id", all.x = TRUE) 
   sim$agent_characteristics[ , opinion := ifelse(is.na(opinion_receiver_new), opinion, opinion_receiver_new)] 
   sim$agent_characteristics[ , opinion_receiver_new := NULL ]

    sim$agent_characteristics_temp <- unique(copy(sim$actions_overall)[ , best_axn_overall := best_action][ , .(agent_id, best_axn_overall)])
    sim$agent_characteristics_temp <- merge(sim$agent_characteristics_temp, copy(sim$discourse_memory)[ , .(sender, receiver_business, sender_business)], by.x = "agent_id", by.y = "sender", all = TRUE) 
    sim$agent_characteristics_temp <- unique(sim$agent_characteristics_temp) 
    sim$agent_characteristics_temp[ best_axn_overall == "Send" , energy_loss := sender_business ] 
    sim$agent_characteristics_temp[ best_axn_overall == "Receive", energy_loss := receiver_business ] 
    sim$agent_characteristics_temp[ best_axn_overall == "Both", energy_loss := sender_business + receiver_business ] 
    sim$agent_characteristics_temp[ best_axn_overall == "Nothing", energy_loss := 0 ] 
    sim$agent_characteristics_temp <- sim$agent_characteristics_temp[ , .(agent_id, energy_loss) ] 
    sim$agent_characteristics_temp <- unique(sim$agent_characteristics_temp) 
    sim$agent_characteristics_temp[ , energy_loss := sum(energy_loss), by="agent_id" ] 
    sim$agent_characteristics_temp <- unique(sim$agent_characteristics_temp) 
    sim$agent_characteristics <-  sim$agent_characteristics_temp[sim$agent_characteristics, on="agent_id"] 
    sim$agent_characteristics[ , energy_loss := ifelse(is.na(energy_loss), 0, energy_loss) ] 
    sim$agent_characteristics[ , energy := energy - energy_loss + params(sim)$rc_energy_model$restoration_factor ] 
    sim$agent_characteristics[ , energy_loss := NULL ]
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

  nbh_assumptions <- sim$messages[ , .(sender, receiver, assumption_receiver) ][ sender == id & receiver %in% nbh_indices , assumption_receiver ]

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

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

  sim$agent_characteristics <- data.table(sim$agent_characteristics, key="agent_id") 
  sim$agent_characteristics[ , energy := params(sim)$rc_sh_model$energy_level] 
  sim$agent_characteristics[ , group := sample(c(1:params(sim)$rc_sh_model$no_groups), size=sim$no_agents, replace=TRUE) ]  
  sim$agent_characteristics[ , expert := sample(c(0,1), size=params(sim)$basic_setup$no_agents, replace=TRUE, prob=c((1-params(sim)$rc_sh_model$expert_percentage), params(sim)$rc_sh_model$expert_percentage)) ] 
  sim$agent_characteristics[ , op_compl := rlogitnorm(0, params(sim)$rc_sh_model$sigma_complexity, params(sim)$basic_setup$no_agents) ]
  
  ######################################## 
  #### CONSTRUCT CHOSEN_ACTIONS TABLE ####
  ########################################

  sim$chosen_actions <- data.table(

    agent_id = rep(sim$agent_characteristics$agent_id, each=3),
    action_type = rep(c("actions_overall", "actions_send", "actions_receive"), sim$no_agents),
    best_action = rep(c("Not assigned"), length(sim$no_agents*3))

  ) 
  setkey(sim$chosen_actions, agent_id)
  
  ######################################### 
  #### CONSTRUCT OVERALL_ACTIONS TABLE ####
  #########################################

  sim$actions_overall <- data.table(

    agent_id = rep(sim$agent_characteristics$agent_id, each=4),
    actions = rep(c("Send", "Receive", "Both", "Nothing"), sim$no_agents),
    util_score = rep(0, length(sim$no_agents*4))

  )
  sim$actions_overall[ .("Both") , util_score := 1, on="actions" ] 
  sim$actions_overall <- dcast(sim$actions_overall, agent_id ~ actions, value.var = "util_score") 
  sim$actions_overall[, best_action :=  names(sim$actions_overall[ , -c("agent_id")])[apply(sim$actions_overall[ , -c("agent_id") ], 1, which.max)]] 
  sim$actions_overall <- melt( sim$actions_overall,
	  id.vars = c("agent_id", "best_action"),
          measure.vars = c("Send", "Receive", "Both", "Nothing"),
          variable.name = "actions",
          value.name = "util_score" ) 
  setkey(sim$actions_overall, agent_id, best_action)
  sim$actions_overall <- sim$actions_overall[ , .SD[1], .SDcols=c("best_action"), by=agent_id ]
  setkey(sim$actions_overall, agent_id)
  
  
  ########################################### 
  #### CONSTRUCT RECEIVING_ACTIONS TABLE ####
  ###########################################

  sim$actions_receive <- data.table(

    agent_id = rep(sim$agent_characteristics$agent_id, each=2),
    actions = rep(c("Systematic", "Heuristic"), sim$no_agents),
    util_score = rep(0, length(sim$no_agents*2))

  ) 
  setkey(sim$actions_receive, agent_id)

  sim$bothers <- sim$actions_overall[ "Both", .SD[1], by="agent_id", on="best_action" ]$agent_id
  sim$senders <- sim$actions_overall[ "Send", .SD[1], by="agent_id", on="best_action" ]$agent_id
  sim$receivers <- sim$actions_overall[ "Receiver", .SD[1], by="agent_id", on="best_action" ]$agent_id
  sim$nothingers <- sim$actions_overall[ "Nothing", .SD[1], by="agent_id", on="best_action" ]$agent_id

  #######################################################
  #### ASSIGN BEST OVERALL ACTIONS TO CHOSEN ACTIONS ####
  #######################################################

  sim$chosen_actions <- merge(sim$actions_overall, sim$chosen_actions, by=c("agent_id"), all.x=TRUE, all.y=TRUE ) 
  sim$chosen_actions[ .("actions_overall") , best_action := ifelse(!is.na(best_action.x), best_action.x,  "NOT ASSIGNED"), on="action_type"] 
  sim$chosen_actions[ , best_action.x := NULL ][ , best_action.y := NULL ]

  #############################################
  #### CONSTRUCT TABLE FOR SENDING ACTIONS ####
  #############################################

  sim$actions_send <- data.table(

    agent_id = rep(agent_characteristics$agent_id, each=4),
    actions = rep(c("Unoptimized", "Optimized", "Unoptimized_appeal", "Optimized_appeal"), sim$no_agents),
    util_score = rep(0, length(sim$no_agents*4))

  )
  setkey(sim$actions_send, agent_id) 
  
  #################################
  #### CONSTRUCT MESSAGE TABLE ####
  #################################

  # 1.)
  sim$messages <- sim$environment %>%
    activate(edges) %>%
    as_tibble() %>%
    data.table() 
  setnames(sim$messages, old = c("from", "to"), new = c("receiver", "sender"))

  # 2.)
  sim$messages_temp <- sim$environment %>%
    activate(edges) %>%
    as_tibble() %>%
    data.table() 
  setnames(sim$messages_temp, old = c("from", "to"), new = c("sender", "receiver")) 
  sim$messages <- rbind(sim$messages_temp, sim$messages) 
  setkey(sim$messages, sender, receiver)
  sim$messages <- sim$messages[sim$agent_characteristics[, .(agent_id, opinion)], nomatch = 0L, on = c("sender" = "agent_id")] 
  setnames(sim$messages, "opinion", "opinion_sender")
  setkey(sim$messages, "sender", "receiver")

  ###################################
  #### PRODUCE POSSIBLE MESSAGES ####
  ###################################

  assumption_receiver <- runif(nrow(sim$messages), 0, 1)
  sim$messages <- cbind(sim$messages, assumption_receiver)

  sim$message_matrix <- outer(sim$messages$opinion_sender, sim$messages$assumption_receiver, produce_altered_message) 
  row.names(sim$message_matrix) <- sim$messages[ , sender]
  colnames(sim$message_matrix) <- sim$messages[ , receiver]
  
  sim$messages_temp <- data.table(sim$message_matrix) 
  sim$messages_temp[ , sender := as.numeric(row.names(sim$message_matrix))] 
  sim$messages_temp <- melt( sim$messages_temp,
	  id.vars = c("sender"),
          measure.vars = as.character(seq(1, sim$no_agents, 1)),
          variable.name = "receiver",
          value.name = "opt_message" ) 
  sim$messages_temp <- sim$messages_temp[ receiver != sender ] 
  sim$messages_temp <- sim$messages_temp[ , receiver := as.integer(receiver) ]
  sim$messages_temp <- sim$messages_temp[ , sender := as.integer(sender) ]
  setkey(sim$messages_temp, "sender") 
  sim$messages_temp <- unique(sim$messages_temp) 
  sim$messages <- sim$messages_temp[sim$messages, on=c("sender", "receiver"), nomatch = 0L] 
  sim$messages[ , opt_message := median(opt_message), by = .(sender)] 
  sim$messages <- merge(sim$messages, sim$agent_characteristics[ , .(agent_id, op_compl) ], by.x=c("sender"), by.y=c("agent_id")) 
  sim$messages[ , msg_compl := op_compl ]

  ##########################################
  #### CONSTRUCT DISCOURSE MEMORY TABLE ####
  ##########################################

  sim$discourse_memory <- sim$messages[ , -c("receiver", "assumption_receiver") ][ , .SD[1], by="sender"]
  sim$discourse_memory[ , past_opinions := sapply(opinion_sender, function(x) {list(
		    ifelse(rnorm(params(sim)$rc_sh_model$opinion_memory_depth, x, params(sim)$rc_sh_model$initial_opinion_confidence) > 1, 1,
			    ifelse(rnorm(params(sim)$rc_sh_model$opinion_memory_depth, x, params(sim)$rc_sh_model$initial_opinion_confidence) < 0, 0, rnorm(params(sim)$rc_sh_model$opinion_memory_depth, x, params(sim)$rc_sh_model$initial_opinion_confidence) ) ) )  } )] 
  setnames(sim$discourse_memory, "opinion_sender", "opinion") 
  sim$discourse_memory <- sim$discourse_memory[ , .(sender, opinion, past_opinions, op_compl)] 
  sim$discourse_memory[ , past_op_compls := sapply(op_compl, function(x) {list(x)} ) ] 
  sim$discourse_memory[ , sender_business := 0 ] 
  sim$discourse_memory[ , receiver_business := 0 ]
  sim$discourse_memory <- sim$discourse_memory[ !duplicated(sim$discourse_memory[ , as.logical(lapply(sim$discourse_memory, function(x) !is.list(x) ) ) , with = FALSE ]) , ]
  setkey(sim$discourse_memory, sender)

   if( length(c(bothers, senders)) > 0 ) {

    #######################################
    #### DETERMINE BEST SENDING ACTION ####
    #######################################

    sim$actions_send_temp <- sim$messages[sim$actions_overall, on = c("sender" = "agent_id"), nomatch = 0L ] 
    sim$actions_send_temp <- sim$actions_send_temp[ , .(sender , receiver , opt_message , opinion_sender , assumption_receiver) ] 
    sim$actions_send <- sim$actions_send_temp[sim$actions_send, on = c("sender" = "agent_id"), nomatch = 0L , allow.cartesian = TRUE ] 
    sim$actions_send <- sim$actions_send[sim$discourse_memory, on = c("sender"), nomatch = 0L] 
    sim$actions_send[ , involvement := sapply( past_op_compls, function(x) mean(x) )  ] 
    sim$actions_send[ , msg_compl := mapply(function(a, b, k) {
        switch(k,
               "Unoptimized" = {
                 b + ifelse(mean(c(unlist(a)) - b) > 0, mean(c(unlist(a)) - b), 0) 
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
      }, a=past_op_compls, b=op_compl, k=actions)] 
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
      }, a=past_opinions, b=opt_message, c=opinion_sender, k=actions)]
    sim$actions_send[, distance_message_opinion := mapply(function(a,b,k) {
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
      }, a=opinion_sender, b=opt_message, k=actions)] 
    sim$actions_send[, distance_message_assumption := mapply(function(a,b,c,k) {
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
      }, a=opinion_sender, b=opt_message, c=assumption_receiver, k=actions)] 
    sim$actions_send[ , distance_message_assumption := sum(distance_message_assumption), by=sender]
    sim$actions_send <- sim$actions_send[ , .(sender, actions, util_score, distance_to_past_opinions, distance_message_opinion, distance_message_assumption, msg_compl, involvement)]
    sim$actions_send <- unique(sim$actions_send)
    sim$actions_send[ .("Unoptimized") , util_score := 0 - distance_to_past_opinions - distance_message_opinion - distance_message_assumption - msg_compl + involvement, on="actions"] 
    sim$actions_send[ .("Unoptimized_appeal"), util_score := 0 - distance_to_past_opinions - distance_message_opinion - distance_message_assumption + msg_compl - involvement, on="actions"] 
    sim$actions_send[ .("Optimized"), util_score := 0 - distance_to_past_opinions - distance_message_opinion - distance_message_assumption - msg_compl + involvement, on="actions"] 
    sim$actions_send[ .("Optimized_appeal"), util_score := 0 - distance_to_past_opinions - distance_message_opinion - distance_message_assumption + msg_compl - involvement, on="actions"] 
    sim$actions_send[ , agent_id := sender ] 
    sim$actions_send[ , .(agent_id, actions, util_score) ] 
    setkey(sim$actions_send, "agent_id") 
    sim$actions_send <- unique(sim$actions_send) 
    sim$actions_send[, util_score := sum(util_score), by=c("agent_id", "actions")] 
    sim$actions_send <- unique(sim$actions_send) 
    sim$actions_send <- dcast(sim$actions_send,
	    agent_id ~ actions, value.var = "util_score") 
    sim$actions_send[, best_action :=  names(sim$actions_send[ , -c("agent_id")])[apply(sim$actions_send[ , -c("agent_id") ], 1, which.max)]] 
    sim$actions_send <- melt( sim$actions_send,
	    id.vars = c("agent_id", "best_action"),
            measure.vars = c("Optimized", "Unoptimized", "Unoptimized_appeal", "Optimized_appeal"),
            variable.name = "actions",
            value.name = "util_score" ) 
    sim$actions_send[ , agent_id := as.integer(agent_id) ]
    sim$actions_send <- sim$actions_send[ , .(agent_id, best_action) ]
    sim$actions_send <- unique(sim$actions_send)
    setkey(sim$actions_send, agent_id)

    #####################################
    #### UPDATE CHOSEN_ACTIONS TABLE ####
    #####################################

    sim$chosen_actions <- merge(sim$actions_send, sim$chosen_actions, by="agent_id", all.x=TRUE, all.y=TRUE) 
    sim$chosen_actions[ best_action.y == "Receive" , action_type := "actions_overall" ] 
    sim$chosen_actions[ best_action.y == "Nothing" , action_type := "actions_overall" ] 
    sim$chosen_actions[ action_type == "actions_send" , best_action := ifelse(!is.na(best_action.x), best_action.x, "NOT ASSIGNED")] 
    sim$chosen_actions[ action_type == "actions_overall" , best_action := ifelse(!is.na(best_action.y), best_action.y, "NOT ASSIGNED")] 
    sim$chosen_actions[ , best_action.x := NULL ][ , best_action.y := NULL ]
    
    #################################
    #### UPDATE DISCOURSE_MEMORY ####
    #################################
    
    sim$discourse_memory_temp <- sim$actions_send[sim$messages, on = c("agent_id" = "sender"), nomatch = 0L] 
    setnames(sim$discourse_memory_temp, "agent_id", "sender") 
    setkey(sim$discourse_memory_temp, "sender") 
    sim$discourse_memory_temp[ , sender_business := .N , by=sender ] 
    sim$discourse_memory_temp[ , sender_business := ifelse(is.na(sender_business), 0, sender_business) ] 
    sim$discourse_memory_temp[ , receiver_business := .N, by = receiver ]
    sim$discourse_memory <- merge(sim$discourse_memory_temp, sim$discourse_memory[ , -c("sender_business", "receiver_business", "assumption_receiver", "op_compl")], by=c("sender"), all.x=TRUE) 
    sim$discourse_memory[ , sender_business := mapply(function(a, b, c, k) {
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
      }, a=past_op_compls, b=op_compl, c= sender_business, k=best_action)] 
    sim$discourse_memory[ , sender_business := ifelse(is.na(sender_business), 0, sender_business) ] 
    sim$discourse_memory[ , receiver_business := ifelse(is.na(receiver_business), 0, receiver_business) ] 
    sim$discourse_memory[ , past_messages := mapply(function(x,y,z) {

	      ifelse(x == "Unoptimized",
		      y,
		      z)

				   }, x=best_action, y=opinion_sender, z=opt_message) ] 
    sim$discourse_memory[ , past_msg_compls := sapply(op_compl, function(x) { list(x) } ) ] 
    sim$discourse_memory[ , past_opinions := ifelse(lengths(past_opinions) < params(sim)$rc_sh_model$opinion_memory_depth,
                                   mapply(function(x, y) {
                                     list(c(unlist(x), y))
                                   }, x=past_opinions, y=opinion_sender),
                                   mapply(function(x, y) {
                                     list(c(unlist(x)[1:params(sim)$rc_sh_model$opinion_memory_depth], y))
                                   }, x=past_opinions, y=opinion_sender)
      )] 
    setnames(sim$discourse_memory, "opinion_sender", "opinion") 
    setnames(sim$discourse_memory, "opt_message", "message") 
    sim$discourse_memory <- sim$discourse_memory[ , .(sender, receiver, opinion, message, op_compl, msg_compl, past_messages, past_opinions, sender_business, receiver_business, past_msg_compls, past_op_compls)] 
    sim$discourse_memory <- sim$discourse_memory[ , .SD[1], by="sender"] 

    temp <- copy(sim$discourse_memory)[ , .(receiver, receiver_business)] 
    temp[ , receiver_business := max(receiver_business), by=receiver] 
    temp <- unique(temp)

    sim$discourse_memory <- sim$discourse_memory[ , receiver_business := NULL ]  
    sim$discourse_memory <- merge(sim$discourse_memory, temp, by.x = c("sender"), by.y = c("receiver"), all.x = TRUE) 
    sim$discourse_memory[ , receiver := NULL ] 
    sim$discourse_memory[ , past_receiver_business := ifelse( !is.na(receiver_business), sapply(receiver_business, function(x) list(x) ), 0 ) ] 
    sim$discourse_memory[ , past_sender_business := ifelse( !is.na(sender_business), sapply(sender_business, function(x) list(x) ), 0 ) ] 
    sim$discourse_memory[ , nbh_incohesion := vec_get_nbh_incohesion(sender) ] 
    sim$discourse_memory[ , past_nbh_incohesion := ifelse( !is.na(nbh_incohesion), sapply(nbh_incohesion, function(x) list(x) ), 0 ) ] 
    sim$discourse_memory[ , self_incohesion := vec_get_self_incohesion( sender ) ] 
    sim$discourse_memory[ , past_self_incohesion := ifelse( !is.na(self_incohesion), sapply(self_incohesion, function(x) list(x) ), 0 ) ] 
    sim$discourse_memory <- sim$discourse_memory[ , .SD[1], by="sender"] 
    
    #########################
    #### "SEND" MESSAGES ####
    #########################

    sim$messages_temp <- copy(sim$messages)
    setnames(sim$messages_temp, old = c("sender", "receiver"), new = c("receiver", "sender")) 
    sim$messages_temp <- sim$messages_temp[ , .(receiver, opinion_sender, op_compl)] 
    setnames(sim$messages_temp, old=c("opinion_sender", "op_compl"), new=c("opinion_receiver", "op_compl_receiver"))
    sim$messages_temp <- unique(sim$messages_temp) 
    setkey(sim$messages_temp, receiver)

    sim$messages <- sim$messages_temp[sim$messages, on="receiver", nomatch=0L] 
    setkey(sim$messages, sender, receiver)
    sim$messages <- sim$messages[sim$actions_send, nomatch=0L, on=c("sender" = "agent_id")]  
    sim$messages <- merge(sim$messages, sim$discourse_memory[ , .(sender, past_msg_compls, past_op_compls)], nomatch=0L, by=c("sender")) 
    sim$messages[, msg_compl := mapply(function(a, b, k) {
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
      }, a=past_op_compls, b=op_compl, k=best_action)] 
    sim$messages[ , assumption_sender := ifelse( ( best_action == "Unoptimized" | best_action == "Unoptimized_appeal") , opinion_sender, opt_message)] 
    sim$messages[ , past_msg_compls := NULL ][ , past_op_compls := NULL ][ , best_action == NULL ]
    setkey(sim$messages, "sender", "receiver") 
    sim$messages <- unique(sim$messages)

    ###################
    #### RECEIVING ####
    ###################
   
    ###################################################
    #### DETERMINE UTILITIES FOR RECEIVING ACTIONS ####
    ###################################################

    sim$actions_receive_temp <- sim$messages[ , .(receiver, msg_compl, op_compl_receiver)][sim$actions_overall[ .("Both", "Receive"), .(agent_id), on="best_action" ], on = c("receiver"="agent_id"), nomatch = 0L ] 
    sim$actions_receive_temp[ , agent_id := receiver ][ , receiver := NULL ]
    sim$actions_receive <- sim$actions_receive_temp[copy(sim$actions_receive), on = c("agent_id"), nomatch = 0L, allow.cartesian = TRUE] 
    setkey(sim$actions_receive, agent_id)
    sim$actions_receive[ , complexity := mean(ifelse(msg_compl > op_compl_receiver, 1, 0)) , by = agent_id ] 
    sim$actions_receive <- sim$actions_receive[sim$discourse_memory[ , .(sender, past_op_compls, past_msg_compls) ], on = c("agent_id" =  "sender"), nomatch = 0L] 
    sim$actions_receive[ , involvement := mean(c(c(unlist(past_op_compls)), c(unlist(past_msg_compls)))), by=agent_id ] 
    sim$actions_receive[ .("Systematic") , util_score := involvement , on = "actions" ] 
    sim$actions_receive[ .("Heuristic") , util_score := complexity , on = "actions" ] 
    sim$actions_receive <- sim$actions_receive[ , .(agent_id, actions, util_score)] 
    setkey(sim$actions_receive, "agent_id") 
    sim$actions_receive[ , past_op_compls := NULL ][ , past_msg_compls := NULL  ] 
    sim$actions_receive <- unique(sim$actions_receive) 
    sim$actions_receive[, util_score := sum(util_score), by=c("agent_id", "actions")] 
    sim$actions_receive <- dcast(sim$actions_receive,
	    agent_id ~ actions, value.var = "util_score") 
    sim$actions_receive[, best_action :=  ifelse(Systematic > Heuristic, "Systematic", "Heuristic")] 
    sim$actions_receive <- melt( sim$actions_receive,
	    id.vars = c("agent_id", "best_action"),
            measure.vars = c("Systematic", "Heuristic"),
            variable.name = "actions",
            value.name = "util_score" ) 
    sim$actions_receive[ , agent_id := as.integer(agent_id) ]
    sim$actions_receive <- sim$actions_receive[ , .(agent_id, best_action)  ]
    sim$actions_receive <- unique(sim$actions_receive)
    setkey(sim$actions_receive, agent_id)
   
    # Heuristic Mode
    
    if (nrow(sim$actions_receive[ .("Heuristic") , on="best_action" ]) > 0) {

	    sim$opinion_updating_h <- sim$messages[sim$actions_receive[ .("Heuristic") , on="best_action" ]
, on = c("receiver" = "agent_id"), nomatch = 0L ] 
	    sim$opinion_updating_h <- sim$opinion_updating_h[ , .(sender, receiver, opinion_receiver, assumption_sender, opt_message, op_compl_receiver, msg_compl, best_action)] 
	    setkey(sim$opinion_updating_h, receiver)
	    sim$opinion_updating_h[ , message_popularity := mapply(function(a,b) {
		sum(
		  sapply(a, function(x) {
		    sum( abs(x - b) ) / .N
		  })
		)
	      }, a=assumption_sender, b=assumption_sender), by = sender] 
	    sim$opinion_updating_h <- sim$opinion_updating_h[sim$agent_characteristics[ , .(agent_id, group, expert) ], on=c("sender"="agent_id"), nomatch=0L] 
	    sim$opinion_updating_h[ , acceptance_util := message_popularity + group + expert ] 
	    sim$opinion_updating_h[ , max_util := max(acceptance_util), by=receiver ] 
	    sim$opinion_updating_h[ , accepted := acceptance_util >= max_util ] 
	    sim$opinion_updating_h <- sim$opinion_updating_h[ accepted == TRUE ]  
	    sim$opinion_updating_h[ , msg_compl := 0 ]

	    seen <- vector()
	    sim$opinion_updating_h[ , unique_within_sender := TRUE ] 
	    for (i in 1:length(sim$opinion_updating_h$sender)) {
	      if (!(sim$opinion_updating_h$receiver[i] %in% seen)) {
		      
		sim$opinion_updating_h$unique_within_sender[i] <- FALSE } else {
		sim$opinion_updating_h$unique_within_sender[i] <- TRUE

	       }

	    seen <- c(seen, sim$opinion_updating_h$sender[i])

	    }

	setkey(sim$opinion_updating_h, unique_within_sender) 
	sim$opinion_updating_h <- sim$opinion_updating_h[ .(TRUE) ] 
	setkey(sim$opinion_updating_h, receiver) 
	sim$opinion_updating_h[ , opinion_receiver_new := sum(assumption_sender) / .N, by="receiver" ] 
	setnames(sim$opinion_updating_h, "receiver", "agent_id") 
	sim$opinion_updating_h[ , new_op_compl := 0 ] 
	sim$opinion_updating_h <- sim$opinion_updating_h[ , .(agent_id, opinion_receiver_new, new_op_compl)] 
	sim$opinion_updating_h <- unique(sim$opinion_updating_h) 
	setkey(sim$opinion_updating_h, "agent_id") 
	sim$opinion_updating_h[ , receiver_business := .N, by=agent_id ]

	    }
       
    # Systematic Mode

    if (nrow(sim$actions_receive[ .("Systematic"), on="best_action" ]) > 0) {
	   
 merge_receiver_opinions <- sim$discourse_memory[ , .(sender, past_opinions)]

	    sim$opinion_updating_s <- sim$messages[sim$actions_receive[ .("Systematic"), on="best_action" ], on = c("receiver" = "agent_id"), nomatch = 0L ] 
	    sim$opinion_updating_s <- sim$opinion_updating_s[ , .(sender, receiver, opinion_receiver, assumption_sender, opt_message, op_compl_receiver, msg_compl, best_action)] 
            sim$opinion_updating_s <- sim$opinion_updating_s[merge_receiver_opinions, on=c("receiver"="sender"), nomatch = 0L] 
	    sim$opinion_updating_s[ , distance_to_past_opinions := mapply(function(a,b) {
		mean(
		  sapply(a, function(x) {
		    abs(x - b)
		  })
		)
	      }, a=past_opinions, b=assumption_sender)] 
	    sim$opinion_updating_s[ , sum_msg_compls := sum(msg_compl) , by=receiver ] 
	    sim$opinion_updating_s[ , within_epsilon := abs(opinion_receiver - assumption_sender) < params(sim)$rc_sh_model$epsilon] 
	    sim$opinion_updating_s[ , self_consistent := distance_to_past_opinions < params(sim)$rc_sh_model$self_incons_tolerance] 
	    sim$opinion_updating_s[ , well_argumented := msg_compl > op_compl_receiver ] 

           sim$s_mode_for_energy <- copy(sim$opinion_updating_s) 
	   sim$s_mode_for_energy[ , receiver_business := .N + sum_msg_compls, by = receiver ] 
	   setnames(sim$s_mode_for_energy, "receiver", "agent_id")

	   sim$opinion_updating_s <- sim$opinion_updating_s[ within_epsilon == TRUE & self_consistent == TRUE & well_argumented == TRUE ] 
           sim$opinion_updating_s <- sim$opinion_updating_s[ , .(opinion_receiver, assumption_sender, receiver, within_epsilon, self_consistent, well_argumented, msg_compl) ] 
	   sim$opinion_updating_s[ , sum_assumptions := sum(assumption_sender), by=receiver] 
	   sim$opinion_updating_s[ , denominator := .N, by=receiver ] 
	   sim$opinion_updating_s[ , opinion_receiver_new := ifelse( denominator != 0, sum_assumptions / denominator, opinion_receiver )] 
	   sim$opinion_updating_s[ , new_op_compl := sum(msg_compl) / .N , by=receiver ] 
	   setnames(sim$opinion_updating_s, "receiver", "agent_id") 
	   sim$opinion_updating_s <- sim$opinion_updating_s[ , .(agent_id, opinion_receiver_new, new_op_compl)] 
	   sim$opinion_updating_s <- unique(sim$opinion_updating_s)
	   setkey(sim$opinion_updating_s, agent_id)

    } 
    
    #################################
    #### UPDATE DISCOURSE_MEMORY ####
    #################################

   if ( (nrow(sim$actions_receive[ .("Heuristic"), on="best_action" ]) > 0 && (nrow(sim$actions_receive[ .("Systematic"), on="best_action" ]) > 0 ) ) )  {

	sim$discourse_memory_temp <- sim$s_mode_for_energy[ , .(agent_id, receiver_business) ] 
	sim$discourse_memory_temp <- rbind(sim$discourse_memory_temp, sim$opinion_updating_h[ , .(agent_id, receiver_business) ]) 
	setkey(sim$discourse_memory_temp, "agent_id") 
	sim$discourse_memory_temp <- unique(sim$discourse_memory_temp) 
	sim$discourse_memory <- sim$discourse_memory_temp[sim$discourse_memory[ , -c("receiver_business") ], on=c("agent_id"="sender") ] 
	setnames(sim$discourse_memory, "agent_id", "sender") 
	sim$discourse_memory[ , receiver_business := ifelse(is.na(receiver_business), 0, receiver_business ) ]  

      sim$opinion_updating <- rbind(sim$opinion_updating_s[ , .(agent_id, opinion_receiver_new, new_op_compl) ], sim$opinion_updating_h[ , .(agent_id, opinion_receiver_new, new_op_compl) ] ) 
      setkey(sim$opinion_updating, "agent_id") 

      } else {
	   if ( (nrow(sim$actions_receive[ .("Heuristic"), on="best_action" ]) > 0 ) ) {

	sim$discourse_memory_temp <- sim$opinion_updating_h[ , .(agent_id, receiver_business) ] 
	setkey(sim$discourse_memory_temp, "agent_id") 
	sim$discourse_memory <- sim$discourse_memory_temp[sim$discourse_memory[ , -c("receiver_business") ], on=c("agent_id"="sender") ] 
	setnames(sim$discourse_memory, "agent_id", "sender") 
	sim$discourse_memory[ , receiver_business := ifelse(is.na(receiver_business), 0, receiver_business ) ]  

    sim$opinion_updating <- sim$opinion_updating_h[ , .(agent_id, opinion_receiver_new, new_op_compl) ] 
    setkey(sim$opinion_updating, "agent_id") 

	   } else {

	sim$discourse_memory_temp <- sim$s_mode_for_energy[ , .(agent_id, receiver_business) ] 
	setkey(sim$discourse_memory_temp, "agent_id") 
	sim$discourse_memory <- sim$discourse_memory_temp[sim$discourse_memory[ , -c("receiver_business") ], on=c("agent_id"="sender") ] 
	setnames(sim$discourse_memory, "agent_id", "sender") 
	sim$discourse_memory[ , receiver_business := ifelse(is.na(receiver_business), 0, receiver_business ) ]  

    sim$opinion_updating <- sim$opinion_updating_s[ , .(agent_id, opinion_receiver_new, new_op_compl) ] 
    setkey(sim$opinion_updating_s, "agent_id") 

	   }

	      }
   
    ######################################
    #### UPDATE agent_characteristics ####
    ######################################

    sim$agent_characteristics <- merge(sim$agent_characteristics, sim$opinion_updating, by="agent_id", all.x = TRUE) 
    sim$agent_characteristics[ , opinion := ifelse(is.na(opinion_receiver_new), opinion, opinion_receiver_new)] 
    sim$agent_characteristics[ , op_compl := ifelse(is.na(opinion_receiver_new), op_compl, new_op_compl) ] 
    sim$agent_characteristics[ , opinion_receiver_new := NULL ][ ,  new_op_compl := NULL ]

    sim$agent_characteristics_temp <- sim$actions_overall[ , .(agent_id, best_action)] 
    sim$agent_characteristics_temp <- merge(sim$agent_characteristics_temp, sim$discourse_memory[ , .(sender, receiver_business, sender_business)], by.x = "agent_id", by.y = "sender") 
    sim$agent_characteristics_temp[ best_action == "Send" , energy_loss := sender_business ] 
    sim$agent_characteristics_temp[ best_action == "Receive", energy_loss := receiver_business ] 
    sim$agent_characteristics_temp[ best_action == "Both", energy_loss := sender_business + receiver_business ] 
    sim$agent_characteristics_temp[ best_action == "Nothing", energy_loss := 0 ] 
    sim$agent_characteristics_temp <- sim$agent_characteristics_temp[ , .(agent_id, energy_loss) ] 
    sim$agent_characteristics_temp <-  unique(sim$agent_characteristics_temp) 
    sim$agent_characteristics_temp[ , energy_loss := sum(energy_loss), keyby="agent_id" ] 
    sim$agent_characteristics_temp <- unique(sim$agent_characteristics_temp) 
    sim$agent_characteristics <- sim$agent_characteristics_temp[sim$agent_characteristics, on="agent_id"] 
    sim$agent_characteristics[ , energy := energy - energy_loss + params(sim)$rc_sh_model$restoration_factor ] 
    sim$agent_characteristics[ , energy_loss := NULL ]
 
    ##################################################
    #### BUSINESS UPDATE FOR sim$discourse_memory ####
    ##################################################

    sim$discourse_memory <- sim$agent_characteristics[ , .(agent_id, opinion) ][sim$discourse_memory[ , -c("opinion")], on=c("agent_id" = "sender") ] 
    setnames(sim$discourse_memory, "agent_id", "sender") 
    setkey(sim$discourse_memory, sender)
    sim$discourse_memory[ , past_receiver_business := ifelse(lengths(past_receiver_business) < params(sim)$rc_sh_model$energy_params_memory_depth,
                                            mapply(function(x, y) {
                                              list(c(unlist(x), y))
                                            }, x=past_receiver_business, y=receiver_business),
                                            mapply(function(x, y) {
                                              list(c(unlist(x)[1:params(sim)$rc_sh_model$opinion_memory_depth], y))
                                            }, x=past_receiver_business, y=receiver_business)
      )] 
    sim$discourse_memory[ , past_sender_business := ifelse(lengths(past_sender_business) < params(sim)$rc_sh_model$energy_params_memory_depth,
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

    # nothing here, because this does not happen in the init

  }

  return(invisible(sim))

}

rc_sh_modelStep <- function(sim) {

  print(time(sim))

  ########################
  #### REBUILD TABLES ####
  ########################

  # tables that are rebuilt at the start of each round: sim$actions_send, sim$actions_overall, sim$messages

  sim$actions_send <- data.table(
    
    agent_id = rep(agent_characteristics$agent_id, each=4),
    actions = rep(c("Unoptimized", "Optimized", "Unoptimized_appeal", "Optimized_appeal"), sim$no_agents),
    util_score = rep(0, length(sim$no_agents*4))
    
  ) 
  sim$actions_send[ , agent_id := as.integer(agent_id) ]
  setkey(sim$actions_send, agent_id)
  
  sim$actions_overall <- data.table(

    agent_id = rep(sim$agent_characteristics$agent_id, each=4),
    actions = rep(c("Send", "Receive", "Both", "Nothing"), sim$no_agents),
    util_score = rep(0, length(sim$no_agents*4))

  ) 
  setkey(sim$actions_overall, agent_id)

  sim$actions_receive <- data.table(

    agent_id = rep(sim$agent_characteristics$agent_id, each=2),
    actions = rep(c("Systematic", "Heuristic"), sim$no_agents),
    util_score = rep(0, length(sim$no_agents*2))

  ) 
  setkey(sim$actions_receive, agent_id)

  sim$messages_temp <- sim$environment %>%
    activate(edges) %>%
    as_tibble() %>%
    data.table() 
  setnames(sim$messages_temp, old = c("from", "to"), new = c("receiver", "sender"))
  setkey(sim$messages_temp, receiver, sender)

  sim$messages <- sim$environment %>%
    activate(edges) %>%
    as_tibble() %>%
    data.table() 
  setnames(sim$messages, old = c("from", "to"), new = c("sender", "receiver")) 
  setkey(sim$messages, receiver, sender)
  sim$messages <- rbind(sim$messages_temp, sim$messages) 
  sim$messages <- sim$messages[sim$agent_characteristics[, .(agent_id, opinion, op_compl)], nomatch = 0L, on = c("sender" = "agent_id")] 
  setnames(sim$messages, "opinion", "opinion_sender")
  setkey(sim$messages, sender, receiver)

  ######################################## 
  #### CONSTRUCT CHOSEN_ACTIONS TABLE ####
  ########################################

  sim$chosen_actions <- data.table(

    agent_id = rep(agent_characteristics$agent_id, each=3),
    action_type = rep(c("actions_overall", "actions_send", "actions_receive"), sim$no_agents),
    best_action = rep(c("Not assigned"), length(sim$no_agents*3))

  ) 
  sim$chosen_actions[ , agent_id := as.integer(agent_id) ]


  #################################################################
  #### BUILD sim$actions_overall AND FIND BEST OVERALL ACTIONS ####
  #################################################################

  sim$actions_overall_temp <- sim$messages[ , .(sender, receiver)] 
  sim$actions_overall_temp[ , max_send_energy_loss := .N*2, by = sender ] 
  sim$actions_overall_temp[ , max_receive_energy_loss := .N*2, by = receiver] 
  sim$actions_overall_temp <- sim$actions_overall_temp[sim$discourse_memory, on=c("sender") ] 
  sim$actions_overall_temp <- sim$actions_overall_temp[sim$discourse_memory[, .(sender, message)][ , assumption_sender := message][ , -c("message")], on=c("receiver"="sender"), nomatch=0L] 
  sim$actions_overall_temp[ , distance_to_past_opinions := mapply(function(a,b) {
        mean(
          sapply(a, function(x) {
            abs(x - b)
          })
        )
      }, a=past_opinions, b=assumption_sender)] 
  sim$actions_overall_temp <- sim$actions_overall_temp[ , .(distance_to_past_opinions, sender, past_self_incohesion, past_nbh_incohesion, sender_business, receiver_business, past_receiver_business, past_sender_business, nbh_incohesion, self_incohesion, max_send_energy_loss, max_receive_energy_loss)] 
  sim$actions_overall_temp[ , control := sum(distance_to_past_opinions) , by=sender ] 
  sim$actions_overall_temp <- sim$actions_overall_temp[ , .SD[1], by=sender]
  sim$actions_overall_temp <- sim$actions_overall_temp[sim$agent_characteristics[ , .(agent_id, energy)], on=c("sender" = "agent_id") ] 
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

	      if (runif(1, 0, 1) > i / params(sim)$rc_sh_model$energy_params_memory_depth) {
	      new_series <- c(new_series, series[i])
      }
      }
      ifelse(is.na(mean(new_series)), y, new_series)

    }, x=past_receiver_business, y=receiver_business   )] 
  sim$actions_overall_temp[ , send_business_mean := mapply(function(x, y) {

series <- rev(unlist(x))
new_series <- vector()

      for (i in 1:length(x)) {

	      if (runif(1, 0, 1) > i / params(sim)$rc_sh_model$energy_params_memory_depth) {
	      new_series <- c(new_series, series[i])
      }
      }
      ifelse(is.na(mean(new_series)), y, new_series)

    }, x=past_sender_business, y=sender_business   )] 
  sim$actions_overall_temp[ , both_business_mean := mapply(function(x, y, a, b) {

series <- rev(unlist(x))
new_series_one <- vector()

      for (i in 1:length(x)) {

	      if (runif(1, 0, 1) > i / params(sim)$rc_sh_model$energy_params_memory_depth) {
	      new_series_one <- c(new_series_one, series[i])
      }
      }

      mean_one <- ifelse(is.na(mean(new_series_one)), a, new_series_one)

series <- rev(unlist(y))
new_series_two <- vector()

      for (i in 1:length(y)) {

	      if (runif(1, 0, 1) > i / params(sim)$rc_sh_model$energy_params_memory_depth) {
	      new_series_two <- c(new_series_two, series[i])
      }
      }
      mean_two <- ifelse(is.na(mean(new_series_two)), b, new_series_two)

      mean( mean_one, mean_two )

    }, x=past_sender_business, y=past_receiver_business, a=sender_business, b=receiver_business   )] 
  sim$actions_overall_temp[ , both_business_mean_index := mapply(function(x, y, z, a) {

      numerator <- sum( c(unlist(x), unlist(y)) )

      denominator <- z*length(unlist(y)) + a*length(unlist(x))

      is_na <- numerator / denominator
      
      ifelse(is.na(is_na), 0, is_na)

    }, x=past_sender_business, y=past_receiver_business, z = max_receive_energy_loss, a = max_send_energy_loss )] 
  sim$actions_overall <- sim$actions_overall_temp[sim$actions_overall[ , -c("best_action")], on=c("sender" = "agent_id"), allow.cartesian=TRUE] 
  setnames(sim$actions_overall, "sender", "agent_id") 
  setkey(sim$actions_overall, agent_id)
  sim$actions_overall[ .("Send") , energy_loss := ( energy - sender_business ), on="actions"]
  sim$actions_overall[ .("Receive") , energy_loss := ( energy - receiver_business ), on="actions"] 
  sim$actions_overall[ .("Both") , energy_loss := ( energy - sender_business - receiver_business ), on="actions" ] 
  sim$actions_overall[ .("Send") , projected_energy := ifelse(energy_loss <= 0, 0, energy_loss) / ( energy + params(sim)$rc_sh_model$restoration_factor ) , on="actions" ] 
  sim$actions_overall[ .("Receive") , projected_energy := ifelse(energy_loss <= 0, 0, energy_loss) / ( energy + params(sim)$rc_sh_model$restoration_factor ), on="actions" ] 
  sim$actions_overall[ .("Both") , projected_energy := ifelse(energy_loss <= 0, 0, energy_loss) / ( energy + params(sim)$rc_sh_model$restoration_factor ), on="actions" ] 
  sim$actions_overall[ , projected_energy := ifelse(is.na(projected_energy), 0, projected_energy) ] 
  sim$actions_overall[ .("Send"), util_score :=
         ifelse(projected_energy <= 0, 0, projected_energy) + ( 1 - self_incohesion), on="actions" ] 
  sim$actions_overall[ .("Receive"), util_score := 
         ifelse(projected_energy <= 0, 0, projected_energy) + ( 1 - nbh_incohesion ), on="actions" ] 
  sim$actions_overall[ .("Both"), util_score :=  
         ifelse(projected_energy <= 0, 0, projected_energy) + control + ( ( self_incohesion + nbh_incohesion ) * ( 1 - abs( self_incohesion - nbh_incohesion) ) ), on="actions" ] 
  sim$actions_overall[ .("Nothing"), util_score := ifelse( ( energy - sender_business <= 0 | energy - receiver_business <= 0) ,
	    max(util_score)+10000, min(util_score)-10000), on="actions" ] 
  sim$actions_overall[ , past_self_incohesion := NULL ][ , past_nbh_incohesion := NULL ][ , past_receiver_business := NULL ][ , past_sender_business := NULL ] 
  sim$actions_overall <- unique(sim$actions_overall) 
  sim$actions_overall <- dcast(sim$actions_overall,
	  agent_id ~ actions, value.var = "util_score", fun.aggregate = sum) 
  sim$actions_overall[ , agent_id := as.character(agent_id)] 
  sim$actions_overall[, best_action :=  names( sim$actions_overall[ , -c("agent_id")] )[ unlist(apply(sim$actions_overall[ , -c("agent_id") ], 1, which.max)) ] ] 
  sim$actions_overall <- melt( sim$actions_overall,
	  id.vars = c("agent_id", "best_action"),
          measure.vars = c("Send", "Receive", "Both", "Nothing"),
          variable.name = "actions",
          value.name = "util_score" ) 
  sim$actions_overall[ , agent_id := as.integer(agent_id) ]
  sim$actions_overall <- sim$actions_overall[ , .(agent_id, best_action) ]
  sim$actions_overall <- unique(sim$actions_overall)
  setkey(sim$actions_overall, agent_id)

  sim$bothers <- sim$actions_overall[ "Both", .SD[1], by="agent_id", on="best_action" ]$agent_id
  sim$senders <- sim$actions_overall[ "Send", .SD[1], by="agent_id", on="best_action" ]$agent_id
  sim$receivers <- sim$actions_overall[ "Receiver", .SD[1], by="agent_id", on="best_action" ]$agent_id
  sim$nothingers <- sim$actions_overall[ "Nothing", .SD[1], by="agent_id", on="best_action" ]$agent_id

  ###################################
  #### UPDATE sim$chosen_actions ####
  ###################################

  sim$chosen_actions <- merge(sim$actions_overall, sim$chosen_actions, by=c("agent_id"), all.x=TRUE, all.y=TRUE) 
  sim$chosen_actions[ is.na(action_type) , action_type := "actions_overall" ] 
  sim$chosen_actions[ action_type == "actions_overall" , best_action := ifelse(!is.na(best_action.x), best_action.x,  "NOT ASSIGNED")] 
  sim$chosen_actions[ , best_action.x := NULL ][ , best_action.y := NULL ]
  setkey(sim$chosen_actions, agent_id)

  ##############################################################
  #### UPDATE sim$messages WITH ASSUMPTIONS ABOUT NEIGHBORS ####
  ##############################################################

  sim$messages_temp <-  sim$discourse_memory[ , .(sender, message) ]
  setnames(sim$messages_temp, "sender", "receiver") 
  sim$messages <- merge(sim$messages_temp, sim$messages, by = c("receiver") ) 
  setnames(sim$messages, "message", "assumption_receiver") 

  ####################################
  #### PRODUCE OPTIMIZED MESSAGES ####
  ####################################

  sim$message_matrix <- outer(sim$messages$opinion_sender, sim$messages$assumption_receiver, produce_altered_message) 
  row.names(sim$message_matrix) <- sim$messages[, sender]
  colnames(sim$message_matrix) <- sim$messages[, receiver]

  sim$messages_temp <- data.table(sim$message_matrix) 
  sim$messages_temp[ , sender := as.numeric(row.names(sim$message_matrix))] 
  sim$messages_temp <- melt( sim$messages_temp,
	  id.vars = c("sender"),
          measure.vars = as.character(seq(1, sim$no_agents, 1)),
          variable.name = "receiver",
          value.name = "opt_message" ) 
  setkey(sim$messages_temp, "sender", "receiver")
  sim$messages_temp <- sim$messages_temp[ sender != receiver ] 
  sim$messages_temp <- setkey(sim$messages_temp, "sender") 
  sim$messages_temp <- unique(sim$messages_temp) 
  sim$messages_temp[ , sender := as.integer(sender)] 
  sim$messages_temp[ , receiver := as.integer(receiver)] 
  sim$messages <- sim$messages_temp[sim$messages, on=c("sender", "receiver"), nomatch = 0L] 
  sim$messages[ , opt_message := median(opt_message), by = "sender" ] 
  setkey(sim$messages, receiver, sender)
 
  #######################################################
  #### UPDATE sim$discourse_memory WITH NEW OPINIONS ####
  #######################################################

  sim$discourse_memory <- sim$discourse_memory[ , -c("opinion", "op_compl") ][sim$agent_characteristics[ , .(agent_id, opinion, op_compl) ], on=c("sender" = "agent_id")]  
  sim$discourse_memory[ , past_opinions := ifelse(lengths(past_opinions) < params(sim)$rc_sh_model$opinion_memory_depth,
                                 mapply(function(x, y) {
                                   list(c(unlist(x), y))
                                 }, x=past_opinions, y=opinion),
                                 mapply(function(x, y) {
                                   list(c(unlist(x)[1:params(sim)$rc_sh_model$opinion_memory_depth], y))
                                 }, x=past_opinions, y=opinion)
    )] 
   sim$discourse_memory[ ,  past_op_compls := ifelse(lengths(past_op_compls) < params(sim)$rc_sh_model$argumentation_memory_depth,
                                 mapply(function(x, y) {
                                   list(c(unlist(x), y))
                                 }, x=past_op_compls, y=op_compl),
                                 mapply(function(x, y) {
                                   list(c(unlist(x)[1:params(sim)$rc_sh_model$argumentation_memory_depth], y))
                                 }, x=past_op_compls, y=op_compl)
    )] 
   sim$discourse_memory[ , assumption_receiver := NULL ][ , opt_message := NULL ]
   setkey(sim$discourse_memory, sender)
  

    ###############################################################
    #### COMPUTE SENDING UTILITIES AND CHOOSE MAX UTIL ACTIONS ####
    ###############################################################

   if( (length(sim$bothers) > 0) | (length(sim$senders) > 0)  ) {

    sim$actions_send_temp <- sim$messages[sim$actions_overall[ .(c("Both", "Send")), on="best_action" ]  , on = c("sender" = "agent_id"), nomatch = 0L ] 
    sim$actions_send_temp <- sim$actions_send_temp[ , .(sender , receiver , opt_message , opinion_sender , assumption_receiver)] 
    sim$actions_send <- sim$actions_send_temp[sim$actions_send, on = c("sender" = "agent_id"), nomatch = 0L, allow.cartesian = TRUE] 
    sim$actions_send <- sim$actions_send[sim$discourse_memory, on = c("sender"), nomatch = 0L] 
    sim$actions_send[, involvement := sapply( past_op_compls, function(x) mean(unlist(x)) )  ] 
    sim$actions_send[, msg_compl := mapply(function(a, b, c, k) {
        switch(k,
               "Unoptimized" = {
                 b + ifelse(mean(c(c(unlist(a)), c(unlist(c))) - b) > 0, mean(c(c(unlist(a)), c(unlist(c)))), 0) 
	       },
               "Optimized" = {
                 b + ifelse(mean(c(c(unlist(a)), c(unlist(c)))- b) > 0, mean(c(c(unlist(a)), c(unlist(c)))), 0) 
	       },
               "Unoptimized_appeal" = {
                 0
	       },
               "Optimized_appeal" = {
                 0 
	       }
        )
      }, a=past_op_compls, b=op_compl, c=past_msg_compls, k=actions)]
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
      }, a=past_messages, b=opt_message, c=opinion_sender, k=actions)] 
    sim$actions_send[, distance_message_opinion := mapply(function(a,b,k) {
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
      }, a=opinion_sender, b=opt_message, k=actions)] 
    sim$actions_send[, distance_message_assumption := mapply(function(a,b,c,k) {
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
      }, a=opinion_sender, b=opt_message, c=assumption_receiver, k=actions)] 
    sim$actions_send[ , past_opinions := NULL ][ , receiver := NULL ] 
    sim$actions_send[ .("Unoptimized") , util_score := 0 - distance_to_past_opinions - distance_message_opinion - distance_message_assumption - msg_compl + involvement, on="actions"] 
    sim$actions_send[ .("Unoptimized_appeal"), util_score := 0 - distance_to_past_opinions - distance_message_opinion - distance_message_assumption + msg_compl - involvement, on="actions"] 
    sim$actions_send[ .("Optimized"), util_score := 0 - distance_to_past_opinions - distance_message_opinion - distance_message_assumption - msg_compl + involvement, on="actions"] 
    sim$actions_send[ .("Optimized_appeal"), util_score := 0 - distance_to_past_opinions - distance_message_opinion - distance_message_assumption + msg_compl - involvement, on="actions"] 
    sim$actions_send <- sim$actions_send[ , agent_id := sender ] 
    sim$actions_send <- sim$actions_send[ , .(agent_id, actions, util_score)] 
    sim$actions_send <- unique(sim$actions_send) 
    sim$actions_send[, util_score := sum(util_score), by=c("agent_id", "actions")] 
    sim$actions_send <- unique(sim$actions_send) 
    sim$actions_send <- dcast(sim$actions_send,
	    agent_id ~ actions, value.var = "util_score") 
    sim$actions_send[, best_action :=  names(sim$actions_send[ , -c("agent_id")])[apply(sim$actions_send[ , -c("agent_id") ], 1, which.max)]] 
    sim$actions_send <- melt( sim$actions_send,
	    id.vars = c("agent_id", "best_action"),
            measure.vars = c("Optimized", "Unoptimized", "Unoptimized_appeal", "Optimized_appeal"),
            variable.name = "actions",
            value.name = "util_score" ) 
    sim$actions_send[ , agent_id := as.integer(agent_id) ]
    sim$actions_send <- unique(sim$actions_send[ , .(agent_id, best_action)]) 	
    setkey(sim$actions_send, "agent_id") 

    sim$chosen_actions <- merge(sim$actions_send, sim$chosen_actions, by="agent_id", all.x=TRUE , all.y=TRUE) 
    sim$chosen_actions[ best_action.y == "Receive" , action_type := "actions_overall" ] 
    sim$chosen_actions[ best_action.y == "Nothing" , action_type := "actions_overall" ] 
    sim$chosen_actions[ action_type == "actions_send" , best_action := ifelse(!is.na(best_action.x), best_action.x, "NOT ASSIGNED")] 
    sim$chosen_actions[ action_type == "actions_overall" , best_action := ifelse(!is.na(best_action.y), best_action.y, "NOT ASSIGNED")] 
    sim$chosen_actions[ , best_action.x := NULL ][ , best_action.y := NULL ]

    print(table(sim$chosen_actions$best_action))

    #####################################
    #### UPDATE sim$discourse_memory ####
    #####################################

    sim$send_busy <- sim$actions_send[sim$messages[ , .(sender, receiver, opt_message) ], on = c("agent_id" = "sender"), nomatch = 0L]  
    setnames(sim$send_busy, "agent_id", "sender") 
    setkey(sim$send_busy, "sender") 
    sim$send_busy[ , sender_business := .N, by = "sender" ] 
    sim$send_busy[ , receiver_business := .N, by = "receiver" ] 
    sim$send_busy[ , opinion_sender := NULL ] 

    sim$receive_busy <- copy(sim$send_busy)[ , .(receiver, receiver_business)] 
    sim$receive_busy <- sim$receive_busy[ !is.na(receiver) ] 
    sim$receive_busy[ , receiver_business := max(receiver_business), by=receiver] 
    sim$receive_busy <- unique(sim$receive_busy) 

    sim$discourse_memory <- merge(copy(sim$discourse_memory)[ , -c("sender_business", "receiver_business")], sim$send_busy[ , -c("receiver_business") ], by=c("sender"), all.x=TRUE, all.y=TRUE) 
    sim$discourse_memory <- sim$discourse_memory[sim$receive_busy, on=c("sender"="receiver") ] 
    sim$discourse_memory[ , receiver_business := ifelse(is.na(receiver_business), 0, receiver_business) ] 

    sim$discourse_memory[ !is.na(best_action) , msg_compl := mapply(function(a, b, c, k) {
        switch(k,
               "Unoptimized" = {
                 b + ifelse(mean(c(unlist(a)) - b) > 0 | mean(c(unlist(a)) - c) > 0, max(c(mean(c(unlist(a)) - b), mean(c(unlist(a)) - c))), 0) 
	       },
               "Optimized" = {
                 b + ifelse(mean(c(unlist(a)) - b) > 0 | mean(c(unlist(a)) - c) > 0, max(c(mean(c(unlist(a)) - b), mean(c(unlist(a)) - c))), 0) 
	       },
               "Unoptimized_appeal" = {
                 0
	       },
               "Optimized_appeal" = {
                 0 
	       }
        )
      }, a=past_op_compls, b=op_compl, c=past_msg_compls, k=best_action)] 
    sim$discourse_memory[ !is.na(best_action), sender_business := mapply(function(a, b, c, k) {
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
      }, a=msg_compl , b=op_compl, c= sender_business, k=best_action)] 
    sim$discourse_memory[ , sender_business := ifelse(is.na(sender_business), 0, sender_business) ] 
    sim$discourse_memory[ , past_messages := ifelse( is.na(best_action),
            past_messages,
            ifelse(lengths(past_messages) < params(sim)$rc_sh_model$message_memory_depth,
                                   ifelse( best_action == "Unoptimized",
                                          mapply(function(x, y) {
                                            list(c(unlist(x), y))
                                          }, x=past_messages, y=opinion),
                                          mapply(function(x, y) {
                                            list(c(unlist(x), y))
                                          }, x=past_messages, y=opt_message)),
                                   ifelse( best_action == "Unoptimized",
                                          mapply(function(x, y) {
                                            list(c(unlist(x)[1:params(sim)$rc_sh_model$message_memory_depth], y))
                                          }, x=past_messages, y=opinion),
                                          mapply(function(x, y) {
                                            list(c(unlist(x)[1:params(sim)$rc_sh_model$message_memory_depth], y))
                                          }, x=past_messages, y=opt_message)
                                   )
                                 )
      )]
    sim$discourse_memory[ , message := ifelse(is.na(opt_message), message, opt_message) ] 
    sim$discourse_memory[ , opt_message := NULL ] 
    sim$discourse_memory <- sim$discourse_memory[ , .(sender, opinion, message, op_compl, msg_compl, past_messages, past_opinions, past_op_compls, past_msg_compls, sender_business, receiver_business, past_sender_business, past_receiver_business, past_nbh_incohesion, past_self_incohesion)] 
    sim$discourse_memory <- sim$discourse_memory[ !duplicated(sim$discourse_memory[ , as.logical(lapply(sim$discourse_memory, function(x) !is.list(x) ) ) , with = FALSE ]) , ]
    sim$discourse_memory[ , past_sender_business := ifelse(lengths(past_sender_business) < params(sim)$rc_sh_model$energy_params_memory_depth,
                                          mapply(function(x, y) {
                                            list(c(unlist(x), y))
                                          }, x=past_sender_business, y=sender_business),
                                          mapply(function(x, y) {
                                            list(c(unlist(x)[1:params(sim)$rc_sh_model$energy_params_memory_depth], y))
                                          }, x=past_sender_business, y=sender_business)
      )]
    sim$discourse_memory[ , nbh_incohesion := vec_get_nbh_incohesion(sender) ] 
    sim$discourse_memory[ , past_nbh_incohesion := ifelse(lengths(past_nbh_incohesion) < params(sim)$rc_sh_model$energy_params_memory_depth,
                                         mapply(function(x, y) {
                                           list(c(unlist(x), y))
                                         }, x=past_nbh_incohesion, y=nbh_incohesion),
                                         mapply(function(x, y) {
                                           list(c(unlist(x)[1:params(sim)$rc_sh_model$opinion_memory_depth], y))
                                         }, x=past_nbh_incohesion, y=nbh_incohesion)
      )] 
    sim$discourse_memory[ , self_incohesion := vec_get_self_incohesion( sender ) ] 
    sim$discourse_memory[ , past_self_incohesion := ifelse(lengths(past_self_incohesion) < params(sim)$rc_sh_model$energy_params_memory_depth,
                                          mapply(function(x, y) {
                                            list(c(unlist(x), y))
                                          }, x=past_self_incohesion, y=self_incohesion),
                                          mapply(function(x, y) {
                                            list(c(unlist(x)[1:params(sim)$rc_sh_model$opinion_memory_depth], y))
                                          }, x=past_self_incohesion, y=self_incohesion)
      )] 

     sim$messages_temp <- copy(sim$messages) 
     setnames(sim$messages_temp, old = c("sender", "receiver"), new = c("receiver", "sender")) 
     sim$messages_temp <- sim$messages_temp[ , .(receiver, opinion_sender, op_compl)] 
     setnames(sim$messages_temp, old=c("opinion_sender", "op_compl"), new=c("opinion_receiver", "op_compl_receiver")) 
     sim$messages <- unique(sim$messages_temp)[sim$messages, on="receiver", nomatch=0L] 

    sim$messages <- sim$messages[sim$actions_send, nomatch=0L, on=c("sender" = "agent_id")]  
    sim$messages <- merge(sim$messages, sim$discourse_memory[ , .(sender, past_msg_compls, past_op_compls)], nomatch=0L, by=c("sender")) 
    sim$messages[, msg_compl := mapply(function(a, b, k) {
        switch(k,
               "Unoptimized" = {
                 b + ifelse(mean(c(unlist(a)) - b) > 0, mean(c(unlist(a)) - b), 0) 
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
      }, a=past_op_compls, b=op_compl, k=best_action)] 
    sim$messages[ , assumption_sender := ifelse( ( best_action == "Unoptimized" | best_action == "Unoptimized_appeal") , opinion_sender, opt_message )] 
    sim$messages[ , past_msg_compls := NULL ][ , past_op_compls := NULL ][ , actions := NULL ][ , best_action := NULL ] 
    sim$messages <- unique(sim$messages)
    setkey(sim$messages, "sender") 
 
    ###################
    #### RECEIVING ####
    ###################
   
    ###################################################
    #### DETERMINE UTILITIES FOR RECEIVING ACTIONS ####
    ###################################################

    sim$actions_receive_temp <- sim$messages[, agent_id := receiver][ , .(agent_id, msg_compl, op_compl_receiver)] [sim$actions_overall[ .(c("Both", "Receive")), .(agent_id),  on="best_action" ], on = c("agent_id"), nomatch = 0L ] 
    sim$actions_receive <- sim$actions_receive_temp[sim$actions_receive, on = c("agent_id"), nomatch = 0L, allow.cartesian = TRUE] 
    sim$actions_receive[ , complexity := mean(ifelse(msg_compl > op_compl_receiver, 1, 0)) , by = agent_id ] 
    sim$actions_receive <- sim$actions_receive[sim$discourse_memory[ , .(sender, past_op_compls, past_msg_compls) ], on = c("agent_id" =  "sender"), nomatch = 0L] 
    sim$actions_receive[ , involvement := mean(c(c(unlist(past_op_compls)), c(unlist(past_msg_compls)))), by=agent_id ] 
    sim$actions_receive[ .("Systematic") , util_score := involvement , on = "actions" ] 
    sim$actions_receive[ .("Heuristic") , util_score := complexity , on = "actions" ] 
    sim$actions_receive <- sim$actions_receive[ , .(agent_id, actions, util_score)] 
    setkey(sim$actions_receive, "agent_id") 
    sim$actions_receive[ , past_op_compls := NULL ][ , past_msg_compls := NULL  ] 
    sim$actions_receive[, util_score := sum(util_score), by=c("agent_id", "actions")] 
    sim$actions_receive <- unique(sim$actions_receive) 
    sim$actions_receive <- dcast(sim$actions_receive,
	    agent_id ~ actions, value.var = "util_score") 
    sim$actions_receive[, best_action :=  ifelse(Systematic > Heuristic, "Systematic", "Heuristic")] 
    sim$actions_receive <- melt( sim$actions_receive,
	    id.vars = c("agent_id", "best_action"),
            measure.vars = c("Systematic", "Heuristic"),
            variable.name = "actions",
            value.name = "util_score" ) 
    sim$actions_receive[ , agent_id := as.integer(agent_id) ]
    sim$actions_receive <- sim$actions_receive[ , .(agent_id, best_action) ]
    sim$actions_receive <- unique(sim$actions_receive)
    setkey(sim$actions_receive, agent_id)

    sim$chosen_actions <- merge(sim$actions_receive, sim$chosen_actions, by="agent_id", all.x=TRUE , all.y=TRUE) 
    sim$chosen_actions[ .("Receive") , action_type := "actions_overall", on="best_action.y" ] 
    sim$chosen_actions[ .("Nothing") , action_type := "actions_overall", on="best_action.y" ] 
    sim$chosen_actions[ .("actions_send") , best_action := ifelse(!is.na(best_action.y), best_action.y, "NOT ASSIGNED"), on="action_type"] 
    sim$chosen_actions[ .("actions_overall") , best_action := ifelse(!is.na(best_action.y), best_action.y, "NOT ASSIGNED"), on="action_type"] 
    sim$chosen_actions[ .("actions_receive") , best_action := ifelse(!is.na(best_action.x), best_action.x, "NOT ASSIGNED"), on="action_type"] 
    sim$chosen_actions[ , best_action.x := NULL ][ , best_action.y := NULL ]
    setkey(sim$chosen_actions, agent_id)

    print(table(sim$chosen_actions$best_action))

    # Heuristic Mode
    
    if (nrow(sim$actions_receive[ .("Heuristic"), on="best_action" ]) > 0) {

	    sim$opinion_updating_h <- copy(sim$messages) 
	    sim$opinion_updating_h <- sim$messages[sim$actions_receive[ .("Heuristic"), on="best_action" ] , on = c("receiver" = "agent_id"), nomatch = 0L ] 
	    sim$opinion_updating_h[ , .(sender, receiver, opinion_receiver, assumption_sender, opt_message, op_compl_receiver, msg_compl, best_action)] 
	    sim$opinion_updating_h[ , message_popularity := mapply(function(a,b) {
		sum(
		  sapply(a, function(x) {
		    sum( abs(x - b) ) / .N
		  })
		)
	      }, a=assumption_sender, b=assumption_sender), by = sender] 
	    sim$opinion_updating_h <- sim$opinion_updating_h[sim$agent_characteristics[ , .(agent_id, group, expert) ], on=c("sender"="agent_id"), nomatch=0L] 
	    sim$opinion_updating_h[ , acceptance_util := message_popularity + group + expert ] 
	    sim$opinion_updating_h[ , max_util := max(acceptance_util), by=receiver ] 
	    sim$opinion_updating_h[ , accepted := acceptance_util >= max_util ] 
	    sim$opinion_updating_h <- sim$opinion_updating_h[ accepted == TRUE ]  
	    sim$opinion_updating_h[ , msg_compl := 0 ]
test <<- copy(sim$opinion_updating_h)
	    seen <- vector()
	    sim$opinion_updating_h[ , unique_within_receiver := TRUE ] 
	    for (i in 1:length(sim$opinion_updating_h$receiver)) {
	      if (!(sim$opinion_updating_h$receiver[i] %in% seen)) {
		      
		sim$opinion_updating_h$unique_within_receiver[i] <- FALSE } else {
		sim$opinion_updating_h$unique_within_receiver[i] <- TRUE

	       }

	    seen <- c(seen, sim$opinion_updating_h$receiver[i])

	    }
test_two <<- copy(sim$opinion_updating_h)
	sim$opinion_updating_h <- sim$opinion_updating_h[ .(TRUE), opinion_receiver_new := sum(assumption_sender) / .N, by="receiver", on="unique_within_receiver" ] 
	sim$opinion_updating_h <- sim$opinion_updating_h[ .(TRUE), on="unique_within_receiver" ] 
	setnames(sim$opinion_updating_h, "receiver", "agent_id") 
	sim$opinion_updating_h[ , new_op_compl := 0 ] 
	sim$opinion_updating_h <- sim$opinion_updating_h[ , .(agent_id, opinion_receiver_new, new_op_compl)] 
	setkey(sim$opinion_updating_h, "agent_id") 

	    }
       
    # Systematic Mode

    if (nrow(sim$actions_receive[ .("Systematic"), on="best_action" ]) > 0) {
	   
     merge_receiver_opinions <- sim$discourse_memory[ , .(sender, past_opinions)]

    merge_sender_messages <- sim$discourse_memory[ , .(sender, past_messages)]

	    sim$opinion_updating_s <- sim$messages[sim$actions_receive[ .("Systematic"), on="best_action"], on = c("receiver" = "agent_id"), nomatch = 0L ]
	    sim$opinion_updating_s <- sim$opinion_updating_s[ , .(sender, receiver, opinion_receiver, assumption_sender, opt_message, op_compl_receiver, msg_compl, best_action)]
            sim$opinion_updating_s <- sim$opinion_updating_s[merge_receiver_opinions, on=c("receiver"="sender"), nomatch = 0L] 
            sim$opinion_updating_s <- sim$opinion_updating_s[merge_sender_messages, on=c("sender"), nomatch = 0L] 
	    sim$opinion_updating_s[ , distance_to_past_opinions := mapply(function(a,b) {
		mean(
		  sapply(a, function(x) {
		    abs(x - b)
		  })
		)
	      }, a=past_opinions, b=assumption_sender)] 
    sim$opinion_updating_s[ , distance_to_past_messages := mapply(function(a,b) {
	     max(
		sapply(a, function(x) {
		      abs(x - b)
		   })
	   )
      }, a=past_messages, b=assumption_sender  )] 
	    sim$opinion_updating_s[ , sum_msg_compls := sum(msg_compl) , by=receiver ] 
	    sim$opinion_updating_s[ , within_epsilon := abs(opinion_receiver - assumption_sender) < params(sim)$rc_sh_model$epsilon] 
	    sim$opinion_updating_s[ , self_consistent := distance_to_past_opinions < params(sim)$rc_sh_model$self_incons_tolerance] 
	    sim$opinion_updating_s[ , well_argumented := msg_compl >= op_compl_receiver ] 

	   s_mode_for_energy <- copy(sim$opinion_updating_s)[ , receiver_business := .N + sum_msg_compls, by = receiver ] 
	   setnames(s_mode_for_energy, "receiver", "agent_id") 
	   s_mode_for_energy <- s_mode_for_energy[ , .(agent_id, receiver_business) ] 
	   s_mode_for_energy <- unique(s_mode_for_energy)

	   sim$opinion_updating_s <- sim$opinion_updating_s[ within_epsilon == TRUE & self_consistent == TRUE & well_argumented == TRUE ] 
           sim$opinion_updating_s <- sim$opinion_updating_s[ , .(opinion_receiver, assumption_sender, receiver, within_epsilon, self_consistent, well_argumented, msg_compl) ] 
           sim$opinion_updating_s <- unique(sim$opinion_updating_s) 
	   sim$opinion_updating_s[ , sum_assumptions := sum(assumption_sender), by=receiver] 
	   sim$opinion_updating_s[ , denominator := .N, by=receiver ] 
	   sim$opinion_updating_s[ , opinion_receiver_new := ifelse( denominator != 0, sum_assumptions / denominator, opinion_receiver )] 
	   sim$opinion_updating_s[ , new_op_compl := sum(msg_compl) / .N , by=receiver ] 
	   setnames(sim$opinion_updating_s, "receiver", "agent_id") 
	   sim$opinion_updating_s <- sim$opinion_updating_s[ , .(agent_id, opinion_receiver_new, new_op_compl)] 
	   sim$opinion_updating_s <- unique(sim$opinion_updating_s)
	   setkey(sim$opinion_updating_s, agent_id)

    } 
    
    #################################
    #### UPDATE DISCOURSE_MEMORY ####
    #################################

   if ( (nrow(sim$actions_receive[ .("Heuristic"), on="best_action" ]) > 0 && nrow(sim$actions_receive[ .("Systematic"), on="best_action" ]) > 0 ) )  {

	sim$discourse_memory_temp <- sim$s_mode_for_energy[ , .(agent_id, receiver_business) ] 
        setnames(sim$discourse_memory_temp, "receiver_business", "receiver_business_y") 
	setkey(sim$discourse_memory_temp, "agent_id") 
	sim$discourse_memory_temp <- unique(sim$discourse_memory_temp) 
	sim$discourse_memory <- sim$discourse_memory_temp[sim$discourse_memory, on=c("agent_id"="sender") ] 
	setnames(sim$discourse_memory, "agent_id", "sender") 
	sim$discourse_memory[ , receiver_business := ifelse(is.na(receiver_business_y), receiver_business, receiver_business_y ) ] 
	sim$discourse_memory[ , receiver_business_y := NULL ]

    sim$opinion_updating <- rbind(sim$opinion_updating_s, sim$opinion_updating_h ) 
    setkey(sim$opinion_updating, "agent_id") 
    sim$opinion_updating <- unique(sim$opinion_updating) 

      } else {
	   if ( (nrow(sim$actions_receive[ .("Heuristic"), on="best_action" ]) > 0 ) ) {

    sim$opinion_updating <- sim$opinion_updating_h
    setkey(sim$opinion_updating, "agent_id") 
    sim$opinion_updating <- unique(sim$opinion_updating) 

	   } else {

	sim$discourse_memory_temp <- sim$s_mode_for_energy[ , .(agent_id, receiver_business) ] 
        setnames(sim$discourse_memory_temp, "receiver_business", "receiver_business_y") 
	setkey(sim$discourse_memory_temp, "agent_id") 
	sim$discourse_memory_temp <- unique(sim$discourse_memory_temp) 
	sim$discourse_memory <- sim$discourse_memory_temp[sim$discourse_memory, on=c("agent_id"="sender") ] 
	setnames(sim$discourse_memory, "agent_id", "sender") 
	sim$discourse_memory[ , receiver_business := ifelse(is.na(receiver_business_y), receiver_business, receiver_business_y ) ] 
	sim$discourse_memory[ , receiver_business_y := NULL ]
	setkey(sim$discourse_memory, sender)

    sim$opinion_updating <- sim$opinion_updating_s 
    setkey(sim$opinion_updating, "agent_id") 
    sim$opinion_updating <- unique(sim$opinion_updating) 

	   }

	      }
   
    if( length(sim$nothingers) > 0  ) {

          setkey(sim$discourse_memory, "sender")
          sim$opinion_updating_n <- sim$discourse_memory[ .(sender, past_opinions, op_compl)][ .(sim$nothingers), on="sender" ] 
	  sim$opinion_updating_n <- sim$opinion_updating_n[ , .SD[1], by=sender]
          sim$opinion_updating_n[ , opinion_receiver_new := mapply(function(x, y) { 
     
			series <- rev(unlist(x))
			new_series <- vector()

			      for (i in 1:length(x)) {

				      if (runif(1, 0, 1) > i**2 / params(sim)$rc_sh_model$opinion_memory_depth) {
				      new_series <- c(new_series, series[i])
			      }
			      }
			      ifelse(is.na(median(new_series)), y, new_series)

          }, x=past_opinions, y=opinion   ) ] 
          sim$opinion_updating_n[ , new_op_compl := mapply(function(x, y) { 
     
			series <- rev(unlist(x))
			new_series <- vector()

			      for (i in 1:length(x)) {

				      if (runif(1, 0, 1) > i / params(sim)$rc_sh_model$opinion_memory_depth) {
				      new_series <- c(new_series, series[i])
			      }
			      }
			      ifelse(is.na(median(new_series)), y, new_series)

          }, x=past_op_compls, y=op_compl   ) ] 
	  sim$opinion_updating_n <- sim$opinion_updating_n[ , .(sender, opinion_receiver_new, new_op_compl)] 
	  setnames(sim$opinion_updating_n, "sender", "agent_id")
	  setkey(sim$opinion_updating_n, "agent_id") 
	  sim$opinion_updating_n <- unique(sim$opinion_updating_n) 
	  sim$opinion_updating <- rbind(sim$opinion_updating_n, sim$opinion_updating)
	  setkey(sim$opinion_updating, agent_id)

    }

    ######################################
    #### UPDATE agent_characteristics ####
    ######################################

    sim$agent_characteristics <- merge(sim$agent_characteristics, sim$opinion_updating, by="agent_id", all.x = TRUE) 
    sim$agent_characteristics[ , opinion := ifelse(is.na(opinion_receiver_new), opinion, opinion_receiver_new)] 
    sim$agent_characteristics[ , op_compl := ifelse(is.na(opinion_receiver_new), op_compl, new_op_compl) ] 
    sim$agent_characteristics[ , opinion_receiver_new := NULL ][ , new_op_compl := NULL ]

    sim$agent_characteristics_temp <- sim$actions_overall[ , best_axn_overall := best_action][ , .(agent_id, best_axn_overall)] 
    sim$agent_characteristics_temp <- merge(sim$agent_characteristics_temp, sim$discourse_memory[ , .(sender, receiver_business, sender_business)], by.x = "agent_id", by.y = "sender") 
    sim$agent_characteristics_temp[ .("Send") , energy_loss := sender_business, on="best_axn_overall" ] 
    sim$agent_characteristics_temp[ .("Receive"), energy_loss := receiver_business, on="best_axn_overall" ] 
    sim$agent_characteristics_temp[ .("Both"), energy_loss := sender_business + receiver_business, on="best_axn_overall" ] 
    sim$agent_characteristics_temp[ .("Nothing"), energy_loss := 0, on="best_axn_overall" ] 
    sim$agent_characteristics_temp <- sim$agent_characteristics_temp[ , .(agent_id, energy_loss) ] 
    sim$agent_characteristics_temp <- unique(sim$agent_characteristics_temp) 
    sim$agent_characteristics_temp[ , energy_loss := sum(energy_loss), by="agent_id" ] 
    sim$agent_characteristics_temp <- unique(sim$agent_characteristics_temp) 
    sim$agent_characteristics <- sim$agent_characteristics_temp[sim$agent_characteristics, on="agent_id"] 
    sim$agent_characteristics[ , energy := energy - energy_loss + params(sim)$rc_sh_model$restoration_factor ] 
    sim$agent_characteristics[ , energy_loss := NULL ]
    setkey(sim$agent_characteristics, agent_id)
 
    ##################################################
    #### BUSINESS UPDATE FOR sim$discourse_memory ####
    ##################################################

    sim$discourse_memory <- sim$agent_characteristics[ , .(agent_id, opinion) ][sim$discourse_memory[ , -c("opinion")], on=c("agent_id" = "sender") ]  
    setnames(sim$discourse_memory, "agent_id", "sender") 
    sim$discourse_memory[ , past_receiver_business := ifelse(lengths(past_receiver_business) < params(sim)$rc_sh_model$energy_params_memory_depth,
                                            mapply(function(x, y) {
                                              list(c(unlist(x), y))
                                            }, x=past_receiver_business, y=receiver_business),
                                            mapply(function(x, y) {
                                              list(c(unlist(x)[1:params(sim)$rc_sh_model$opinion_memory_depth], y))
                                            }, x=past_receiver_business, y=receiver_business)
      )] 
    sim$discourse_memory[ , past_sender_business := ifelse(lengths(past_sender_business) < params(sim)$rc_sh_model$energy_params_memory_depth,
                                          mapply(function(x, y) {
                                            list(c(unlist(x), y))
                                          }, x=past_sender_business, y=sender_business),
                                          mapply(function(x, y) {
                                            list(c(unlist(x)[1:params(sim)$rc_sh_model$opinion_memory_depth], y))
                                          }, x=past_sender_business, y=sender_business)
      )] 
    setkey(sim$discourse_memory, sender)

  } else {
    
    #################################################
    #### UPDATING OF ENERGY IN CASE OF NO ACTION ####
    #################################################

    # the past_receiver_business and past_sender_business needs to be updated with 0 business for this round
    sim$discourse_memory[ , receiver_business := 0 ] 
    sim$discourse_memory[ , sender_business := 0 ] 
    sim$discourse_memory[ , past_receiver_business := ifelse(lengths(past_receiver_business) < params(sim)$rc_sh_model$energy_params_memory_depth,
                                            mapply(function(x, y) {
                                              list(c(unlist(x), y))
                                            }, x=past_receiver_business, y=receiver_business),
                                            mapply(function(x, y) {
                                              list(c(unlist(x)[1:params(sim)$rc_sh_model$energy_params_memory_depth], y))
                                            }, x=past_receiver_business, y=receiver_business)
      )] 
    sim$discourse_memory[ , past_sender_business := ifelse(lengths(past_sender_business) < params(sim)$rc_sh_model$energy_params_memory_depth,
                                          mapply(function(x, y) {
                                            list(c(unlist(x), y))
                                          }, x=past_sender_business, y=sender_business),
                                          mapply(function(x, y) {
                                            list(c(unlist(x)[1:params(sim)$rc_sh_model$energy_params_memory_depth], y))
                                          }, x=past_sender_business, y=sender_business)
      )]


  sim$opinion_updating <- copy(sim$discourse_memory) 
  sim$opinion_updating[ , opinion_receiver_new := sapply(past_opinions, function(x) { median(x) }) ] 
  sim$opinion_updating[ , opinion_receiver_new := mapply(function(x, y) { 
     
			series <- rev(unlist(x))
			new_series <- vector()

			      for (i in 1:length(x)) {

				      if (runif(1, 0, 1) > i / params(sim)$rc_energy_model$opinion_memory_depth) {
				      new_series <- c(new_series, series[i])
			      }
			      }
			      ifelse(is.na(median(new_series)), y, new_series)

          }, x=past_opinions, y=opinion   ) ] 
  setnames(sim$opinion_updating, "sender", "agent_id") 
  sim$opinion_updating[ , new_op_compl := mapply(function(x, y) { 
     
			series <- rev(unlist(x))
			new_series <- vector()

			      for (i in 1:length(x)) {

				      if (runif(1, 0, 1) > i / params(sim)$rc_energy_model$opinion_memory_depth) {
				      new_series <- c(new_series, series[i])
			      }
			      }
			      ifelse(is.na(median(new_series)), y, new_series)

          }, x=past_op_compls, y=op_compl   ) ] 
  sim$opinion_updating <- sim$opinion_updating[ , .(agent_id, opinion_receiver_new, new_op_compl)] 
  setkey(sim$opinion_updating, "agent_id") 
  sim$opinion_updating <- unique(sim$opinion_updating) 

  sim$agent_characteristics <- merge(sim$agent_characteristics, sim$opinion_updating, by="agent_id", all.x = TRUE)
  sim$agent_characteristics[ , opinion := ifelse(is.na(opinion_receiver_new), opinion, opinion_receiver_new)] 
  sim$agent_characteristics[ , op_compl := ifelse(is.na(opinion_receiver_new), op_compl, new_op_compl) ] 
  sim$agent_characteristics[ , opinion_receiver_new := NULL ][ , new_op_compl := NULL ]

  sim$agent_characteristics[ , energy := energy + params(sim)$rc_sh_model$restoration_factor ]
  setkey(sim$agent_characteristics, agent_id)

  }

  return(invisible(sim))

}

# FUNCTIONS


### produce_altered_message

produce_altered_message <- function(opinion_send, message_receive) {

  # produce altered message without epsilon bound
  if (opinion_send < message_receive) {

  altered_message <- opinion_send + ( abs(opinion_send - message_receive) / 2 )	  

  }

  if (opinion_send > message_receive) {

  altered_message <- opinion_send - ( abs(opinion_send - message_receive) / 2 )	  

  }
  
  if (opinion_send == message_receive) {

  altered_message <- opinion_send	  

  }

  return(altered_message)

}

### get_nbh_incohesion

get_nbh_incohesion <- function( id ) {

  nbh_indices <- sim$agent_characteristics[ agent_id == id , neighborhood ][[1]] 

  nbh_assumptions <- sim$messages[ sender == id & receiver %in% nbh_indices , assumption_receiver ]

  mean_deviations <- sapply(sum(as.vector(abs(outer(nbh_assumptions, nbh_assumptions, Vectorize("-")) ) ) ),  "/", ((length(nbh_assumptions)*length(nbh_assumptions)) - length(nbh_assumptions)) ) # mean not appropriate, 0 values of diagonal factor in denominator

  return(mean_deviations)

}

vec_get_nbh_incohesion <- Vectorize(get_nbh_incohesion)

### get_self_incohesion

get_self_incohesion <- function( id ) {

  past <- sim$discourse_memory[ sender == id, past_opinions ][[1]]

  mean_deviations <- sapply(sum(as.vector(abs(outer(past, past, Vectorize("-")) ) ) ),  "/", ((length(past)*length(past)) - length(past)) )

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

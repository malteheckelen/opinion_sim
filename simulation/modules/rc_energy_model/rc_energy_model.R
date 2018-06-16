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
  reqdPkgs = list("tidyverse", "data.table", "parallel"),
  parameters = rbind(
    # defineParameter("paramName", "paramClass", value, min, max, "parameter description"),
    defineParameter("epsilon", "numeric", 0.1, NA, NA, "The Bounded Confidence parameter."),
    defineParameter("other_incons_tolerance", "numeric", 0.1, NA, NA, "The parameter controlling the tolerance for the degree with which other agents to have varying opinions over time."),
    defineParameter("self_incons_tolerance", "numeric", 0.1, NA, NA, "The parameter controlling the tolerance for the degree with which the agent himself has varying opinions over time."),
    defineParameter("memory", "numeric", 1, NA, NA, "The number of time steps agents remember messages for."),
    defineParameter("energy_level", "numeric", 100, NA, NA, "The starting energy level of every agent."),
    defineParameter("restoration_factor", "numeric", 20, NA, NA, "The number of energy units that are restored to agents at the end of every round."),
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

  sim$agent_characteristics <- data.table(sim$agent_characteristics)
  sim$agent_characteristics[ , energy := params(sim)$rc_energy_model$energy_level]
  
  ######################################## 
  #### CONSTRUCT CHOSEN_ACTIONS TABLE ####
  ########################################

  sim$chosen_actions <- data.table(

    agent_id = rep(agent_characteristics$agent_id, each=2),
    action_type = rep(c("actions_overall", "actions_send"), sim$no_agents),
    best_action = rep(c("Not assigned"), length(sim$no_agents*2))

  ) 
  sim$chosen_actions[ , agent_id := as.integer(agent_id) ]
  setkey(sim$chosen_actions, agent_id)
  
  ######################################### 
  #### CONSTRUCT OVERALL_ACTIONS TABLE ####
  #########################################

  sim$actions_overall <- data.table(

    agent_id = rep(agent_characteristics$agent_id, each=4),
    actions = rep(c("Send", "Receive", "Both", "Nothing"), sim$no_agents),
    util_score = rep(0, length(sim$no_agents*4))

  ) 

  sim$actions_overall[ .("Both"), util_score := 1, on="actions" ] 
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
  sim$actions_overall <- unique(sim$actions_overall[ , .(agent_id, best_action) ])
  setkey(sim$actions_overall, agent_id)
  
  #######################################################
  #### ASSIGN BEST OVERALL ACTIONS TO CHOSEN ACTIONS ####
  #######################################################

  sim$chosen_actions <-  merge(sim$actions_overall, sim$chosen_actions, by=c("agent_id"), all.x=TRUE, all.y=TRUE) 
  sim$chosen_actions[ action_type == "actions_overall" , best_action := ifelse(!is.na(best_action.x), best_action.x,  "NOT ASSIGNED")]
  sim$chosen_actions[ , -c("best_action.x", "best_action.y")]
  setkey(sim$chosen_actions, agent_id)

  #############################################
  #### CONSTRUCT TABLE FOR SENDING ACTIONS ####
  #############################################

  sim$actions_send <- data.table(

    agent_id = rep(agent_characteristics$agent_id, each=2),
    actions = rep(c("Unoptimized", "Optimized"), sim$no_agents),
    util_score = rep(0, length(sim$no_agents*2))

  ) 
 sim$actions_send[ , agent_id := as.integer(agent_id) ]
 setkey(sim$actions_send, agent_id)
  
  #################################
  #### CONSTRUCT MESSAGE TABLE ####
  #################################

  # 1.)
  sim$messages <- sim$environment %>%
    activate(edges) %>%
    as_tibble()
  sim$messages_temp <- data.table(sim$messages_temp) 
  setnames(sim$messages, old = c("from", "to"), new = c("receiver", "sender"))

  # 2.)
  sim$messages_temp <- sim$environment %>%
    activate(edges) %>%
    as_tibble() 
  sim$messages_temp <- data.table(sim$messages_temp)
  setnames(sim$messages_temp, old = c("from", "to"), new = c("sender", "receiver"))

  sim$messages <- rbind(sim$messages_temp, sim$messages)
  sim$messages <- sim$messages[sim$agent_characteristics[, .(agent_id, opinion)], nomatch = 0L, on = c("sender" = "agent_id")]
  setnames(sim$messages, "opinion", "opinion_sender")
  setkey(sim$messages, sender, receiver)

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
  sim$messages_temp[ sender != receiver ]
  sim$messages_temp <- unique(sim$messages_temp)
  setkey(sim$messages_temp, "sender") 
  sim$messages_temp <- unique(sim$messages_temp)
  sim$messages_temp[ , sender := as.integer(sender)] 
  sim$messages_temp[ , receiver := as.integer(receiver)] 
  setkey(sim$messages_temp, sender, receiver)
  setkey(sim$messages, sender, receiver)
  sim$messages <- sim$messages_temp[unique(sim$messages), on=c("sender", "receiver"), nomatch = 0L] 
  sim$messages[ , opt_message := median(opt_message), by = .(sender)] 
  setkey(sim$messages, sender, receiver)

  ##########################################
  #### CONSTRUCT DISCOURSE MEMORY TABLE ####
  ##########################################

  sim$discourse_memory <- sim$messages[ , -c("receiver", "assumption_receiver") ] 
  sim$discourse_memory[ , past_opinions := sapply(opinion_sender, function(x) {list(
		    ifelse(rnorm(times(sim)$end[1], x, params(sim)$rc_energy_model$initial_opinion_confidence) > 1, 1,
			    ifelse(rnorm(times(sim)$end[1], x, params(sim)$rc_energy_model$initial_opinion_confidence) < 0, 0, rnorm(times(sim)$end[1], x, params(sim)$rc_energy_model$initial_opinion_confidence) ) ) )  } )] 
  setnames(sim$discourse_memory, "opinion_sender", "opinion") 
  sim$discourse_memory <- sim$discourse_memory[ , .(sender, opinion, past_opinions)] 
  sim$discourse_memory <- sim$discourse_memory[ , .SD[1], by="sender" ]
  sim$discourse_memory[ , sender_business := 0 ] 
  sim$discourse_memory[ , receiver_business := 0 ]
  setkey(sim$discourse_memory, sender)
  
   if( length(sim$actions_overall[ .(c("Both", "Send")) , best_action, on="best_action", nomatch=0L ] > 0 ) ) {

    #######################################
    #### DETERMINE BEST SENDING ACTION ####
    #######################################
	   
    sim$actions_send_temp <- sim$messages[sim$actions_overall, on = c("sender" = "agent_id"), nomatch = 0L ] 
    sim$actions_send_temp <- sim$actions_send_temp[ .(c("Both", "Send")), on="best_action", nomatch=0L ]
    sim$actions_send_temp <- sim$actions_send_temp[ , .(sender , receiver , opt_message , opinion_sender , assumption_receiver) ]
    sim$actions_send <- sim$actions_send_temp[sim$actions_send, on = c("sender" = "agent_id"), nomatch = 0L, allow.cartesian = TRUE ] 
    sim$actions_send <- sim$actions_send[sim$discourse_memory, on = c("sender"), nomatch = 0L] 
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
    sim$actions_send[ , past_opinions := mapply(function(x) list(eval(parse(text = x))), x=past_opinions )] 
    sim$actions_send_temp <- copy(sim$actions_send)[ , -c("receiver", "assumption_receiver") ]
    sim$actions_send_temp <- sim$actions_send_temp[ , .SD[1], by="sender", nomatch=0L ]
    sim$actions_send_temp[, distance_to_past_opinions := mapply(function(a,b,c,k) {

    series <- rev(unlist(a))
    new_series <- vector()

      for (i in 1:length(a)) {

	      if (runif(1, 0, 1) < 1/i**params(sim)$rc_energy_model$memory ) {
	      new_series <- c(new_series, series[i])
      }
      }

        switch(k,
               "Unoptimized" = {
                 mean(
                   sapply(new_series, function(x) {
                     abs(x - c)
                   })
                 )
               },
               "Optimized" = {
                 mean(
                   sapply(new_series, function(x) {
                     abs(x - b)
                   })
                 )
               },
               "Unoptimized_appeal" = {
                 mean(
                   sapply(new_series, function(x) {
                     abs(x - c)
                   })
                 )
               },
               "Optimized_appeal" = {
                 mean(
                   sapply(new_series, function(x) {
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
      }, a=opinion_sender, b=opt_message, k=actions  )] 
    sim$actions_send[, distance_message_assumption := mapply(function(a,b,c,k) {
        switch(k,
               "Unoptimized" = {
                 abs(c - a)
               },
               "Optimized" = {
                 abs(c - b)
               }
        )
      }, a=opinion_sender, b=opt_message, c=assumption_receiver, k=actions )]
    sim$actions_send[ , past_opinions := NULL ][ , receiver := NULL ] 
    sim$actions_send <- sim$actions_send_temp[ , .(sender, distance_to_past_opinions) ][sim$actions_send, on="sender" ]
    sim$actions_send[ , util_score := 0 - distance_to_past_opinions - distance_message_opinion - distance_message_assumption] 
    sim$actions_send[ , agent_id := sender ] 
    sim$actions_send[ , .(agent_id, actions, util_score)]
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
    sim$actions_send <- sim$actions_send[ , .(agent_id, best_action) ]
    sim$actions_send <- unique(sim$actions_send)
    setkey(sim$actions_send, agent_id)

    #####################################
    #### UPDATE CHOSEN_ACTIONS TABLE ####
    #####################################

    sim$chosen_actions <- merge(sim$actions_send, sim$chosen_actions, by="agent_id", all.x=TRUE, all.y=TRUE) 
    sim$chosen_actions[ .("Receive") , action_type := "actions_overall", on="best_action.y", nomatch=0L ] 
    sim$chosen_actions[ .("Nothing") , action_type := "actions_overall", on="best_action.y", nomatch=0L ] 
    sim$chosen_actions[ .("actions_send") , best_action := ifelse(!is.na(best_action.x), best_action.x, "NOT ASSIGNED"), on="action_type", nomatch=0L ] 
    sim$chosen_actions[ .("actions_overall") , best_action := ifelse(!is.na(best_action.y), best_action.y, "NOT ASSIGNED"), on="action_type", nomatch=0L]
    sim$chosen_actions[ , -c("best_action.x", "best_action.y")]
    setkey(sim$chosen_actions, agent_id)
    
    #################################
    #### UPDATE DISCOURSE_MEMORY ####
    #################################

    sim$discourse_memory_temp <- sim$actions_send[sim$messages, on = c("agent_id" = "sender"), nomatch = 0L]
    setnames(sim$discourse_memory_temp,"agent_id", "sender")
    setkey(sim$discourse_memory_temp, "sender", "receiver")
    sim$discourse_memory_temp[ , sender_business := .N, by = "sender" ]
    sim$discourse_memory_temp[ , receiver_business := .N, by = "receiver" ]
    sim$discourse_memory <- merge(sim$discourse_memory_temp, sim$discourse_memory[ , -c("sender_business", "receiver_business", "assumption_receiver")], by=c("sender"), all.x=TRUE)
    sim$discourse_memory[ , sender_business := ifelse(is.na(sender_business), 0, sender_business) ] 
    sim$discourse_memory[ , receiver_business := ifelse(is.na(receiver_business), 0, receiver_business) ] 
    sim$discourse_memory[ , past_messages := mapply(function(x,y,z) {

	      ifelse(x == "Unoptimized",
		      y,
		      z)

				   }, x=best_action, y=opinion_sender, z=opt_message  ) ]
    sim$discourse_memory[ , past_opinions := mapply(function(x) list(eval(parse(text = x))), x=past_opinions)] 
    sim$discourse_memory[ , past_messages:= mapply(function(x) list(eval(parse(text = x))), x=past_messages)] 
    sim$discourse_memory[ , past_opinions := mapply(function(x,y) {
	    list(c(unlist(x), y))}
	    , x=past_opinions, y=opinion_sender )
   ]
    setnames(sim$discourse_memory, "opinion_sender", "opinion")
    setnames(sim$discourse_memory, "opt_message", "message") 
    sim$discourse_memory <- sim$discourse_memory[ , .(sender, receiver, opinion, message, past_messages, past_opinions, sender_business, receiver_business)]

    temp <- sim$discourse_memory[ , .(receiver, receiver_business)] 
    temp[ , receiver_business := max(receiver_business), by=receiver]
    temp <- unique(temp) 

    print(nrow(sim$discourse_memory))
    sim$discourse_memory <- merge(sim$discourse_memory[ , -c("receiver_business")], temp, by.x = c("sender"), by.y = c("receiver"), all.x = TRUE) 
    sim$discourse_memory[ , receiver := NULL ] 
    test <<- sim$discourse_memory
    sim$discourse_memory <- sim$discourse_memory[ , .SD[1], by="sender", nomatch=0L ]
    sim$discourse_memory[ , past_receiver_business := ifelse( !is.na(receiver_business), mapply(function(x) list(x), x=receiver_business   ), 0 ) ]
    sim$discourse_memory[ , past_sender_business := ifelse( !is.na(sender_business), mapply(function(x) list(x), x=sender_business   ), 0 ) ] 
    setkey(sim$messages, "sender", "receiver") # for nbh_incohesion lookup
    sim$discourse_memory[ , nbh_incohesion := vec_get_nbh_incohesion( sender ) ] 
    sim$discourse_memory[ , past_nbh_incohesion := ifelse( !is.na(nbh_incohesion), sapply(nbh_incohesion, function(x) list(x) ), 0 ) ]
    sim$discourse_memory[ , self_incohesion := vec_get_self_incohesion( sender ) ]
    sim$discourse_memory[ , past_self_incohesion := ifelse( !is.na(self_incohesion), sapply(self_incohesion, function(x) list(x) ), 0 ) ] 
    
    #########################
    #### "SEND" MESSAGES ####
    #########################

    sim$messages_temp <- copy(sim$messages)
    setnames(sim$messages_temp, old = c("sender", "receiver"), new = c("receiver", "sender")) 
    sim$messages_temp <- sim$messages_temp[ , .(receiver, opinion_sender, opt_message)] 
    setnames(sim$messages_temp, old=c("opinion_sender"), new=c("opinion_receiver"))
    sim$messages_temp <- unique(sim$messages_temp) 
    sim$messages <- sim$messages_temp[sim$messages[ , -c("opt_message") ], on="receiver", nomatch=0L] 

    sim$messages <- sim$messages[sim$actions_send, nomatch=0L, on=c("sender" = "agent_id")] 
    sim$messages[ , assumption_sender := ifelse( best_action == "Unoptimized", opinion_sender, opt_message)] 
    sim$messages[ , actions := NULL ][ , best_action := NULL ]
    setkey(sim$messages, "sender", "receiver")

    ###################
    #### RECEIVING ####
    ###################
    
 merge_receiver_opinions <- sim$discourse_memory[ , .(sender, past_opinions)]

 sim$opinion_updating <- sim$messages[ , -c("actions", "best_action")][actions_overall, on = c("receiver" = "agent_id"), nomatch = 0L ] 
 sim$opinion_updating <- sim$opinion_updating[ , .(sender, receiver, opinion_receiver, assumption_sender, opt_message, best_action)]
 sim$opinion_updating <- sim$opinion_updating[merge_receiver_opinions, on=c("receiver"="sender"), nomatch = 0L] 
 sim$opinion_updating[ , distance_to_past_opinions := mapply(function(a,b) {

    series <- rev(unlist(a))
    new_series <- vector()

      for (i in 1:length(a)) {

	      if (runif(1, 0, 1) < 1/i**params(sim)$rc_energy_model$memory ) {
	      new_series <- c(new_series, series[i])
      }
      }

		mean(
		  sapply(new_series, function(x) {
		    abs(x - b)
		  })
		)
 }, a=past_opinions, b=assumption_sender)] 
 sim$opinion_updating[ , past_opinions := NULL ]
 sim$opinion_updating[ , within_epsilon := abs(opinion_receiver - assumption_sender) < params(sim)$rc_energy_model$epsilon]
 sim$opinion_updating[ , self_consistent := distance_to_past_opinions < params(sim)$rc_energy_model$self_incons_tolerance]
 sim$opinion_updating <- sim$opinion_updating[ within_epsilon == TRUE & self_consistent == TRUE ] 
 sim$opinion_updating[ , sum_assumptions := sum(assumption_sender), by=receiver] 
 sim$opinion_updating[ , denominator := .N, by=receiver ]
 sim$opinion_updating[ , reception_energy_loss := .N, by=receiver ] 
 sim$opinion_updating[ , opinion_receiver_new := ifelse( denominator != 0, sum_assumptions / denominator, opinion_receiver )]
 setnames(sim$opinion_updating, "receiver", "agent_id") 
 sim$opinion_updating <- sim$opinion_updating[ , .(agent_id, opinion_receiver_new)]
 sim$opinion_updating <-unique(sim$opinion_updating) 
 setkey(sim$opinion_updating, "agent_id")

 ######################################
 #### UPDATE agent_characteristics ####
 ######################################

 sim$agent_characteristics <- merge(sim$agent_characteristics, sim$opinion_updating, by="agent_id", all.x = TRUE) 
 sim$agent_characteristics[ , opinion := ifelse(is.na(opinion_receiver_new), opinion, opinion_receiver_new)] 
 sim$agent_characteristics[ , opinion_receiver_new := NULL ]
 
 sim$agent_characteristics_temp <- sim$actions_overall[ , best_axn_overall := best_action][ , .(agent_id, best_axn_overall)] 
 sim$agent_characteristics_temp <- merge(sim$agent_characteristics_temp, copy(sim$actions_send)[ , best_axn_send := best_action][ , .(agent_id, best_axn_send)], by="agent_id")
 sim$agent_characteristics_temp <- merge(sim$agent_characteristics_temp, copy(sim$discourse_memory)[ , .(sender, receiver_business, sender_business)], by.x = "agent_id", by.y = "sender") 
 sim$agent_characteristics_temp[ .("Send") , energy_loss := sender_business, on="best_axn_overall", nomatch=0L ]
 sim$agent_characteristics_temp[ .("Receive"), energy_loss := receiver_business, on="best_axn_overall", nomatch=0L ]
 sim$agent_characteristics_temp[ .("Both"), energy_loss := sender_business + receiver_business, on="best_axn_overall", nomatch=0L ]
 sim$agent_characteristics_temp[ .("Nothing"), energy_loss := 0, on="best_axn_overall", nomatch=0L ]
 sim$agent_characteristics_temp[ .("Unoptimized"), energy_loss := energy_loss, on="best_axn_send", nomatch=0L ] 
 sim$agent_characteristics_temp[ .("Optimized"), energy_loss := energy_loss + sender_business, on="best_axn_send", nomatch=0L ] 
 sim$agent_characteristics_temp <- sim$agent_characteristics_temp[ , .(agent_id, energy_loss) ]
 sim$agent_characteristics_temp <- unique(sim$agent_characteristics_temp) 
 sim$agent_characteristics_temp[ , energy_loss := sum(energy_loss), by="agent_id" ] 
 sim$agent_characteristics_temp <- unique(sim$agent_characteristics_temp) 

 sim$agent_characteristics <- sim$agent_characteristics_temp[sim$agent_characteristics, on="agent_id"]
 sim$agent_characteristics[ , energy := energy - energy_loss + params(sim)$rc_energy_model$restoration_factor ]
 sim$agent_characteristics[ , energy_loss := NULL ]
 setkey(sim$agent_characteristics, agent_id)

 ##################################################
 #### BUSINESS UPDATE FOR sim$discourse_memory ####
 ##################################################

 sim$discourse_memory <- sim$agent_characteristics[ , .(agent_id, opinion) ][sim$discourse_memory[ , -c("opinion")], on=c("agent_id" = "sender") ]  
 setnames(sim$discourse_memory, "agent_id", "sender") 
 setkey(sim$discourse_memory, sender)

  } else {
    
    #################################################
    #### UPDATING OF ENERGY IN CASE OF NO ACTION ####
    #################################################

  # irrelevant, it does not happen
	  
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
  setkey(sim$actions_send, "agent_id", "actions")
  
  sim$actions_overall <- data.table(

    agent_id = rep(sim$agent_characteristics$agent_id, each=4),
    actions = rep(c("Send", "Receive", "Both", "Nothing"), sim$no_agents),
    util_score = rep(0, length(sim$no_agents*4))

  ) 
setkey(sim$actions_overall, "agent_id", "actions")

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
  setkey(sim$messages, "sender", "receiver")
  sim$messages <- sim$messages[sim$agent_characteristics[, .(agent_id, opinion)], nomatch = 0L, on = c("sender" = "agent_id") ]
  setnames(sim$messages, "opinion", "opinion_sender")
  setkey(sim$messages, "sender", "receiver")

  ######################################## 
  #### CONSTRUCT CHOSEN_ACTIONS TABLE ####
  ########################################

  sim$chosen_actions <- data.table(

    agent_id = rep(agent_characteristics$agent_id, each=2),
    action_type = rep(c("actions_overall", "actions_send"), sim$no_agents),
    best_action = rep(c("Not assigned"), length(sim$no_agents*2))

  ) 
  sim$chosen_actions[ , agent_id := as.integer(agent_id) ]
  setkey(sim$chosen_actions, "agent_id", "action_type", "best_action")

  #################################################################
  #### BUILD sim$actions_overall AND FIND BEST OVERALL ACTIONS ####
  #################################################################

  sim$actions_overall_temp <- sim$messages[ , .(sender, receiver)]
  sim$actions_overall_temp[ , max_send_energy_loss := .N, by = sender ] 
  sim$actions_overall_temp[ , max_receive_energy_loss := .N, by = sender ] 
  sim$actions_overall_temp <- sim$actions_overall_temp[sim$discourse_memory, on=c("sender") ] 
  sim$actions_overall_temp <- sim$actions_overall_temp[sim$discourse_memory[, .(sender, message)][ , assumption_receiver := message][ , -c("message")], on=c("receiver"="sender"), nomatch=0L] 
  sim$actions_overall_temp[, distance_to_past_opinions := mapply(function(a,b) {

    series <- rev(unlist(a))
    new_series <- vector()

      for (i in 1:length(a)) {

	      if (runif(1, 0, 1) < 1/i**params(sim)$rc_energy_model$memory ) {
	      new_series <- c(new_series, series[i])
      }
      }

        mean(
          sapply(new_series, function(x) {
            abs(x - b)
          })
        )
      }, a=past_opinions, b=assumption_receiver)]
  sim$actions_overall_temp <- sim$actions_overall_temp[ , .(distance_to_past_opinions, sender, past_self_incohesion, past_nbh_incohesion, sender_business, receiver_business, past_receiver_business, past_sender_business, nbh_incohesion, self_incohesion, max_send_energy_loss, max_receive_energy_loss)] 
  sim$actions_overall_temp[ , control := mean(distance_to_past_opinions) , by=sender ] 
  sim$actions_overall_temp <- sim$actions_overall_temp[ , .SD[1], by="sender", nomatch=0L]
  sim$actions_overall_temp <- sim$actions_overall_temp[sim$agent_characteristics[ , .(agent_id, energy)], on=c("sender" = "agent_id") ] 
  sim$actions_overall_temp[ , rec_business_mean_index := mapply(function(x, y) {

      is_na <- sum(unlist(x)) / (y*length(unlist(x)))
      
      ifelse(is.na(is_na), 0, is_na)

    }, x=past_receiver_business, y=max_receive_energy_loss   )] 
  sim$actions_overall_temp[ , send_business_mean_index := mapply(function(x, y) {

      is_na <- sum(unlist(x)) / (y*length(unlist(x)))
      
      ifelse(is.na(is_na), 0, is_na)

    }, x=past_sender_business, y=max_send_energy_loss   )] 
  sim$actions_overall_temp[ , rec_business_mean := mapply(function(x, y) {
     
series <- rev(unlist(x))
new_series <- vector()

      for (i in 1:length(x)) {

	      if (runif(1, 0, 1) < 1/i**params(sim)$rc_energy_model$memory ) {
	      new_series <- c(new_series, series[i])
      }
      }
      ifelse(is.na(mean(new_series)), y, new_series)

    }, x=past_receiver_business, y=receiver_business   )] 
  sim$actions_overall_temp[ , send_business_mean := mapply(function(x, y) {

series <- rev(unlist(x))
new_series <- vector()

      for (i in 1:length(x)) {

	      if (runif(1, 0, 1) < 1/i**params(sim)$rc_energy_model$memory) {
	      new_series <- c(new_series, series[i])
      }
      }
      ifelse(is.na(mean(new_series)), y, new_series)

    }, x=past_sender_business, y=sender_business   )] 
  sim$actions_overall_temp[ , both_business_mean := mapply(function(x, y, a, b) {

series <- rev(unlist(x))
new_series_one <- vector()

      for (i in 1:length(x)) {

	      if (runif(1, 0, 1) < 1/i**params(sim)$rc_energy_model$memory) {
	      new_series_one <- c(new_series_one, series[i])
      }
      }

      mean_one <- ifelse(is.na(mean(new_series_one)), a, new_series_one)

series <- rev(unlist(y))
new_series_two <- vector()

      for (i in 1:length(y)) {

	      if (runif(1, 0, 1) < 1/i**params(sim)$rc_energy_model$memory) {
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

    }, x=past_sender_business, y=past_receiver_business, z = max_receive_energy_loss, a = max_send_energy_loss   )] 
  sim$actions_overall <- sim$actions_overall_temp[sim$actions_overall[ , -c("best_action")], on=c("sender" = "agent_id"), allow.cartesian = TRUE] 
  setnames(sim$actions_overall, "sender", "agent_id") 
  sim$actions_overall[ .("Send") , energy_loss := ( energy - sender_business ), on="actions", nomatch=0L] 
  sim$actions_overall[ .("Receive") , energy_loss := ( energy - receiver_business ), on="actions", nomatch=0L] 
  sim$actions_overall[ .("Both") , energy_loss := ( energy - sender_business - receiver_business ), on="actions", nomatch=0L] 
  sim$actions_overall[ .("Send") , projected_energy := ifelse(energy_loss <= 0, 0, energy_loss) / ( energy + params(sim)$rc_energy_model$restoration_factor ), on="actions", nomatch=0L ] 
  sim$actions_overall[ .("Receive") , projected_energy := ifelse(energy_loss <= 0, 0, energy_loss) / ( energy + params(sim)$rc_energy_model$restoration_factor ), on="actions", nomatch=0L ] 
  sim$actions_overall[ .("Both") , projected_energy := ifelse(energy_loss <= 0, 0, energy_loss) / ( energy + params(sim)$rc_energy_model$restoration_factor ), on="actions", nomatch=0L ]  
  sim$actions_overall[ , projected_energy := ifelse(is.na(projected_energy), 0, projected_energy) ] 
  sim$actions_overall[ , dissonance := mapply(function(x, y) {
	    
	    sum(x, y)
	   
    }, x=nbh_incohesion, y=self_incohesion  )  ] 
  sim$actions_overall[ .("Send"), util_score :=
         ifelse(projected_energy <= 0, 0, projected_energy) + ( 1 - self_incohesion), on="actions", nomatch=0L  ] 
  sim$actions_overall[ .("Receive"), util_score := 
         ifelse(projected_energy <= 0, 0, projected_energy) + ( 1 - nbh_incohesion ), on="actions", nomatch=0L ] 
  sim$actions_overall[ .("Both"), util_score :=  
         ifelse(projected_energy <= 0, 0, projected_energy) + control + ( ( self_incohesion + nbh_incohesion ) * ( 1 - abs( self_incohesion - nbh_incohesion) ) ), on="actions", nomatch=0L ] 
  sim$actions_overall[ .("Nothing"), util_score := ifelse( ( energy - sender_business <= 0 | energy - receiver_business <= 0) ,
	    max(util_score)+10000, min(util_score)-10000), on="actions", nomatch=0L ] 
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
  sim$actions_overall <- unique(sim$actions_overall[ , .(agent_id, best_action) ])
  setkey(sim$actions_overall, "agent_id")
  
  # define lists

  sim$bothers <- unique(sim$actions_overall[ .("Both"), on="best_action", nomatch=0L ]$agent_id)
  sim$senders <- unique(sim$actions_overall[ .("Send"), on="best_action", nomatch=0L ]$agent_id)
  sim$receivers <- unique(sim$actions_overall[ .("Receive"), on="best_action", nomatch=0L ]$agent_id)
  sim$nothingers <- unique(sim$actions_overall[ .("Nothing"), on="best_action", nomatch=0L ]$agent_id)

  ###################################
  #### UPDATE sim$chosen_actions ####
  ###################################

  sim$chosen_actions <- merge(sim$actions_overall, sim$chosen_actions, by=c("agent_id"), all.x=TRUE, all.y=TRUE) 
  sim$chosen_actions[ is.na(action_type) , action_type := "actions_overall" ] 
  sim$chosen_actions[ .("actions_overall") , best_action := ifelse(!is.na(best_action.x), best_action.x,  "NOT ASSIGNED"), on="action_type", nomatch=0L] 
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
  sim$messages_temp <- unique(sim$messages_temp)
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

  sim$discourse_memory <- sim$discourse_memory[ , opinion := NULL ][sim$agent_characteristics[ , .(agent_id, opinion) ], on=c("sender" = "agent_id")] 
  sim$discourse_memory[ , past_opinions := mapply(function(x,y) {
	    list(c(unlist(x), y))}
	    , x=past_opinions, y=opinion )
   ]
  setkey(sim$discourse_memory, sender)

  if( length(c(sim$bothers, sim$senders)) > 0 )  {
   
    ###############################################################
    #### COMPUTE SENDING UTILITIES AND CHOOSE MAX UTIL ACTIONS ####
    ###############################################################

    sim$actions_send_temp <- sim$messages[ .(c(sim$senders, sim$bothers)), on="sender", nomatch=0L ]
    sim$actions_send_temp <- sim$actions_send_temp[ , .(sender, receiver , opt_message , opinion_sender , assumption_receiver)] 
    sim$actions_send <- sim$actions_send_temp[sim$actions_send, on = c("sender" = "agent_id"), nomatch = 0L, allow.cartesian = TRUE ] 
    sim$actions_send <- sim$actions_send[sim$discourse_memory, on = c("sender"), nomatch = 0L ] 
    sim$actions_send_temp <- copy(sim$actions_send)[ , -c("receiver", "assumption_receiver") ]
    sim$actions_send_temp <- sim$actions_send_temp[ , .SD[1], by="sender", nomatch=0L ]
    sim$actions_send_temp[, distance_to_past_opinions := mapply(function(a,b,c,k) {

    series <- rev(unlist(a))
    new_series <- vector()

      for (i in 1:length(a)) {

	      if (runif(1, 0, 1) < 1/i**params(sim)$rc_energy_model$memory ) {
	      new_series <- c(new_series, series[i])
      }
      }

        switch(k,
               "Unoptimized" = {
                 mean(
                   sapply(new_series, function(x) {
                     abs(x - c)
                   })
                 )
               },
               "Optimized" = {
                 mean(
                   sapply(new_series, function(x) {
                     abs(x - b)
                   })
                 )
               },
               "Unoptimized_appeal" = {
                 mean(
                   sapply(new_series, function(x) {
                     abs(x - c)
                   })
                 )
               },
               "Optimized_appeal" = {
                 mean(
                   sapply(new_series, function(x) {
                     abs(x - b)
                   })
                 )
               }
        )
      }, a=past_opinions, b=opt_message, c=opinion_sender, k=actions)]
    sim$actions_send_temp[, distance_to_past_messages := mapply(function(a,b,c,k) {

    series <- rev(unlist(a))
    new_series <- vector()

      for (i in 1:length(a)) {

	      if (runif(1, 0, 1) < 1/i**params(sim)$rc_energy_model$memory ) {
	      new_series <- c(new_series, series[i])
      }
      }

        switch(k,
               "Unoptimized" = {
                 mean(
                   sapply(new_series, function(x) {
                     abs(x - c)
                   })
                 )
               },
               "Optimized" = {
                 mean(
                   sapply(new_series, function(x) {
                     abs(x - b)
                   })
                 )
               },
               "Unoptimized_appeal" = {
                 mean(
                   sapply(new_series, function(x) {
                     abs(x - c)
                   })
                 )
               },
               "Optimized_appeal" = {
                 mean(
                   sapply(new_series, function(x) {
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
      }, a=opinion_sender, b=opt_message, k=actions  )] 
    sim$actions_send[, distance_message_assumption := mapply(function(a,b,c,k) {
        switch(k,
               "Unoptimized" = {
                 abs(c - a)
               },
               "Optimized" = {
                 abs(c - b)
               }
        )
      }, a=opinion_sender, b=opt_message, c=assumption_receiver, k=actions  )] 
    sim$actions_send[ , past_opinions := NULL ][ , receiver := NULL ][ , past_messages := NULL ] 
    sim$actions_send <- sim$actions_send_temp[ , .(sender, distance_to_past_messages, distance_to_past_opinions) ][sim$actions_send, on="sender" ]
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
    sim$actions_send <- unique(sim$actions_send[ , .(agent_id, best_action) ])
    setkey(sim$actions_send, agent_id)

    ###################################
    #### UPDATE sim$chosen_actions ####
    ###################################

    sim$chosen_actions <- merge(sim$actions_send, sim$chosen_actions, by="agent_id", all.x=TRUE , all.y=TRUE) 
    sim$chosen_actions[ .("Receive") , action_type := "actions_overall", on="best_action.y", nomatch=0L ] 
    sim$chosen_actions[ .("Nothing") , action_type := "actions_overall", on="best_action.y", nomatch=0L ] 
    sim$chosen_actions[ .("actions_send") , best_action := ifelse(!is.na(best_action.x), best_action.x, "NOT ASSIGNED"), on="action_type", nomatch=0L] 
    sim$chosen_actions[ .("actions_overall") , best_action := ifelse(!is.na(best_action.y), best_action.y, "NOT ASSIGNED"), on="action_type", nomatch=0L ] 
    sim$chosen_actions[ , best_action.x := NULL ][ , best_action.y := NULL ]

    print(table(sim$chosen_actions$best_action))

    #####################################
    #### UPDATE sim$discourse_memory ####
    #####################################

    sim$discourse_memory_temp <- sim$actions_send[ , -c("util_score")][sim$messages, on = c("agent_id" = "sender"), nomatch = 0L]  
    setnames(sim$discourse_memory_temp, "agent_id", "sender") 
    setkey(sim$discourse_memory_temp, "sender") 
    sim$discourse_memory_temp[ , opinion_sender := NULL ] 
    sim$discourse_memory <- merge(sim$discourse_memory_temp, sim$discourse_memory[ , -c("sender_business", "receiver_business", "assumption_receiver")], by=c("sender"), all.x=TRUE, all.y=TRUE) 
    sim$discourse_memory[ is.na(best_action) , past_messages := past_messages ]
    sim$discourse_memory[ .(c("Unoptimized", "Unoptimized_appeal")) , 
	    past_messages := mapply(function(x, y) {
			    list(c(unlist(x), y))
		 }
	    , x=past_messages, y=opinion ), on="best_action" ]
    sim$discourse_memory[ .(c("Optimized", "Optimized_appeal")) , 
	    past_messages := mapply(function(x, y) {
			    list(c(unlist(x), y))
		 }
	    , x=past_messages, y=opinion ), on="best_action" ]
    
    sim$discourse_memory[ is.na(opt_message) , message := message ] 
    sim$discourse_memory[ !is.na(opt_message) , message := ifelse( best_action == "Unoptimized", opinion, opt_message) ] 
    sim$discourse_memory[ , opt_message := NULL ]
    sim$discourse_memory <- sim$discourse_memory[ , .(sender, receiver, opinion, message, past_messages, past_opinions, past_sender_business, past_receiver_business, past_nbh_incohesion, past_self_incohesion)] 
    sim$discourse_memory <- sim$discourse_memory[ , .SD[1], by=c("sender", "receiver"), nomatch=0L]
    setkey(sim$discourse_memory, sender)
bingbong <<- sim$discourse_memory
    sim$send_busy <- sim$actions_send[ , -c("best_action") ][sim$messages[ , .(sender, receiver, opt_message) ], on = c("agent_id" = "sender"), nomatch = 0L]  
    setnames(sim$send_busy, "agent_id", "sender") 
    setkey(sim$send_busy, "sender") 
    sim$send_busy[ , sender_business := .N, by = "sender" ] 
    sim$send_busy[ , receiver_business := .N, by = "receiver" ] 
    sim$send_busy[ , opinion_sender := NULL ] 

    sim$receive_busy <- copy(sim$send_busy)[ , .(receiver, receiver_business)] 
    sim$receive_busy <- sim$receive_busy[ !is.na(receiver) ] 
    sim$receive_busy[ , receiver_business := max(receiver_business), by=receiver] 
    sim$receive_busy <- unique(sim$receive_busy) 

    sim$discourse_memory <- merge(sim$discourse_memory, unique(sim$send_busy[ , -c("receiver", "receiver_business") ]), by=c("sender"), all.x=TRUE, all.y=TRUE) 
    sim$discourse_memory <- sim$discourse_memory[ , .SD[1], by="sender", nomatch=0L ]
    sim$discourse_memory <- merge(sim$discourse_memory, sim$receive_busy, by=c("sender"="receiver"), all.x=TRUE ) 
    sim$discourse_memory[ , sender_business := ifelse(is.na(sender_business), 0, sender_business) ] 
    sim$discourse_memory[ , receiver_business := ifelse(is.na(receiver_business), 0, receiver_business) ] 
    setkey(sim$discourse_memory, "sender")
    sim$discourse_memory[ , nbh_incohesion := vec_get_nbh_incohesion(sender) ] 
    sim$discourse_memory[ , self_incohesion := vec_get_self_incohesion( sender ) ] 
    sim$discourse_memory <- sim$discourse_memory[ , .(sender, opinion, message, nbh_incohesion, self_incohesion, past_messages, past_opinions, sender_business, receiver_business, past_sender_business, past_receiver_business, past_nbh_incohesion, past_self_incohesion)] 
    sim$discourse_memory[ , past_sender_business := mapply(function(x,y) {
	    list(c(unlist(x), y))}
	    , x=past_sender_business, y=sender_business )
   ]
    sim$discourse_memory[ , past_receiver_business := mapply(function(x,y) {
	    list(c(unlist(x), y))}
	    , x=past_receiver_business, y=receiver_business )
   ]
    sim$discourse_memory[ , past_nbh_incohesion := mapply(function(x,y) {
	    list(c(unlist(x), y))}
	    , x=past_nbh_incohesion, y=nbh_incohesion )
   ]
    sim$discourse_memory[ , past_self_incohesion := mapply(function(x,y) {
	    list(c(unlist(x), y))}
	    , x=past_self_incohesion, y=self_incohesion )
   ]

    sim$messages_temp <- copy(sim$messages) 
    setnames(sim$messages_temp, old = c("sender", "receiver"), new = c("receiver", "sender")) 
    sim$messages_temp <- sim$messages_temp[ , .(receiver, opinion_sender, opt_message)] 
    setnames(sim$messages_temp, "opinion_sender", "opinion_receiver")

    sim$messages <- unique(sim$messages_temp)[sim$messages[ , -c("opt_message") ], on="receiver", nomatch=0L] 
    sim$messages <- sim$messages[sim$actions_send, nomatch=0L, on=c("sender" = "agent_id")]  
    sim$messages[ , assumption_sender := ifelse( best_action == "Unoptimized", opinion_sender, opt_message)] 
    sim$messages[ , actions := NULL ][ , best_action := NULL ] 
    setkey(sim$messages, "sender") 
    sim$messages <- unique(sim$messages)


    # Receiving

if ( length(c(sim$bothers, sim$receivers)) > 0) {

    merge_receiver_opinions <- sim$discourse_memory[ , .(sender, past_opinions)]

    merge_sender_messages <- sim$discourse_memory[ , .(sender, past_messages)]

    sim$opinion_updating <- sim$messages[ .(c(sim$bothers, sim$receivers)), on="receiver", nomatch=0L  ]
    sim$opinion_updating <- sim$opinion_updating[ , .(sender, receiver, opinion_receiver, assumption_sender, opt_message )] 
    sim$opinion_updating <- sim$opinion_updating[merge_receiver_opinions, on=c("receiver"="sender"), nomatch = 0L] 
    sim$opinion_updating <- sim$opinion_updating[merge_sender_messages, on=c("sender"), nomatch = 0L] 
    sim$opinion_updating[ , distance_to_past_opinions := mapply(function(a,b) {

    series <- rev(unlist(a))
    new_series <- vector()

      for (i in 1:length(a)) {

	      if (runif(1, 0, 1) < 1/i**params(sim)$rc_energy_model$memory ) {
	      new_series <- c(new_series, series[i])
      }
      }

		mean(
		  sapply(new_series, function(x) {
		    abs(x - b)
		  })
		)
	      }, a=past_opinions, b=assumption_sender)] 
    sim$opinion_updating[ , distance_to_past_messages := mapply(function(a,b) {

    series <- rev(unlist(a))
    new_series <- vector()

      for (i in 1:length(a)) {

	      if (runif(1, 0, 1) < 1/i**params(sim)$rc_energy_model$memory ) {
	      new_series <- c(new_series, series[i])
      }
      }

	     max(
		sapply(new_series, function(x) {
		      abs(x - b)
		   })
	   )
      }, a=past_messages, b=assumption_sender  )] 

    sim$opinion_updating[ , distance_to_past_opinions := mapply(function(a,b) {
        mean(
          sapply(a, function(x) {
            abs(x - b)
          })
        )
      }, a=past_opinions, b=assumption_sender  )] 
    sim$opinion_updating[ , distance_to_past_messages := mapply(function(a,b) {
	     max(
		sapply(a, function(x) {
		      abs(x - b)
		   })
	   )
      }, a=past_messages, b=assumption_sender  )] 
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
    sim$opinion_updating <- unique(sim$opinion_updating) 
    setkey(sim$opinion_updating, "agent_id") 

    if( length(sim$nothingers) > 0  ) {

          sim$opinion_updating_n <- sim$discourse_memory[ .(sender, past_opinions) ][ .(sim$nothingers), on="sender", nomatch=0L ] 
          sim$opinion_updating_n[ , opinion_receiver_new := mapply(function(x, y) { 
     
			series <- rev(unlist(x))
			new_series <- vector()

			      for (i in 1:length(x)) {

				      if (runif(1, 0, 1) > 1/(i**params(sim)$rc_energy_model$memory)) {
				      new_series <- c(new_series, series[i])
			      }
			      }
			      ifelse(is.na(median(new_series)), y, new_series)

          }, x=past_opinions, y=opinion   ) ] 
	  sim$opinion_updating_n <- sim$opinion_updating_n[ , .(sender, opinion_receiver_new)] 
	  setnames(sim$opinion_updating_n, "sender", "agent_id")
	  setkey(sim$opinion_updating_n, "agent_id") 
	  sim$opinion_updating <- rbind(sim$opinion_updating_n, sim$opinion_updating)

    }

    sim$agent_characteristics <- merge(sim$agent_characteristics, sim$opinion_updating, by="agent_id", all.x = TRUE) 
    sim$agent_characteristics[ , opinion := ifelse(is.na(opinion_receiver_new), opinion, opinion_receiver_new)] 
    sim$agent_characteristics[ , opinion_receiver_new := NULL ]

    sim$agent_characteristics$energy[is.na(sim$agent_characteristics$energy)] <- 0

    sim$agent_characteristics_temp <- copy(sim$actions_overall)[ , best_axn_overall := best_action][ , .(agent_id, best_axn_overall)]
    sim$agent_characteristics_temp <- merge(sim$agent_characteristics_temp, sim$actions_send[ , best_axn_send := best_action][ , .(agent_id, best_axn_send)], by="agent_id") 
    sim$agent_characteristics_temp <- unique(sim$agent_characteristics_temp) 
    sim$agent_characteristics_temp <- merge(sim$agent_characteristics_temp, copy(sim$discourse_memory)[ , .(sender, receiver_business, sender_business)], by.x = "agent_id", by.y = "sender", all = TRUE) 
    sim$agent_characteristics_temp <- unique(sim$agent_characteristics_temp) 
    sim$agent_characteristics_temp[ .("Send") , energy_loss := sender_business, on="best_axn_overall", nomatch=0L ] 
    sim$agent_characteristics_temp[ .("Receive"), energy_loss := receiver_business, on="best_axn_overall", nomatch=0L ] 
    sim$agent_characteristics_temp[ .("Both"), energy_loss := sender_business + receiver_business, on="best_axn_overall", nomatch=0L ] 
    sim$agent_characteristics_temp[ .("Nothing"), energy_loss := 0, on="best_axn_overall", nomatch=0L ] 
    sim$agent_characteristics_temp[ .("Unoptimized"), energy_loss := energy_loss, on="best_axn_send", nomatch=0L ] 
    sim$agent_characteristics_temp[ .("Optimized"), energy_loss := energy_loss + sender_business, on="best_axn_send", nomatch=0L ] 
    sim$agent_characteristics_temp <- sim$agent_characteristics_temp[ , .(agent_id, energy_loss) ] 
    sim$agent_characteristics_temp <- unique(sim$agent_characteristics_temp) 
    sim$agent_characteristics_temp[ , energy_loss := sum(energy_loss), by="agent_id" ] 
    sim$agent_characteristics_temp <- unique(sim$agent_characteristics_temp) 
    sim$agent_characteristics <- sim$agent_characteristics_temp[sim$agent_characteristics, on="agent_id"] 
    sim$agent_characteristics[ , energy_loss := ifelse(is.na(energy_loss), 0, energy_loss) ] 
    sim$agent_characteristics[ , energy := energy - energy_loss + params(sim)$rc_energy_model$restoration_factor ] 
    sim$agent_characteristics[ , energy_loss := NULL ] 
    sim$agent_characteristics <- unique(sim$agent_characteristics)
    setkey(sim$agent_characteristics, agent_id)

    sim$agent_characteristics$energy[is.na(sim$agent_characteristics$energy)] <- 0

    sim$discourse_memory <- sim$agent_characteristics[ , .(agent_id, opinion) ][sim$discourse_memory[ , -c("opinion")], on=c("agent_id" = "sender") ] 
    setnames(sim$discourse_memory, "agent_id", "sender") 
    setkey(sim$discourse_memory, sender)

      } else {

   sim$opinion_updating <- sim$actions_overall[sim$agent_characteristics, on=c("agent_id"), nomatch = 0L] 
   sim$opinion_updating[ , opinion_receiver_new := opinion ] 
   sim$opinion_updating <- sim$opinion_updating[ , .(agent_id, opinion_receiver_new)] 
   unique(sim$opinion_updating) 
   setkey(sim$opinion_updating, "agent_id") 

   sim$agent_characteristics <- merge(sim$agent_characteristics, sim$opinion_updating, by="agent_id", all.x = TRUE) 
   sim$agent_characteristics[ , opinion := ifelse(is.na(opinion_receiver_new), opinion, opinion_receiver_new)] 
   sim$agent_characteristics[ , opinion_receiver_new := NULL ]

    sim$agent_characteristics_temp <-copy(sim$actions_overall)[ , best_axn_overall := best_action][ , .(agent_id, best_axn_overall)]
    sim$agent_characteristics_temp <- merge(sim$agent_characteristics_temp, copy(sim$actions_send)[ , best_axn_send := best_action][ , .(agent_id, best_axn_send)], by="agent_id") 
    sim$agent_characteristics_temp <- merge(sim$agent_characteristics_temp, sim$discourse_memory[ , .(sender, receiver_business, sender_business)], by.x = "agent_id", by.y = "sender", all = TRUE) 
    sim$agent_characteristics_temp <- unique(sim$agent_characteristics_temp) 
    sim$agent_characteristics_temp[ .("Send") , energy_loss := sender_business, on="best_axn_overall", nomatch=0L ] 
    sim$agent_characteristics_temp[ .("Receive"), energy_loss := receiver_business, on="best_axn_overall", nomatch=0L ] 
    sim$agent_characteristics_temp[ .("Both"), energy_loss := sender_business + receiver_business, on="best_axn_overall", nomatch=0L ] 
    sim$agent_characteristics_temp[ .("Nothing"), energy_loss := 0, on="best_axn_overall", nomatch=0L ] 
    sim$agent_characteristics_temp[ .("Unoptimized"), energy_loss := energy_loss, on="best_axn_send", nomatch=0L ] 
    sim$agent_characteristics_temp[ .("Optimized"), energy_loss := energy_loss + sender_business, on="best_axn_send", nomatch=0L ] 
    sim$agent_characteristics_temp <- sim$agent_characteristics_temp[ , .(agent_id, energy_loss) ] 
    sim$agent_characteristics_temp <- unique(sim$agent_characteristics_temp) 
    sim$agent_characteristics_temp[ , energy_loss := sum(energy_loss), by="agent_id" ] 
    sim$agent_characteristics_temp <- unique(sim$agent_characteristics_temp) 
    sim$agent_characteristics <- sim$agent_characteristics_temp[sim$agent_characteristics, on="agent_id"] 
    sim$agent_characteristics[ , energy_loss := ifelse(is.na(energy_loss), 0, energy_loss) ] 
    sim$agent_characteristics[ , energy := energy - energy_loss + params(sim)$rc_energy_model$restoration_factor ] 
    sim$agent_characteristics[ , energy_loss := NULL ]
    setkey(sim$agent_characteristics, agent_id)

    sim$discourse_memory <- sim$agent_characteristics[ , .(agent_id, opinion) ][sim$discourse_memory[ , -c("opinion")], on=c("agent_id" = "sender") ] 
    setnames(sim$discourse_memory, "agent_id", "sender") 
    setkey(sim$discourse_memory, sender)

      }
   
  } else {

    sim$discourse_memory[ , receiver_business := 0 ] 
    sim$discourse_memory[ , sender_business := 0 ] 
    sim$discourse_memory[ , past_sender_business := mapply(function(x,y) {
	    list(c(unlist(x), y))}
	    , x=past_sender_business, y=sender_business )
   ]
    sim$discourse_memory[ , past_receiver_business := mapply(function(x,y) {
	    list(c(unlist(x), y))}
	    , x=past_receiver_business, y=receiver_business )
   ]

sim$opinion_updating <- copy(sim$discourse_memory) 
sim$opinion_updating[ , opinion_receiver_new := mapply(function(x, y) { 
     
series <- rev(unlist(x))
new_series <- vector()

      for (i in 1:length(x)) {

	      if (runif(1, 0, 1) > 1/(i**params(sim)$rc_energy_model$memory)) {
	      new_series <- c(new_series, series[i])
      }
      }
      ifelse(is.na(median(new_series)), y, new_series)

       }, x=past_opinions, y=opinion   ) ] 
setnames(sim$opinion_updating, "sender", "agent_id") 
sim$opinion_updating <- sim$opinion_updating[ , .(agent_id, opinion_receiver_new)] 
sim$opinion_updating <- unique(sim$opinion_updating)
setkey(sim$opinion_updating, "agent_id") 

   sim$agent_characteristics <- merge(sim$agent_characteristics, sim$opinion_updating, by="agent_id", all.x = TRUE) 
   sim$agent_characteristics[ , opinion := ifelse(is.na(opinion_receiver_new), opinion, opinion_receiver_new)] 
   sim$agent_characteristics[ , opinion_receiver_new := NULL ]

    sim$agent_characteristics_temp <- copy(sim$actions_overall)[ , best_axn_overall := best_action][ , .(agent_id, best_axn_overall)]
    sim$agent_characteristics_temp <- merge(sim$agent_characteristics_temp, sim$discourse_memory[ , .(sender, receiver_business, sender_business)], by.x = "agent_id", by.y = "sender", all = TRUE) 
    sim$agent_characteristics_temp[ .("Send") , energy_loss := sender_business, on="best_axn_overall", nomatch=0L ] 
    sim$agent_characteristics_temp[ .("Receive"), energy_loss := receiver_business, on="best_axn_overall", nomatch=0L ] 
    sim$agent_characteristics_temp[ .("Both"), energy_loss := sender_business + receiver_business, on="best_axn_overall", nomatch=0L ] 
    sim$agent_characteristics_temp[ .("Nothing"), energy_loss := 0, on="best_axn_overall", nomatch=0L ] 
    sim$agent_characteristics_temp <- sim$agent_characteristics_temp[ , .(agent_id, energy_loss) ] 
    sim$agent_characteristics_temp <- unique(sim$agent_characteristics_temp) 
    sim$agent_characteristics_temp[ , energy_loss := sum(energy_loss), by="agent_id" ] 
    sim$agent_characteristics_temp <- unique(sim$agent_characteristics_temp) 
    sim$agent_characteristics <-  sim$agent_characteristics_temp[sim$agent_characteristics, on="agent_id"] 
    sim$agent_characteristics[ , energy_loss := ifelse(is.na(energy_loss), 0, energy_loss) ] 
    sim$agent_characteristics[ , energy := energy - energy_loss + params(sim)$rc_energy_model$restoration_factor ] 
    sim$agent_characteristics[ , energy_loss := NULL ]
    setkey(sim$agent_characteristics, agent_id)

    sim$discourse_memory <- sim$agent_characteristics[ , .(agent_id, opinion) ][sim$discourse_memory[ , -c("opinion")], on=c("agent_id" = "sender") ] 
    setnames(sim$discourse_memory, "agent_id", "sender") 
    setkey(sim$discourse_memory, sender)

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

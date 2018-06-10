### Rational Choice Model

# for simplicity and comparability, epsilon is taken to be a constant
# however, it is taken as a variable here, that could take input in the future
# if this model is sourced together with such a modification

# we will have an adjacency matrix

# the parameters are set only with default values
stopifnot(packageVersion("SpaDES") >= "1.2.0.9006")

defineModule(sim, list(
  name = "rc_model",
  description = "Simulate bounded confidence opinion dynamics model in Rational Choice framework.",
  keywords = c("opinion dynamics", "hegselmann krause", "rational choice"),
  childModules = character(),
  authors = c(person(c("Malte", "Lars"), "Heckelen", email = "malte.heckelen@ilw.uni-stuttgart.de", role = c("aut", "cre"))),
  version = numeric_version("0.0.1"),
  spatialExtent = raster::extent(rep(NA_real_, 4)),
  timeframe = as.POSIXlt(c(NA, NA)),
  timeunit = "hour",
  citation = list("citation.bib"),
  documentation = list("README.txt", "rc_model.Rmd"),
  reqdPkgs = list("tidyverse", "data.table", "parallel"),
  parameters = rbind(
    # defineParameter("paramName", "paramClass", value, min, max, "parameter description"),
    defineParameter("epsilon", "numeric", 0.1, NA, NA, "The Bounded Confidence parameter."),
    defineParameter("other_incons_tolerance", "numeric", 0.1, NA, NA, "The parameter controlling the tolerance for the degree with which other agents to have varying opinions over time."),
    defineParameter("self_incons_tolerance", "numeric", 0.1, NA, NA, "The parameter controlling the tolerance for the degree with which the agent himself has varying opinions over time."),
    defineParameter("message_memory_depth", "numeric", 1, NA, NA, "The number of time steps agents remember messages for."),
    defineParameter("opinion_memory_depth", "numeric", 1, NA, NA, "The number of time steps agents remember opinions for.")
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


doEvent.rc_model <- function(sim, eventTime, eventType, debug = FALSE) {

  switch(
    eventType,
    init = {
      ## do stuff for this event
      sim <- rc_modelInit(sim)

      ## schedule future event(s)
      sim <- scheduleEvent(sim, eventTime = start(sim)+1, moduleName = "rc_model", eventType = "step")
    },
    step = {
      ## do stuff for this event
      sim <- rc_modelStep(sim)

      ## schedule future event(s)
      sim <- scheduleEvent(sim, eventTime = time(sim)+1, moduleName = "rc_model", eventType = "step")
    },
    warning(paste("Undefined event type: '", current(sim)[1, "eventType", with = FALSE],
                  "' in module '", current(sim)[1, "moduleName", with = FALSE], "'", sep = ""))
  )

  return(invisible(sim))

}

rc_modelInit <- function(sim) {
  # set within_epsilon column in opinions simulation attribute
  # reserve the Init in every of these simulations to place special attributes for modules
  print(time(sim))
  sim$agent_characteristics <- sim$agent_characteristics

  sim$actions_send <- data.table(

    agent_id = rep(agent_characteristics$agent_id, each=2),
    actions = rep(c("Unoptimized", "Optimized"), sim$no_agents),
    util_score = rep(0, length(no_agents*2))

  )
  
  sim$chosen_actions <- data.table(

    agent_id = rep(agent_characteristics$agent_id, each=1),
    action_type = rep(c("actions_send"), sim$no_agents),
    best_action = rep(c("Not assigned"), length(sim$no_agents))

  )
  sim$chosen_actions[ , agent_id := as.integer(agent_id) ]


  sim$messages <- sim$environment %>%
    activate(edges) %>%
    as_tibble() %>%
    data.table()
  setnames(sim$messages, old = c("from", "to"), new = c("receiver", "sender"))

  temp <- sim$environment %>%
    activate(edges) %>%
    as_tibble() %>%
    data.table()
  setnames(temp, old = c("from", "to"), new = c("sender", "receiver"))
  
  sim$messages <- rbind(temp, sim$messages)
  sim$messages <- sim$messages[copy(sim$agent_characteristics)[, .(agent_id, opinion)], nomatch = 0L, on = c("sender" = "agent_id"), allow.cartesian=TRUE]
  setnames(sim$messages, "opinion", "opinion_sender")

  ###################################
  #### PRODUCE POSSIBLE MESSAGES ####
  ###################################

  assumption_receiver <- runif(nrow(sim$messages), 0, 1)
  sim$messages <- cbind(sim$messages, assumption_receiver)
  messages_copy_opinion_assumption <- copy(sim$messages)

  sim$message_matrix <- outer(sim$messages$opinion_sender, sim$messages$assumption_receiver, produce_altered_message) 
  row.names(sim$message_matrix) <- sim$messages[ , sender]
  colnames(sim$message_matrix) <- sim$messages[ , receiver]
  
  sim$messages <- data.table(copy(sim$message_matrix))
  sim$messages[ , sender := as.numeric(row.names(sim$message_matrix))]

  sim$messages <-  melt( sim$messages,
		id.vars = c("sender"),
		measure.vars = as.character(seq(1, sim$no_agents, 1)),
		variable.name = "receiver",
		value.name = "opt_message" )
  sim$messages <- sim$messages[ receiver != sender ]
  setkey(sim$messages, "sender")
  sim$messages <- unique(sim$messages)
  sim$messages[ , sender := as.integer(sender) ]
  sim$messages[ , receiver := as.integer(receiver)] 

  sim$messages <- sim$messages[copy(unique(messages_copy_opinion_assumption)), on=c("sender", "receiver"), nomatch = 0L, allow.cartesian = TRUE] 
  sim$messages[ , opt_message := median(opt_message), by = .(sender)] 
  sim$discourse_memory <- copy(sim$messages)
  sim$discourse_memory[ , past_opinions := mapply(function(x) {list(x)}, x=opinion_sender )]
  setnames(sim$discourse_memory, "opinion_sender", "opinion")
  sim$discourse_memory <- sim$discourse_memory[ , .(sender, opinion, past_opinions)]
  sim$discourse_memory[ , past_opinions := as.character(past_opinions) ]
  sim$discourse_memory <- unique(sim$discourse_memory)
  sim$discourse_memory[ , past_opinions := mapply(function(x) {list(eval(parse(text = x)))}, x=past_opinions ) ] 
  sim$discourse_memory[ , sender_business := 0 ]
  sim$discourse_memory[ , receiver_business := 0 ]

  # Sending

  sim$actions_send <- copy(sim$messages)[copy(sim$actions_send), on = c("sender" = "agent_id"), nomatch = 0L, allow.cartesian = TRUE]
  sim$actions_send <- sim$actions_send[copy(sim$discourse_memory), on = c("sender"), nomatch = 0L, allow.cartesian = TRUE]
  sim$actions_send[, distance_to_past_opinions := mapply(function(a,b,c,k) {
      switch(k,
             "Unoptimized" = {
               mean(
                 unlist(lapply(a, function(x) {
                   abs(x - c)
                 } ) )
               )
             },
             "Optimized" = {
               mean(
                 unlist(lapply(a, function(x) {
                   abs(x - b)
                 } ) )
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
      }, a=opinion_sender, b=opt_message, k=actions )]
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
 sim$actions_send[ , past_opinions := NULL][ , receiver := NULL ]
 sim$actions_send[ , util_score := 0 - distance_to_past_opinions - distance_message_opinion - distance_message_assumption]
 setnames(sim$actions_send, "sender", "agent_id")
 sim$actions_send <- sim$actions_send[,.(agent_id, actions, util_score)]
 setkey(sim$actions_send, "agent_id")
 sim$actions_send <- unique(sim$actions_send)
 sim$actions_send[, util_score := sum(util_score), by=c("agent_id", "actions")]
 sim$actions_send <- unique(sim$actions_send)

 sim$actions_send_cast <- dcast(sim$actions_send, agent_id ~ actions, value.var = "util_score")
 sim$actions_send_cast[, best_action :=  ifelse(Optimized > Unoptimized, "Optimized", "Unoptimized")]

 sim$actions_send <- melt( sim$actions_send_cast,
	 id.vars = c("agent_id", "best_action"),
         measure.vars = c("Optimized", "Unoptimized"),
         variable.name = "actions",
         value.name = "util_score" )
 sim$actions_send[ , agent_id := as.integer(agent_id)]
 
 sim$chosen_actions <- merge(copy(sim$actions_send)[ actions == best_action , .(agent_id, best_action) ], sim$chosen_actions, by=c("agent_id"), all.x=TRUE, all.y=TRUE)
 sim$chosen_actions[ , best_action := ifelse(!is.na(best_action.x), best_action.x,  "NOT ASSIGNED")] 
 sim$chosen_actions[ , best_action.x := NULL ][ , best_action.y := NULL ]

 sim$discourse_memory_temp <- copy(sim$actions_send)[unique(copy(sim$messages)[ , receiver := NULL ]), on = c("agent_id" = "sender"), nomatch = 0L, allow.cartesian = TRUE]
 setnames(sim$discourse_memory_temp, "agent_id", "sender")
 setkey(sim$discourse_memory_temp, "sender")
 sim$discourse_memory_temp <- unique(sim$discourse_memory_temp)
 sim$discourse_memory <- sim$discourse_memory[copy(sim$discourse_memory_temp), on = c("sender") ]
 sim$discourse_memory <- sim$discourse_memory[actions==best_action]
 sim$discourse_memory[ , past_messages := ifelse( best_action == "Unoptimized",
                                 list(opinion_sender[[1]]),
                                 list(opt_message[[1]]))]
 sim$discourse_memory[ , past_opinions := ifelse(lengths(past_opinions) < params(sim)$rc_model$opinion_memory_depth,
                                 mapply(function(x, y) {
                                   list(c(unlist(x), y))
                                 }, x=past_opinions, y=opinion_sender),
                                 mapply(function(x, y) {
                                   list(c(unlist(x)[1:(params(sim)$rc_model$opinion_memory_depth-1)], y))
                                 }, x=past_opinions, y=opinion_sender )
    ) ]
 setnames(sim$discourse_memory, "opinion_sender", "opinion")
 sim$discourse_memory[ , message := past_messages[[1]] ]
 sim$discourse_memory <- sim$discourse_memory[ , .(sender, past_messages, message, opinion, past_opinions)] 
 sim$discourse_memory[ , past_messages := as.character(past_messages) ]
 sim$discourse_memory[ , past_opinions := as.character(past_opinions) ] 
 sim$discourse_memory <- unique(sim$discourse_memory)
 sim$discourse_memory[ , past_messages := mapply(function(x) list(eval(parse(text = x))), x=past_messages ) ]
 sim$discourse_memory[ , past_opinions := mapply(function(x) list(eval(parse(text = x))), x=past_opinions ) ]

 sim$messages_reverse <- unique(copy(sim$messages))
 setnames(sim$messages_reverse, old = c("sender", "receiver"), new = c("receiver", "sender"))
 sim$messages_reverse <- sim$messages_reverse[ , .(receiver, opinion_sender, opt_message)]
 sim$messages_reverse[ , opinion_receiver := opinion_sender]
 sim$messages_reverse[ , opinion_sender := NULL ][ , opt_message := NULL ]
 sim$messages[sim$messages_reverse, on="receiver", nomatch=0L, allow.cartesian=TRUE] 

 sim$messages <- sim$messages[sim$actions_send[ , .(agent_id, actions, best_action)], nomatch=0L, on=c("sender" = "agent_id"), allow.cartesian=TRUE]
 sim$messages[ , assumption_sender := ifelse( best_action == "Unoptimized", opinion_sender, opt_message)] 
 sim$messages[ , actions := NULL ][ , best_action := NULL ]
 setkey(sim$messages, "sender") 
 unique(sim$messages)

  # Receiving

 merge_receiver_opinions <- copy(sim$discourse_memory)[ , .(sender, past_opinions)]
 merge_receiver_opinions[ , past_opinions := as.character(past_opinions) ]
 merge_receiver_opinions <- unique(merge_receiver_opinions)
 merge_receiver_opinions[ , past_opinions := mapply(function(x) list(eval(parse(text = x))), x=past_opinions ) ]

 merge_sender_messages <- copy(sim$discourse_memory)[ , .(sender, past_messages)]
 merge_sender_messages[ , past_messages := as.character(past_messages) ]
 merge_sender_messages <- unique(merge_sender_messages)
 merge_sender_messages[ , past_messages := mapply(function(x) list(eval(parse(text = x))), x=past_messages ) ]

 sim$opinion_updating <- copy(sim$messages)[merge_receiver_opinions, on=c("receiver"="sender"), nomatch = 0L, allow.cartesian = TRUE] 
 sim$opinion_updating[ , distance_to_past_opinions := mapply(function(a,b) {
      mean(
        unlist(lapply(a, function(x) {
          abs(x - b)
        } ) )
      )
    }, a=past_opinions, b=assumption_sender)] 
 sim$opinion_updating[ , past_opinions := NULL ] 
 sim$opinion_updating <- unique(sim$opinion_updating)
 sim$opinion_updating[ , within_epsilon := abs(opinion_sender - assumption_sender) < params(sim)$rc_model$epsilon] 
 sim$opinion_updating[ , self_consistent := distance_to_past_opinions < params(sim)$rc_model$self_incons_tolerance] 
 sim$opinion_updating <- sim$opinion_updating[ within_epsilon == TRUE & self_consistent == TRUE ] 
 sim$opinion_updating[ , sum_assumptions := sum(assumption_sender), by=receiver] 
 sim$opinion_updating[ , denominator := .N, by=receiver ] 
 sim$opinion_updating[ , opinion_receiver_new := ifelse( denominator != 0, sum_assumptions / denominator, opinion_receiver )]
 setnames(sim$opinion_updating, "receiver", "agent_id")
 sim$opinion_updating <- sim$opinion_updating[ , .(agent_id, opinion_receiver_new)]
 setkey(sim$opinion_updating, "agent_id")
 sim$opinion_updating <- unique(sim$opinion_updating)

 sim$agent_characteristics <- merge(sim$agent_characteristics, sim$opinion_updating, by="agent_id", all.x = TRUE)
 sim$agent_characteristics[ , opinion := ifelse(is.na(opinion_receiver_new), opinion, opinion_receiver_new)] 
 sim$agent_characteristics[ , opinion_receiver_new := NULL ] 

  return(invisible(sim))

}

rc_modelStep <- function(sim) {
  print(time(sim))

  sim$actions_send <- data.table(

    agent_id = rep(agent_characteristics$agent_id, each=2),
    actions = rep(c("Unoptimized", "Optimized"), sim$no_agents),
    util_score = rep(0, length(sim$no_agents))

  )
  
  sim$chosen_actions <- data.table(

    agent_id = rep(agent_characteristics$agent_id, each=1),
    action_type = rep(c("actions_send"), sim$no_agents),
    best_action = rep(c("Not assigned"), length(sim$no_agents))

  )
  sim$chosen_actions[ , agent_id := as.integer(agent_id) ]

  sim$messages <- copy(sim$environment) %>%
    activate(edges) %>%
    as_tibble() %>%
    data.table()
  setnames(sim$messages, old = c("from", "to"), new = c("receiver", "sender"))

  temp <- sim$environment %>%
    activate(edges) %>%
    as_tibble() %>%
    data.table()
  setnames(temp, old = c("from", "to"), new = c("sender", "receiver"))
  
  sim$messages <- rbind(temp, sim$messages)
  sim$messages <- sim$messages[copy(sim$agent_characteristics)[, .(agent_id, opinion)], nomatch = 0L, on = c("sender" = "agent_id"), allow.cartesian=TRUE]
  setnames(sim$messages, "opinion", "opinion_sender")

  temp_discmem_messages <-  unique(sim$discourse_memory[ , .(sender, message) ])
  setnames(temp_discmem_messages, "sender", "receiver") 
  sim$messages <- merge(temp_discmem_messages, sim$messages, by = c("receiver") ) 
  setnames(sim$messages, "message", "assumption_receiver") 

  messages_copy_opinion_assumption <- copy(sim$messages)
  
  # make matrix of all possible optimized messages
  sim$message_matrix <- outer(sim$messages$opinion_sender, sim$messages$assumption_receiver, produce_altered_message) # works
  row.names(sim$message_matrix) <- sim$messages[, sender]
  colnames(sim$message_matrix) <- sim$messages[, receiver]
  
  sim$messages <- data.table(copy(sim$message_matrix))
  sim$messages[ , sender := as.numeric(row.names(sim$message_matrix))]

  sim$messages <-  melt( sim$messages,
		id.vars = c("sender"),
		measure.vars = as.character(seq(1, sim$no_agents, 1)),
		variable.name = "receiver",
		value.name = "opt_message" )
  sim$messages <- sim$messages[ receiver != sender ]
  setkey(sim$messages, "sender")
  sim$messages <- unique(sim$messages)
  sim$messages[ , sender := as.integer(sender) ]
  sim$messages[ , receiver := as.integer(receiver)] 

  sim$messages <- sim$messages[copy(unique(messages_copy_opinion_assumption)), on=c("sender", "receiver"), nomatch = 0L, allow.cartesian = TRUE] 
  sim$messages[ , opt_message := median(opt_message), by = .(sender)] 
  
  # Sending
print("messages")
print(nrow(sim$messages))
  sim$actions_send <- copy(sim$messages)[copy(sim$actions_send), on = c("sender" = "agent_id"), nomatch = 0L, allow.cartesian = TRUE]
  sim$actions_send <- sim$actions_send[copy(sim$discourse_memory), on = c("sender"), nomatch = 0L, allow.cartesian = TRUE]
  sim$actions_send[, distance_to_past_opinions := mapply(function(a,b,c,k) {
      switch(k,
             "Unoptimized" = {
               mean(
                 unlist(lapply(a, function(x) {
                   abs(x - c)
                 } ) )
               )
             },
             "Optimized" = {
               mean(
                 unlist(lapply(a, function(x) {
                   abs(x - b)
                 } ) )
               )
             }
      )
    }, a=past_opinions, b=opt_message, c=opinion_sender, k=actions )]
 sim$actions_send[, distance_message_opinion := mapply(function(a,b,k) {
        switch(k,
               "Unoptimized" = {
                 abs(a - a)
               },
               "Optimized" = {
                 abs(a - b)
               }
        )
      }, a=opinion_sender, b=opt_message, k=actions )]
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
 sim$actions_send[ , past_opinions := NULL][ , receiver := NULL ]
 sim$actions_send[ , util_score := 0 - distance_to_past_opinions - distance_message_opinion - distance_message_assumption]
 setnames(sim$actions_send, "sender", "agent_id")
 sim$actions_send <- sim$actions_send[,.(agent_id, actions, util_score)]
 setkey(sim$actions_send, "agent_id")
 sim$actions_send <- unique(sim$actions_send)
 sim$actions_send[, util_score := sum(util_score), by=c("agent_id", "actions")]
 sim$actions_send <- unique(sim$actions_send)
print(nrow(sim$messages))
 sim$actions_send_cast <- dcast(sim$actions_send, agent_id ~ actions, value.var = "util_score")
 sim$actions_send_cast[, best_action :=  ifelse(Optimized > Unoptimized, "Optimized", "Unoptimized")]
print(nrow(sim$messages))
 sim$actions_send <- melt( sim$actions_send_cast,
	 id.vars = c("agent_id", "best_action"),
         measure.vars = c("Optimized", "Unoptimized"),
         variable.name = "actions",
         value.name = "util_score" )
 sim$actions_send[ , agent_id := as.integer(agent_id) ]
print(nrow(sim$actions_send))
 sim$chosen_actions <- merge(copy(sim$actions_send)[ actions == best_action , .(agent_id, best_action) ], sim$chosen_actions, by=c("agent_id"), all.x=TRUE, all.y=TRUE)
 sim$chosen_actions[ , best_action := ifelse(!is.na(best_action.x), best_action.x,  "NOT ASSIGNED")] 
 sim$chosen_actions[ , best_action.x := NULL ][ , best_action.y := NULL ]

 sim$discourse_memory_temp <- copy(sim$actions_send)[unique(copy(sim$messages)[ , receiver := NULL ][ , assumption_receiver := NULL ]), on = c("agent_id" = "sender"), nomatch = 0L, allow.cartesian = TRUE]
 setnames(sim$discourse_memory_temp, "agent_id", "sender")
 setkey(sim$discourse_memory_temp, "sender")
 sim$discourse_memory_temp <- unique(sim$discourse_memory_temp)

 sim$discourse_memory <- sim$discourse_memory[ , message := NULL ][copy(sim$discourse_memory_temp), on = c("sender") ]
 sim$discourse_memory <- sim$discourse_memory[actions==best_action]
 sim$discourse_memory[ , past_messages := ifelse(lengths(past_messages) < params(sim)$rc_model$message_memory_depth,
                                 ifelse( best_action == "Unoptimized",
                                        mapply(function(x, y) {
                                          list(c(unlist(x), y))
                                        }, x=past_messages, y=opinion_sender ),
                                        mapply(function(x, y) {
                                          list(c(unlist(x), y))
                                        }, x=past_messages, y=opt_message )),
                                 ifelse( best_action == "Unoptimized",
                                        mapply(function(x, y) {
                                          list(c(unlist(x)[1:(params(sim)$rc_model$message_memory_depth-1)], y))
                                        }, x=past_messages, y=opinion_sender),
                                        mapply(function(x, y) {
                                          list(c(unlist(x)[1:(params(sim)$rc_model$message_memory_depth-1)], y))
                                        }, x=past_messages, y=opt_message )
                                 )
    )] 
 sim$discourse_memory[ , past_opinions := ifelse(lengths(past_opinions) < params(sim)$rc_model$opinion_memory_depth,
                                 mapply(function(x, y) {
                                   list(c(unlist(x), y))
                                 }, x=past_opinions, y=opinion_sender),
                                 mapply(function(x, y) {
                                   list(c(unlist(x)[1:(params(sim)$rc_model$opinion_memory_depth-1)], y))
                                 }, x=past_opinions, y=opinion_sender)
    ) ]
 setnames(sim$discourse_memory, "opinion_sender", "opinion")
 sim$discourse_memory[ , message := past_messages[[1]] ]
 sim$discourse_memory <- sim$discourse_memory[ , .(sender, past_messages, message, opinion, past_opinions)] 
 sim$discourse_memory[ , past_messages := as.character(past_messages) ]
 sim$discourse_memory[ , past_opinions := as.character(past_opinions) ] 
 sim$discourse_memory <- unique(sim$discourse_memory)
 sim$discourse_memory[ , past_messages := mapply(function(x) list(eval(parse(text = x))), x=past_messages )]
 sim$discourse_memory[ , past_opinions := mapply(function(x) list(eval(parse(text = x))), x=past_opinions ) ]
 sim$messages_reverse <- unique(copy(sim$messages))
 setnames(sim$messages_reverse, old = c("sender", "receiver"), new = c("receiver", "sender"))
 sim$messages_reverse <- sim$messages_reverse[ , .(receiver, opinion_sender, opt_message)]
 sim$messages_reverse[ , opinion_receiver := opinion_sender]
 sim$messages_reverse[ , opinion_sender := NULL ][ , opt_message := NULL ]
 sim$messages[sim$messages_reverse, on="receiver", nomatch=0L, allow.cartesian=TRUE] 

 sim$messages <- sim$messages[sim$actions_send[ , .(agent_id, actions, best_action)], nomatch=0L, on=c("sender" = "agent_id"), allow.cartesian=TRUE]
 sim$messages[ , assumption_sender := ifelse( best_action == "Unoptimized", opinion_sender, opt_message)] 
 sim$messages[ , actions := NULL ][ , best_action := NULL ]
 setkey(sim$messages, "sender") 
 unique(sim$messages)

 # Receiving

 merge_receiver_opinions <- copy(sim$discourse_memory)[ , .(sender, past_opinions)]
 merge_receiver_opinions[ , past_opinions := as.character(past_opinions) ]
 merge_receiver_opinions <- unique(merge_receiver_opinions)
 merge_receiver_opinions[ , past_opinions := mapply(function(x) list(eval(parse(text = x))), x=past_opinions ) ]

 merge_sender_messages <- copy(sim$discourse_memory)[ , .(sender, past_messages)]
 merge_sender_messages[ , past_messages := as.character(past_messages) ]
 merge_sender_messages <- unique(merge_sender_messages)
 merge_sender_messages[ , past_messages := mapply(function(x) list(eval(parse(text = x))), x=past_messages ) ]

 sim$opinion_updating <- copy(sim$messages)[merge_receiver_opinions, on=c("receiver"="sender"), nomatch = 0L, allow.cartesian = TRUE] 
 sim$opinion_updating <- sim$opinion_updating[merge_sender_messages, on=c("receiver"="sender"), nomatch = 0L, allow.cartesian = TRUE] 
 sim$opinion_updating[ , distance_to_past_opinions := mapply(function(a,b) {
      mean(
        sapply(a, function(x) {
          abs(x - b)
        })
      )
    }, a=past_opinions, b=assumption_sender )] 
 sim$opinion_updating[ , distance_to_past_messages := mapply(function(a,b) {
      max(
        sapply(a, function(x) {
          abs(x - b)
        })
      )
    }, a=past_messages, b=assumption_sender )] 
 sim$opinion_updating[ , past_opinions := NULL ] 
 sim$opinion_updating[ , past_messages := NULL ] 
 sim$opinion_updating <- unique(sim$opinion_updating)
 sim$opinion_updating[ , within_epsilon := abs(opinion_sender - assumption_sender) < params(sim)$rc_model$epsilon] 
 sim$opinion_updating[ , self_consistent := distance_to_past_opinions < params(sim)$rc_model$self_incons_tolerance] 
 sim$opinion_updating[ , trust := distance_to_past_messages < params(sim)$rc_model$other_incons_tolerance ] 
 sim$opinion_updating <- sim$opinion_updating[ within_epsilon == TRUE & self_consistent == TRUE & trust == TRUE ] 
 sim$opinion_updating[ , sum_assumptions := sum(assumption_sender), by=receiver] 
 sim$opinion_updating[ , denominator := .N, by=receiver ] 
 sim$opinion_updating[ , opinion_receiver_new := ifelse( denominator != 0, sum_assumptions / denominator, opinion_receiver )]
 setnames(sim$opinion_updating, "receiver", "agent_id")
 sim$opinion_updating <- sim$opinion_updating[ , .(agent_id, opinion_receiver_new)]
 setkey(sim$opinion_updating, "agent_id")
 sim$opinion_updating <- unique(sim$opinion_updating)

 sim$agent_characteristics <- merge(sim$agent_characteristics, sim$opinion_updating, by="agent_id", all.x = TRUE) 
 sim$agent_characteristics[ , opinion := ifelse(is.na(opinion_receiver_new), opinion, opinion_receiver_new)] 
 sim$agent_characteristics[ , opinion_receiver_new := NULL ] 

 return(invisible(sim))

}

# FUNCTIONS

produce_altered_message <- function(opinion_send, message_receive) {

  # produce altered message without epsilon bound
  altered_message <- ifelse(opinion_send < message_receive,
                            opinion_send + abs(opinion_send - message_receive) / 2,
                            opinion_send - abs(opinion_send - message_receive) / 2)

  # check if within epsilon to own opinion and correct
  # commenting this for now, see Evernote: Design
  #altered_message <- ifelse(abs(opinion_send - altered_message) > epsilon,
  #                          ifelse((opinion_send - altered_message) <= 0, 0, opinion_send - altered_message),
  #                          altered_message)

  return(altered_message)

} # works

vec_rnorm <- Vectorize(rnorm)

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
  reqdPkgs = list("tidyverse", "data.table"),
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

  sim$actions_send <- tibble(

    agent_id = rep(agent_characteristics$agent_id, each=2),
    actions = rep(c("Unoptimized", "Optimized"), no_agents),
    util_score = rep(0, length(actions))

  ) %>%  data.table()

  sim$chosen_actions <- tibble(

    agent_id = rep(agent_characteristics$agent_id, each=1),
    action_type = rep(c("actions_send"), no_agents),
    best_action = rep(c("Not assigned"), length(action_type))

  ) %>% data.table() %>%
    .[ , agent_id := as.integer(agent_id) ]


  sim$messages <- sim$environment %>%
    activate(edges) %>%
    as_tibble() %>%
    data.table() %>%
    setnames(old = c("from", "to"), new = c("to", "from"))

  sim$messages <- sim$environment %>%
    activate(edges) %>%
    as_tibble() %>%
    data.table() %>%
    rbind(messages) %>%
    .[copy(sim$agent_characteristics[, .(agent_id, opinion)]), nomatch = 0L, on = c("from" = "agent_id"), allow.cartesian=TRUE] %>%
    setnames("opinion", "opinion_from")

  ###################################
  #### PRODUCE POSSIBLE MESSAGES ####
  ###################################

  # in init round this is just a uniformly random chosen number on the interval [0,1]
  # the vector of random draws functions as egos assumption about alters opinion
  assumption_to <- runif(nrow(sim$messages), 0, 1)
  sim$messages <- cbind(sim$messages, assumption_to)

  # make matrix of all possible optimized messages
  # the outer product of the opinion and assumption vector is constructed by applying the function produce_altered_message
  sim$message_matrix <- outer(sim$messages$opinion_from, sim$messages$assumption_to, produce_altered_message) 
  row.names(sim$message_matrix) <- sim$messages[ , from]
  colnames(sim$message_matrix) <- sim$messages[ , to]
  
  # the message matrix is melted into a long format
  # entries where to == from are excluded
  # this table is then joined with sim$messages
  # agents send messages optimized for their whole neighborhood: thus opt_message is the mean of all optimized messages per agent
  # if this optimized message exceeds epsilon, the maximum possible deviation of opinion_from is used 
  sim$messages <- copy(sim$message_matrix) %>%
    data.table() %>%
    .[ , from := as.numeric(row.names(sim$message_matrix))] %>%
    melt( id.vars = c("from"),
          measure.vars = as.character(seq(1, sim$no_agents, 1)),
          variable.name = "to",
          value.name = "opt_message" ) %>%
    .[ to != from] %>%
    setkey("from") %>%
    unique() %>%
    .[ , from := as.integer(from)] %>%
    .[ , to := as.integer(to)] %>%
    .[copy(unique(sim$messages)), on=c("from", "to"), nomatch = 0L, allow.cartesian = TRUE] %>%
    .[ , opt_message := median(opt_message), by = .(from)] 
  
  #### sim$messages table specs at this point:
  # rowlength: ( sim$environment %>% as_tibble() %>% nrow() )*2
  # columns: from (int), to (int), opinion_from (int), opt_message (int)

  sim$discourse_memory <- copy(sim$messages) %>%
    .[ , past_opinions := sapply(opinion_from, function(x) {list(x)} )] %>%
    setnames("opinion_from", "opinion") %>%
    .[ , .(from, opinion, past_opinions)] %>%
    .[ , past_opinions := as.character(past_opinions) ] %>% # unique() can't handle list columns, so first transform to character
    unique() %>%
    .[ , past_opinions := sapply(past_opinions, function(x) list(eval(parse(text = x))))] %>% # then transform back
    .[ , sender_business := 0 ] %>%
    .[ , receiver_business := 0 ]



  # Sending
  sim$actions_send <- copy(sim$messages) %>%
    .[copy(sim$actions_send), on = c("from" = "agent_id"), nomatch = 0L, allow.cartesian = TRUE] %>%
    .[copy(sim$discourse_memory), on = c("from"), nomatch = 0L, allow.cartesian = TRUE] %>%
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
    }, a=past_opinions, b=opt_message, c=opinion_from, k=actions)] %>%
      .[, distance_message_opinion := mapply(function(a,b,k) {
        switch(k,
               "Unoptimized" = {
                 abs(a - a)
               },
               "Optimized" = {
                 abs(a - b)
               }
        )
      }, a=opinion_from, b=opt_message, k=actions)] %>%
      .[, distance_message_assumption := mapply(function(a,b,c,k) {
        switch(k,
               "Unoptimized" = {
                 abs(c - a)
               },
               "Optimized" = {
                 abs(c - b)
               }
        )
      }, a=opinion_from, b=opt_message, c=assumption_to, k=actions)] %>%
      .[ , -c("past_opinions", "to")] %>% # include past_messages here in the step function
      .[ , util_score := 0 - distance_to_past_opinions - distance_message_opinion - distance_message_assumption] %>% # include distance_to_past_messages in step function
      setnames("from", "agent_id") %>%
      .[,.(agent_id, actions, util_score)] %>%
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
      .[ , agent_id := as.integer(agent_id)]
 
  sim$chosen_actions <- copy(sim$actions_send)[ actions == best_action , .(agent_id, best_action) ] %>%
    merge(sim$chosen_actions, by=c("agent_id"), all.x=TRUE, all.y=TRUE) %>%
    .[ , best_action := ifelse(!is.na(best_action.x), best_action.x,  "NOT ASSIGNED")] %>% 
    .[ , -c("best_action.x", "best_action.y")]

  sim$discourse_memory <- copy(sim$actions_send) %>%
    .[copy(sim$messages), on = c("agent_id" = "from"), nomatch = 0L, allow.cartesian = TRUE] %>%
    setnames("agent_id", "from") %>%
    setkey("from") %>%
    unique() %>%
    .[copy(sim$discourse_memory), on = c("from"), nomatch = 0L, allow.cartesian = TRUE] %>%
    .[actions==best_action] %>%
    .[ , past_messages := ifelse(.[ , best_action] == "Unoptimized",
                                 list(opinion_from[[1]]),
                                 list(opt_message[[1]]))] %>%
    .[ , past_opinions := ifelse(lengths(past_opinions) < params(sim)$rc_model$opinion_memory_depth,
                                 mapply(function(x, y) {
                                   list(c(unlist(x), y))
                                 }, x=past_opinions, y=opinion_from),
                                 mapply(function(x, y) {
                                   list(c(unlist(x)[1:params(sim)$rc_model$opinion_memory_depth], y))
                                 }, x=past_opinions, y=opinion_from)
    )
    ] %>%
    setnames("opinion_from", "opinion") %>%
    .[ , message := past_messages[[1]] ] %>%
    .[ , .(from, to, past_messages, message, opinion, past_opinions)] %>%
    .[ , past_messages := as.character(past_messages) ] %>%
    .[ , past_opinions := as.character(past_opinions) ] %>%
    unique() %>%
    .[ , past_messages := sapply(past_messages, function(x) list(eval(parse(text = x))))] %>%
    .[ , past_opinions := sapply(past_opinions, function(x) list(eval(parse(text = x))))]


  sim$messages <- copy(sim$messages) %>%
    setnames(old=c("opinion_from", "opt_message"), new=c("opinion_from_y", "opt_message_y")) %>%
    unique() %>%
    setnames(old = c("from", "to"), new = c("to", "from")) %>%
    .[ , .(to, opinion_from_y, opt_message_y)] %>%
    .[sim$messages, on="to", nomatch=0L, allow.cartesian=TRUE] %>% # WORKS
    .[sim$actions_send[, .(agent_id, actions, best_action)], nomatch=0L, on=c("from" = "agent_id"), allow.cartesian=TRUE] %>%
    .[ , assumption_to := ifelse(.[, best_action] == "Unoptimized", opinion_from_y, opt_message_y)] %>%
    .[ , -c("opinion_from_y", "opt_message_y")] %>%
    setkey("from") %>%
    unique()

  # Receiving

  sim$opinion_updating <- copy(sim$messages) %>%
    .[sim$discourse_memory, on=c("from"), nomatch = 0L, allow.cartesian = TRUE] %>%
    .[best_action == actions] %>%
    .[ , distance_to_past_opinions := mapply(function(a,b) {
      mean(
        sapply(a, function(x) {
          abs(x - b)
        })
      )
    }, a=past_opinions, b=assumption_to)] %>%
    .[ , within_epsilon := abs(opinion_from - assumption_to) < params(sim)$rc_model$epsilon] %>%
    .[ , self_consistent := distance_to_past_opinions < params(sim)$rc_model$self_incons_tolerance] %>%  #.[ , self_inconsistent] ->> self_inconsistent
    .[ within_epsilon == TRUE & self_consistent == TRUE ] %>%
    .[ , sum_assumptions := sum(assumption_to), by=from] %>%
    .[ , denominator := .N, by=from ] %>%
    .[ , opinion_y := ifelse( denominator != 0, sum_assumptions / denominator, opinion_from )] %>%
    setnames("from", "agent_id") %>%
    .[ , .(agent_id, opinion_y)] %>%
    setkey("agent_id") %>%
    unique()

  sim$agent_characteristics <- merge(sim$agent_characteristics, sim$opinion_updating, by="agent_id", all.x = TRUE) %>%
    .[ , opinion := ifelse(is.na(opinion_y), opinion, opinion_y)] %>%
    .[ , -c("opinion_y")] 

  return(invisible(sim))

}

rc_modelStep <- function(sim) {
  print(time(sim))

  sim$chosen_actions <- tibble(

    agent_id = rep(agent_characteristics$agent_id, each=1),
    action_type = rep(c("actions_send"), no_agents),
    best_action = rep(c("Not assigned"), length(action_type))

  ) %>% data.table() %>%
    .[ , agent_id := as.integer(agent_id) ]


  sim$messages <- sim$environment %>%
    activate(edges) %>%
    as_tibble() %>%
    data.table() %>%
    setnames(old = c("from", "to"), new = c("to", "from"))

  sim$messages <- sim$environment %>%
    activate(edges) %>%
    as_tibble() %>%
    data.table() %>%
    rbind(sim$messages) %>%
    .[copy(sim$agent_characteristics)[, .(agent_id, opinion)], nomatch = 0L, on = c("from" = "agent_id"), allow.cartesian=TRUE] %>%
    setnames("opinion", "opinion_from")

  sim$messages <-  sim$discourse_memory[ , .(from, to, message) ] %>%
    unique() %>%
    setnames( old=c("from", "to"), new=c("to", "from") ) %>%
    merge(sim$messages, by.x = c("from", "to"), by.y = c("from", "to") ) %>%
    setnames("message", "assumption_to") # works like a charm until here

  # make matrix of all possible optimized messages
  sim$message_matrix <- outer(sim$messages$opinion_from, sim$messages$assumption_to, produce_altered_message) # works
  row.names(sim$message_matrix) <- sim$messages[, from]
  colnames(sim$message_matrix) <- sim$messages[, to]

  sim$messages <- copy(sim$message_matrix) %>%
    data.table() %>%
    .[ , from := as.numeric(row.names(message_matrix))] %>%
    melt( id.vars = c("from"),
          measure.vars = as.character(seq(1, no_agents, 1)),
          variable.name = "to",
          value.name = "opt_message" ) %>%
    .[ to != from] %>%
    setkey("from") %>%
    unique() %>%
    .[ , from := as.integer(from)] %>%
    .[ , to := as.integer(to)] %>%
    .[copy(unique(sim$messages)), on=c("from", "to"), nomatch = 0L, allow.cartesian = TRUE] %>%
    .[ , opt_message := mean(opt_message), by = .(from)]

  # Sending

  sim$actions_send <- copy(sim$messages) %>%
    .[copy(sim$actions_send), on = c("from" = "agent_id"), nomatch = 0L, allow.cartesian = TRUE] %>%
    .[copy(sim$discourse_memory), on = c("from"), nomatch = 0L, allow.cartesian = TRUE] %>%
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
    }, a=past_messages, b=opt_message, c=opinion_from, k=actions)] %>%
    .[, distance_to_past_opinions := mapply(function(a,b,c,k) {
      switch(k,
             "Unoptimized" = {
               max(
                 sapply(a, function(x) {
                   abs(x - c)
                 })
               )
             },
             "Optimized" = {
               max(
                 sapply(a, function(x) {
                   abs(x - b)
                 })
               )
             }
      )
    }, a=past_opinions, b=opt_message, c=opinion_from, k=actions)] %>%
      .[, distance_message_opinion := mapply(function(a,b,k) {
        switch(k,
               "Unoptimized" = {
                 abs(a - a)
               },
               "Optimized" = {
                 abs(a - b)
               }
        )
      }, a=opinion_from, b=opt_message, k=actions)] %>%
      .[, distance_message_assumption := mapply(function(a,b,c,k) {
        switch(k,
               "Unoptimized" = {
                 abs(c - a)
               },
               "Optimized" = {
                 abs(c - b)
               }
        )
      }, a=opinion_from, b=opt_message, c=assumption_to, k=actions)] %>%
      .[ , -c("past_opinions", "past_messages", "to")] %>% # include past_messages here in the step function
      .[ , util_score := 0 - distance_to_past_opinions - distance_to_past_messages - distance_message_opinion - distance_message_assumption] %>% # include distance_to_past_messages in step function
      setnames("from", "agent_id") %>%
      .[,.(agent_id, actions, util_score)] %>%
      setkey("agent_id") %>%
      unique() %>%
      .[, util_score := sum(util_score), by=c("agent_id", "actions")] %>%
      unique() %>%
      dcast(agent_id ~ actions, value.var = "util_score") %>%
      .[, best_action :=  ifelse(Optimized > Unoptimized, "Optimized", "Unoptimized")] %>%
      melt( id.vars = c("agent_id", "best_action"),
            measure.vars = c("Optimized", "Unoptimized"),
            variable.name = "actions",
            value.name = "util_score" )
 
  sim$chosen_actions <- copy(sim$actions_send)[ actions == best_action , .(agent_id, best_action) ] %>%
    merge(sim$chosen_actions, by=c("agent_id"), all.x=TRUE, all.y=TRUE) %>%
    .[ , best_action := ifelse(!is.na(best_action.x), best_action.x,  "NOT ASSIGNED")] %>% 
    .[ , -c("best_action.x", "best_action.y")]

  sim$discourse_memory <- copy(sim$actions_send) %>%
    .[copy(sim$messages), on = c("agent_id" = "from"), nomatch = 0L, allow.cartesian = TRUE] %>%
    setnames("agent_id", "from") %>%
    setkey("from") %>%
    unique() %>%
    .[copy(sim$discourse_memory), on = c("from"), nomatch = 0L, allow.cartesian = TRUE] %>%
    .[actions==best_action] %>%
    .[ , past_messages := ifelse(lengths(past_messages) < params(sim)$rc_model$message_memory_depth,
                                 ifelse(.[ , best_action] == "Unoptimized",
                                        mapply(function(x, y) {
                                          list(c(unlist(x), y))
                                        }, x=past_messages, y=opinion_from),
                                        mapply(function(x, y) {
                                          list(c(unlist(x), y))
                                        }, x=past_messages, y=opt_message)),
                                 ifelse(.[ , best_action] == "Unoptimized",
                                        mapply(function(x, y) {
                                          list(c(unlist(x)[1:params(sim)$rc_model$message_memory_depth], y))
                                        }, x=past_messages, y=opinion_from),
                                        mapply(function(x, y) {
                                          list(c(unlist(x)[1:params(sim)$rc_model$message_memory_depth], y))
                                        }, x=past_messages, y=opt_message)
                                 )
    )] %>%
    .[ , past_opinions := ifelse(lengths(past_opinions) < params(sim)$rc_model$opinion_memory_depth,
                                 mapply(function(x, y) {
                                   list(c(unlist(x), y))
                                 }, x=past_opinions, y=opinion_from),
                                 mapply(function(x, y) {
                                   list(c(unlist(x)[1:2], y))
                                 }, x=past_opinions, y=opinion_from)
    )
    ] %>%
    setnames("opinion_from", "opinion") %>%
    setnames("opt_message", "message") %>%
    .[ , .(from, to, message, past_messages, opinion, past_opinions)] %>%
    .[ , past_messages := as.character(past_messages) ] %>%
    .[ , past_opinions := as.character(past_opinions) ] %>%
    unique() %>%
    .[ , past_messages := sapply(past_messages, function(x) list(eval(parse(text = x))))] %>%
    .[ , past_opinions := sapply(past_opinions, function(x) list(eval(parse(text = x))))]

  sim$messages <- copy(sim$messages) %>%
    setnames(old=c("opinion_from", "opt_message"), new=c("opinion_from_y", "opt_message_y")) %>%
    unique() %>%
    setnames(old = c("from", "to"), new = c("to", "from")) %>%
    .[ , .(to, opinion_from_y, opt_message_y)] %>%
    .[sim$messages, on="to", nomatch=0L, allow.cartesian=TRUE] %>% # WORKS
    .[sim$actions_send[, .(agent_id, actions, best_action)], nomatch=0L, on=c("from" = "agent_id"), allow.cartesian=TRUE] %>%
    .[ , assumption_to := ifelse(.[, best_action] == "Unoptimized", opinion_from_y, opt_message_y)] %>%
    .[ , -c("opinion_from_y", "opt_message_y")] %>%
    setkey("from") %>%
    unique()

  # Receiving

  sim$opinion_updating <- copy(sim$messages) %>%
    .[sim$discourse_memory, on=c("from"="to"), nomatch = 0L, allow.cartesian = TRUE] %>% 
    .[best_action == actions] %>%
    .[ , distance_to_past_messages := mapply(function(a,b) {
      max(
        sapply(a, function(x) {
          abs(x - b)
        })
      )
    }, a=past_messages, b=assumption_to)] %>% 
    .[ , -c("past_messages", "past_opinions")] %>%
    .[sim$discourse_memory, on=c("from"), nomatch = 0L, allow.cartesian = TRUE] %>%
    .[ , distance_to_past_opinions := mapply(function(a,b) {
      max(
        sapply(a, function(x) {
          abs(x - b)
        })
      )
    }, a=past_opinions, b=assumption_to)] %>% 
    .[ , within_epsilon := abs(opinion_from - assumption_to) < params(sim)$rc_model$epsilon] %>%
    .[ , self_consistent := distance_to_past_opinions < params(sim)$rc_model$self_incons_tolerance] %>%  
    .[ , trust := distance_to_past_messages < params(sim)$rc_model$other_incons_tolerance] %>% 
    .[ within_epsilon == TRUE & trust == TRUE & self_consistent == TRUE ] %>%
    .[ , sum_assumptions := sum(assumption_to), by=from] %>%
    .[ , denominator := .N, by=from ] %>%
    .[ , opinion_y := ifelse( denominator != 0, sum_assumptions / denominator, opinion_from )] %>%
    setnames("from", "agent_id") %>%
    .[ , .(agent_id, opinion_y)] %>%
    setkey("agent_id") %>%
    unique()

  sim$agent_characteristics <- merge(sim$agent_characteristics, sim$opinion_updating, by="agent_id", all.x = TRUE) %>%
    .[ , opinion := ifelse(is.na(opinion_y), opinion, opinion_y)] %>%
    .[ , -c("opinion_y")] 

  sim$messages <- sim$messages %>%
    .[best_action == actions] %>%
    .[ , -c("assumption_to", "opt_message", "opinion_from", "actions", "best_action")]

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

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
    defineParameter("memory_depth", "numeric", 1, NA, NA, "The number of time steps agents remember messages and opinions for.")
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
  
  sim$messages <- sim$environment %>%
    activate(edges) %>%
    as_tibble() %>%
    data.table() %>%
    setnames(old = c("from", "to"), new = c("to", "from"))
  before_starting_state <<- sim$agent_characteristics
  sim$messages <- sim$environment %>%
    activate(edges) %>%
    as_tibble() %>%
    data.table() %>%
    rbind(messages) %>% 
    .[copy(sim$agent_characteristics[, .(agent_id, opinion)]), nomatch = 0L, on = c("from" = "agent_id"), allow.cartesian=TRUE] %>%
    setnames("opinion", "opinion_from") %>%
    .[ , assumption_to := opinion_from] ->> starting_state
  # vec_rnorm(1, .[, opinion_from], 0.1)
  starting_state_reduced <<- starting_state %>%
    .[ , -c("to", "assumption_to")] %>%
    setkey("from") %>%
    unique()
  
  #plot(density(sim$agent_characteristics$opinion))
  
  # make matrix of all possible optimized messages
  sim$message_matrix <- outer(sim$messages$opinion_from, sim$messages$assumption_to, produce_altered_message) # works
  row.names(sim$message_matrix) <- sim$messages[ , from]
  colnames(sim$message_matrix) <- sim$messages[ , to]
  mess_mat <<- sim$message_matrix
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
    .[ , opt_message := mean(opt_message), by = .(from)] ->> messages_distro
  
  messages_distro_reduced <<- messages_distro %>%
    .[ , .(from, opt_message)] %>%
    setkey("from") %>%
    unique()
  
  # in future rounds, past_messages will be assigned too
  sim$discourse_memory <- copy(sim$messages) %>%
    #.[ , past_messages := sapply(opt_message, function(x) {list(x)} )] %>% # this will get reassigned at end of init
    .[ , past_opinions := sapply(opinion_from, function(x) {list(x)} )] %>%
    .[ , .(from, past_opinions)] %>%
    as_tibble() %>% # unique() does not yet work for data.table with list columns -> casting to tibble for this
    distinct() %>%
    data.table() %>%
    #.[ , past_messages := as.character(past_messages) ] %>%
    .[ , past_opinions := as.character(past_opinions) ] %>% 
    unique() %>% 
    #.[ , past_messages := sapply(past_messages, function(x) list(eval(parse(text = x))))] %>%
    .[ , past_opinions := sapply(past_opinions, function(x) list(eval(parse(text = x))))]
  
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
      .[ , util_score := distance_to_past_opinions - distance_message_opinion - distance_message_assumption] %>% # include distance_to_past_messages in step function
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
  actions <<- sim$actions_send %>%
    .[messages, on=c("agent_id" = "from"), nomatch=0L, allow.cartesian=TRUE]
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
    .[ , past_opinions := ifelse(lengths(past_opinions) < params(sim)$rc_model$memory_depth,
                                 mapply(function(x, y) {
                                   list(c(unlist(x), y))
                                 }, x=past_opinions, y=opinion_from),
                                 mapply(function(x, y) {
                                   list(c(unlist(x)[1:params(sim)$rc_model$memory_depth], y))
                                 }, x=past_opinions, y=opinion_from)
    )
    ] %>%
    .[ , .(from, to, past_messages, past_opinions)] %>%
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
    #.[ , distance_to_past_messages := mapply(function(a,b) {
    #  mean(
    #    sapply(a, function(x) {
    #      abs(x - b)
    #    })
    #  )
    #}, a=past_messages, b=assumption_to)] %>% 
    .[ , within_epsilon := abs(opinion_from - assumption_to) < params(sim)$rc_model$epsilon] %>%
    .[ , no_within := sum(within_epsilon), by=from] %>%
    #.[ , doubt := distance_to_past_messages] ->> diagnose %>% 
    .[ within_epsilon == TRUE ] %>%
    .[ , opinion_y := ifelse( no_within != 0, sum(assumption_to) / no_within, opinion_from ), by=from ] %>%
    setnames("from", "agent_id") %>%
    .[ , .(agent_id, opinion_y)] %>%
    setkey("agent_id") %>%
    unique() ->> after_updating
  before_merge <<- sim$agent_characteristics
  sim$agent_characteristics <- merge(sim$agent_characteristics, sim$opinion_updating, by="agent_id", all.x = TRUE) %>%
    .[ , opinion := ifelse(is.na(opinion_y), opinion, opinion_y)] %>%
    .[ , -c("opinion_y")] ->> after_merge

  sim$messages <- sim$messages %>% 
    .[best_action == actions] %>% 
    .[ , -c("assumption_to", "opt_message", "opinion_from", "actions", "best_action")]
  
  #plot(density(sim$agent_characteristics$opinion))
  return(invisible(sim))
  
}

rc_modelStep <- function(sim) {
  print(time(sim))
  
  mat <<- sim$message_matrix
  mess <<- sim$messages
  ag_car <<- sim$agent_characteristics
  sim$messages <- sim$messages %>%
    .[copy(sim$agent_characteristics[, .(agent_id, opinion)]), nomatch = 0L, on = c("from" = "agent_id"), allow.cartesian=TRUE] %>%
    setnames("opinion", "opinion_from") %>%
    .[ , assumption_to := vec_rnorm(1, .[, opinion_from], 0.1)]
  
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
      .[ , util_score := distance_to_past_opinions - distance_to_past_messages - distance_message_opinion - distance_message_assumption] %>% # include distance_to_past_messages in step function
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
  
  sim$discourse_memory <- copy(sim$actions_send) %>%
    .[copy(sim$messages), on = c("agent_id" = "from"), nomatch = 0L, allow.cartesian = TRUE] %>%
    setnames("agent_id", "from") %>%
    setkey("from") %>%
    unique() %>%
    .[copy(sim$discourse_memory), on = c("from"), nomatch = 0L, allow.cartesian = TRUE] %>%
    .[actions==best_action] %>%
    .[ , past_messages := ifelse(lengths(past_messages) < params(sim)$rc_model$memory_depth, 
                                 ifelse(.[ , best_action] == "Unoptimized",
                                        mapply(function(x, y) {
                                          list(c(unlist(x), y))
                                        }, x=past_messages, y=opinion_from),
                                        mapply(function(x, y) {
                                          list(c(unlist(x), y))
                                        }, x=past_messages, y=opt_message)),
                                 ifelse(.[ , best_action] == "Unoptimized",
                                        mapply(function(x, y) {
                                          list(c(unlist(x)[1:2], y))
                                        }, x=past_messages, y=opinion_from),
                                        mapply(function(x, y) {
                                          list(c(unlist(x)[1:2], y))
                                        }, x=past_messages, y=opt_message)
                                 )
    )] %>%
    .[ , past_opinions := ifelse(lengths(past_opinions) < params(sim)$rc_model$memory_depth,
                                 mapply(function(x, y) {
                                   list(c(unlist(x), y))
                                 }, x=past_opinions, y=opinion_from),
                                 mapply(function(x, y) {
                                   list(c(unlist(x)[1:2], y))
                                 }, x=past_opinions, y=opinion_from)
    )
    ] %>%
    .[ , .(from, to, past_messages, past_opinions)] %>%
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
    .[ , within_epsilon := abs(opinion_from - assumption_to) < params(sim)$rc_model$epsilon] %>%
    .[ , no_within := sum(within_epsilon), by=from] %>%
    .[ , doubt := runif(1, 0, 1) < distance_to_past_messages] %>% # .[ , doubt] ->> doubt
    .[ within_epsilon == TRUE & doubt == FALSE ] %>%
    .[ , opinion_y := ifelse( no_within != 0, sum(assumption_to) / no_within, opinion_from ), by=from ] %>%
    setnames("from", "agent_id") %>%
    .[ , .(agent_id, opinion_y)] %>%
    setkey("agent_id") %>%
    unique()->> after_updating
  before_merge <<- sim$agent_characteristics
  
  sim$agent_characteristics <- merge(sim$agent_characteristics, sim$opinion_updating, by="agent_id", all.x = TRUE) %>%
    .[ , opinion := ifelse(is.na(opinion_y), opinion, opinion_y)] %>%
    .[ , -c("opinion_y")] ->> after_merge
  #fuck_me <<- sim$agent_characteristics
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
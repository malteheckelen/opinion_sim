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
  reqdPkgs = list("tidyverse"),
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
  
  sim$messages <- sim$environment %>%
    activate(edges) %>%
    as_tibble() %>%
    data.table() %>%
    rbind(messages) %>%
    merge(sim$agent_characteristics[, .(agent_id, opinion)], by.x = "from", by.y = "agent_id") %>%
    setnames("opinion", "opinion_from") %>%
    .[, assumption_to := vec_rnorm(1, .[, opinion_from], 0.1)] 
  
  
  # make matrix of all possible optimized messages
  sim$message_matrix <- outer(sim$messages$opinion_from, sim$messages$assumption_to, produce_altered_message) # works
  row.names(message_matrix) <- sim$messages[, from]
  colnames(message_matrix) <- sim$messages[, to]
  
  sim$messages <- copy(sim$message_matrix) %>% 
    data.table() %>% 
    .[, from := as.numeric(row.names(sim$message_matrix))] %>% 
    melt( id.vars = c("from"),
          measure.vars = as.character(seq(1, no_agents, 1)),
          variable.name = "to",
          value.name = "opt_message" ) %>%
    .[, opt_message := mean(opt_message), by = .(from)] %>%
    .[ to != from] %>%
    setkey("from") %>%
    unique() %>%
    merge(unique(sim$messages), by.x = c("from", "to"), by.y = c("from", "to"), allow.cartesian = TRUE) %>%
    .[, past_messages := sapply(opt_message, function(x) {list(x)} )] %>% 
    .[, past_opinions := sapply(opinion_from, function(x) {list(x)} )]
  
  # Sending
  sim$actions_send <- copy(sim$messages) %>% 
    merge(sim$actions_send, by.x = c("from"), by.y = c("agent_id"), allow.cartesian = TRUE) %>%
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
    .[, util_score := distance_to_past_messages - distance_to_past_opinions - distance_message_opinion - distance_message_assumption]
  
  sim$for_receive_temp <- copy(sim$actions_send[, .(from, actions, distance_to_past_messages)]) %>%
    setkey("from") %>%
    unique()
  
  sim$actions_send <- sim$actions_send %>%
    setnames("from", "agent_id") %>%
    .[,.(agent_id, actions, util_score)] %>%
    setkey("agent_id") %>%
    unique() %>%
    .[, util_score := sum(util_score), by=c("agent_id", "actions")] %>% 
    setkey("agent_id") %>%
    unique() %>% 
    dcast(agent_id ~ actions, value.var = "util_score") %>% 
    .[, best_action :=  ifelse(Optimized > Unoptimized, "Optimized", "Unoptimized")] %>% 
    melt( id.vars = c("agent_id", "best_action"),
          measure.vars = c("Optimized", "Unoptimized"),
          variable.name = "actions",
          value.name = "util_score" ) 
  
  sim$messages <- copy(sim$messages) %>%
    .[, -c("past_messages", "past_opinions")] %>%
    setnames(old=c("opinion_from", "opt_message"), new=c("opinion_from_y", "opt_message_y")) %>%
    setkey("from") %>%
    unique() %>%
    setnames(old = c("from", "to"), new = c("to", "from")) %>% 
    .[, to := as.factor(to)] %>%
    .[, .(to, opinion_from_y, opt_message_y)] %>%
    merge(unique(sim$messages[, -c("past_messages", "past_opinions")]), by.x = c("to"), by.y = c("to"), allow.cartesian = TRUE) %>%
    setkey(from) %>%
    unique() %>%
    merge(unique(sim$actions_send[, .(agent_id, best_action)]), by.x = c("from"), by.y = c("agent_id")) %>%
    .[ , past_messages := ifelse(lengths(past_messages) < params(sim)$rc_model$memory_depth, c(past_messages, ))]
    .[ , assumption_to := ifelse(.[, best_action] == "Unoptimized", opinion_from_y, message_y)] %>% 
    .[ , -c("opinion_from_y", "opt_message_y")] %>%
    setkey("from") %>%
    unique() 
    test <- message_metrics_table %>%
      mutate(past_messages = ifelse(lengths(past_messages) < 3, vec_append_to_list(message, past_messages, FALSE), vec_append_to_list(message, past_messages, TRUE))) %>%
      mutate(past_opinions = ifelse(lengths(past_opinions) < 3, vec_append_to_list(opinion_from, past_opinions, FALSE), vec_append_to_list(opinion_from, past_opinions, TRUE)))
  # Receiving
  
  sim$agent_characteristics <- copy(sim$messages) %>%
    merge(sim$for_receive_temp, by=c("from"), allow.cartesian = TRUE) %>% 
    .[best_action == actions] %>% 
    .[ , .(from, distance_to_past_messages)] %>%
    .[ , from := as.factor(from)] %>%
    setnames("from", "to") %>%
    setkey("to") %>%
    unique() %>%
    merge(unique(sim$messages), by.x = c("to"), by.y = c("to")) %>%
    .[ , within_epsilon := abs(opinion_from - assumption_to) < epsilon] %>%
    .[ , no_within := sum(within_epsilon), by=from] %>%
    .[ , doubt := (1 - distance_to_past_messages)] %>%
    .[ , opinion := ifelse( no_within != 0, mean(assumption_to * doubt) / no_within, opinion_from ) ] %>%
    setnames("opinion_from", "opinion") %>%
    .[ , .(from, opinion)] %>%
    setkey("from") %>%
    unique() %>%
    setnames("from", "agent_id") %>%
    merge(sim$agent_characteristics[ , -c("opinion")], by=c("agent_id"))
  
  return(invisible(sim))
  
}

rc_modelStep <- function(sim) {
  print(time(sim))

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
  
  sim$messages <- sim$environment %>%
    activate(edges) %>%
    as_tibble() %>%
    data.table() %>%
    rbind(messages) %>%
    merge(sim$agent_characteristics[, .(agent_id, opinion)], by.x = "from", by.y = "agent_id") %>%
    setnames("opinion", "opinion_from") %>%
    .[, assumption_to := vec_rnorm(1, .[, opinion_from], 0.1)] 
  
  
  # make matrix of all possible optimized messages
  sim$message_matrix <- outer(sim$messages$opinion_from, sim$messages$assumption_to, produce_altered_message) # works
  row.names(message_matrix) <- sim$messages[, from]
  colnames(message_matrix) <- sim$messages[, to]
  
  sim$messages <- copy(sim$message_matrix) %>% 
    data.table() %>% 
    .[, from := as.numeric(row.names(sim$message_matrix))] %>% 
    melt( id.vars = c("from"),
          measure.vars = as.character(seq(1, no_agents, 1)),
          variable.name = "to",
          value.name = "opt_message" ) %>%
    .[, opt_message := mean(opt_message), by = .(from)] %>%
    .[ to != from] %>%
    setkey("from") %>%
    unique() %>%
    merge(unique(sim$messages), by.x = c("from", "to"), by.y = c("from", "to"), allow.cartesian = TRUE) %>%
    .[, past_messages := sapply(opt_message, function(x) {list(x)} )] %>% 
    .[, past_opinions := sapply(opinion_from, function(x) {list(x)} )]
  
  # Sending
  sim$actions_send <- copy(sim$messages) %>% 
    merge(sim$actions_send, by.x = c("from"), by.y = c("agent_id"), allow.cartesian = TRUE) %>%
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
    .[, util_score := distance_to_past_messages - distance_to_past_opinions - distance_message_opinion - distance_message_assumption]
  
  sim$for_receive_temp <- copy(sim$actions_send[, .(from, actions, distance_to_past_messages)]) %>%
    setkey("from") %>%
    unique()
  
  sim$actions_send <- sim$actions_send %>%
    setnames("from", "agent_id") %>%
    .[,.(agent_id, actions, util_score)] %>%
    setkey("agent_id") %>%
    unique() %>%
    .[, util_score := sum(util_score), by=c("agent_id", "actions")] %>% 
    setkey("agent_id") %>%
    unique() %>% 
    dcast(agent_id ~ actions, value.var = "util_score") %>% 
    .[, best_action :=  ifelse(Optimized > Unoptimized, "Optimized", "Unoptimized")] %>% 
    melt( id.vars = c("agent_id", "best_action"),
          measure.vars = c("Optimized", "Unoptimized"),
          variable.name = "actions",
          value.name = "util_score" ) 
  
  sim$messages <- copy(sim$messages) %>%
    .[, -c("past_messages", "past_opinions")] %>%
    setnames(old=c("opinion_from", "opt_message"), new=c("opinion_from_y", "opt_message_y")) %>%
    setkey("from") %>%
    unique() %>%
    setnames(old = c("from", "to"), new = c("to", "from")) %>% 
    .[, to := as.factor(to)] %>%
    .[, .(to, opinion_from_y, opt_message_y)] %>%
    merge(unique(sim$messages[, -c("past_messages", "past_opinions")]), by.x = c("to"), by.y = c("to"), allow.cartesian = TRUE) %>%
    setkey(from) %>%
    unique() %>%
    merge(unique(sim$actions_send[, .(agent_id, best_action)]), by.x = c("from"), by.y = c("agent_id")) %>%
    .[ , assumption_to := ifelse(.[, best_action] == "Unoptimized", opinion_from_y, message_y)] %>% 
    .[ , -c("opinion_from_y", "opt_message_y")] %>%
    setkey("from") %>%
    unique() 
  
  # Receiving
  
  sim$agent_characteristics <- copy(sim$messages) %>%
    merge(sim$for_receive_temp, by=c("from"), allow.cartesian = TRUE) %>% 
    .[best_action == actions] %>% 
    .[ , .(from, distance_to_past_messages)] %>%
    .[ , from := as.factor(from)] %>%
    setnames("from", "to") %>%
    setkey("to") %>%
    unique() %>%
    merge(unique(sim$messages), by.x = c("to"), by.y = c("to")) %>%
    .[ , within_epsilon := abs(opinion_from - assumption_to) < epsilon] %>%
    .[ , no_within := sum(within_epsilon), by=from] %>%
    .[ , doubt := (1 - distance_to_past_messages)] %>%
    .[ , opinion := ifelse( no_within != 0, mean(assumption_to * doubt) / no_within, opinion_from ) ] %>%
    setnames("opinion_from", "opinion") %>%
    .[ , .(from, opinion)] %>%
    setkey("from") %>%
    unique() %>%
    setnames("from", "agent_id") %>%
    merge(sim$agent_characteristics[ , -c("opinion")], by=c("agent_id"))

  return(invisible(sim))
  
}


# FUNCTIONS

# generate first assumed opinions (for Init)
produce_initial_assumption <- function(sender, receiver, chars=SIM$agent_characteristics) {
  
  opinion <- filter(chars, agent_id == sender)$opinion
  
  sender_neighborhood <- unlist(filter(chars, agent_id == sender)$neighborhood)
  receiver_neighborhood <- unlist(filter(chars, agent_id == receiver)$neighborhood)
  
  size_intersect <- length(intersect(sender_neighborhood, receiver_neighborhood))
  
  if (runif(1, 0, 1) < 0.5) {
    assumed_opinion <- (opinion + epsilon) / size_intersect
  } else {
    assumed_opinion <- (opinion - epsilon) / size_intersect
  }
  
  return(assumed_opinion)
  
} # works

vec_produce_initial_assumption <- Vectorize(produce_initial_assumption) # works

# generate altered optimized messages to be taken into consideration
produce_altered_message <- function(opinion_send, message_receive) {
  
  # produce altered message without epsilon bound
  altered_message <- ifelse(opinion_send < message_receive, 
                            opinion_send + abs(opinion_send - message_receive) / 2, 
                            opinion_send - abs(opinion_send - message_receive) / 2)
  
  return(altered_message)
  
} # works

### switch statement for distance to past messages

distance_to_past <- function(opinion_or_message_vector, opinion, message, action) {
  
  distance <- switch(action,
                     Unoptimized = {
                       mean( abs(opinion - sapply(opinion_or_message_vector, function(x) { unlist(x) })) )
                     },
                     Optimized = {
                       mean( abs(message - sapply(opinion_or_message_vector, function(x) { unlist(x) })) )
                     })
  
  return(distance)
  
}

vec_distance_to_past <- Vectorize(distance_to_past)

### switch statement for distance computation

distance_switch <- function(opinion, message, assumption, action) {
  
  distance <- switch(action,
                     Unoptimized = {
                       opinion - assumption
                     },
                     Optimized = {
                       message - assumption
                     })
  
  return(distance)
  
}

vec_distance_switch <- Vectorize(distance_switch)

### retrieve messages

get_message <- function(id, best) {
  
  message <- switch(best,
                    "Unoptimized" = {
                      SIM$message_metrics %>%
                        select(from, opinion_from) %>%
                        filter(from==id) %>%
                        distinct() %>%
                        select(opinion_from) %>%
                        summarise(opinion_from = mean(opinion_from)) %>%
                        as.numeric()
                    },
                    "Optimized" = {
                      SIM$message_metrics %>%
                        select(from, message) %>%
                        filter(from==id) %>%
                        distinct() %>%
                        select(message) %>%
                        summarise(message = mean(message)) %>%
                        as.numeric()
                    })
  
  return(message)
  
}

vec_get_message <- Vectorize(get_message)

append_to_list <- function(list, element, pop) {
  
  new_list <- ifelse(
    pop == FALSE,
    list(c(list, element[1:(length(element)-1)])),
    list(c(list, element))
  )
  
  
  
  return(new_list)
  
}

vec_append_to_list <- Vectorize(append_to_list)
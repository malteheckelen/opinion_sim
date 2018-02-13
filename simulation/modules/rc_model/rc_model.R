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
  
  sim$agent_characteristics <- sim$agent_characteristics

  sim$actions_send <- tibble(
    
    agent_id = rep(agent_characteristics$agent_id, each=2),
    actions = rep(c("Unoptimized", "Optimized"), no_agents),
    util_score = rep(0, length(actions))
    
  )
  
  print(time(sim))
  sim$message_metrics <- sim$environment %>%
    activate(edges) %>%
    as_tibble() %>%
    data.table() %>%
    setnames(old = c("from", "to"), new = c()) %>%
    mutate(from_two = from) %>%
    mutate(to_two = to) %>%
    mutate(from = to_two) %>%
    mutate(to = from_two) %>%
    select(from, to)
  
  sim$message_metrics <- sim$environment %>%
    activate(edges) %>%
    as_tibble() %>%
    rbind(sim$message_metrics) %>%
    inner_join(sim$agent_characteristics, by=c("from" = "agent_id")) %>% 
    mutate(opinion_from = opinion) %>%
    select(from, to, opinion_from) %>%
    mutate(assumption_to = sapply(opinion_from, function(x) { ifelse(rnorm(1, x, 0.2) < 0, 0, rnorm(1, x, 0.2)) } ))
  
  # make matrix of all possible optimized messages
  sim$message_matrix <- outer(sim$message_metrics$opinion_from, sim$message_metrics$assumption_to, produce_altered_message) # works
  row.names(sim$message_matrix) <- sim$message_metrics$from
  colnames(sim$message_matrix) <- sim$message_metrics$to
  
  sim$messages <- sim$message_matrix %>% 
    as_tibble() %>% 
    mutate(from = as.numeric(row.names(sim$message_matrix))) %>% 
    gather(to, message, "1":as.character(sim$no_agents) ) %>%
    group_by(from) %>%
    mutate(message = mean(message)) %>%
    ungroup() %>%
    select(from, message) %>%
    distinct()
  
  sim$message_metrics <- sim$messages %>% 
    inner_join(sim$message_metrics, by=c("from" = "from")) %>%
    mutate(past_messages = sapply(opinion_from, function(x) { list(x) } )) %>%
    mutate(past_opinions = sapply(opinion_from, function(x) { list(x) } )) %>%
    inner_join(sim$actions_send, by=c("from" = "agent_id")) %>%
    mutate(distance_to_past_messages = vec_distance_to_past(past_messages, opinion_from, message, actions)) %>%
    mutate(distance_to_past_opinions = vec_distance_to_past(past_opinions, opinion_from, message, actions)) %>% # works
    mutate(distance_to_past_messages = abs(distance_to_past_messages)) %>%
    mutate(distance_to_past_opinions = abs(distance_to_past_opinions)) %>%
    mutate(distance = vec_distance_switch(opinion_from, message, assumption_to, action=actions)) %>%
    group_by(from) %>%
    mutate(distance = mean(distance)) %>%
    mutate(distance = abs(distance)) %>% # works
    ungroup()
  
  # Sending
  sim$actions_send <- sim$message_metrics %>%
    mutate(util_score = distance - distance_to_past_messages - distance_to_past_opinions) %>%
    mutate(agent_id = from) %>%
    select(agent_id, actions, util_score) %>%
    distinct() %>%
    tidyr::spread(actions, util_score) %>%
    mutate(best_action = ifelse(Optimized > Unoptimized, "Optimized", "Unoptimized")) %>%
    gather(actions, util_score, "Optimized":"Unoptimized")
  
  sim$message_metrics <- sim$actions_send %>%
    mutate(to = agent_id) %>%
    select(to, best_action) %>%
    distinct() %>%
    inner_join(sim$message_metrics) %>% 
    mutate(assumption_to = filter(to == from)$message[1]) %>%
    distinct() %>% # MAYBE I CAN FILTER WITH DATATABLE MORE EFFICIENTLY
    mutate(within_epsilon = distance < params(sim)$rc_model$epsilon) %>%
    group_by(from) %>%
    mutate(no_within = sum(within_epsilon)) %>%
    ungroup() %>%
    mutate(doubt = 1 - distance_to_past_messages) %>%
    mutate(numerator_elements = ifelse(within_epsilon==TRUE, assumption_to * doubt, 0)) %>%
    mutate(numerator_mean = sum(numerator_elements)) %>%
    mutate(opinion_from = numerator_mean / no_within)
  
  # Receiving
  
  sim$agent_characteristics <- sim$message_characteristics %>%
    select(from, opinion_from) %>%
    inner_join(sim$agent_characteristics, by=c("from" = "agent_id")) %>%
    select(agent_id = from) %>%
    select(opinion = opinion_from) %>%
    distinct()
  
  # past data update for next step
  
  sim$message_metrics <- sim$message_metrics %>%
    mutate(past_messages = ifelse(lengths(past_messages) < params(sim)$rc_model$memory_depth, vec_append_to_list(message, past_messages, FALSE), vec_append_to_list(message, past_messages, TRUE))) %>%
    mutate(past_opinions = ifelse(lengths(past_opinions) < params(sim)$rc_model$memory_depth, vec_append_to_list(opinion_from, past_opinions, FALSE), vec_append_to_list(opinion_from, past_opinions, TRUE)))
  
  return(invisible(sim))
}

rc_modelStep <- function(sim) {
  print(time(sim))

  # create a distance table with initial assumptions
  sim$message_metrics <- sim$message_metrics %>%
    select(from, to, message, past_messages, past_opinions) %>%
    distinct() %>%
    mutate(assumption_to = filter(to == from)$message[1]) %>%
    select(-message) 
  
  # make matrix of all possible optimized messages
  sim$message_matrix <- outer(sim$message_metrics$opinion_from, sim$message_metrics$assumption_to, produce_altered_message) # works
  row.names(sim$message_matrix) <- sim$message_metrics$from
  colnames(sim$message_matrix) <- sim$message_metrics$to
  
  sim$messages <- sim$message_matrix %>% 
    as_tibble() %>% 
    mutate(from = as.numeric(row.names(sim$message_matrix))) %>% 
    gather(to, message, "1":as.character(sim$no_agents) ) %>%
    group_by(from) %>%
    mutate(message = mean(message)) %>%
    ungroup() %>%
    select(from, message) %>%
    distinct()
  
  sim$message_metrics <- sim$messages %>% 
    inner_join(sim$message_metrics, by=c("from" = "from")) %>%
    mutate(distance_to_past_messages = vec_distance_to_past(past_messages, opinion_from, message, actions)) %>%
    mutate(distance_to_past_opinions = vec_distance_to_past(past_opinions, opinion_from, message, actions)) %>% # works
    mutate(distance_to_past_messages = abs(distance_to_past_messages)) %>%
    mutate(distance_to_past_opinions = abs(distance_to_past_opinions)) %>%
    mutate(distance = vec_distance_switch(opinion_from, message, assumption_to, action=actions)) %>%
    group_by(from) %>%
    mutate(distance = mean(distance)) %>%
    mutate(distance = abs(distance)) %>%
    ungroup() # works
  
  # Sending
  sim$actions_send <- sim$message_metrics %>%
    mutate(util_score = distance - distance_to_past_messages - distance_to_past_opinions) %>%
    mutate(agent_id = from) %>%
    select(agent_id, actions, util_score) %>%
    distinct() %>%
    tidyr::spread(actions, util_score) %>%
    mutate(best_action = ifelse(Optimized > Unoptimized, "Optimized", "Unoptimized")) %>%
    gather(actions, util_score, "Optimized":"Unoptimized")
  
  sim$message_metrics <- sim$actions_send %>%
    mutate(to = agent_id) %>%
    select(to, best_action) %>%
    distinct() %>%
    inner_join(sim$message_metrics) %>%
    mutate(assumption_to = filter(to == from)$message[1]) %>%
    distinct() %>%
    mutate(within_epsilon = distance < params(sim)$rc_model$epsilon) %>%
    group_by(agent_id) %>%
    mutate(no_within = sum(within_epsilon)) %>%
    ungroup() %>%
    mutate(doubt = 1 - distance_to_past_messages) %>%
    mutate(numerator_elements = ifelse(within_epsilon==TRUE, assumption_to * doubt, 0)) %>%
    mutate(numerator_mean = sum(numerator_elements)) %>%
    mutate(opinion_from = numerator_mean / no_within)
  
  # Receiving
  
  sim$agent_characteristics <- sim$message_characteristics %>%
    select(from, opinion_from) %>%
    inner_join(sim$agent_characteristics, by=c("from" = "agent_id")) %>%
    select(agent_id = from) %>%
    select(opinion = opinion_from) %>%
    distinct()
  
  # past data update for next step
  
  sim$message_metrics <- sim$message_metrics %>%
    mutate(past_messages = ifelse(lengths(past_messages) < params(sim)$rc_model$memory_depth, vec_append_to_list(message, past_messages, FALSE), vec_append_to_list(message, past_messages, TRUE))) %>%
    mutate(past_opinions = ifelse(lengths(past_opinions) < params(sim)$rc_model$memory_depth, vec_append_to_list(opinion_from, past_opinions, FALSE), vec_append_to_list(opinion_from, past_opinions, TRUE)))
  
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
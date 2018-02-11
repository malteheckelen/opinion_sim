
## PREP FROM basic_setup.R

no_agents <- 10
epsilon <- 0.1

agent_characteristics <- tibble(
  
  agent_id = seq(1, no_agents, 1),
  opinion = runif(no_agents, 0, 1)
  
)

## PREP FROM small_world.R

environment <- play_smallworld(n_dim=1, dim_size=no_agents, order=3, p_rewire=0.1)

environment <- environment %>%
  activate(nodes) %>%
  mutate(agent_id = agent_characteristics$agent_id)

agent_characteristics <- environment %>%
  activate(nodes) %>%
  mutate(neighborhood = local_members(mindist = 1)) %>%
  mutate(nbh_size = local_size(mindist = 1)) %>%
  as_tibble() %>%
  select(agent_id, neighborhood, nbh_size) %>%
  inner_join(agent_characteristics)

# INIT

# add tibbles for available actions and utility scores
actions_overall <- tibble(
  
  agent_id = rep(agent_characteristics$agent_id, each=4),
  actions = rep(c("Receive", "Send", "Both", "Nothing"), no_agents),
  score = rep(0, length(actions))
  
)

actions_send <- tibble(
  
  agent_id = rep(agent_characteristics$agent_id, each=2),
  actions = rep(c("Unoptimized", "Optimized"), no_agents),
  util_score = rep(0, length(actions))
  
)

### Distances

message_part <- environment %>%
  activate(edges) %>%
  as_tibble() %>%
  mutate(from_two = from) %>%
  mutate(to_two = to) %>%
  mutate(from = to_two) %>%
  mutate(to = from_two) %>%
  select(from, to)

# create a distance table with initial assumptions
message_metrics_table <- environment %>%
  activate(edges) %>%
  as_tibble() %>%
  rbind(message_part) %>%
  inner_join(agent_characteristics, by=c("from" = "agent_id")) %>% 
  mutate(opinion_from = opinion) %>%
  mutate(assumption_to = vec_produce_initial_assumption(from, to)) %>% 
  select(from, to, opinion_from, assumption_to)

# make matrix of all possible optimized messages
message_matrix <- outer(agent_characteristics$opinion, agent_characteristics$opinion, produce_altered_message) # works
row.names(message_matrix) <- seq(1, no_agents, 1)
colnames(message_matrix) <- seq(1, no_agents, 1)

messages <- message_matrix %>% 
  as_tibble() %>% 
  mutate(sender = as.numeric(row.names(message_matrix))) %>% 
  gather(receiver, message, "1":as.character(length(colnames(message_matrix))) )

message_metrics_table <- messages %>% 
  group_by(sender) %>%
  mutate(message = mean(message)) %>%
  ungroup() %>%
  select(-receiver) %>%
  inner_join(message_metrics_table, by=c("sender" = "from")) %>%
  mutate(past_messages = sapply(opinion_from, function(x) { list(x) } )) %>%
  mutate(past_opinions = sapply(opinion_from, function(x) { list(x) } )) %>%
  inner_join(actions_send, by=c("sender" = "agent_id")) %>%
  mutate(distance_to_past_messages = vec_distance_to_past(past_messages, opinion_from, message, actions)) %>%
  mutate(distance_to_past_opinions = vec_distance_to_past(past_opinions, opinion_from, message, actions)) %>% # works
  mutate(distance = vec_distance_switch(opinion_from, message, assumption_to, actions)) %>%
  group_by(sender) %>%
  mutate(distance = mean(distance)) %>%
  ungroup() %>%
  mutate(distance_to_past_messages = abs(distance_to_past_messages)) %>%
  mutate(distance_to_past_opinions = abs(distance_to_past_opinions)) %>%
  mutate(distance = abs(distance)) %>%
  mutate(within_epsilon = distance < epsilon)

# Sending
actions_send <- message_metrics_table %>%
  mutate(util_score = distance - distance_to_past_messages - distance_to_past_opinions) %>%
  mutate(agent_id = sender) %>%
  select(agent_id, actions, util_score) %>%
  distinct()


# Receiving
agent_characteristics <- agent_characteristics %>%
  inner_join(message_table, by=c("agent_id" = "from")) %>%
  group_by(agent_id) %>%
  mutate(no_within = sum(within_epsilon)) %>%
  filter(within_epsilon == TRUE) %>%
  mutate(opinion = ifelse(no_within != 0, sum(opinion_to) / no_within, opinion)) %>%
  ungroup() %>%
  select(agent_id, opinion) %>% 
  full_join(agent_characteristics, by="agent_id") %>% 
  mutate(opinion = coalesce(opinion.x, opinion.y)) %>%
  select(agent_id, opinion) %>%
  distinct()

# STEP

# make matrix of all possible optimized messages
message_matrix <- outer(agent_characteristics$opinion, agent_characteristics$opinion, produce_altered_message) # works
row.names(message_matrix) <- seq(1, no_agents, 1)
colnames(message_matrix) <- seq(1, no_agents, 1)

message_tibble <- message_matrix %>% 
  as_tibble() %>% 
  mutate(sender = row.names(message_matrix)) %>% 
  gather(receiver, message, "1":as.character(length(colnames(message_matrix))) ) # works

### Distances

distances_part <- environment %>%
  activate(edges) %>%
  as_tibble() %>%
  mutate(from_two = from) %>%
  mutate(to_two = to) %>%
  mutate(from = to_two) %>%
  mutate(to = from_two) %>%
  select(from, to)

# create a distance table
distances_table <- environment %>%
  activate(edges) %>%
  as_tibble() %>%
  rbind(distances_part) %>%
  inner_join(agent_characteristics, by=c("from" = "agent_id")) %>% 
  mutate(opinion_from = opinion) %>%
  select(from, to, opinion_from) %>%
  inner_join(agent_characteristics, by=c("to" = "agent_id")) %>%
  mutate(opinion_to = opinion) %>% 
  select(from, to, opinion_from, opinion_to) %>%
  mutate(distance = opinion_from - opinion_to) %>%
  mutate(distance = abs(distance)) %>%
  mutate(within_epsilon = distance < 0.1)

agent_characteristics <- agent_characteristics %>%
  inner_join(distances_table, by=c("agent_id" = "from")) %>%
  group_by(agent_id) %>%
  mutate(no_within = sum(within_epsilon)) %>%
  filter(within_epsilon == TRUE) %>%
  mutate(opinion = ifelse(no_within != 0, sum(opinion_to) / no_within, opinion)) %>%
  ungroup() %>%
  select(agent_id, opinion) %>% 
  full_join(agent_characteristics, by="agent_id") %>% 
  mutate(opinion = coalesce(opinion.x, opinion.y)) %>%
  select(agent_id, opinion) %>%
  distinct()

# FUNCTIONS

# generate first assumed opinions (for Init)
produce_initial_assumption <- function(sender, receiver) {
  
  opinion <- filter(agent_characteristics, agent_id == sender)$opinion
  
  sender_neighborhood <- unlist(filter(agent_characteristics, agent_id == sender)$neighborhood)
  receiver_neighborhood <- unlist(filter(agent_characteristics, agent_id == receiver)$neighborhood)
  
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
  
  # check if within epsilon to own opinion and correct
  # commenting this for now, see Evernote: Design
  #altered_message <- ifelse(abs(opinion_send - altered_message) > epsilon,
  #                          ifelse((opinion_send - altered_message) <= 0, 0, opinion_send - altered_message),
  #                          altered_message)
  
  return(altered_message)
  
} # works

### switch statement for distance to past messages

distance_to_past <- function(opinion_or_message_vector, opinion, message, action) {
  
  distance <- switch(action,
         Unoptimized = {
           mean(opinion - sapply(opinion_or_message_vector, function(x) { abs(unlist(x)) }) )
         },
         Optimized = {
           mean(message - sapply(opinion_or_message_vector, function(x) { abs(unlist(x)) }) )
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

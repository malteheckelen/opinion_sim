
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
  actions = rep(c("Receive", "Send", "Both", "Nothing"), each=4),
  score = rep(0, length(agent_characteristics$agent_id))
  
)

actions_send <- tibble(
  
  agent_id = rep(agent_characteristics$agent_id, each=2),
  strategies = c("Unoptimized", "Optimized"),
  score = rep(0, length(agent_characteristics$agent_id))
  
)

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
  mutate(assumption_to = vec_produce_initial_assumption(from, to))

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

# STEP
# make matrix of all possible optimized messages
message_matrix <- outer(agent_characteristics$opinion, agent_characteristics$opinion, produce_altered_message) # works
row.names(message_matrix) <- seq(1, no_agents, 1)
colnames(message_matrix) <- seq(1, no_agents, 1)

message_tibble <- message_matrix %>% 
  as_tibble() %>% 
  mutate(sender = row.names(message_matrix)) %>% 
  gather(receiver, message, "1":"10") # works

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
  
  # generate
  assumed_opinion <- sapply(unlist(sender_neighborhood), function(x) {
    
    size_intersect <- length(intersect(sender_neighborhood, receiver_neighborhood))
    
    if (runif(1, 0, 1) < 0.5) {
      (opinion + epsilon) / size_intersect
    } else {
      (opinion - epsilon) / size_intersect
    }
    
    
  })
  
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

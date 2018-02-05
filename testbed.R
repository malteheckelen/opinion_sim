

environment <- create_lattice(c(10, 10)) 

no_agents <- 100

agent_characteristics <- tibble(
  
  agent_id = seq(1, no_agents, 1),
  opinion = rep(1, no_agents)
  
)

agent_characteristics$opinion <- runif(no_agents, 0, 1)

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

# OLD


distances_part <- sim$environment %>%
  activate(edges) %>%
  as_tibble() %>%
  mutate(from_two = from) %>%
  mutate(to_two = to) %>%
  mutate(from = to_two) %>%
  mutate(to = from_two) %>%
  select(from, to)

# create a distance table
sim$distances_table <- sim$environment %>%
  activate(edges) %>%
  as_tibble() %>%
  rbind(distances_part) %>%
  inner_join(sim$agent_characteristics, by=c("from" = "agent_id")) %>%
  mutate(opinion_from = opinion) %>%
  select(from, to, opinion_from)

sim$distances_table <- sim$distances_table %>%
  inner_join(sim$agent_characteristics, by=c("to" = "agent_id")) %>%
  mutate(opinion_to = opinion) %>% 
  select(from, to, opinion_from, opinion_to) %>%
  mutate(distance = opinion_from - opinion_to) %>%
  mutate(distance = abs(distance)) %>%
  mutate(within_epsilon = distance < params(sim)$hegselmann_krause$epsilon)

print(sim$distances_table)
print(sim$agent_characteristics) # NEXT STEP: DOUBLED COLUMNS NEED TO BE SINGULAR
# create number of all agents within epsilon in agent_characteristics as length of boolean vector within_epsilon
# compute new opinion vector
sim$agent_characteristics <- sim$agent_characteristics %>%
  inner_join(sim$distances_table, by=c("agent_id" = "from")) %>%
  group_by(agent_id) %>%
  mutate(no_within = sum(within_epsilon)) %>%
  mutate(opinion = sum(distance) / no_within)

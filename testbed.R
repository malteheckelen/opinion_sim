
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
  data.table() %>%
  .[, .(agent_id, neighborhood, nbh_size )] %>%
  merge(agent_characteristics, by.x = "agent_id", by.y = "agent_id")


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
  
) %>%  data.table()

messages <- environment %>%
  activate(edges) %>%
  as_tibble() %>%
  data.table() %>%
  setnames(old = c("from", "to"), new = c("to", "from"))

messages <- environment %>%
  activate(edges) %>%
  as_tibble() %>%
  data.table() %>%
  rbind(messages) %>%
  merge(agent_characteristics[, .(agent_id, opinion)], by.x = "from", by.y = "agent_id") %>%
  setnames("opinion", "opinion_from") %>%
  .[, assumption_to := vec_rnorm(1, .[, opinion_from], 0.1)] 


# make matrix of all possible optimized messages
message_matrix <- outer(messages$opinion_from, messages$assumption_to, produce_altered_message) # works
row.names(message_matrix) <- messages[, from]
colnames(message_matrix) <- messages[, to]

messages <- copy(message_matrix) %>% 
  data.table() %>% 
  .[, from := as.numeric(row.names(message_matrix))] %>% 
  melt( id.vars = c("from"),
        measure.vars = as.character(seq(1, no_agents, 1)),
        variable.name = "to",
        value.name = "opt_message" ) %>%
  .[, opt_message := mean(opt_message), by = .(from)] %>%
  .[ to != from] %>%
  setkey("from") %>%
  unique() %>%
  merge(unique(messages), by.x = c("from", "to"), by.y = c("from", "to"), allow.cartesian = TRUE) %>%
  .[, past_messages := sapply(opt_message, function(x) {list(x)} )] %>% 
  .[, past_opinions := sapply(opinion_from, function(x) {list(x)} )]

# Sending
actions_send <- copy(messages) %>% 
  merge(actions_send, by.x = c("from"), by.y = c("agent_id"), allow.cartesian = TRUE) %>%
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

for_receive_temp <- copy(actions_send[, .(from, actions, distance_to_past_messages)]) %>%
  setkey("from") %>%
  unique()

actions_send <- actions_send %>%
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

messages <- copy(actions_send) %>%
  merge(messages, by.x = c("agent_id"), by.y = c("from"), allow.cartesian = TRUE) %>%
  setnames("agent_id", "from") %>%
  setkey("from") %>%
  unique() %>%
  .[actions==best_action] %>%
  .[ , past_messages := ifelse(lengths(past_messages) < 3, 
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
  .[ , past_opinions := ifelse(lengths(past_opinions) < 3, 
                               ifelse(.[ , best_action] == "Unoptimized",
                                      mapply(function(x, y) {
                                        list(c(unlist(x), y))
                                      }, x=past_opinions, y=opinion_from),
                                      mapply(function(x, y) {
                                        list(c(unlist(x), y))
                                      }, x=past_opinions, y=opt_message)),
                               ifelse(.[ , best_action] == "Unoptimized",
                                      mapply(function(x, y) {
                                        list(c(unlist(x)[1:2], y))
                                      }, x=past_opinions, y=opinion_from),
                                      mapply(function(x, y) {
                                        list(c(unlist(x)[1:2], y))
                                      }, x=past_opinions, y=opt_message)
                               )
  )]

messages <- messages %>% setkey(to)
actions_send <- actions_send %>% setkey(agent_id)
messages <- copy(messages) %>%
  setnames(old=c("opinion_from", "opt_message"), new=c("opinion_from_y", "opt_message_y")) %>%
  unique() %>%
  setnames(old = c("from", "to"), new = c("to", "from")) %>% 
  .[, to := as.factor(to)] %>%
  .[, .(to, opinion_from_y, opt_message_y)] %>%
  .[messages, on="to", allow.cartesian=TRUE] %>% # WORKS
  #merge(unique(messages), by.x = c("to"), by.y = c("to"), allow.cartesian = TRUE) %>%
  setkey(from) %>%
  unique() %>%
  .[actions_send[, .(agent_id, best_action)]] %>%
  #merge(unique(actions_send[, .(agent_id, best_action)]), by.x = c("from"), by.y = c("agent_id")) %>%
  .[ , assumption_to := ifelse(.[, best_action] == "Unoptimized", opinion_from_y, message_y)] %>% 
  .[ , -c("opinion_from_y", "opt_message_y")] %>%
  setkey("from") %>%
  unique() 
 
# Receiving

agent_characteristics <- copy(messages) %>%
  merge(for_receive_temp, by=c("from"), allow.cartesian = TRUE) %>% 
  .[best_action == actions] %>% 
  .[ , .(from, distance_to_past_messages)] %>%
  .[ , from := as.factor(from)] %>%
  setnames("from", "to") %>%
  setkey("to") %>%
  unique() %>%
  .[messages] %>%
  merge(unique(messages), by.x = c("to"), by.y = c("to")) %>%
  .[ , within_epsilon := abs(opinion_from - assumption_to) < epsilon] %>%
  .[ , no_within := sum(within_epsilon), by=from] %>%
  .[ , doubt := (1 - distance_to_past_messages)] %>%
  .[ , opinion := ifelse( no_within != 0, mean(assumption_to * doubt) / no_within, opinion_from ) ] %>%
  setnames("opinion_from", "opinion") %>%
  .[ , .(from, opinion)] %>%
  setkey("from") %>%
  unique() %>%
  setnames("from", "agent_id") %>%
  merge(agent_characteristics[ , -c("opinion")], by=c("agent_id")) %>% print()

# prep for next step



# STEP

receiving_table <- receiving_table %>%
  select(from, to, assumption_to) %>%
  inner_join(agent_characteristics, by=c("from" = "agent_id")) %>% 
  select(from, to, opinion) %>%
  inner_join(message_metrics_table, by=c("to" = "from")) %>% 
  filter(actions == best_action) %>%
  mutate(assumption_to = message) %>%
  select(from, to, opinion, assumption_to) %>%
  mutate(opinion_from = opinion) %>%
  select(-opinion) %>%
  distinct() # works

### Distances

message_metrics_table <- environment %>%
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
  rbind(message_metrics_table) %>%
  inner_join(agent_characteristics, by=c("from" = "agent_id")) %>% 
  mutate(opinion_from = opinion) %>%
  select(from, to, opinion_from)

# make matrix of all possible optimized messages
message_matrix <- outer(receiving_table$opinion_from, receiving_table$assumption_to, produce_altered_message) # works
row.names(message_matrix) <- receiving_table$from
colnames(message_matrix) <- receiving_table$to

messages <- message_matrix %>% 
  as_tibble() %>% 
  mutate(from = as.numeric(row.names(message_matrix))) %>% 
  gather(to, message, "1":as.character(no_agents) ) %>%
  group_by(from) %>%
  mutate(message = mean(message)) %>%
  ungroup() %>%
  select(from, message) %>%
  distinct()

message_metrics_table <- messages %>% 
  inner_join(message_metrics_table, by=c("from" = "from")) %>%
  mutate(past_messages = sapply(opinion_from, function(x) { list(x) } )) %>%
  mutate(past_opinions = sapply(opinion_from, function(x) { list(x) } )) %>%
  inner_join(actions_send, by=c("from" = "agent_id")) %>%
  mutate(distance_to_past_messages = vec_distance_to_past(past_messages, opinion_from, message, actions)) %>%
  mutate(distance_to_past_opinions = vec_distance_to_past(past_opinions, opinion_from, message, actions)) %>% # works
  mutate(distance = vec_distance_switch(opinion_from, message, filter(receiving_table, from == from, to == to)$assumption_to, action=actions)) %>%
  group_by(from) %>%
  mutate(distance = mean(distance)) %>%
  ungroup() %>%
  mutate(distance_to_past_messages = abs(distance_to_past_messages)) %>%
  mutate(distance_to_past_opinions = abs(distance_to_past_opinions)) %>%
  mutate(distance = abs(distance)) # works

# Sending
actions_send <- message_metrics_table %>%
  mutate(util_score = distance - distance_to_past_messages - distance_to_past_opinions) %>%
  mutate(agent_id = from) %>%
  select(agent_id, actions, util_score) %>%
  distinct() %>%
  spread(actions, util_score) %>%
  mutate(best_action = ifelse(Optimized > Unoptimized, "Optimized", "Unoptimized")) %>%
  gather(actions, util_score, "Optimized":"Unoptimized")

message_metrics_table <- actions_send %>%
  mutate(to = agent_id) %>%
  select(to, best_action) %>%
  distinct() %>%
  inner_join(message_metrics_table) %>%
  mutate(assumption_to = vec_get_message(to, best_action)) %>%
  distinct() %>%
  mutate(within_epsilon = distance < epsilon) # works

# Receiving

agent_characteristics <- agent_characteristics %>%
  inner_join(message_metrics_table, by=c("agent_id" = "from")) %>%
  group_by(agent_id) %>%
  mutate(no_within = sum(within_epsilon)) %>%
  filter(actions == best_action) %>%
  mutate(doubt = 1 - distance_to_past_messages) %>%
  mutate(opinion = ifelse(no_within != 0, mean(assumption_to * doubt) / no_within, opinion)) %>%
  ungroup() %>%
  select(agent_id, opinion, neighborhood, nbh_size) %>%
  distinct()

test <- message_metrics_table %>%
  mutate(past_messages = ifelse(lengths(past_messages) < 3, vec_append_to_list(message, past_messages, FALSE), vec_append_to_list(message, past_messages, TRUE))) %>%
  mutate(past_opinions = ifelse(lengths(past_opinions) < 3, vec_append_to_list(opinion_from, past_opinions, FALSE), vec_append_to_list(opinion_from, past_opinions, TRUE)))

vec_c <- Vectorize(c)

# FUNCTIONS

# generate first assumed opinions (for Init)
produce_initial_assumption <- function(sender, receiver) {
  
  opinion <- agent_characteristics[ agent_id == sender][, opinion]
  
  sender_neighborhood <- unlist(agent_characteristics[ agent_id == sender ]$neighborhood)
  receiver_neighborhood <- unlist(agent_characteristics[ agent_id == receiver ]$neighborhood)
  
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
                      message_metrics_table %>%
                        select(from, opinion_from) %>%
                        filter(from==id) %>%
                        distinct() %>%
                        select(opinion_from) %>%
                        summarise(opinion_from = mean(opinion_from)) %>%
                        as.numeric()
                    },
                    "Optimized" = {
                      message_metrics_table %>%
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

vec_rnorm <- Vectorize(rnorm)

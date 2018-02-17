actions_overall <- tibble(
  
  agent_id = rep(agent_characteristics$agent_id, each=4),
  actions = rep(c("Send", "Receive", "Both", "Nothing"), no_agents),
  util_score = rep(0, length(actions))
  
) %>% data.table() %>%
  .[ actions == "Both", util_score := 1 ] %>%
  dcast(agent_id ~ actions, value.var = "util_score") %>% 
  .[ , agent_id := as.character(agent_id)] %>%
  .[, best_action :=  names(.[ , -c("agent_id")])[apply(.[ , -c("agent_id") ], 1, which.max)]] %>% 
  melt( id.vars = c("agent_id", "best_action"),
        measure.vars = c("Send", "Receive", "Both", "Nothing"),
        variable.name = "actions",
        value.name = "util_score" ) 
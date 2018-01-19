### Minimal Hegselmann Krause Model

# for simplicity and comparability, epsilon is taken to be a constant
# however, it is taken as a variable here, that could take input in the future
# if this model is sourced together with such a modification

# we will have an adjacency matrix


init_mods <- function() {
  
}

interact <- function(agent, epsilon) {
  
  ego <- agent
  
  within_epsilon <- numeric(length = no_agents)
  within_epsilon <- opinions$opinion %>%
    ifelse( abs( opinions[ ego[["id"]], ] - ) < epsilon, 1, 0) %>%
  sum()
  
  opinions[ ego[["id"]], ] <- (adjacency_matrix[ ego[["id"]], ] * opinions) %>%
    mean() / sum(within_epsilon)
  
}
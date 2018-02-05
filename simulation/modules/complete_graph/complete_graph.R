### SMALL-WORLD ENVIRONMENT FOR AGENTS

stopifnot(packageVersion("SpaDES") >= "1.2.0.9006")

defineModule(sim, list(
  name = "complete_graph",
  description = "Complete network environment generator for use with different models.",
  keywords = c("network", "graph", "environment", "complete"),
  childModules = character(),
  authors = c(person(c("Malte", "Lars"), "Heckelen", email = "malte.heckelen@ilw.uni-stuttgart.de", role = c("aut", "cre"))),
  version = numeric_version("0.0.1"),
  spatialExtent = raster::extent(rep(NA_real_, 4)),
  timeframe = as.POSIXlt(c(NA, NA)),
  timeunit = "hour",
  citation = list("citation.bib"),
  documentation = list("README.txt", "lattice.Rmd"),
  reqdPkgs = list("igraph", "tidygraph", "dplyr"),
  inputObjects = bind_rows(
    expectsInput("no_agents", "numeric", "The number of agents.")
  ),
  outputObjects = bind_rows(
    createsOutput("environment", "tbl_graph", "The network environment for the agents.")
  )
  )
)

doEvent.complete_graph <- function(sim, eventTime, eventType, debug = FALSE) {
  
  switch (
    eventType, 
    init = {
      
      # first instance
      complete_graph_Init(sim)

    }
  )
  
  return(invisible(sim))
}

complete_graph_Init <- function(sim) {
  
  sim$environment <- construct_environment(sim) %>%
    activate(nodes) %>%
    mutate(agent_id = sim$agent_characteristics$agent_id)
  
  sim$agent_characteristics <- sim$environment %>%
    activate(nodes) %>%
    mutate(neighborhood = local_members(mindist = 1)) %>%
    mutate(nbh_size = local_size(mindist = 1)) %>%
    as_tibble() %>%
    select(agent_id, neighborhood, nbh_size) %>%
    inner_join(sim$agent_characteristics)
  
}

construct_environment <- function(sim) {
  
  envir <- create_complete(sim$no_agents)
  
  return(envir)
  
}

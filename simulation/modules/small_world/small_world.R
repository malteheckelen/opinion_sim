### SMALL-WORLD ENVIRONMENT FOR AGENTS

stopifnot(packageVersion("SpaDES") >= "1.2.0.9006")

defineModule(sim, list(
  name = "small_world",
  description = "Small-world network environment generator for use with different models.",
  keywords = c("network", "graph", "environment", "small_world"),
  childModules = character(),
  authors = c(person(c("Malte", "Lars"), "Heckelen", email = "malte.heckelen@ilw.uni-stuttgart.de", role = c("aut", "cre"))),
  version = numeric_version("0.0.1"),
  spatialExtent = raster::extent(rep(NA_real_, 4)),
  timeframe = as.POSIXlt(c(NA, NA)),
  timeunit = "hour",
  citation = list("citation.bib"),
  documentation = list("README.txt", "lattice.Rmd"),
  reqdPkgs = list("igraph", "tidygraph", "dplyr"),
  parameters = bind_rows(
    # defineParameter("paramName", "paramClass", value, min, max, "parameter description"),
    defineParameter("dim", "numeric", 1, NA, NA, "The dimension of the starting lattice."),
    defineParameter("nbh_size", "numeric", 3, NA, NA, "The neighborhood size."),
    defineParameter("rewire_p", "numeric", 0.5, NA, NA, "The rewiring probability.")
  ),
  inputObjects = bind_rows(
    expectsInput("no_agents", "numeric", "The number of agents.")
  ),
  outputObjects = bind_rows(
    createsOutput("environment", "tbl_graph", "The network environment for the agents.")
  )
  )
)

doEvent.small_world <- function(sim, eventTime, eventType, debug = FALSE) {
  
  switch (
    eventType, 
    init = {
      
      # first instance
      small_world_Init(sim)

    }
  )
  
  return(invisible(sim))
}

small_world_Init <- function(sim) {
  
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
  
  envir <- play_smallworld(params(sim)$small_world$dim, sim$no_agents, params(sim)$small_world$nbh_size, params(sim)$small_world$rewire_p)
  
  return(envir)
  
}

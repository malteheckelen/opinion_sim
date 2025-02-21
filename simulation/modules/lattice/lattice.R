### LATTICE ENVIRONMENT FOR AGENTS

stopifnot(packageVersion("SpaDES") >= "1.2.0.9006")

defineModule(sim, list(
  name = "lattice",
  description = "Lattice network environment generator for use with different models.",
  keywords = c("network", "graph", "environment", "lattice"),
  childModules = character(),
  authors = c(person(c("Malte", "Lars"), "Heckelen", email = "malte.heckelen@ilw.uni-stuttgart.de", role = c("aut", "cre"))),
  version = numeric_version("0.0.1"),
  spatialExtent = raster::extent(rep(NA_real_, 4)),
  timeframe = as.POSIXlt(c(NA, NA)),
  timeunit = "hour",
  citation = list("citation.bib"),
  documentation = list("README.txt", "lattice.Rmd"),
  reqdPkgs = list("tidygraph"),
  parameters = rbind(
    # defineParameter("paramName", "paramClass", value, min, max, "parameter description"),
    defineParameter("directed", "logical", "FALSE", NA, NA, "Determines if the graph should be directed.")
  ),
  inputObjects = bind_rows(
    expectsInput("no_agents", "numeric", "The number of agents.")
  ),
  outputObjects = bind_rows(
    createsOutput("environment", "tbl_graph", "The network environment for the agents.")
  )
  )
)

doEvent.lattice <- function(sim, eventTime, eventType, debug = FALSE) {
  
  switch (
    eventType, 
    init = {
      
      # first instance
      Init(sim)

    }
  )
  
  return(invisible(sim))
}

Init <- function(sim) {
  
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
  
  directed <- params(sim)$lattice$directed
  
  dims <- c(floor(sqrt(sim$no_agents)), ceiling(sqrt(sim$no_agents)))
  envir <- create_lattice(dims, directed=directed)
  
  return(envir)
  
}

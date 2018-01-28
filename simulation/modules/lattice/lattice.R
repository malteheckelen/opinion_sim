### LATTICE ENVIRONMENT FOR AGENTS

stopifnot(packageVersion("SpaDES") >= "1.2.0.9006")

defineModule(sim, list(
  name = "lattice",
  description = "Lattice network environment generator for use with different.",
  keywords = c("network", "graph", "environment", "lattice"),
  childModules = character(),
  authors = c(person(c("Malte", "Lars"), "Heckelen", email = "malte.heckelen@ilw.uni-stuttgart.de", role = c("aut", "cre"))),
  version = numeric_version("0.0.1"),
  spatialExtent = raster::extent(rep(NA_real_, 4)),
  timeframe = as.POSIXlt(c(NA, NA)),
  timeunit = "tick",
  citation = list("citation.bib"),
  documentation = list("README.txt", "lattice.Rmd"),
  reqdPkgs = list("igraph", "tidygraph"),
  parameters = rbind(
    # defineParameter("paramName", "paramClass", value, min, max, "parameter description"),
    defineParameter("directed", "logical", FALSE, NA, NA, "Determines if the graph should be directed.")
  ),
  inputObjects = bind_rows(
    expectsInput("no_agents", "numeric", NA, NA, NA)
  ),
  outputObjects = bind_rows(
    createsOutput("environment", "tbl_graph", NA, NA, NA)
  )
  )
)

doEvent.lattice <- function(sim, eventTime, eventType, debug = FALSE) {
  
  switch (
    eventType, 
    init = {
      
      # first instance
      sim <- lattice_Init(sim)
      
      # future events (if necessary)
      
    }
  )
  
  return(invisible(sim))
}

lattice_Init <- function(sim) {
  
  construct_environment()
  
}

construct_environment <- function() {
  
  directed <- directed
    
  envir <- create_lattice(n=sim$no_agents, dim=2, directed=directed) %>%
    activate(nodes) %>%
    mutate(neighborhood = local_members(mindist = 1)) %>%
    mutate(nbh_size = local_size(mindist = 1))
  
  return(envir)
  
}
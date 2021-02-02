stopifnot(packageVersion("SpaDES") >= "1.2.0.9006")

defineModule(sim, list(
  name = "basic_setup",
  description = "Barebones setup for combination with the actual Bounded Confidence Model simulation modules.",
  keywords = c("opinion dynamics", "setup", "bounded confidence"),
  childModules = character(),
  authors = c(person(c("Malte", "Lars"), "Heckelen", email = "malte.heckelen@ilw.uni-stuttgart.de", role = c("aut", "cre"))),
  version = numeric_version("0.0.1"),
  spatialExtent = raster::extent(rep(NA_real_, 4)),
  timeframe = as.POSIXlt(c(NA, NA)),
  timeunit = "hour",
  citation = list("citation.bib"),
  documentation = list("README.txt", "data_collection.Rmd"),
  reqdPkgs = list(),
  parameters = bind_rows(
    # defineParameter("paramName", "paramClass", value, min, max, "parameter description"),
    defineParameter("no_agents", "numeric", 0.1, NA, NA, "The Bounded Confidence parameter.")
  ),
  outputObjects = bind_rows(
    createsOutput("no_agents", "numeric", "The number of agents."),
    createsOutput("agent_characteristics", "tbl_df", "The characteristics of each agent.")
  )
))

doEvent.basic_setup <- function(sim, eventTime, eventType, debug=FALSE) {
  switch(
    eventType,
    init = {
      sim <- Init(sim)
    }
  )
}

Init <- function(sim) {
  
  sim$no_agents <- params(sim)$basic_setup$no_agents
  
  sim$agent_characteristics <- tibble(
    
    agent_id = seq(1, sim$no_agents, 1),
    opinion = rep(runif(1), sim$no_agents)
  )
 
  return(invisible(sim))

}

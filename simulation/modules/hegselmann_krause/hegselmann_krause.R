### Minimal Hegselmann Krause Model

# for simplicity and comparability, epsilon is taken to be a constant
# however, it is taken as a variable here, that could take input in the future
# if this model is sourced together with such a modification

# we will have an adjacency matrix

# the parameters are set only with default values
stopifnot(packageVersion("SpaDES") >= "1.2.0.9006")

defineModule(sim, list(
  name = "hegselmann_krause",
  description = "Simulate bounded confidence opinion dynamics model according to Krause (1997) and Hegselmann / Krause (2000)",
  keywords = c("opinion dynamics", "hegselmann krause", "bounded confidence"),
  childModules = character(),
  authors = c(person(c("Malte", "Lars"), "Heckelen", email = "malte.heckelen@ilw.uni-stuttgart.de", role = c("aut", "cre"))),
  version = numeric_version("0.0.1"),
  spatialExtent = raster::extent(rep(NA_real_, 4)),
  timeframe = as.POSIXlt(c(NA, NA)),
  timeunit = "hour",
  citation = list("citation.bib"),
  documentation = list("README.txt", "hegselmann_krause.Rmd"),
  reqdPkgs = list("dplyr"),
  parameters = rbind(
    # defineParameter("paramName", "paramClass", value, min, max, "parameter description"),
    defineParameter("epsilon", "numeric", 0.1, NA, NA, "The Bounded Confidence parameter.")
    ),
  inputObjects = bind_rows(
    expectsInput("environment", "tbl_graph", NA, NA, NA)
  ),
  outputObjects = bind_rows(
    createsOutput("agent_characteristics", "tbl_graph", NA, NA, NA),
    createsOutput("distances_table", "tbl_df", NA, NA, NA)
  )
  )
)


doEvent.hegselmann_krause <- function(sim, eventTime, eventType, debug = FALSE) {
  switch(
    eventType,
    init = {
      ## do stuff for this event
      sim <- hegselmann_krauseInit(sim)
      
      ## schedule future event(s)
      sim <- scheduleEvent(sim, eventTime = start(sim), moduleName = "hegselmann_krause", eventType = "step")
    },
    step = {
      ## do stuff for this event
      sim <- hegselmann_krauseStep(sim)
      
      ## schedule future event(s)
      sim <- scheduleEvent(sim, eventTime = time(sim)+1, moduleName = "hegselmann_krause", eventType = "step")
    },
    warning(paste("Undefined event type: '", current(sim)[1, "eventType", with = FALSE],
                  "' in module '", current(sim)[1, "moduleName", with = FALSE], "'", sep = ""))
  )
  return(invisible(sim))
}

hegselmann_krauseInit <- function(sim) {
  # set within_epsilon column in opinions simulation attribute
  # reserve the Init in every of these simulations to place special attributes for modules

  return(invisible(sim))
}

hegselmann_krauseStep <- function(sim) {
  
  # create a distance table
  sim$distances_table <- sim$environment %>%
    activate(edges) %>%
    as.tibble() %>%
    mutate(distance = abs(filter(sim$agent_characteristics, agent_id == from)$opinion - filter(sim$agent_characteristics, agent_id == to)$opinion)) %>%
    mutate(within_epsilon = distance < epsilon)
  
  # create number of all agents within epsilon in agent_characteristics as length of boolean vector within_epsilon
  # compute new opinion vector
  sim$agent_characteristics <- sim$agent_characteristics %>%
    mutate(no_within = length(
      filter(sim$distances_table, from == agent_id)$within_epsilon
    )) %>%
    mutate(opinion = sum(filter(sim$agent_characteristics, agent_id == from)$distance) / no_within)
  
}

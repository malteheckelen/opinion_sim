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
  timeunit = "tick",
  citation = list("citation.bib"),
  documentation = list("README.txt", "hegselmann_krause.Rmd"),
  reqdPkgs = list("dplyr"),
  parameters = rbind(
    # defineParameter("paramName", "paramClass", value, min, max, "parameter description"),
    defineParameter("epsilon", "numeric", 0.1, NA, NA, "The Bounded Confidence parameter.")
    ),
  inputObjects = bind_rows(
    expectsInput("environment", "tbl_graph", NA, NA, NA)
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
      sim <- scheduleEvent(sim, eventTime = start(sim), moduleName = "hegselmann_krause", eventType = "Step")
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
  
  # get distances to all neighbors on edge table as well as logical for within_epsilon
  sim$environment <- sim$environment %>%
    activate(edges) %>%
    mutate(distance = abs(sim$agent_characteristics$opinion[sim$agent_characteristics$opinions$agent == from] - sim$agent_characteristics$opinions[sim$agent_characteristics$opinions$agent == to])) %>%
    mutate(within = distance < sim$epsilon)
  
  # create number of all agents within epsilon
  sim$environment <- sim$environment %>%
    activate(nodes) %>%
    mutate(within_epsilon = length(sim$environment$
    
    
}

get_epsilon <- function(sim) {
  
  difference <- sim$environment %>%
    activate(edges) %>% 
    
  
}

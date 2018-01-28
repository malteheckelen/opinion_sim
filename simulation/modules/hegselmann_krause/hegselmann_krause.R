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
  inputObjects = data.frame(
    objectName = c("adjacency_matrix"),
    objectClass = c("dgCMatrix"),
    sourceURL  =  c(NA_character_, NA_character_),
    other = rep(NA_character_, 2L), stringsAsFactors = FALSE),
  outputObjects = data.frame(
    objectName = c("caribou", "caribouRas", "glmPlot", "glmPVals"),
    objectClass = c("SpatialPointsDataFrame", "RasterLayer", "gg", "numeric"),
    other = rep(NA_character_, 4L), stringsAsFactors = FALSE)
))

doEvent.hegselmann_krause <- function(sim, eventTime, eventType, debug = FALSE) {
  switch(
    eventType,
    init = {
      ## do stuff for this event
      sim <- hegselmann_krauseInit(sim)
      
      ## schedule future event(s)
      sim <- scheduleEvent(sim, eventTime = start(sim), moduleName = "hegselmann_krause", eventType = "Step")
    },
    Step = {
      ## do stuff for this event
      sim <- hegselmann_krauseStep(sim)
      
      ## schedule future event(s)
      sim <- scheduleEvent(sim, eventTime = time(sim)+1, moduleName = "hegselmann_krause", eventType = "Step")
    },
    warning(paste("Undefined event type: '", current(sim)[1, "eventType", with = FALSE],
                  "' in module '", current(sim)[1, "moduleName", with = FALSE], "'", sep = ""))
  )
  return(invisible(sim))
}

hegselmann_krauseInit <- function(sim) {
  # set within_epsilon column in opinions simulation attribute
  # reserve the Init in ever of these simulations to place special

  sim$agent_characteristics$opinions <- sim$agent_characteristics$opinions %>% mutate(within_epsilon = 0)
  
  return(invisible(sim))
}

hegselmann_krauseStep <- function(sim) {
  
  # this function successively narrows the "eligible" opinions down
  
  # distance matrix: computed the same way in every one of these models
  distance_matrix <- matrix(nrow=sim$no_agents, ncol=sim$no_agents) # pre-allocate
  distance_matrix <- outer(sim$agent_characteristics$opinions, sim$agent_characteristics$opinions, FUN="-") # outer product
  
  # neighbor distances; retains only distances to neighbors (all others 0)
  neighbor_distance_matrix <- matrix(nrow=sim$no_agents, ncol=sim$no_agents) # pre-allocate
  neighbor_distance_matrix <- sim$adjacency_matrix * distance_matrix
    
  # boolean matrix of "eligible" opinions of neighbors according to distance criterion: only for some models
  within_epsilon_matrix <- matrix(nrow=sim$no_agents, ncol=sim$no_agents) # pre-allocate
  within_epsilon_matrix <- neighbor_distance_matrix > epsilon
  
  # opinion updating
  sim$agent_characteristics$opinions <- apply(within_epsilon_matrix, 1, function(x) { 
    sum(sim$agent_characteristics$opinions[x]) / length(x) # hk updating rule: mean of opinions of eligible neigbors
  })
      
}

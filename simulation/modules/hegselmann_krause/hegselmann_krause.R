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
  authors = c(person(c("Malte", "Lars"), "Heckelen", email = "malte.heckelen@ilw.uni-stuttgar.de", role = c("aut", "cre"))),
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
    objectName = c("adjacency_matrix", "epsilon"),
    objectClass = c("dgCMatrix", "numeric"),
    sourceURL  =  c(NA_character_, NA_character_),
    other = rep(NA_character_, 2L), stringsAsFactors = FALSE),
  outputObjects = data.frame(
    objectName = c("caribou", "caribouRas", "glmPlot", "glmPVals"),
    objectClass = c("SpatialPointsDataFrame", "RasterLayer", "gg", "numeric"),
    other = rep(NA_character_, 4L), stringsAsFactors = FALSE)
))

doEvent.hegselmann_krause = function(sim, eventTime, eventType, debug = FALSE) {
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

  sim$opinions <- sim$opinions %>% mutate(within_epsilon = 0)
  
  return(invisible(sim))
}

hegselmann_krauseStep <- function(sim) {
  
  distance_matrix <- matrix(length=no_agents^2, ncol=2)
  
  apply(seq(1, no_agents, 1), function(x) { apply(seq(1, no_agents, 1), function(x) {
    
    distance_matrix[x,y] <- sim$opinions$opinion
    
    
  })
  })
  
  opinions_temp <- as.numeric(sim$opinions$opinions)
  
  distance_matrix <- 
  sim$opinions$within_epsilon <- sim$opinions %>%
    mutate(ifelse( abs( sim$opinions[ ego[["id"]], ] - opinions) < sim$epsilon, 1, 0))
  
  sim$opinions <- (sim$adjacency_matrix[ ego[["id"]], ] * sim$opinions) %>%
    mean() / sum(sim$opinions$within_epsilon)
  
}


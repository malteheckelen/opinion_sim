stopifnot(packageVersion("SpaDES") >= "1.2.0.9006")

defineModule(sim, list(
  name = "data_collection",
  description = "Collects data on the opinion distributions of the Bounded Confidence models. Data on different model runs is collected via the module metadata_collection.",
  keywords = c("opinion dynamics", "data collection", "bounded confidence"),
  childModules = character(),
  authors = c(person(c("Malte", "Lars"), "Heckelen", email = "malte.heckelen@ilw.uni-stuttgart.de", role = c("aut", "cre"))),
  version = numeric_version("0.0.1"),
  spatialExtent = raster::extent(rep(NA_real_, 4)),
  timeframe = as.POSIXlt(c(NA, NA)),
  timeunit = "hour",
  citation = list("citation.bib"),
  documentation = list("README.txt", "data.collection.Rmd"),
  reqdPkgs = list(),
  inputObjects = bind_rows(
    expectsInput("agent_characteristics", "data.table", "The characteristics of each agent.")
  )
))

doEvent.data_collection <- function(sim, eventTime, eventType, debug=FALSE) {
  switch(
    eventType,
    init = {
      ## do stuff for this event
      sim <- Init(sim)
      
      ## schedule future event(s)
      sim <- scheduleEvent(sim, eventTime = start(sim), moduleName = "data_collection", eventType = "step")
    },
    step = {
      
      ## do stuff for this event
      sim <- Step(sim)
      
      ## schedule future event(s)
      sim <- scheduleEvent(sim, eventTime = time(sim)+1, moduleName = "data_collection", eventType = "step")
      
    }
  )
}

Init <- function(sim) {
  
  sim$data_collect <- tibble(
    	time = time(sim),
  epsilon = params(sim)$hegselmann_krause$epsilon,
        no_agents = params(sim)$basic_setup$no_agents, 
    	agent_id = sim$agent_characteristics[ , agent_id],
    	opinions = sim$agent_characteristics[ , opinion]
  	) %>% 
     	data.table()
   
  return(invisible(sim))
  
}

Step <- function(sim) {
	
  temp <- tibble(
    	time = time(sim),
  epsilon = params(sim)$hegselmann_krause$epsilon,
        no_agents = params(sim)$basic_setup$no_agents, 
    	agent_id = sim$agent_characteristics$agent_id,
    	opinions = sim$agent_characteristics$opinion
  	) %>% 
     	data.table()
   
  sim$data_collect <- temp %>%
    rbind(sim$data_collect)
  
  return(invisible(sim))
    
}
    

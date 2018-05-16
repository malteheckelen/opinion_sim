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
  reqdPkgs = list("dplyr"),
  inputObjects = bind_rows(
    expectsInput("agent_characteristics", "data.table", "The characteristics of each agent.")
  )
))

doEvent.data_collection <- function(sim, eventTime, eventType, debug=FALSE) {
  switch(
    eventType,
    init = {
      ## do stuff for this event
      sim <- data_collectionInit(sim)
      
      ## schedule future event(s)
      sim <- scheduleEvent(sim, eventTime = start(sim), moduleName = "data_collection", eventType = "step")
    },
    step = {
      
      ## do stuff for this event
      sim <- data_collectionStep(sim)
      
      ## schedule future event(s)
      sim <- scheduleEvent(sim, eventTime = time(sim)+1, moduleName = "data_collection", eventType = "step")
      
    }
  )
}

data_collectionInit <- function(sim) {
  
	
  if ( ("rc_energy_model" %in% modules) | ("rc_model" %in% modules) ) {
          no_Unoptimized <-  sim$chosen_actions[best_action == "Unoptimized"] %>% nrow
          no_Optimized <-  sim$chosen_actions[best_action == "Optimized"] %>% nrow
          
	  if ("rc_model" %in% modules) {
               
	       sim$data_collect <- tibble(
              
               time = time(sim),
               agent_id = sim$agent_characteristics[ , agent_id],
               opinions = sim$agent_characteristics[ , opinion],
	       no_Unoptimized = no_Unoptimized,
	       no_Optimized = no_Optimized
	       
               ) %>% 
               data.table() 

	  }

	  if ("rc_energy_model" %in% modules) {
		  
               no_Both <-  sim$chosen_actions[best_action == "Both"] %>% nrow
               no_Send <-  sim$chosen_actions[best_action == "Send"] %>% nrow
               no_Receive <-  sim$chosen_actions[best_action == "Receive"] %>% nrow
               no_Nothing <-  sim$chosen_actions[best_action == "Nothing"] %>% nrow

	       sim$data_collect <- tibble(
              
               time = time(sim),
               agent_id = sim$agent_characteristics[ , agent_id],
               opinions = sim$agent_characteristics[ , opinion],
	       no_Unoptimized = no_Unoptimized,
	       no_Optimized = no_Optimized,
	       no_Both = no_Both,
	       no_Send = no_Send,
	       no_Receive = no_Receive,
	       no_Nothing = no_Nothing
	       
               ) %>% 
               data.table() 
	  }

  
  } else {

	sim$data_collect <- tibble(
    	agent_id = sim$agent_characteristics[ , agent_id],
    	opinions = sim$agent_characteristics[ , opinion],
    	time = time(sim)
  	) %>% 
     	data.table()
   
  }

  return(invisible(sim))

}

data_collectionStep <- function(sim) {

     if ( ("rc_energy_model" %in% modules) | ("rc_model" %in% modules) ) {
          no_Unoptimized <-  sim$chosen_actions[best_action == "Unoptimized"] %>% nrow
          no_Optimized <-  sim$chosen_actions[best_action == "Optimized"] %>% nrow
          
	  if ("rc_model" %in% modules) {
               
	       temp <- tibble(
              
               time = time(sim),
               agent_id = sim$agent_characteristics[ , agent_id],
               opinions = sim$agent_characteristics[ , opinion],
	       no_Unoptimized = no_Unoptimized,
	       no_Optimized = no_Optimized
	       
               ) %>% 
               data.table() 

	  }

	  if ("rc_energy_model" %in% modules) {
		  
               no_Both <-  sim$chosen_actions[best_action == "Both"] %>% nrow
               no_Send <-  sim$chosen_actions[best_action == "Send"] %>% nrow
               no_Receive <-  sim$chosen_actions[best_action == "Receive"] %>% nrow
               no_Nothing <-  sim$chosen_actions[best_action == "Nothing"] %>% nrow

	       temp <- tibble(
              
               time = time(sim),
               agent_id = sim$agent_characteristics[ , agent_id],
               opinions = sim$agent_characteristics[ , opinion],
	       no_Unoptimized = no_Unoptimized,
	       no_Optimized = no_Optimized,
	       no_Both = no_Both,
	       no_Send = no_Send,
	       no_Receive = no_Receive,
	       no_Nothing = no_Nothing
	       
               ) %>% 
               data.table() 
	  }

  
  } else {

	temp <- tibble(
    	agent_id = sim$agent_characteristics[ , agent_id],
    	opinions = sim$agent_characteristics[ , opinion],
    	time = time(sim)
  	) %>% 
     	data.table()
   
  }
  
  sim$data_collect <- temp %>%
    rbind(sim$data_collect)
  
  return(invisible(sim))
  
}
    

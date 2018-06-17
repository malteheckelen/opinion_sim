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
  
	
  if ( ("rc_energy_model" %in% modules) | ("rc_model" %in% modules) | ("rc_sh_model" %in% modules) ) {
          no_Unoptimized <-  sim$chosen_actions[best_action == "Unoptimized"] %>% nrow
          no_Optimized <-  sim$chosen_actions[best_action == "Optimized"] %>% nrow
          
	  if ("rc_model" %in% modules) {
   
	       sim$data_collect <- tibble(
              
               time = time(sim),
               mu_opinion_distribution = params(sim)$basic_setup$mu_opinion_distribution ,
               sigma_opinion_distribution = params(sim)$basic_setup$sigma_opinion_distribution,
               no_agents = params(sim)$basic_setup$no_agents, 
               no.cores = params(sim)$basic_setup$no.cores, 
	       opinion_homophily = params(sim)$small_world$opinion_homophily,
               epsilon = params(sim)$rc_model$epsilon,
               other_incons_tolerance = params(sim)$rc_model$other_incons_tolerance,
               self_incons_tolerance = params(sim)$rc_model$self_incons_tolerance,
               memory = params(sim)$rc_model$memory, 
               initial_opinion_confidence = params(sim)$rc_model$initial_opinion_confidence, 
               agent_id = sim$agent_characteristics[ , agent_id],
               opinions = sim$agent_characteristics[ , opinion],
	       action_send = sim$actions_send[ , best_action ],
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
               mu_opinion_distribution = params(sim)$basic_setup$mu_opinion_distribution ,
               sigma_opinion_distribution = params(sim)$basic_setup$sigma_opinion_distribution,
               no_agents = params(sim)$basic_setup$no_agents, 
               no.cores = params(sim)$basic_setup$no.cores, 
	       opinion_homophily = params(sim)$small_world$opinion_homophily,
               epsilon = params(sim)$rc_energy_model$epsilon,
               other_incons_tolerance = params(sim)$rc_energy_model$other_incons_tolerance,
               self_incons_tolerance = params(sim)$rc_energy_model$self_incons_tolerance,
               memory = params(sim)$rc_energy_model$memory, 
               initial_opinion_confidence = params(sim)$rc_energy_model$initial_opinion_confidence, 
               energy_level = params(sim)$rc_energy_model$energy_level,
               restoration_factor = params(sim)$rc_energy_model$restoration_factor,
               agent_id = sim$agent_characteristics[ , agent_id],
               opinions = sim$agent_characteristics[ , opinion],
	       action_overall = sim$actions_overall[ , best_action ],
	       #action_send = sim$actions_send[ , best_action ],
	       no_Unoptimized = no_Unoptimized,
	       no_Optimized = no_Optimized,
	       no_Both = no_Both,
	       no_Send = no_Send,
	       no_Receive = no_Receive,
	       no_Nothing = no_Nothing
	       
               ) %>% 
               data.table() 
	  }

	  if ("rc_sh_model" %in% modules) {
		  
               no_Both <-  sim$chosen_actions[best_action == "Both"] %>% nrow
               no_Send <-  sim$chosen_actions[best_action == "Send"] %>% nrow
               no_Receive <-  sim$chosen_actions[best_action == "Receive"] %>% nrow
               no_Nothing <-  sim$chosen_actions[best_action == "Nothing"] %>% nrow

               no_Unoptimized_appeal <-  sim$chosen_actions[best_action == "Unoptimized_appeal"] %>% nrow
               no_Optimized_appeal <-  sim$chosen_actions[best_action == "Optimized_appeal"] %>% nrow
               no_Heuristic <-  sim$chosen_actions[best_action == "Heuristic"] %>% nrow
               no_Systematic <-  sim$chosen_actions[best_action == "Systematic"] %>% nrow
              
	       sim$data_collect <- tibble(

               time = time(sim),
               mu_opinion_distribution = params(sim)$basic_setup$mu_opinion_distribution ,
               sigma_opinion_distribution = params(sim)$basic_setup$sigma_opinion_distribution,
               no_agents = params(sim)$basic_setup$no_agents, 
               no.cores = params(sim)$basic_setup$no.cores, 
               epsilon = params(sim)$rc_sh_model$epsilon,
	       opinion_homophily = params(sim)$small_world$opinion_homophily,
               other_incons_tolerance = params(sim)$rc_sh_model$other_incons_tolerance,
               self_incons_tolerance = params(sim)$rc_sh_model$self_incons_tolerance,
               memory = params(sim)$rc_sh_model$memory, 
               initial_opinion_confidence = params(sim)$rc_sh_model$initial_opinion_confidence, 
               energy_level = params(sim)$rc_sh_model$energy_level,
               restoration_factor = params(sim)$rc_sh_model$restoration_factor,
               no_groups = params(sim)$rc_sh_model$no_groups,
               expert_percentage = params(sim)$rc_sh_model$expert_percentage,
               sigma_complexity = params(sim)$rc_sh_model$sigma_complexity,
               agent_id = sim$agent_characteristics[ , agent_id],
               opinions = sim$agent_characteristics[ , opinion],
	       actions_overall = sim$actions_overall[ , best_action ],
	       #action_send = sim$actions_send[ , best_action ],
	       #action_receive = sim$actions_receive[ , best_action ],
	       no_Unoptimized = no_Unoptimized,
	       no_Optimized = no_Optimized,
	       no_Both = no_Both,
	       no_Send = no_Send,
	       no_Receive = no_Receive,
	       no_Nothing = no_Nothing,
               no_Unoptimized_appeal = no_Unoptimized_appeal,
               no_Optimized_appeal = no_Optimized_appeal,
               no_Heuristic = no_Heuristic, 
               no_Systematic = no_Systematic
	       
               ) %>% 
               data.table() 
	  }

  
  } else {

	sim$data_collect <- tibble(
    	time = time(sim),
        mu_opinion_distribution = params(sim)$basic_setup$mu_opinion_distribution ,
        sigma_opinion_distribution = params(sim)$basic_setup$sigma_opinion_distribution,
	opinion_homophily = params(sim)$small_world$opinion_homophily,
	epsilon = params(sim)$hegselmann_krause$epsilon,
        no_agents = params(sim)$basic_setup$no_agents, 
        no.cores = params(sim)$basic_setup$no.cores, 
    	agent_id = sim$agent_characteristics[ , agent_id],
    	opinions = sim$agent_characteristics[ , opinion]
  	) %>% 
     	data.table()
   
  }

  return(invisible(sim))

}

data_collectionStep <- function(sim) {
	
  if ( ("rc_energy_model" %in% modules) | ("rc_model" %in% modules) | ("rc_sh_model" %in% modules) ) {
          no_Unoptimized <-  sim$chosen_actions[best_action == "Unoptimized"] %>% nrow
          no_Optimized <-  sim$chosen_actions[best_action == "Optimized"] %>% nrow
          
	  if ("rc_model" %in% modules) {
   
	       temp <- tibble(
              
               time = time(sim),
               mu_opinion_distribution = params(sim)$basic_setup$mu_opinion_distribution ,
               sigma_opinion_distribution = params(sim)$basic_setup$sigma_opinion_distribution,
	       opinion_homophily = params(sim)$small_world$opinion_homophily,
               no_agents = params(sim)$basic_setup$no_agents, 
               no.cores = params(sim)$basic_setup$no.cores, 
               epsilon = params(sim)$rc_model$epsilon,
               other_incons_tolerance = params(sim)$rc_model$other_incons_tolerance,
               self_incons_tolerance = params(sim)$rc_model$self_incons_tolerance,
               memory = params(sim)$rc_model$memory, 
               initial_opinion_confidence = params(sim)$rc_model$initial_opinion_confidence, 
               agent_id = sim$agent_characteristics[ , agent_id],
               opinions = sim$agent_characteristics[ , opinion],
	       action_send = sim$actions_send[ , best_action ],
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
               mu_opinion_distribution = params(sim)$basic_setup$mu_opinion_distribution ,
               sigma_opinion_distribution = params(sim)$basic_setup$sigma_opinion_distribution,
               no_agents = params(sim)$basic_setup$no_agents, 
               no.cores = params(sim)$basic_setup$no.cores, 
	       opinion_homophily = params(sim)$small_world$opinion_homophily,
               epsilon = params(sim)$rc_energy_model$epsilon,
               other_incons_tolerance = params(sim)$rc_energy_model$other_incons_tolerance,
               self_incons_tolerance = params(sim)$rc_energy_model$self_incons_tolerance,
               memory = params(sim)$rc_energy_model$memory, 
               initial_opinion_confidence = params(sim)$rc_energy_model$initial_opinion_confidence, 
               energy_level = params(sim)$rc_energy_model$energy_level,
               restoration_factor = params(sim)$rc_energy_model$restoration_factor,
               agent_id = sim$agent_characteristics[ , agent_id],
               opinions = sim$agent_characteristics[ , opinion],
	       action_overall = sim$actions_overall[ , best_action ],
	       #action_send = sim$actions_send[ , best_action ],
	       no_Unoptimized = no_Unoptimized,
	       no_Optimized = no_Optimized,
	       no_Both = no_Both,
	       no_Send = no_Send,
	       no_Receive = no_Receive,
	       no_Nothing = no_Nothing
	       
               ) %>% 
               data.table() 
	  }

	  if ("rc_sh_model" %in% modules) {
		  
               no_Both <-  sim$chosen_actions[best_action == "Both"] %>% nrow
               no_Send <-  sim$chosen_actions[best_action == "Send"] %>% nrow
               no_Receive <-  sim$chosen_actions[best_action == "Receive"] %>% nrow
               no_Nothing <-  sim$chosen_actions[best_action == "Nothing"] %>% nrow

               no_Unoptimized_appeal <-  sim$chosen_actions[best_action == "Unoptimized_appeal"] %>% nrow
               no_Optimized_appeal <-  sim$chosen_actions[best_action == "Optimized_appeal"] %>% nrow
               no_Heuristic <-  sim$chosen_actions[best_action == "Heuristic"] %>% nrow
               no_Systematic <-  sim$chosen_actions[best_action == "Systematic"] %>% nrow
              
	       temp <- tibble(

               time = time(sim),
               mu_opinion_distribution = params(sim)$basic_setup$mu_opinion_distribution ,
               sigma_opinion_distribution = params(sim)$basic_setup$sigma_opinion_distribution,
               no_agents = params(sim)$basic_setup$no_agents, 
               no.cores = params(sim)$basic_setup$no.cores, 
	       opinion_homophily = params(sim)$small_world$opinion_homophily,
               epsilon = params(sim)$rc_sh_model$epsilon,
               other_incons_tolerance = params(sim)$rc_sh_model$other_incons_tolerance,
               self_incons_tolerance = params(sim)$rc_sh_model$self_incons_tolerance,
               memory = params(sim)$rc_sh_model$memory, 
               initial_opinion_confidence = params(sim)$rc_sh_model$initial_opinion_confidence, 
               energy_level = params(sim)$rc_sh_model$energy_level,
               restoration_factor = params(sim)$rc_sh_model$restoration_factor,
               no_groups = params(sim)$rc_sh_model$no_groups,
               expert_percentage = params(sim)$rc_sh_model$expert_percentage,
               sigma_complexity = params(sim)$rc_sh_model$sigma_complexity,
               agent_id = sim$agent_characteristics[ , agent_id],
               opinions = sim$agent_characteristics[ , opinion],
	       actions_overall = sim$actions_overall[ , best_action ],
	       #action_send = sim$actions_send[ , best_action ],
	       #action_receive = sim$actions_receive[ , best_action ],
	       no_Unoptimized = no_Unoptimized,
	       no_Optimized = no_Optimized,
	       no_Both = no_Both,
	       no_Send = no_Send,
	       no_Receive = no_Receive,
	       no_Nothing = no_Nothing,
               no_Unoptimized_appeal = no_Unoptimized_appeal,
               no_Optimized_appeal = no_Optimized_appeal,
               no_Heuristic = no_Heuristic, 
               no_Systematic = no_Systematic
	       
               ) %>% 
               data.table() 
	  }

  
  } else {

	temp <- tibble(
    	time = time(sim),
        mu_opinion_distribution = params(sim)$basic_setup$mu_opinion_distribution ,
        sigma_opinion_distribution = params(sim)$basic_setup$sigma_opinion_distribution,
	opinion_homophily = params(sim)$small_world$opinion_homophily,
	epsilon = params(sim)$hegselmann_krause$epsilon,
        no_agents = params(sim)$basic_setup$no_agents, 
        no.cores = params(sim)$basic_setup$no.cores, 
    	agent_id = sim$agent_characteristics$agent_id,
    	opinions = sim$agent_characteristics$opinion
  	) %>% 
     	data.table()
   
  }

  sim$data_collect <- temp %>%
    rbind(sim$data_collect)
  
  return(invisible(sim))
  
}
    

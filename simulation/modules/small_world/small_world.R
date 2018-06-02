### SMALL-WORLD ENVIRONMENT FOR AGENTS

stopifnot(packageVersion("SpaDES") >= "1.2.0.9006")

defineModule(sim, list(
  name = "small_world",
  description = "Small-world network environment generator for use with different models.",
  keywords = c("network", "graph", "environment", "small_world"),
  childModules = character(),
  authors = c(person(c("Malte", "Lars"), "Heckelen", email = "malte.heckelen@ilw.uni-stuttgart.de", role = c("aut", "cre"))),
  version = numeric_version("0.0.1"),
  spatialExtent = raster::extent(rep(NA_real_, 4)),
  timeframe = as.POSIXlt(c(NA, NA)),
  timeunit = "hour",
  citation = list("citation.bib"),
  documentation = list("README.txt", "lattice.Rmd"),
  reqdPkgs = list("igraph", "tidygraph", "dplyr", "data.table"),
  parameters = bind_rows(
    # defineParameter("paramName", "paramClass", value, min, max, "parameter description"),
    defineParameter("dim", "numeric", 1, NA, NA, "The dimension of the starting lattice."),
    defineParameter("nbh_size", "numeric", 3, NA, NA, "The neighborhood size."),
    defineParameter("rewire_p", "numeric", 0.5, NA, NA, "The rewiring probability.")
  ),
  inputObjects = bind_rows(
    expectsInput("no_agents", "numeric", "The number of agents.")
  ),
  outputObjects = bind_rows(
    createsOutput("environment", "tbl_graph", "The network environment for the agents.")
  )
  )
)

doEvent.small_world <- function(sim, eventTime, eventType, debug = FALSE) {
  
  switch (
    eventType, 
    init = {
      
      # first instance
      small_world_Init(sim)

    }
  )
  
  return(invisible(sim))
}

small_world_Init <- function(sim) {
  
  sim$environment <- construct_environment(sim) %>%
    activate(nodes) %>%
    mutate(agent_id = sim$agent_characteristics$agent_id)
  
  sim$agent_characteristics <- sim$environment %>%
    activate(nodes) %>%
    mutate(neighborhood = local_members(mindist = 1)) %>%
    mutate(nbh_size = local_size(mindist = 1)) %>%
    as_tibble() %>%
    data.table() %>%
    .[, .(agent_id, neighborhood, nbh_size )] %>%
    .[copy(data.table(agent_characteristics)), on = c("agent_id")]

  distribute_opinions(params(sim)$small_world$opinion_homophily)

  start_config <<- sim$agent_characteristics
  
}

construct_environment <- function(sim) {
  
  envir <- play_smallworld(params(sim)$small_world$dim, sim$no_agents, params(sim)$small_world$nbh_size, params(sim)$small_world$rewire_p)
  
  return(envir)
  
}

distribute_opinions <- function(opinion_homophily) {

	# 1. Randomly choose an agent
	# 2. Give him an opinion determined by mean opinion
	# 3. For all neighbors of this agent: Assign a randomly chosen opinion that lies within interval +/- Homophily Score of this agents opinion
	# 4. For all neighbors of this agent: Repeat 4
       
        for (i in 1:nrow(agent_characteristics)) {

		if (i == 1) {
                           
			available_opinions <- sort(sim$agent_characteristics$opinion)
			available_opinionawdawds <<- sort(sim$agent_characteristics$opinion)

			next_agent <- sample(c(1:sim$no_agents), size=1)
			opinion_id <- floor(length(available_opinions)/2)
			starting_agent_opinion <- available_opinions[opinion_id]
			sim$agent_characteristics[ agent_id == next_agent  , opinion := starting_agent_opinion ]
			available_opinions <- available_opinions[ -opinion_id ]

			next_list <- unlist(sim$agent_characteristics[ agent_id == next_agent ]$neighbors)

		} else {

			next_agent <- next_list[1]

		}
        
	for (focus_i in 1:length(unlist(sim$agent_characteristics[ agent_id == next_agent ]$neighborhood))) {
                
		focus_agent <- unlist(sim$agent_characteristics[ agent_id == next_agent ]$neighborhood)[focus_i]

		nbh_list <- unlist(sim$agent_characteristics[ agent_id == focus_agent ]$neighborhood)

		next_list <- append(next_list, nbh_list[!(nbh_list %in% next_list)])

		if (is.na(sim$agent_characteristics[ agent_id == focus_agent ]$opinion)) {
                        
			opinions_in_neighborhood <- c(sim$agent_characteristics[ agent_id == starting_agent , opinion ])

			for (fagent_neighbors_i in 1:length(unlist(sim$agent_characteristics[ agent_id == focus_agent ]$neighborhood))) {

				fagent_neighbor <- sim$agent_characteristics[ agent_id == focus_agent ]$neighborhood[fagent_neighbors_i]
				
				if (!is.na(sim$agent_characteristics[ agent_id == fagent_neighbor ]$opinion)) {

					opinions_in_neighborhood <- c(opinions_in_neighborhood, sim$agent_characteristics[ agent_id == fagent_neighbor ]$opinion)

				}

			}

			temp <- data.table(
                                
				index = c(1:length(available_opinions)),
				avail_ops = available_opinions,
				distance = rep(0, length(available_opinions)),
				within_homo = rep(FALSE, length(available_opinions))

				)
				
			opinion_bounds <- c( min(opinions_in_neighborhood)+opinion_homophily, max(opinions_in_neighborhood)-opinion_homophily ) 

			temp <- temp[ , within_homo := ( avail_ops < opinion_bounds[1] & avail_ops > opinion_bounds[2] ) ]
			temp <- temp[ , distance := abs(median(opinions_in_neighborhood) - avail_ops ) ]

			within_homo_indices <- temp[ within_homo == TRUE ]$index
			minimum_distance_index <- temp[ distance == min(distance) ]$index[1]

			if (opinion_bounds[1] <= opinion_bounds[2]) {

				sim$agent_characteristics[ agent_id = focus_agent , opinion := temp[ minimum_distance_index , ]$avail_ops ] 

				available_opinions <- available_opinions[ -minimum_distance_index ]


			} else {
                                
				chosen_index <- sample(within_homo_indices, 1)
				sim$agent_characteristics[ agent_id = focus_agent , opinion := temp[ chosen_index, ]$avail_ops ]

				available_opinions <- available_opinions[ -chosen_index ]

			}

		}

	}

	}

}

	

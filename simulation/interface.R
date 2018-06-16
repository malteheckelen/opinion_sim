### Interface

setwd('/home/malte/GitHub/R_MaDisBe/simulation')
#setwd('~/GitHub/R_MaDisBe/simulation')
library(SpaDES)  ## should automatically download all packages in the SpaDES family and their dependencies

## decide where you're working
mainDir <- '/home/malte/GitHub/R_MaDisBe/simulation' # SET YOUR MAIN DIRECTORY HERE.
#mainDir <- '~/GitHub/R_MaDisBe/simulation' # SET YOUR MAIN DIRECTORY HERE.
setPaths(cachePath = "cache",
         modulePath = "./modules",
         inputPath = "../data/inputs",
         outputPath = "../data/outputs")

# FUNCTION FOR PARAMETER SELECTION THAT INCLUDES simInit()

# FUNCTION FOR SETTING EXPERIMENT RUN PARAMETERS

# FUNCTION FOR EXECUTION
# - options=save output, plot etc

###### GLOBAL STRUCTURES

# agents = list of lists

# adjacency_matrix = what the name says

# opinions = tibble with id and opinion columns


# construct_environment()

# interact()

modules <- list("basic_setup", "small_world", "rc_sh_model", "data_collection" )

times <- list(start = 0, end = 100)

parameters <- list(
  basic_setup = list(
    mu_opinion_distribution = 0,
    sigma_opinion_distribution = 1.65,
    no_agents = 50, 
    no.cores = 1
    ),
  small_world = list(
    dim = 1,
    nbh_size = 8,
    rewire_p = 0.6,
    opinion_homophily = 0.3
  ),
  #hegselmann_krause = list(
  #  epsilon = 0.3
  #),
  #rc_model = list(
  #  epsilon = 0.3,
  #  other_incons_tolerance = 0.4,
  #  self_incons_tolerance = 0.2,
  #  memory = 1.3,
  #  initial_opinion_confidence = 0.5
  #)#,
  #rc_energy_model = list(
  #  epsilon = 0.3,
  #  other_incons_tolerance = 0.4,
  #  self_incons_tolerance = 0.4,
  #  energy_level = 30,
  #  restoration_factor = 5,
  #  memory = 1.3,
  #  initial_opinion_confidence = 0.5
  #  )#,
  rc_sh_model = list(
  epsilon = 0.3,
  other_incons_tolerance = 0.3,
  self_incons_tolerance = 0.2,
  energy_level = 100,
  restoration_factor = 20,
  memory = 1.3,
  no_groups = 3,
  expert_percentage = 0.1,
  sigma_complexity = 0.6,
  initial_opinion_confidence = 0.2
  )
)

paths <- getPaths()

sim <- simInit(times = times, params = parameters, modules = modules, paths = paths)

set.seed(1234)

out <- spades(sim)


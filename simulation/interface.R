### Interface

setwd('/home/malte/GitHub/R_MaDisBe/simulation')
#setwd('~/GitHub/R_MaDisBe/simulation')
library(SpaDES)  ## should automatically download all packages in the SpaDES family and their dependencies

## decide where you're working
mainDir <- '/home/malte/GitHub/R_MaDisBe/simulation' # SET YOUR MAIN DIRECTORY HERE.
#mainDir <- '~/GitHub/R_MaDisBe/simulation' # SET YOUR MAIN DIRECTORY HERE.
setPaths(cachePath = "cache",
         modulePath = "modules",
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

modules <- list("basic_setup", "small_world", "rc_sh_model", "data_collection")

times <- list(start = 0, end = 100)

parameters <- list(
  basic_setup = list(
    opinion_distribution = "uniform",
    no_agents = 50
    ),
  small_world = list(
    dim = 1,
    nbh_size = 10,
    rewire_p = 0.6
  ),
  #hegselmann_krause = list(
  #  epsilon = 0.3
  #),
  #rc_model = list(
  #  epsilon = 0.3,
  #  other_incons_tolerance = 0.4,
  #  self_incons_tolerance = 0.2,
  #  opinion_memory_depth = 10,
  #  message_memory_depth = 20
  #)#,
  rc_energy_model = list(
    epsilon = 0.3,
    other_incons_tolerance = 0.4,
    self_incons_tolerance = 0.2,
    energy_level = 100,
    restoration_factor = 30,
    opinion_memory_depth = 10,
    message_memory_depth = 20,
    energy_params_memory_depth = 100,
    ),
   rc_sh_model = list(
    epsilon = 0.3,
    other_incons_tolerance = 0.4,
    self_incons_tolerance = 0.2,
    energy_level = 100,
    restoration_factor = 30,
    opinion_memory_depth = 10,
    message_memory_depth = 20,
    energy_params_memory_depth = 100,
    no_groups = 4,
    expert_percentage = 0.05,
    sigma_complexity = 1.75,
    argumentation_memory_depth = 10
  )
)

paths <- getPaths()

sim <- simInit(times = times, params = parameters, modules = modules, paths = paths)

set.seed(1234)
#profvis({
  out <- spades(sim)
#})

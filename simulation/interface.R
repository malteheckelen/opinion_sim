### Interface

setwd('C://Users/Heckelen/Documents/GitHub/R_MaDisBe/simulation')
#setwd('~/GitHub/R_MaDisBe/simulation')
library(SpaDES)  ## should automatically download all packages in the SpaDES family and their dependencies

## decide where you're working
mainDir <- 'C://Users/Heckelen/Documents/GitHub/R_MaDisBe/simulation' # SET YOUR MAIN DIRECTORY HERE.
mainDir <- '~/GitHub/R_MaDisBe/simulation' # SET YOUR MAIN DIRECTORY HERE.
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

modules <- list("basic_setup", "small_world", "rc_energy_model", "data_collection")

times <- list(start = 0, end = 5)

parameters <- list(
  basic_setup = list(
    opinion_distribution = "uniform",
    no_agents = 10
    ),
  small_world = list(
    dim = 1,
    rewire_p = 0.6
  ),
  rc_energy_model = list(
    epsilon = 0.15,
    other_incons_tolerance = 0.6,
    self_incons_tolerance = 0.6,
    opinion_memory_depth = 10,
    message_memory_depth = 10,
    energy_params_memory_depth = 10
  )
)

paths <- getPaths()

sim <- simInit(times = times, params = parameters, modules = modules, paths = paths)

set.seed(1234)
#profvis({
  out <- spades(sim)
#})

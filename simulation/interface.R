### Interface

setwd('~/GitHub/R_MaDisBe/simulation')
library(SpaDES)  ## should automatically download all packages in the SpaDES family and their dependencies

## decide where you're working
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

modules <- list("basic_setup", "small_world", "rc_model", "data_collection")

times <- list(start = 0, end = 50)

parameters <- list(
  basic_setup = list(
    opinion_distribution = "uniform",
    no_agents = 500
    ),
  small_world = list(
    dim = 1,
    rewire_p = 0.5
  ),
  rc_model = list(
    epsilon = 0.15,
    memory_depth = 10
  )
)

paths <- getPaths()

SIM <- simInit(times = times, params = parameters, modules = modules, paths = paths)

#profvis({
  out <- spades(SIM)
#})

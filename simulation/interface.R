### Interface

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

modules <- list("basic_setup", "lattice", "hegselmann_krause", "data_collection")

times <- list(start = 0.0, end = 1000)

parameters <- list(
  basic_setup = list(
    no_agents = 100
    ),
  lattice = list(
    directed = FALSE
  ),
  hegselmann_krause = list(
    epsilon = 0.1
  )
)

paths <- getPaths()

SIM <- simInit(times = times, params = parameters, modules = modules, paths = paths)

out <- spades(SIM, .plotInitialTime = NA) # parameter means plotting is off (faster)

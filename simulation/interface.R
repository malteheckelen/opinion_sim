### Interface

setwd('/home/malte/projects/opinion_sim/simulation')
library(SpaDES)  ## should automatically download all packages in the SpaDES family and their dependencies

## decide where you're working
mainDir <- '/home/malte/projects/opinion_sim/simulation' # SET YOUR MAIN DIRECTORY HERE.
setPaths(cachePath = "cache",
         modulePath = "./modules",
         inputPath = "../data/inputs",
         outputPath = "../data/outputs")

modules <- list("basic_setup", "hegselmann_krause", "data_collection" )

times <- list(start = 0, end = 100, timeunit='day')

parameters <- list(
  basic_setup = list(
    no_agents = 50
  ),
  hegselmann_krause = list(
    epsilon = 0.3
  )
)

paths <- getPaths()

sim <- simInit(times = times, params = parameters, modules = modules)

set.seed(1234)

out <- spades(sim)
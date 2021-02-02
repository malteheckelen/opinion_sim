### Experiments

setwd('/home/malte/projects/opinion_sim/simulation')

library(SpaDES)  ## should automatically download all packages in the SpaDES family and their dependencies
library(parallel)
library(raster)

mainDir <- '/home/malte/projects/opinion_sim/simulation' # SET YOUR MAIN DIRECTORY HERE.

setPaths(cachePath = "cache",
         modulePath = "./modules",
         inputPath = "./../data/inputs",
         outputPath = "./../data/outputs")

#### run a Hegselmann-Kraus model

## load previously defined modules

modules <- list("basic_setup", "lattice", "hegselmann_krause", "data_collection" )

times <- list(start = 0, end = 100)

# set parameters for initial setup of model

hk_parameters <- list(
  basic_setup = list(
    mu_opinion_distribution = 0,
    sigma_opinion_distribution = 0.3,
    no_agents = 100 
    ),
  lattice = list(
    directed=FALSE
  ),
  hegselmann_krause = list(
    epsilon = 0.2
  )
)

paths <- getPaths()

sim <- simInit(times = times, params = hk_parameters, modules = modules, paths = paths)
out <- spades(sim)
demoSim <- suppressMessages(simInit(
  times = list(start = 0, end = 100),
  modules = "SpaDES_sampleModules",
  params = list(
    .globals = list(burnStats = "nPixelsBurned"),
    randomLandscapes = list(
      nx = 1e2, ny = 1e2, .saveObjects = "landscape",
      .plotInitialTime = NA, .plotInterval = NA, inRAM = TRUE
    ),
    caribouMovement = list(
      N = 1e2, .saveObjects = c("caribou"),
      .plotInitialTime = 1, .plotInterval = 1, moveInterval = 1
    ),
    fireSpread = list(
      nFires = 1e1, spreadprob = 0.235, persistprob = 0, its = 1e6,
      returnInterval = 10, startTime = 0,
      .plotInitialTime = 0, .plotInterval = 10
    )
  ),
  path = list(modulePath = system.file("sampleModules", package = "SpaDES.core"))
))

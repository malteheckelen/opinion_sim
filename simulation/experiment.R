### Experiments

setwd('/home/malte/GitHub/R_MaDisBe/simulation')
#setwd('~/GitHub/R_MaDisBe/simulation')
library(SpaDES)  ## should automatically download all packages in the SpaDES family and their dependencies
library(parallel)
library(raster)
## decide where you're working
mainDir <- '/home/malte/GitHub/R_MaDisBe/simulation' # SET YOUR MAIN DIRECTORY HERE.
#mainDir <- '~/GitHub/R_MaDisBe/simulation' # SET YOUR MAIN DIRECTORY HERE.
setPaths(cachePath = "cache",
         modulePath = "./modules",
         inputPath = "../data/inputs",
         outputPath = "../data/outputs")

modules <- list("basic_setup", "small_world", "hegselmann_krause", "data_collection" )

times <- list(start = 0, end = 100)

hk_parameters <- list(
  basic_setup = list(
    mu_opinion_distribution = 0,
    sigma_opinion_distribution = 0.3,
    no_agents = 100 
    ),
  small_world = list(
    dim = 1,
    nbh_size = 8,
    rewire_p = 0.6,
    opinion_homophily = 0.2
  ),
  hegselmann_krause = list(
    epsilon = 0.2
  )
)

hk_experiment_parameters <- list(
  basic_setup = list(
    mu_opinion_distribution = 0,
    sigma_opinion_distribution = list(0.7, 1.1, 1.5, 1.9)
    ),
  small_world = list(
    dim = 1,
    nbh_size = 8,
    rewire_p = 0.6,
    opinion_homophily = list(0.4,0.6,0.8)
  ),
  hegselmann_krause = list(
    epsilon = list(0.4,0.6,0.8)
  )
)

paths <- getPaths()

#no_cores <- detectCores() - 1
#cl <- makeCluster(no_cores)
beginCluster()
sim <- simInit(times = times, params = hk_parameters, modules = modules, paths = paths)
out <- spades(sim)
hk_experiments <- experiment(sim, params=hk_experiment_parameters, replicates = 3, saveExperiment=TRUE, experimentFile="hk_experiments.RData" )
endCluster()

modules <- list("basic_setup", "small_world", "rc_model", "data_collection" )

times <- list(start = 0, end = 100)

rc_parameters <- list(
  basic_setup = list(
    mu_opinion_distribution = 0,
    sigma_opinion_distribution = 0.3,
    no_agents = 100 
    ),
  small_world = list(
    dim = 1,
    nbh_size = 8,
    rewire_p = 0.6,
    opinion_homophily = 0.2
  ),
  rc_model = list(
    epsilon = 0.2,
    other_incons_tolerance = 0.2,
    self_incons_tolerance = 0.2,
    memory = 0.5,
    initial_opinion_confidence = 0.2
  )
)

rc_experiment_parameters <- list(
  basic_setup = list(
    mu_opinion_distribution = 0,
    sigma_opinion_distribution = list(0.7, 1.1, 1.5, 1.9)
    ),
  small_world = list(
    dim = 1,
    nbh_size = 8,
    rewire_p = 0.6,
    opinion_homophily = list(0.4,0.6,0.8)
  ),
  rc_model = list(
    epsilon = list(0.4,0.6,0.8),
    other_incons_tolerance = list(0.4,0.6,0.8),
    self_incons_tolerance = list(0.4,0.6,0.8),
    memory = list(1.0, 1.5, 2.0),
    initial_opinion_confidence = list(0.4,0.6,0.8)
  )
)

paths <- getPaths()

#no_cores <- detectCores() - 1
#cl <- makeCluster(no_cores)
beginCluster()
sim <- simInit(times = times, params = rc_parameters, modules = modules, paths = paths)
out <- spades(sim)
rc_experiments <- experiment(sim, params=rc_experiment_parameters, replicates = 3, saveExperiment=TRUE, experimentFile="rc_experiments.RData" )
endCluster()

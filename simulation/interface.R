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
  .globals = list(adjacency_matrix = "adjacency_matrix", no_agents = 100), # globals set global variables
  basic_setup = list()
  hegselmann_krause = list()
)

paths <- list(modulePath = system.file("sampleModules", package = "SpaDES.core"))

SIM <- simInit(times = times, params = parameters, modules = modules, paths = paths)

out <- spades(SIM, .plotInitialTime = NA) # parameter means plotting is off (faster)

params(sim)$caribouMovementLcc$glmInitialTime # Parameter sind ansprechbar über die params()Funtion
# Der Output ist ein Objekt, dass für die Modulobjekte einzelne Attribute hat
# die Modulparameter werden in den Modulen nur per Default-Wert gesettet
# In simInit() werden die Werte dann nach Gebrauch verändert
# glmInitialTime wird z.B. in caribouMovement als Startzeitpunkt der GLMs benutzt (welche als Grundlage für spätere Plotting-Events dienen)


#if(!dir.exists(file.path(getPaths()$modulePath, "speciesAbundance"))){
#  newModule(name = "speciesAbundance", path = getPaths()$modulePath)
#}
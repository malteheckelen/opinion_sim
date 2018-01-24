### Minimal Hegselmann Krause Model

# for simplicity and comparability, epsilon is taken to be a constant
# however, it is taken as a variable here, that could take input in the future
# if this model is sourced together with such a modification

# we will have an adjacency matrix

# the parameters are set only with default values
stopifnot(packageVersion("SpaDES") >= "1.2.0.9006")

defineModule(sim, list(
  name = "hegselmann_krause",
  description = "Simulate bounded confidence opinion dynamics model according to Krause (1997) and Hegselmann / Krause (2000)",
  keywords = c("opinion dynamics", "hegselmann krause", "bounded confidence"),
  childModules = character(),
  authors = c(person(c("Malte", "Lars"), "Heckelen", email = "matle.heckelen@ilw.uni-stuttgar.de", role = c("aut", "cre"))),
  version = numeric_version("0.0.1"),
  spatialExtent = raster::extent(rep(NA_real_, 4)),
  timeframe = as.POSIXlt(c(NA, NA)),
  timeunit = "tick",
  citation = list("citation.bib"),
  documentation = list("README.txt", "hegselmann_krause.Rmd"),
  reqdPkgs = list("dplyr"),
  parameters = rbind(
    # defineParameter("paramName", "paramClass", value, min, max, "parameter description"),
    ),
  inputObjects = data.frame(
    objectName = c("adjacency_matrix", "vegMap"),
    objectClass = c("RasterLayer", "RasterLayer"),
    sourceURL  =  c(NA_character_, NA_character_),
    other = rep(NA_character_, 2L), stringsAsFactors = FALSE),
  outputObjects = data.frame(
    objectName = c("caribou", "caribouRas", "glmPlot", "glmPVals"),
    objectClass = c("SpatialPointsDataFrame", "RasterLayer", "gg", "numeric"),
    other = rep(NA_character_, 4L), stringsAsFactors = FALSE)
))


init_mods <- function() {
  
}


abundanceInit <- function(sim) {
  ## Template raster
  sim$r <- raster(nrows = 100, ncols = 100, xmn = -50, xmx = 50, ymn = -50, ymx = 50)
  
  ## create storage list of species abundance
  sim$abundRasters <- list()
  
  return(invisible(sim))
}

interact <- function(agent, epsilon) {
  
  ego <- agent
  
  within_epsilon <- numeric(length = no_agents)
  within_epsilon <- opinions %>%
    ifelse( abs( opinions[ ego[["id"]], ] - opinion) < epsilon, 1, 0)
  
  opinions[ ego[["id"]], ] <- (adjacency_matrix[ ego[["id"]], ] * opinions) %>%
    mean() / sum(within_epsilon)
  
}

turn <- function(epsilon) {
  
  lapply()
  
}
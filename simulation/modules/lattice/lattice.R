### LATTICE ENVIRONMENT FOR AGENTS

stopifnot(packageVersion("SpaDES") >= "1.2.0.9006")

defineModule(sim, list(
  name = "hegselmann_krause",
  description = "Simulate bounded confidence opinion dynamics model in the sense of Krause (1997) and Hegselmann / Krause (2000)",
  keywords = c("network", "graph", "environment", "lattice"),
  childModules = character(),
  authors = c(person(c("Malte", "Lars"), "Heckelen", email = "matle.heckelen@ilw.uni-stuttgar.de", role = c("aut", "cre"))),
  version = numeric_version("0.0.1"),
  spatialExtent = raster::extent(rep(NA_real_, 4)),
  timeframe = as.POSIXlt(c(NA, NA)),
  timeunit = "tick",
  citation = list("citation.bib"),
  documentation = list("README.txt", "hegselmann_krause.Rmd"),
  reqdPkgs = list("igraph"),
  parameters = rbind(
    # defineParameter("paramName", "paramClass", value, min, max, "parameter description"),
  ),
  inputObjects = data.frame(
    objectName = c("no_agents", "directed"),
    objectClass = c("numeric", "logical")
    )
  outputObjects = data.frame(
    objectName = c("caribou", "caribouRas", "glmPlot", "glmPVals"),
    objectClass = c("SpatialPointsDataFrame", "RasterLayer", "gg", "numeric"),
    other = rep(NA_character_, 4L), stringsAsFactors = FALSE)
))

make_lattice(dimvector = NULL, length = NULL, dim = NULL, nei = 1,
             directed = FALSE, mutual = FALSE, circular = FALSE)

construct_environment <- function(no_agents, directed=FALSE) {
  
  directed <- directed
  
  length <- no_agents / 2
    
  envir <- make_lattice(length=length, dim=2, directed=directed)
  
  return(get.adjacency(envir))
  
}
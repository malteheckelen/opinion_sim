### LATTICE ENVIRONMENT FOR AGENTS

stopifnot(packageVersion("SpaDES") >= "1.2.0.9006")

defineModule(sim, list(
  name = "lattice",
  description = "Lattice network environment generator for use with different.",
  keywords = c("network", "graph", "environment", "lattice"),
  childModules = character(),
  authors = c(person(c("Malte", "Lars"), "Heckelen", email = "malte.heckelen@ilw.uni-stuttgar.de", role = c("aut", "cre"))),
  version = numeric_version("0.0.1"),
  spatialExtent = raster::extent(rep(NA_real_, 4)),
  timeframe = as.POSIXlt(c(NA, NA)),
  timeunit = "tick",
  citation = list("citation.bib"),
  documentation = list("README.txt", "lattice.Rmd"),
  reqdPkgs = list("igraph"),
  parameters = rbind(
    # defineParameter("paramName", "paramClass", value, min, max, "parameter description"),
  ),
  inputObjects = data.frame(
    objectName = c("no_agents", "directed"),
    objectClass = c("numeric", "logical")
    ),
  outputObjects = data.frame(
    objectName = c("adjacency_matrix"),
    objectClass = c("dgCMatrix")
    )
  )
)


construct_environment <- function() {
  
  directed <- directed
  
  length <- no_agents / 2
    
  envir <- make_lattice(length=length, dim=2, directed=directed)
  
  return(get.adjacency(envir))
  
}
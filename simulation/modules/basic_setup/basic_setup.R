stopifnot(packageVersion("SpaDES") >= "1.2.0.9006")

defineModule(sim, list(
  name = "basic_setup",
  description = "Barebones setup for combination with the actual Bounded Confidence Model simulation modules.",
  keywords = c("opinion dynamics", "setup", "bounded confidence"),
  childModules = character(),
  authors = c(person(c("Malte", "Lars"), "Heckelen", email = "malte.heckelen@ilw.uni-stuttgart.de", role = c("aut", "cre"))),
  version = numeric_version("0.0.1"),
  spatialExtent = raster::extent(rep(NA_real_, 4)),
  timeframe = as.POSIXlt(c(NA, NA)),
  timeunit = "tick",
  citation = list("citation.bib"),
  documentation = list("README.txt", "hegselmann_krause.Rmd"),
  reqdPkgs = list("dplyr"),
  parameters = rbind(
    # defineParameter("paramName", "paramClass", value, min, max, "parameter description"),
    defineParameter("no_agents", "numeric", 0.1, NA, NA, "The Bounded Confidence parameter.")
  ),
  outputObjects = data.frame(
    objectName = c("no_agents"),
    objectClass = c("numeric"),
    other = rep(NA_character_, 4L), stringsAsFactors = FALSE)
))

doEvent.basic_setup <- function(sim, eventTime, eventType, debug=FALSE) {
  switch(
    eventType,
    init = {
      sim <- basic_setupInit(sim)
    }
  )
}

basic_setupInit <- function(sim) {
  
  sim$no_agents <- 
}







# make agents_characteristics attribute as tibble, which in this most basic form only contains a column named opinions
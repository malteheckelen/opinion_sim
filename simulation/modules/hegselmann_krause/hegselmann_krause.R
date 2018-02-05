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
  authors = c(person(c("Malte", "Lars"), "Heckelen", email = "malte.heckelen@ilw.uni-stuttgart.de", role = c("aut", "cre"))),
  version = numeric_version("0.0.1"),
  spatialExtent = raster::extent(rep(NA_real_, 4)),
  timeframe = as.POSIXlt(c(NA, NA)),
  timeunit = "hour",
  citation = list("citation.bib"),
  documentation = list("README.txt", "hegselmann_krause.Rmd"),
  reqdPkgs = list("dplyr"),
  parameters = rbind(
    # defineParameter("paramName", "paramClass", value, min, max, "parameter description"),
    defineParameter("epsilon", "numeric", 0.1, NA, NA, "The Bounded Confidence parameter.")
    ),
  inputObjects = bind_rows(
    expectsInput("environment", "tbl_graph", "The environment for the agents"),
    expectsInput("agent_characteristics", "tbl_df", "The characteristics of each agent.")
  ),
  outputObjects = bind_rows(
    createsOutput("agent_characteristics", "tbl_df", "The characteristics of each agent."),
    createsOutput("distances_table", "tbl_df", "The table of opinion distances.")
  )
  )
)


doEvent.hegselmann_krause <- function(sim, eventTime, eventType, debug = FALSE) {
  switch(
    eventType,
    init = {
      ## do stuff for this event
      sim <- hegselmann_krauseInit(sim)
      
      ## schedule future event(s)
      sim <- scheduleEvent(sim, eventTime = start(sim), moduleName = "hegselmann_krause", eventType = "step")
    },
    step = {
      ## do stuff for this event
      sim <- hegselmann_krauseStep(sim)
      
      ## schedule future event(s)
      sim <- scheduleEvent(sim, eventTime = time(sim)+1, moduleName = "hegselmann_krause", eventType = "step")
    },
    warning(paste("Undefined event type: '", current(sim)[1, "eventType", with = FALSE],
                  "' in module '", current(sim)[1, "moduleName", with = FALSE], "'", sep = ""))
  )
  return(invisible(sim))
}

hegselmann_krauseInit <- function(sim) {
  # set within_epsilon column in opinions simulation attribute
  # reserve the Init in every of these simulations to place special attributes for modules

  return(invisible(sim))
}

hegselmann_krauseStep <- function(sim) {
  
  distances_part <- sim$environment %>%
    activate(edges) %>%
    as_tibble() %>%
    mutate(from_two = from) %>%
    mutate(to_two = to) %>%
    mutate(from = to_two) %>%
    mutate(to = from_two) %>%
    select(from, to)
  
  # create a distance table
  sim$distances_table <- sim$environment %>%
    activate(edges) %>%
    as_tibble() %>%
    rbind(distances_part) %>%
    inner_join(sim$agent_characteristics, by=c("from" = "agent_id")) %>% 
    mutate(opinion_from = opinion) %>%
    select(from, to, opinion_from) %>%
    inner_join(sim$agent_characteristics, by=c("to" = "agent_id")) %>%
    mutate(opinion_to = opinion) %>% 
    select(from, to, opinion_from, opinion_to) %>%
    mutate(distance = opinion_from - opinion_to) %>%
    mutate(distance = abs(distance)) %>%
    mutate(within_epsilon = distance < params(sim)$hegselmann_krause$epsilon)
  
  sim$agent_characteristics <- sim$agent_characteristics %>%
    inner_join(sim$distances_table, by=c("agent_id" = "from")) %>%
    group_by(agent_id) %>%
    mutate(no_within = sum(within_epsilon)) %>%
    filter(within_epsilon == TRUE) %>%
    mutate(opinion = ifelse(no_within != 0, sum(opinion_to) / no_within, opinion)) %>%
    ungroup() %>%
    select(agent_id, opinion) %>% 
    full_join(sim$agent_characteristics, by="agent_id") %>% 
    mutate(opinion = coalesce(opinion.x, opinion.y)) %>%
    select(agent_id, opinion) %>%
    distinct()
  
  return(invisible(sim))
  
}

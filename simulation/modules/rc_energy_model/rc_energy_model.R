### Rational Choice Model

# for simplicity and comparability, epsilon is taken to be a constant
# however, it is taken as a variable here, that could take input in the future
# if this model is sourced together with such a modification

# we will have an adjacency matrix

# the parameters are set only with default values
stopifnot(packageVersion("SpaDES") >= "1.2.0.9006")

defineModule(sim, list(
  name = "rc_model",
  description = "Simulate bounded confidence opinion dynamics model in Rational Choice framework.",
  keywords = c("opinion dynamics", "hegselmann krause", "rational choice"),
  childModules = character(),
  authors = c(person(c("Malte", "Lars"), "Heckelen", email = "malte.heckelen@ilw.uni-stuttgart.de", role = c("aut", "cre"))),
  version = numeric_version("0.0.1"),
  spatialExtent = raster::extent(rep(NA_real_, 4)),
  timeframe = as.POSIXlt(c(NA, NA)),
  timeunit = "hour",
  citation = list("citation.bib"),
  documentation = list("README.txt", "rc_model.Rmd"),
  reqdPkgs = list("tidyverse", "data.table"),
  parameters = rbind(
    # defineParameter("paramName", "paramClass", value, min, max, "parameter description"),
    defineParameter("epsilon", "numeric", 0.1, NA, NA, "The Bounded Confidence parameter."),
    defineParameter("other_incons_tolerance", "numeric", 0.1, NA, NA, "The parameter controlling the tolerance for the degree with which other agents to have varying opinions over time."),
    defineParameter("self_incons_tolerance", "numeric", 0.1, NA, NA, "The parameter controlling the tolerance for the degree with which the agent himself has varying opinions over time."),
    defineParameter("message_memory_depth", "numeric", 1, NA, NA, "The number of time steps agents remember messages for."),
    defineParameter("opinion_memory_depth", "numeric", 1, NA, NA, "The number of time steps agents remember opinions for.")
    ),
  inputObjects = bind_rows(
    expectsInput("environment", "tbl_graph", "The environment for the agents"),
    expectsInput("agent_characteristics", "tbl_df", "The characteristics of each agent.")
  ),
  outputObjects = bind_rows(
    createsOutput("agent_characteristics", "tbl_df", "The characteristics of each agent."),
    createsOutput("data_collection_prompt", "logical", "A prompt to collect data.")
  )
  )
)


doEvent.rc_model <- function(sim, eventTime, eventType, debug = FALSE) {
  
  switch(
    eventType,
    init = {
      ## do stuff for this event
      sim <- rc_modelInit(sim)
      
      ## schedule future event(s)
      sim <- scheduleEvent(sim, eventTime = start(sim)+1, moduleName = "rc_model", eventType = "step")
    },
    step = {
      ## do stuff for this event
      sim <- rc_modelStep(sim)
      
      ## schedule future event(s)
      sim <- scheduleEvent(sim, eventTime = time(sim)+1, moduleName = "rc_model", eventType = "step")
    },
    warning(paste("Undefined event type: '", current(sim)[1, "eventType", with = FALSE],
                  "' in module '", current(sim)[1, "moduleName", with = FALSE], "'", sep = ""))
  )
  
  return(invisible(sim))
  
}

rc_modelInit <- function(sim) {
  # set within_epsilon column in opinions simulation attribute
  # reserve the Init in every of these simulations to place special attributes for modules
  
  print(time(sim))
  
  sim$agent_characteristics <- sim$agent_characteristics %>%
    data.table() %>%
    .[ , energy := params(sim)$rc_energy_model$energy_level] %>%
    .[ , neighborhood_dissimilarity := 0] %>%
    .[ , self_discrepancy := 0] %>%
    .[ , self_inconsistenty := 0]
    
  
  sim$actions_overall <- tibble(
    
    agent_id = rep(agent_characteristics$agent_id, each=4),
    actions = rep(c("Send", "Receive", "Both", "Nothing"), no_agents),
    util_score = rep(0, length(actions))
    
  ) %>% data.table() %>%
    .[ actions == "Both", util_score := 1 ] %>%
    dcast(agent_id ~ actions, value.var = "util_score") %>% 
    .[ , agent_id := as.character(agent_id)] %>%
    .[, best_action :=  names(.[ , -c("agent_id")])[apply(.[ , -c("agent_id") ], 1, which.max)]] %>% 
    melt( id.vars = c("agent_id", "best_action"),
          measure.vars = c("Send", "Receive", "Both", "Nothing"),
          variable.name = "actions",
          value.name = "util_score" )  
  
  sim$actions_send <- tibble(
    
    agent_id = rep(agent_characteristics$agent_id, each=2),
    actions = rep(c("Unoptimized", "Optimized"), no_agents),
    util_score = rep(0, length(actions))
    
  ) %>%  data.table()
  
  sim$messages <- sim$environment %>%
    activate(edges) %>%
    as_tibble() %>%
    data.table() %>%
    setnames(old = c("from", "to"), new = c("to", "from"))

  sim$messages <- sim$environment %>%
    activate(edges) %>%
    as_tibble() %>%
    data.table() %>%
    rbind(messages) %>% 
    .[copy(sim$agent_characteristics[, .(agent_id, opinion)]), nomatch = 0L, on = c("from" = "agent_id"), allow.cartesian=TRUE] %>%
    setnames("opinion", "opinion_from") %>%
    .[ , assumption_to := opinion_from]

  # make matrix of all possible optimized messages
  sim$message_matrix <- outer(sim$messages$opinion_from, sim$messages$assumption_to, produce_altered_message) # works
  row.names(sim$message_matrix) <- sim$messages[ , from]
  colnames(sim$message_matrix) <- sim$messages[ , to]
  mess_mat <<- sim$message_matrix
  sim$messages <- copy(sim$message_matrix) %>% 
    data.table() %>% 
    .[ , from := as.numeric(row.names(message_matrix))] %>% 
    melt( id.vars = c("from"),
          measure.vars = as.character(seq(1, no_agents, 1)),
          variable.name = "to",
          value.name = "opt_message" ) %>%
    .[ to != from] %>%
    setkey("from") %>%
    unique() %>%
    .[ , from := as.integer(from)] %>%
    .[ , to := as.integer(to)] %>%
    .[copy(unique(sim$messages)), on=c("from", "to"), nomatch = 0L, allow.cartesian = TRUE] %>%
    .[ , opt_message := mean(opt_message), by = .(from)]

  # at the end of Init, past_messages will be assigned too
  sim$discourse_memory <- copy(sim$messages) %>%
    .[ , past_opinions := sapply(opinion_from, function(x) {list(x)} )] %>%
    setnames("opinion_from", "opinion") %>%
    .[ , .(from, opinion, past_opinions)] %>%
    .[ , past_opinions := as.character(past_opinions) ] %>% # unique() can't handle list columns, so first transform to character
    unique() %>% 
    .[ , past_opinions := sapply(past_opinions, function(x) list(eval(parse(text = x))))] # then transform back
  
  # Sending
  
  # first, the set of rows is reduced to those that want to send
  # then the numbers are computed 
  
  sim$actions_send <- copy(sim$messages) %>% 
    .[copy(actions_overall), on = c("from" = "agent_id"), nomatch = 0L, allow.cartesian = TRUE ] %>%
    .[ best_action == actions ] %>%
    .[ (best_action == "Both" | best_action == "Send") ] %>%
    .[ .(colnames(sim$messages))] %>%
    .[copy(sim$actions_send), on = c("from" = "agent_id"), nomatch = 0L, allow.cartesian = TRUE ] %>%
    .[copy(sim$discourse_memory), on = c("from"), nomatch = 0L, allow.cartesian = TRUE] %>%
    .[, distance_to_past_opinions := mapply(function(a,b,c,k) {
      switch(k,
             "Unoptimized" = {
               mean(
                 sapply(a, function(x) {
                   abs(x - c)
                 })
               )
             },
             "Optimized" = {
               mean(
                 sapply(a, function(x) {
                   abs(x - b)
                 })
               )
             }
      )
    }, a=past_opinions, b=opt_message, c=opinion_from, k=actions)] %>%
      .[, distance_message_opinion := mapply(function(a,b,k) {
        switch(k,
               "Unoptimized" = {
                 abs(a - a)
               },
               "Optimized" = {
                 abs(a - b)
               }
        )
      }, a=opinion_from, b=opt_message, k=actions)] %>%
      .[, distance_message_assumption := mapply(function(a,b,c,k) {
        switch(k,
               "Unoptimized" = {
                 abs(c - a)
               },
               "Optimized" = {
                 abs(c - b)
               }
        )
      }, a=opinion_from, b=opt_message, c=assumption_to, k=actions)] %>%
      .[ , -c("past_opinions", "to")] %>% # include past_messages here in the step function
      .[ , util_score := distance_to_past_opinions - distance_message_opinion - distance_message_assumption] %>% # include distance_to_past_messages in step function
      setnames("from", "agent_id") %>%
      .[,.(agent_id, actions, util_score)] %>%
      setkey("agent_id") %>%
      unique() %>%
      .[, util_score := sum(util_score), by=c("agent_id", "actions")] %>% 
      unique() %>%
      dcast(agent_id ~ actions, value.var = "util_score") %>% 
      .[, best_action :=  ifelse(Optimized > Unoptimized, "Optimized", "Unoptimized")] %>% 
      melt( id.vars = c("agent_id", "best_action"),
            measure.vars = c("Optimized", "Unoptimized"),
            variable.name = "actions",
            value.name = "util_score" ) 
  
  # discourse memory is updated
  # however, due to actions_send being a reduced row set, the remaining rows with the past values for past_messages et al. need to be merged in
  # in the Init this is okay because action_send actually returns all rows needed
  # it would be a problem if past_messages and opinions were not generated for some agents here, as then where would the columns come from in the corresponding flow in the step function?
  # however, every agent sends something in the Init, so that is not a problem
  
  sim$discourse_memory <- copy(sim$actions_send) %>%
    .[copy(sim$messages), on = c("agent_id" = "from"), nomatch = 0L, allow.cartesian = TRUE] %>%
    setnames("agent_id", "from") %>%
    setkey("from") %>%
    unique() %>%
    .[copy(sim$discourse_memory[ , -c("opinion")]), on = c("from"), nomatch = 0L, allow.cartesian = TRUE] %>% # thisll probably not work
    .[ actions==best_action ] %>%
    .[ , past_messages := ifelse(.[ , best_action] == "Unoptimized",
                                 list(opinion_from[[1]]),
                                 list(opt_message[[1]]))] %>%
    .[ , past_opinions := ifelse(lengths(past_opinions) < params(sim)$rc_model$opinion_memory_depth,
                                 mapply(function(x, y) {
                                   list(c(unlist(x), y))
                                 }, x=past_opinions, y=opinion_from),
                                 mapply(function(x, y) {
                                   list(c(unlist(x)[1:params(sim)$rc_model$opinion_memory_depth], y))
                                 }, x=past_opinions, y=opinion_from)
    )] %>%
    setnames("opinion_from", "opinion") %>%
    setnames("opt_message", "message") %>%
    .[ , .(from, to, opinion, message, past_messages, past_opinions)] %>%
    .[copy(sim$messages), on = c("agent_id" = "from"), nomatch = 0L, allow.cartesian = TRUE] %>%
    .[ , past_messages := as.character(past_messages) ] %>%
    .[ , past_opinions := as.character(past_opinions) ] %>% 
    unique() %>% 
    .[ , past_messages := sapply(past_messages, function(x) list(eval(parse(text = x))))] %>%
    .[ , past_opinions := sapply(past_opinions, function(x) list(eval(parse(text = x))))] %>%
    .[ , sender_business := .N, by = "from" ] %>% # generate business
    .[ , receiver_business := .N, by = "to" ] # generate business
  
  sim$discourse_memory <- copy( sim$discourse_memory[ , -c("receiver_business") ] ) %>%
    .[copy(sim$discourse_memory[ , .(to, business)]), on = c("from", "to")] # remerge it to correspond to receiver business
  
  # here we might run into a problem
  # in a line below, the rows that do not correspond to actions_send are filtered out
  # sim$messages is reset at the start of the next round, but it is used in Receiving, which is a problem
  
  sim$messages <- copy(sim$messages) %>%
    setnames(old=c("opinion_from", "opt_message"), new=c("opinion_from_y", "opt_message_y")) %>%
    unique() %>%
    setnames(old = c("from", "to"), new = c("to", "from")) %>% 
    .[ , .(to, opinion_from_y, opt_message_y)] %>%
    .[sim$messages, on="to", nomatch=0L, allow.cartesian=TRUE] # WORKS
  
  # if we have a reduced row set, we can use that in opinion_updating, because that is the set of agents we want
  # but those agents we want are in "to" and the associated variables are all for the "from" agent
  # this down here might have to be renamed to be more clearly meant for opinion_updating
  # then we need to do some switcheroos with the variables: we might have everything we need, but would also need to get
  # the opinions for the from agents from agent_characteristics
  sim$messages <- sim$messages %>%
    merge(sim$actions_send, by.x = c("from"), by.y = c("agent_id"), all = TRUE) %>% # this will have lots of NAs in Step
    .[sim$actions_send[, .(agent_id, actions, best_action)], on=c("from" = "agent_id"), allow.cartesian=TRUE] %>% # maybe leaving out nomatch works
    .[ !is.na(best_action) , assumption_to := ifelse(.[, best_action] == "Unoptimized", opinion_from_y, opt_message_y)] %>% 
    .[ , -c("opinion_from_y", "opt_message_y")] %>%
    setkey("from") %>%
    unique() 
  
  # Receiving

  sim$opinion_updating <- copy(sim$messages)  %>% 
    .[copy(actions_overall), on = c("from" = "agent_id"), nomatch = 0L, allow.cartesian = TRUE ] %>%
    .[ best_action == actions ] %>%
    .[ (best_action == "Both" | best_action == "Receive") ]  %>%
    .[ .(colnames(sim$messages))] %>%
    .[sim$discourse_memory, on=c("from"), nomatch = 0L, allow.cartesian = TRUE] %>% 
    .[ best_action == actions ] %>%
    .[ , distance_to_past_opinions := mapply(function(a,b) {
      mean(
        sapply(a, function(x) {
          abs(x - b)
        })
      )
    }, a=past_opinions, b=assumption_to)] %>% 
    .[ , within_epsilon := abs(opinion_from - assumption_to) < params(sim)$rc_model$epsilon] %>%
    .[ , self_consistent := distance_to_past_opinions < params(sim)$rc_model$self_incons_tolerance] %>%  #.[ , self_inconsistent] ->> self_inconsistent
    .[ within_epsilon == TRUE & self_consistent == TRUE ] %>%
    .[ , sum_assumptions := sum(assumption_to), by=from] %>%
    .[ , denominator := .N, by=from ] %>%
    .[ , reception_energy_loss := .N, by=from ] %>%
    .[ , opinion_y := ifelse( denominator != 0, sum_assumptions / denominator, opinion_from )] %>%
    setnames("from", "agent_id") %>%
    .[ , .(agent_id, opinion_y)] %>%
    setkey("agent_id") %>%
    unique()
  
  sim$agent_characteristics <- merge(sim$agent_characteristics, sim$opinion_updating, by="agent_id", all.x = TRUE) %>%
    .[ , opinion := ifelse(is.na(opinion_y), opinion, opinion_y)] %>%
    .[ , energy := (energy-4)] %>%
    .[ , -c("opinion_y")] 
  
  return(invisible(sim))
  
}

rc_modelStep <- function(sim) {
  
  print(time(sim))
  
  # reset sim$messages so it has full row-count
  sim$messages <- sim$environment %>%
    activate(edges) %>%
    as_tibble() %>%
    data.table() %>%
    setnames(old = c("from", "to"), new = c("to", "from"))
  
  sim$messages <- sim$environment %>%
    activate(edges) %>%
    as_tibble() %>%
    data.table() %>%
    rbind(messages) %>%
    .[copy(sim$agent_characteristics[, .(agent_id, opinion)]), nomatch = 0L, on = c("from" = "agent_id"), allow.cartesian=TRUE] %>%
    setnames("opinion", "opinion_from") %>%
    .[ , assumption_to := vec_rnorm(1, .[ , opinion_from], 0.1)]
  
  # make matrix of all possible optimized messages
  sim$message_matrix <- outer(sim$messages$opinion_from, sim$messages$assumption_to, produce_altered_message) # works
  row.names(sim$message_matrix) <- sim$messages[, from]
  colnames(sim$message_matrix) <- sim$messages[, to]
  
  sim$messages <- copy(sim$message_matrix) %>% 
    data.table() %>% 
    .[ , from := as.numeric(row.names(message_matrix))] %>% 
    melt( id.vars = c("from"),
          measure.vars = as.character(seq(1, no_agents, 1)),
          variable.name = "to",
          value.name = "opt_message" ) %>%
    .[ to != from] %>%
    setkey("from") %>%
    unique() %>%
    .[ , from := as.integer(from)] %>%
    .[ , to := as.integer(to)] %>%
    .[copy(unique(sim$messages)), on=c("from", "to"), nomatch = 0L, allow.cartesian = TRUE] %>%
    .[ , opt_message := mean(opt_message), by = .(from)]
  
  # Sending
  sim$actions_send <- copy(sim$messages) %>% 
    .[copy(sim$actions_send), on = c("from" = "agent_id"), nomatch = 0L, allow.cartesian = TRUE] %>%
    .[copy(sim$discourse_memory), on = c("from"), nomatch = 0L, allow.cartesian = TRUE] %>%
    .[, distance_to_past_messages := mapply(function(a,b,c,k) {
      switch(k,
             "Unoptimized" = {
               mean(
                 sapply(a, function(x) {
                   abs(x - c)
                 })
               )
             },
             "Optimized" = {
               mean(
                 sapply(a, function(x) {
                   abs(x - b)
                 })
               )
             }
      )
    }, a=past_messages, b=opt_message, c=opinion_from, k=actions)] %>%
    .[, distance_to_past_opinions := mapply(function(a,b,c,k) {
      switch(k,
             "Unoptimized" = {
               max(
                 sapply(a, function(x) {
                   abs(x - c)
                 })
               )
             },
             "Optimized" = {
               max(
                 sapply(a, function(x) {
                   abs(x - b)
                 })
               )
             }
      )
    }, a=past_opinions, b=opt_message, c=opinion_from, k=actions)] %>%
      .[, distance_message_opinion := mapply(function(a,b,k) {
        switch(k,
               "Unoptimized" = {
                 abs(a - a)
               },
               "Optimized" = {
                 abs(a - b)
               }
        )
      }, a=opinion_from, b=opt_message, k=actions)] %>%
      .[, distance_message_assumption := mapply(function(a,b,c,k) {
        switch(k,
               "Unoptimized" = {
                 abs(c - a)
               },
               "Optimized" = {
                 abs(c - b)
               }
        )
      }, a=opinion_from, b=opt_message, c=assumption_to, k=actions)] %>%
      .[ , -c("past_opinions", "past_messages", "to")] %>% # include past_messages here in the step function
      .[ , util_score := distance_to_past_opinions - distance_to_past_messages - distance_message_opinion - distance_message_assumption] %>% # include distance_to_past_messages in step function
      setnames("from", "agent_id") %>%
      .[,.(agent_id, actions, util_score)] %>%
      setkey("agent_id") %>%
      unique() %>%
      .[, util_score := sum(util_score), by=c("agent_id", "actions")] %>% 
      unique() %>%
      dcast(agent_id ~ actions, value.var = "util_score") %>% 
      .[, best_action :=  ifelse(Optimized > Unoptimized, "Optimized", "Unoptimized")] %>% 
      melt( id.vars = c("agent_id", "best_action"),
            measure.vars = c("Optimized", "Unoptimized"),
            variable.name = "actions",
            value.name = "util_score" ) 
  
  sim$discourse_memory <- copy(sim$actions_send) %>%
    .[copy(sim$messages), on = c("agent_id" = "from"), nomatch = 0L, allow.cartesian = TRUE] %>%
    setnames("agent_id", "from") %>%
    setkey("from") %>%
    unique() %>%
    .[copy(sim$discourse_memory), on = c("from"), nomatch = 0L, allow.cartesian = TRUE] %>%
    .[actions==best_action] %>%
    .[ , past_messages := ifelse(lengths(past_messages) < params(sim)$rc_model$message_memory_depth, 
                                 ifelse(.[ , best_action] == "Unoptimized",
                                        mapply(function(x, y) {
                                          list(c(unlist(x), y))
                                        }, x=past_messages, y=opinion_from),
                                        mapply(function(x, y) {
                                          list(c(unlist(x), y))
                                        }, x=past_messages, y=opt_message)),
                                 ifelse(.[ , best_action] == "Unoptimized",
                                        mapply(function(x, y) {
                                          list(c(unlist(x)[1:2], y))
                                        }, x=past_messages, y=opinion_from),
                                        mapply(function(x, y) {
                                          list(c(unlist(x)[1:2], y))
                                        }, x=past_messages, y=opt_message)
                                 )
    )] %>%
    .[ , past_opinions := ifelse(lengths(past_opinions) < params(sim)$rc_model$opinion_memory_depth,
                                 mapply(function(x, y) {
                                   list(c(unlist(x), y))
                                 }, x=past_opinions, y=opinion_from),
                                 mapply(function(x, y) {
                                   list(c(unlist(x)[1:2], y))
                                 }, x=past_opinions, y=opinion_from)
    )
    ] %>%
    .[ , .(from, to, past_messages, past_opinions)] %>%
    .[ , past_messages := as.character(past_messages) ] %>%
    .[ , past_opinions := as.character(past_opinions) ] %>% 
    unique() %>% 
    .[ , past_messages := sapply(past_messages, function(x) list(eval(parse(text = x))))] %>%
    .[ , past_opinions := sapply(past_opinions, function(x) list(eval(parse(text = x))))]
  
  
  sim$messages <- copy(sim$messages) %>%
    setnames(old=c("opinion_from", "opt_message"), new=c("opinion_from_y", "opt_message_y")) %>%
    unique() %>%
    setnames(old = c("from", "to"), new = c("to", "from")) %>% 
    .[ , .(to, opinion_from_y, opt_message_y)] %>%
    .[sim$messages, on="to", nomatch=0L, allow.cartesian=TRUE] %>% # WORKS
    .[sim$actions_send[, .(agent_id, actions, best_action)], nomatch=0L, on=c("from" = "agent_id"), allow.cartesian=TRUE] %>%
    .[ , assumption_to := ifelse(.[, best_action] == "Unoptimized", opinion_from_y, opt_message_y)] %>% 
    .[ , -c("opinion_from_y", "opt_message_y")] %>%
    setkey("from") %>%
    unique() 
  
  # Receiving
  #disc_mem <<- sim$discourse_memory
  sim$opinion_updating <- copy(sim$messages) %>%
    .[sim$discourse_memory, on=c("from"="to"), nomatch = 0L, allow.cartesian = TRUE] %>% #->> opp_up
    .[best_action == actions] %>% 
    .[ , distance_to_past_messages := mapply(function(a,b) {
      max(
        sapply(a, function(x) {
          abs(x - b)
        })
      )
    }, a=past_messages, b=assumption_to)] %>% #.[ , distance_to_past_messages] %>% ->> distance_to_past_messages
    .[ , -c("past_messages", "past_opinions")] %>%
    .[sim$discourse_memory, on=c("from"), nomatch = 0L, allow.cartesian = TRUE] %>%
    .[ , distance_to_past_opinions := mapply(function(a,b) {
      max(
        sapply(a, function(x) {
          abs(x - b)
        })
      )
    }, a=past_opinions, b=assumption_to)] %>% #.[ , distance_to_past_opinions] ->> distance_to_past_opinions
    .[ , within_epsilon := abs(opinion_from - assumption_to) < params(sim)$rc_model$epsilon] %>%
    .[ , self_consistent := distance_to_past_opinions < params(sim)$rc_model$self_incons_tolerance] %>%  #.[ , self_inconsistent] ->> self_inconsistent
    .[ , trust := distance_to_past_messages < params(sim)$rc_model$other_incons_tolerance] %>% # .[ , doubt] ->> doubt
    .[ within_epsilon == TRUE & trust == TRUE & self_consistent == TRUE ] %>%
    .[ , sum_assumptions := sum(assumption_to), by=from] %>%
    .[ , denominator := .N, by=from ] %>%
    .[ , opinion_y := ifelse( denominator != 0, sum_assumptions / denominator, opinion_from )] %>%
    setnames("from", "agent_id") %>%
    .[ , .(agent_id, opinion_y)] %>%
    setkey("agent_id") %>%
    unique()#->> after_updating
  #before_merge <<- sim$agent_characteristics
  
  sim$agent_characteristics <- merge(sim$agent_characteristics, sim$opinion_updating, by="agent_id", all.x = TRUE) %>%
    .[ , opinion := ifelse(is.na(opinion_y), opinion, opinion_y)] %>%
    .[ , -c("opinion_y")] # ->> after_merge
  #fuck_me <<- sim$agent_characteristics
  sim$messages <- sim$messages %>% 
    .[best_action == actions] %>% 
    .[ , -c("assumption_to", "opt_message", "opinion_from", "actions", "best_action")]
  
  return(invisible(sim))
  
}

# FUNCTIONS

produce_altered_message <- function(opinion_send, message_receive) {
  
  # produce altered message without epsilon bound
  altered_message <- ifelse(opinion_send < message_receive, 
                            opinion_send + abs(opinion_send - message_receive) / 2, 
                            opinion_send - abs(opinion_send - message_receive) / 2)
  
  # check if within epsilon to own opinion and correct
  # commenting this for now, see Evernote: Design
  #altered_message <- ifelse(abs(opinion_send - altered_message) > epsilon,
  #                          ifelse((opinion_send - altered_message) <= 0, 0, opinion_send - altered_message),
  #                          altered_message)
  
  return(altered_message)
  
} # works

vec_rnorm <- Vectorize(rnorm)
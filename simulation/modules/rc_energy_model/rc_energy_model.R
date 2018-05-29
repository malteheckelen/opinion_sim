### Rational Choice Energy Model

# for simplicity and comparability, epsilon is taken to be a constant
# however, it is taken as a variable here, that could take input in the future
# if this model is sourced together with such a modification

# we will have an adjacency matrix

# the parameters are set only with default values
stopifnot(packageVersion("SpaDES") >= "1.2.0.9006")

defineModule(sim, list(
  name = "rc_energy_model",
  description = "Simulate bounded confidence opinion dynamics model in Rational Choice framework with utility computation respecting energy levels of agents.",
  keywords = c("opinion dynamics", "hegselmann krause", "rational choice"),
  childModules = character(),
  authors = c(person(c("Malte", "Lars"), "Heckelen", email = "malte.heckelen@ilw.uni-stuttgart.de", role = c("aut", "cre"))),
  version = numeric_version("0.0.1"),
  spatialExtent = raster::extent(rep(NA_real_, 4)),
  timeframe = as.POSIXlt(c(NA, NA)),
  timeunit = "hour",
  citation = list("citation.bib"),
  documentation = list("README.txt", "rc_energy_model.Rmd"),
  reqdPkgs = list("tidyverse", "data.table"),
  parameters = rbind(
    # defineParameter("paramName", "paramClass", value, min, max, "parameter description"),
    defineParameter("epsilon", "numeric", 0.1, NA, NA, "The Bounded Confidence parameter."),
    defineParameter("other_incons_tolerance", "numeric", 0.1, NA, NA, "The parameter controlling the tolerance for the degree with which other agents to have varying opinions over time."),
    defineParameter("self_incons_tolerance", "numeric", 0.1, NA, NA, "The parameter controlling the tolerance for the degree with which the agent himself has varying opinions over time."),
    defineParameter("message_memory_depth", "numeric", 1, NA, NA, "The number of time steps agents remember messages for."),
    defineParameter("opinion_memory_depth", "numeric", 1, NA, NA, "The number of time steps agents remember opinions for."),
    defineParameter("energy_level", "numeric", 100, NA, NA, "The starting energy level of every agent."),
    defineParameter("restoration_factor", "numeric", 20, NA, NA, "The number of energy units that are restored to agents at the end of every round."),
    defineParameter("energy_params_memory_depth", "numeric", 1, NA, NA, "The number of time steps agents remember statistics relevant for overall action selection for.")
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


doEvent.rc_energy_model <- function(sim, eventTime, eventType, debug = FALSE) {

  switch(
    eventType,
    init = {
      ## do stuff for this event
      sim <- rc_energy_modelInit(sim)

      ## schedule future event(s)
      sim <- scheduleEvent(sim, eventTime = start(sim)+1, moduleName = "rc_energy_model", eventType = "step")
    },
    step = {
      ## do stuff for this event
      sim <- rc_energy_modelStep(sim)

      ## schedule future event(s)
      sim <- scheduleEvent(sim, eventTime = time(sim)+1, moduleName = "rc_energy_model", eventType = "step")
    },
    warning(paste("Undefined event type: '", current(sim)[1, "eventType", with = FALSE],
                  "' in module '", current(sim)[1, "moduleName", with = FALSE], "'", sep = ""))
  )

  return(invisible(sim))

}

rc_energy_modelInit <- function(sim) {

  print(time(sim))
  
  ############################################
  #### MODIFY AGENT_CHARACTERISTICS TABLE ####
  ############################################

  # modify agent_characteristics data.table with regard to model additions
  # take starting energy from parameterization and assign to column
  sim$agent_characteristics <- sim$agent_characteristics %>%
    data.table() %>%
    .[ , energy := params(sim)$rc_energy_model$energy_level]
  
  #### sim$agent_characteristics table specs at this point:
  # rowlength: sim$no_agents
  # columns: agent_id (numeric), opinion (numeric), energy (numeric)

  ######################################## 
  #### CONSTRUCT CHOSEN_ACTIONS TABLE ####
  ########################################

  # make data.table for chosen_actions for notation of subj. optimal actions
  # these are selectors for the actions to be executed by an agent
  # gets manipulated and rebuilt over course of each round 
  # agent_id, action_type, best_action
  sim$chosen_actions <- tibble(

    agent_id = rep(agent_characteristics$agent_id, each=2),
    action_type = rep(c("actions_overall", "actions_send"), sim$no_agents),
    best_action = rep(c("Not assigned"), length(action_type))

  ) %>% data.table() %>%
    .[ , agent_id := as.integer(agent_id) ]
  
  #### sim$chosen_actions table specs at this point:
  # rowlength: sim$no_agents*2
  # columns: agent_id (integer), action_type (character), best_action (character)

  ######################################### 
  #### CONSTRUCT OVERALL_ACTIONS TABLE ####
  #########################################

  # make data.table for the overall actions: Send, Receive, Both, Nothing
  # init version of table has max score for "Both", so every agent sends and receives
  sim$actions_overall <- tibble(

    agent_id = rep(agent_characteristics$agent_id, each=4),
    actions = rep(c("Send", "Receive", "Both", "Nothing"), no_agents),
    util_score = rep(0, length(actions))

  ) %>%
    data.table() %>%
    .[ actions == "Both", util_score := 1 ] %>%
    # cast into wide format, so max_utils are "computed" more easily
    dcast(agent_id ~ actions, value.var = "util_score") %>%
    .[ , agent_id := as.character(agent_id)] %>%
    .[, best_action :=  names(.[ , -c("agent_id")])[apply(.[ , -c("agent_id") ], 1, which.max)]] %>%
    # remelt into long format
    melt( id.vars = c("agent_id", "best_action"),
          measure.vars = c("Send", "Receive", "Both", "Nothing"),
          variable.name = "actions",
          value.name = "util_score" ) %>%
    .[ , agent_id := as.integer(agent_id) ]
  
  #### sim$actions_overall table specs at this point:
  # rowlength: sim$no_agents*4
  # columns: agent_id (integer), actions (character), util_score (numeric), best_action (character)

  #######################################################
  #### ASSIGN BEST OVERALL ACTIONS TO CHOSEN ACTIONS ####
  #######################################################

  # the action_type is set to actions_overall for all observations
  # then the best action column for all rows with action_type actions_overall is assigned where applicable
  # the information in this table at this point gives the best action for overall course of action for each agent
  sim$chosen_actions <- copy(sim$actions_overall)[ actions == best_action , .(agent_id, best_action) ] %>%
    merge(sim$chosen_actions, by=c("agent_id"), all.x=TRUE, all.y=TRUE) %>%
    .[ action_type == "actions_overall" , best_action := ifelse(!is.na(best_action.x), best_action.x,  "NOT ASSIGNED")] %>% 
    .[ , -c("best_action.x", "best_action.y")]

  #### sim$chosen_actions table specs at this point:
  # rowlength: sim$no_agents*2
  # columns: agent_id (integer), action_type (character), best_action (character)
  
  #############################################
  #### CONSTRUCT TABLE FOR SENDING ACTIONS ####
  #############################################

  sim$actions_send <- tibble(

    agent_id = rep(agent_characteristics$agent_id, each=2),
    actions = rep(c("Unoptimized", "Optimized"), no_agents),
    util_score = rep(0, length(actions))

  ) %>%  data.table() %>%
    .[ , agent_id := as.integer(agent_id) ]
  
  #### sim$actions_send table specs at this point:
  # rowlength: sim$no_agents*2
  # columns: agent_id (integer), actions (character), util_score (integer)

  #################################
  #### CONSTRUCT MESSAGE TABLE ####
  #################################

  # acts in two steps
  # 1) construct table from environment and reverse edge directions
  # 2) do the same again without reversal
  # this is done because every agent has to be able to send a message to each neigbhor (and thus receive them)

  # 1.)
  sim$messages <- sim$environment %>%
    activate(edges) %>%
    as_tibble() %>%
    data.table() %>%
    setnames(old = c("from", "to"), new = c("receiver", "sender"))

  # 2.)
  sim$messages <- sim$environment %>%
    activate(edges) %>%
    as_tibble() %>%
    data.table() %>%
    setnames(old = c("from", "to"), new = c("sender", "receiver")) %>%
    rbind(messages) %>%
    .[copy(sim$agent_characteristics)[, .(agent_id, opinion)], nomatch = 0L, on = c("sender" = "agent_id"), allow.cartesian=TRUE] %>%
    setnames("opinion", "opinion_sender")
  
  #### sim$messages table specs at this point
  # rowlength: ( sim$environment %>% as_tibble() %>% nrow() )*2
  # columns: from (int), to (int), opinion_from (int)

  ###################################
  #### PRODUCE POSSIBLE MESSAGES ####
  ###################################

  # in init round this is just a uniformly random chosen number on the interval [0,1]
  # the vector of random draws functions as egos assumption about alters opinion
  assumption_receiver <- runif(nrow(sim$messages), 0, 1)
  sim$messages <- cbind(sim$messages, assumption_receiver)

  # make matrix of all possible optimized messages
  # the outer product of the opinion and assumption vector is constructed by applying the function produce_altered_message
  sim$message_matrix <- outer(sim$messages$opinion_sender, sim$messages$assumption_receiver, produce_altered_message) 
  row.names(sim$message_matrix) <- sim$messages[ , sender]
  colnames(sim$message_matrix) <- sim$messages[ , receiver]
  
  # the message matrix is melted into a long format
  # entries where to == from are excluded
  # this table is then joined with sim$messages
  # agents send messages optimized for their whole neighborhood: thus opt_message is the mean of all optimized messages per agent
  # if this optimized message exceeds epsilon, the maximum possible deviation of opinion_from is used 
  sim$messages <- copy(sim$message_matrix) %>%
    data.table() %>%
    .[ , sender := as.numeric(row.names(sim$message_matrix))] %>%
    melt( id.vars = c("sender"),
          measure.vars = as.character(seq(1, sim$no_agents, 1)),
          variable.name = "receiver",
          value.name = "opt_message" ) %>%
    .[ receiver != sender] %>%
    setkey("sender") %>%
    unique() %>%
    .[ , sender := as.integer(sender)] %>%
    .[ , receiver := as.integer(receiver)] %>%
    .[copy(unique(sim$messages)), on=c("sender", "receiver"), nomatch = 0L, allow.cartesian = TRUE] %>%
    .[ , opt_message := median(opt_message), by = .(sender)] 
  
  #### sim$messages table specs at this point:
  # rowlength: ( sim$environment %>% as_tibble() %>% nrow() )*2
  # columns: from (int), to (int), opinion_from (int), opt_message (int)

  ##########################################
  #### CONSTRUCT DISCOURSE MEMORY TABLE ####
  ##########################################

  # at the end of Init, past_messages will be assigned too
  sim$discourse_memory <- copy(sim$messages) %>%
    .[ , past_opinions := sapply(opinion_sender, function(x) {list(x)} )] %>%
    setnames("opinion_sender", "opinion") %>%
    .[ , .(sender, opinion, past_opinions)] %>%
    .[ , past_opinions := as.character(past_opinions) ] %>% # unique() can't handle list columns, so first transform to character
    unique() %>%
    .[ , past_opinions := sapply(past_opinions, function(x) list(eval(parse(text = x))))] %>% # then transform back
    .[ , sender_business := 0 ] %>%
    .[ , receiver_business := 0 ]

  #### sim$discourse_memory table specs at this point:
  # rowlength: ( sim$environment %>% as_tibble() %>% nrow() )*2
  # columns: from (int), opinion (int), past_opinions (list), sender_business (int), receiver_business (int)

  
  #### Control flow: Are there any agents that send in this round?
  # if yes, sending actions are executed for those; if no, only execute receiving actions 
   if( length(sim$actions_overall[ best_action %in% c("Both", "Send") , best_action ] > 0 ) ) {

    #######################################
    #### DETERMINE BEST SENDING ACTION ####
    #######################################

    # This is a complicated sequence, but it comes down to 6 steps
    # Step 1: Merge sim$messages with sim$actions_overall and filter out all non-sending agent rows
    # Step 2: Merge with sim$actions_send to get the columns actions and util_score
    # Step 3: Merge with sim$discourse_memory to get columns past_opinions, sender_business, receiver_business
    # Step 4: Compute distance_to_past_opinions and distance_to_past_messages as mean of per round distances over time - sim$discourse_memory_depth as well as the distance between messages and opinions and assumptions and opinions
    # Step 5: Compute util_score row-wise as 0 - all the distances
    # Step 6: Cast into wide format to select best action more easily by score and then melt back into long format
    sim$actions_send <- copy(sim$messages) %>%
      .[copy(sim$actions_overall), on = c("sender" = "agent_id"), nomatch = 0L, allow.cartesian = TRUE ] %>%
      .[ best_action == actions ] %>% # filter irrelevant rows with not chosen actions out
      .[ (best_action == "Both" | best_action == "Send") ] %>% # only rows with both or send
      .[ , .(sender , receiver , opt_message , opinion_sender , assumption_receiver) ] %>%
      unique() %>%
      .[copy(sim$actions_send), on = c("sender" = "agent_id"), nomatch = 0L, allow.cartesian = TRUE ] %>%
      .[copy(sim$discourse_memory), on = c("sender"), nomatch = 0L, allow.cartesian = TRUE] %>%
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
      }, a=past_opinions, b=opt_message, c=opinion_sender, k=actions)] %>%
      .[, distance_message_opinion := mapply(function(a,b,k) {
        switch(k,
               "Unoptimized" = {
                 abs(a - a)
               },
               "Optimized" = {
                 abs(a - b)
               }
        )
      }, a=opinion_sender, b=opt_message, k=actions)] %>%
      .[, distance_message_assumption := mapply(function(a,b,c,k) {
        switch(k,
               "Unoptimized" = {
                 abs(c - a)
               },
               "Optimized" = {
                 abs(c - b)
               }
        )
      }, a=opinion_sender, b=opt_message, c=assumption_receiver, k=actions)] %>%
      .[ , -c("past_opinions", "receiver")] %>% # include past_messages here in the step function
      .[ , util_score := 0 - distance_to_past_opinions - distance_message_opinion - distance_message_assumption] %>% 
      .[ , agent_id := sender ] %>%
      .[ , .(agent_id, actions, util_score)] %>%
      setkey("agent_id") %>%
      unique() %>%
      .[, util_score := sum(util_score), by=c("agent_id", "actions")] %>%
      unique() %>%
      dcast(agent_id ~ actions, value.var = "util_score") %>%
      .[, best_action :=  ifelse(Optimized > Unoptimized, "Optimized", "Unoptimized")] %>%
      melt( id.vars = c("agent_id", "best_action"),
            measure.vars = c("Optimized", "Unoptimized"),
            variable.name = "actions",
            value.name = "util_score" ) %>%
      .[ , agent_id := as.integer(agent_id) ]

 
    #### sim$actions_send table specs at this point:
    # rowlength: sim$no_agents*2 (in later rounds it will be the count of agents who chose a strategy that encompasses sendin)
    # columns: agent_id (integer), best_action (character), actions (character), util_score (numeric)

    #####################################
    #### UPDATE CHOSEN_ACTIONS TABLE ####
    #####################################

    # merge with actions_send, reduce to relevant columns and rows
    # merge with sim$chosen_actions and update best_action for the value actions_send in action_type
    sim$chosen_actions <- copy(sim$actions_send)[ actions == best_action , .(agent_id, best_action) ] %>%
      merge(sim$chosen_actions, by="agent_id", all.x=TRUE, all.y=TRUE) %>%
      .[ best_action.y == "Receive" , action_type := "actions_overall" ] %>%
      .[ best_action.y == "Nothing" , action_type := "actions_overall" ] %>%
      .[ action_type == "actions_send" , best_action := ifelse(!is.na(best_action.x), best_action.x, "NOT ASSIGNED")] %>%
      .[ action_type == "actions_overall" , best_action := ifelse(!is.na(best_action.y), best_action.y, "NOT ASSIGNED")] %>%
      .[ , -c("best_action.x", "best_action.y")]
    
    #### sim$chosen_actions table specs at this point:
    # rowlength: sim$no_agents*2
    # columns: agent_id (integer), action_type (character), best_action (character)

    #################################
    #### UPDATE DISCOURSE_MEMORY ####
    #################################

    # update with all energy and message / opinion information up to t minus the relevant memory depth
    # take sim$actions_send, merge with sim$messages so that the messages reflect the messages by "from"
    # reduce to rows where the best action is taken
    # Step 3: Compute the business measures in this round
    # Step 4: Append messages and opinions to respective memory lists
    # Step 5: Remerge with itself to get receiver_business
    # Step 6: Append business and cohesion measures to respective memory lists
    sim$discourse_memory <- copy(sim$actions_send)[ , -c("util_score")] %>%
      .[copy(sim$messages), on = c("agent_id" = "sender"), nomatch = 0L, allow.cartesian = TRUE] %>%
      setnames("agent_id", "sender") %>%
      setkey("sender") %>%
      .[ actions==best_action ] %>%
      .[ , -c("actions")]  %>% # we need best_action further down
      unique() %>% # do something about business
      .[ , sender_business := .N, by = "sender" ] %>% # generate business
      .[ , sender_business := ifelse(best_action == "Optimized", sender_business*2, sender_business) ] %>% # generate business
      .[ , receiver_business := .N, by = "receiver" ] %>% # generate business
      merge(sim$discourse_memory[ , -c("sender_business", "receiver_business", "assumption_receiver")], by=c("sender"), all.x=TRUE)%>% # get full row count back, following two lines resolve variable name conflicts
      .[ , sender_business := ifelse(is.na(sender_business), 0, sender_business) ] %>% # generate business
      .[ , receiver_business := ifelse(is.na(receiver_business), 0, receiver_business) ] %>% # generate business
      .[ , past_messages := ifelse(.[ , best_action] == "Unoptimized",
                                   list(opinion_sender[[1]]),
                                   list(opt_message[[1]]))] %>%
      .[ , past_opinions := ifelse(lengths(past_opinions) < params(sim)$rc_energy_model$opinion_memory_depth,
                                   mapply(function(x, y) {
                                     list(c(unlist(x), y))
                                   }, x=past_opinions, y=opinion_sender),
                                   mapply(function(x, y) {
                                     list(c(unlist(x)[1:params(sim)$rc_energy_model$opinion_memory_depth], y))
                                   }, x=past_opinions, y=opinion_sender)
      )] %>%
      .[ , distance_to_past_opinions := mapply(function(a,b) {
        mean(
          sapply(a, function(x) {
            abs(x - b)
          })
        )
      }, a=past_opinions, b=assumption_receiver)] %>%
      setnames("opinion_sender", "opinion") %>%
      setnames("opt_message", "message") %>%
      .[ , .(sender, receiver, opinion, message, past_messages, past_opinions, distance_to_past_opinions, sender_business, receiver_business)] %>%
      .[ , past_messages := as.character(past_messages) ] %>%
      .[ , past_opinions := as.character(past_opinions) ] %>%
      unique() %>%
      .[ , past_messages := sapply(past_messages, function(x) list(eval(parse(text = x))))] %>%
      .[ , past_opinions := sapply(past_opinions, function(x) list(eval(parse(text = x))))]

    temp <- copy(sim$discourse_memory)[ , .(receiver, receiver_business)] %>% 
	    .[ , receiver_business := max(receiver_business), by=receiver] %>%
	    unique() 

    sim$discourse_memory <- copy( sim$discourse_memory)[ , -c("receiver_business")]  %>%
      merge(temp, by.x = c("sender"), by.y = c("receiver"), all.x = TRUE) %>% # remerge it with receiver_business-less version to correspond to receiver business
      .[ , past_receiver_business := ifelse( !is.na(receiver_business), sapply(receiver_business, function(x) list(x) ), 0 ) ] %>%
      .[ , past_sender_business := ifelse( !is.na(sender_business), sapply(sender_business, function(x) list(x) ), 0 ) ] %>%
      .[ , nbh_incohesion := vec_get_nbh_incohesion(sender ) ] %>%
      .[ , past_nbh_incohesion := ifelse( !is.na(nbh_incohesion), sapply(nbh_incohesion, function(x) list(x) ), 0 ) ] %>%
      .[ , self_incohesion := vec_get_self_incohesion( sender ) ] %>%
      .[ , past_self_incohesion := ifelse( !is.na(self_incohesion), sapply(self_incohesion, function(x) list(x) ), 0 ) ] %>%
      .[ , past_messages := as.character(past_messages) ] %>%
      .[ , past_opinions := as.character(past_opinions) ] %>%
      .[ , past_receiver_business := as.character(past_receiver_business) ] %>%
      .[ , past_sender_business := as.character(past_sender_business) ] %>% 
      .[ , past_nbh_incohesion := as.character(past_nbh_incohesion) ] %>%
      .[ , past_self_incohesion := as.character(past_self_incohesion) ] %>%
      unique() %>%
      .[ , past_messages := sapply(past_messages, function(x) list(eval(parse(text = x))))] %>%
      .[ , past_opinions := sapply(past_opinions, function(x) list(eval(parse(text = x))))] %>%
      .[ , past_receiver_business := sapply(past_receiver_business, function(x) list(eval(parse(text = x))))] %>%
      .[ , past_sender_business := sapply(past_sender_business, function(x) list(eval(parse(text = x))))] %>%
      .[ , past_nbh_incohesion := sapply(past_nbh_incohesion, function(x) list(eval(parse(text = x))))] %>%
      .[ , past_self_incohesion := sapply(past_self_incohesion, function(x) list(eval(parse(text = x))))] 
    
    #### sim$discourse_memory table specs at this point
    # rowlength: sim$no_agents**2
    # columns: from (int), to (int), opinion (num), message (num), past_messages (list), past_opinions (list), distance_to_past_opinions (list), sender_business (numeric), receiver_business (numeric), past_receiver_business (list), past_sender_business (list), nbh_incohesion (numeric), past_nbh_incohesion, self_incohesion (numeric), past_self_incohesion (numeric)

    #########################
    #### "SEND" MESSAGES ####
    #########################

    # Due to the functional approach, "sending" is simply the placement of optimized or unoptimized messages (depending on chosen action) in a dedicated column
    # sim$messages becomes a bloated all-purpose table at this point that is meant as a set-up for the opinion updating in the receiving stage; it is essentially a combination of sim$messages and sim$actions_send with some additional aspects (see above)
    # In following rounds, sim$messages will be deleted and newly constructed at the start of every round
    # Step 1: Copy sim$messages, reverse labeling on "from" and "to" and give opinions and messages new name so there is no collide when merging
    # Step 2: Merge with actions_send and use best_action to place the respective values from the renamed columns in assumption_to 
    sim$messages <- copy(sim$messages) %>%
      unique() %>%
      setnames(old = c("sender", "receiver"), new = c("receiver", "sender")) %>%
      .[ , .(receiver, opinion_sender, opt_message)] %>%
      .[ , opinion_receiver := opinion_sender] %>%
      .[sim$messages[ , -c("opt_message") ], on="receiver", nomatch=0L, allow.cartesian=TRUE] # WORKS

    sim$messages <- copy(sim$messages) %>%
      .[sim$actions_send[ , .(agent_id, actions, best_action)], nomatch=0L, on=c("sender" = "agent_id"), allow.cartesian=TRUE] %>%
      .[ , assumption_sender := ifelse(.[, best_action] == "Unoptimized", opinion_sender, opt_message)] %>%
      setkey("sender") %>%
      unique()

    #### sim$messages table specs at this point:
    # rowlength: ( sim$environment %>% as_tibble() %>% nrow() )*2*2 (due to building cartesian product of sim$actions_send and sim$messages; as the number of rows varies for actions_send, this will also vary in further steps)
    # columns: to (integer), from (integer), opt_message (numeric), opinion_from (numeric), assumption_to (numeric), actions (character), best_action (character)

    ###################
    #### RECEIVING ####
    ###################
    
    # Step 1: Delete actions information from sim$messages
    # Step 2: Merge with actions_overall
    # Step 3: Reduce to agent rows that chose "Both" or "Receive"
    # Step 4: Merge with sim$discourse_memory to be able to compute distance_to_past_opinions
    # Step 5: Now we filter out rows with epsilon and distance values above the threshold - the correct assumption_to is already present through sim$messages; this means although the receiver is labeled "from", he actually is in the role of "to" for these operations; this implementation might be a problem for directed networks and need a further merge (or the construction of sim$messages does not double the edges in that instance)
    # Step 6: The appropriate values are summed and new opinions are formed via averaging
    # Step 7: The table is prepared for processing in sim$agent_characteristics
    sim$opinion_updating <- copy(sim$messages)[ , -c("actions", "best_action")] %>%
      unique() %>%
      .[copy(actions_overall), on = c("receiver" = "agent_id"), nomatch = 0L, allow.cartesian = TRUE ] %>% # table is number of edges*8 x 8
      .[ best_action == actions ] %>% # table is no_agents*2 x 8 with sim$no_agents values in variable "from"
      .[ (best_action == "Both" | best_action == "Receive") ] %>% # table is 1000 x 8 (condition 1 is true for all agents in init)
      .[ , .(sender, receiver, opinion_from, assumption_to, opt_message, actions, best_action)] %>%
      .[sim$discourse_memory, on=c("from"), nomatch = 0L, allow.cartesian = TRUE] %>%
      .[ best_action == actions ] %>%
      .[ , distance_to_past_opinions := mapply(function(a,b) {
        mean(
          sapply(a, function(x) {
            abs(x - b)
          })
        )
      }, a=past_opinions, b=assumption_to)] %>%
      .[ , within_epsilon := abs(opinion_from - assumption_to) < params(sim)$rc_energy_model$epsilon] %>%
      .[ , self_consistent := distance_to_past_opinions < params(sim)$rc_energy_model$self_incons_tolerance] %>%
      .[ within_epsilon == TRUE & self_consistent == TRUE ] %>% # at this stage, some agents might be completely deleted from the table, because no messages sent to them fall within the bounds; this has no impact, e.g. for business computation later as it is corrected by merging with sim$discourse_memory (see UPDATE DISCOURSE_MEMORY); sim$opinion_updating only comes into play for assigning the new opinions to sim$agent_characteristics
      .[ , sum_assumptions := sum(assumption_to), by=from] %>%
      .[ , denominator := .N, by=from ] %>%
      .[ , reception_energy_loss := .N, by=from ] %>%
      .[ , opinion_y := ifelse( denominator != 0, sum_assumptions / denominator, opinion_from )] %>%
      setnames("from", "agent_id") %>%
      .[ , .(agent_id, opinion_y)] %>%
      setkey("agent_id") %>%
      unique() 

    #### sim$opinion_updating table specs at this point:
    # rowlength: no_agents*2 - rows that do not match the conditions
    # columns: agent_id (integer), opinion_y (numeric)

    #################################
    #### UPDATE DISCOURSE_MEMORY ####
    #################################
    
    # This line of code replicates the output for sim$opinion_updating to get measures of receiver_business (even if an agent chooses to receive, it is not clear how many messages it will receive) 
    sim$discourse_memory <- copy(sim$messages)[ , -c("actions", "best_action")] %>%
      unique() %>%
      .[copy(sim$actions_overall), on = c("to" = "agent_id"), nomatch = 0L, allow.cartesian = TRUE ] %>%
      .[ best_action == actions ] %>%
      .[ (best_action == "Both" | best_action == "Receive") ]  %>%
      .[ , .(from, to, opinion_from, assumption_to, opt_message, actions, best_action)] %>%
      .[copy(sim$discourse_memory), on=c("from"), nomatch = 0L, allow.cartesian = TRUE] %>%
      .[ best_action == actions ] %>%
      .[ , distance_to_past_opinions := mapply(function(a,b) {
        mean(
          sapply(a, function(x) {
            abs(x - b)
          })
        )
      }, a=past_opinions, b=assumption_to)] %>%
      .[ , within_epsilon := abs(opinion_from - assumption_to) < params(sim)$rc_energy_model$epsilon] %>%
      .[ within_epsilon == TRUE ] %>%
      .[ , .(from, to) ] %>%
      unique() %>%
      .[ , receiver_business := .N, by=from ] %>%
      .[copy(sim$discourse_memory)[ , -c("receiver_business") ], on=c("from", "to") ] %>%
      .[ , receiver_business := ifelse(is.na(receiver_business), 0, receiver_business) ] 

    #### sim$discourse_memory table specs at this point:
    # rowlength: ( sim$environment %>% activate(edges) %>% as_tibble %>% nrow )*2 (in case of undirected)
    # columns: from (integer), to (integer), receiver_business (numeric), opinion (numeric), message (numeric), past_messages, past_opinions, distance_to_past_opinions, sender_business, past_receiver_business, past_sender_business, nbh_incohesion, past_nbh_incohesion, self_incohesion, past_self_incohesion (all numeric)

    ######################################
    #### UPDATE agent_characteristics ####
    ######################################

    # merge with sim$opinion_updating, assign new opinion if applicable
    sim$agent_characteristics <- merge(sim$agent_characteristics, sim$opinion_updating, by="agent_id", all.x = TRUE) %>% # produces NAs for not repoduced rows
      .[ , opinion := ifelse(is.na(opinion_y), opinion, opinion_y)] %>%
      .[ , -c("opinion_y")]

    #### sim$agent_characteristics table specs at this point:
    # rowlength: sim$no_agents
    # columns: agent_id (integer), neighborhood (list), nbh_size (integer), opinion (numeric), energy (numeric)

    # update energy_loss depending on chosen strategies (overall and sending)
    # information is taken from sim$actions_overall and sim$actions_send to get the chosen actions
    # sim$discourse_memory holds the actual measures of receiver and sender business (how many messages were processed and sent)
    # energy_loss is then computed depending on the chosen actions and summed per agent id
    # after a merge with sim$agent_characteristics, current energy can be computed as a sum of current energy, loss and the global restoration factor 
    sim$agent_characteristics <- copy(sim$actions_overall)[ , best_axn_overall := best_action][ , .(agent_id, best_axn_overall)] %>%
      unique() %>%
      merge(copy(sim$actions_send)[ , best_axn_send := best_action][ , .(agent_id, best_axn_send)], by="agent_id", allow.cartesian = TRUE) %>%
      unique() %>%
      merge(copy(sim$discourse_memory)[ , .(from, receiver_business, sender_business)], by.x = "agent_id", by.y = "from", allow.cartesian = TRUE) %>%
      unique() %>%
      .[ best_axn_overall == "Send" , energy_loss := sender_business ] %>%
      .[ best_axn_overall == "Receive", energy_loss := receiver_business ] %>%
      .[ best_axn_overall == "Both", energy_loss := sender_business + receiver_business ] %>%
      .[ best_axn_overall == "Nothing", energy_loss := 0 ] %>%
      .[ best_axn_send == "Unoptimized", energy_loss := energy_loss ] %>%
      .[ best_axn_send == "Optimized", energy_loss := energy_loss + sender_business ] %>%
      .[ , .(agent_id, energy_loss) ] %>%
      unique() %>%
      .[ , energy_loss := sum(energy_loss), by="agent_id" ] %>%
      unique() %>%
      .[sim$agent_characteristics, on="agent_id"] %>%
      .[ , energy := energy - energy_loss + params(sim)$rc_energy_model$restoration_factor ] %>%
      .[ , -c("energy_loss") ]
 
    #### sim$agent_characteristics table specs at this point:
    # rowlength: sim$no_agents
    # columns: agent_id (integer), neighborhood (list), nbh_size (integer), opinion (numeric), energy (numeric)

    ##################################################
    #### BUSINESS UPDATE FOR sim$discourse_memory ####
    ##################################################

    # here, the new opinion present in sim$agent_characteristics is reassigned to sim$discourse_memory by merging
    sim$discourse_memory <- sim$agent_characteristics[ , .(agent_id, opinion) ] %>%
      .[sim$discourse_memory[ , -c("opinion")], on=c("agent_id" = "from") ] %>%
      setnames("agent_id", "from") %>%
      .[ , past_receiver_business := ifelse(lengths(past_receiver_business) < params(sim)$rc_energy_model$energy_params_memory_depth,
                                            mapply(function(x, y) {
                                              list(c(unlist(x), y))
                                            }, x=past_receiver_business, y=receiver_business),
                                            mapply(function(x, y) {
                                              list(c(unlist(x)[1:params(sim)$rc_energy_model$opinion_memory_depth], y))
                                            }, x=past_receiver_business, y=receiver_business)
      )] %>%
      .[ , past_sender_business := ifelse(lengths(past_sender_business) < params(sim)$rc_energy_model$energy_params_memory_depth,
                                          mapply(function(x, y) {
                                            list(c(unlist(x), y))
                                          }, x=past_sender_business, y=sender_business),
                                          mapply(function(x, y) {
                                            list(c(unlist(x)[1:params(sim)$rc_energy_model$opinion_memory_depth], y))
                                          }, x=past_sender_business, y=sender_business)
      )] 

    #### sim$discourse_memory table specs at this point:
    # rowlength: ( sim$environment %>% activate(edges) %>% as_tibble %>% nrow )*2 (in case of undirected)
    # columns: from (integer), to (integer), receiver_business (numeric), opinion (numeric), message (numeric), past_messages, past_opinions, distance_to_past_opinions, sender_business, past_receiver_business, past_sender_business, nbh_incohesion, past_nbh_incohesion, self_incohesion, past_self_incohesion (all numeric)

  } else {
    
    #################################################
    #### UPDATING OF ENERGY IN CASE OF NO ACTION ####
    #################################################

    # the past_receiver_business and past_sender_business needs to be updated with 0 business for this round
    sim$discourse_memory <- copy(sim$discourse_memory) %>%
      .[ , receiver_business := 0 ] %>%
      .[ , sender_business := 0 ] %>%
      .[ , past_receiver_business := ifelse(lengths(past_receiver_business) < params(sim)$rc_energy_model$energy_params_memory_depth,
                                            mapply(function(x, y) {
                                              list(c(unlist(x), y))
                                            }, x=past_receiver_business, y=receiver_business),
                                            mapply(function(x, y) {
                                              list(c(unlist(x)[1:params(sim)$rc_energy_model$energy_params_memory_depth], y))
                                            }, x=past_receiver_business, y=receiver_business)
      )] %>%
      .[ , past_sender_business := ifelse(lengths(past_sender_business) < params(sim)$rc_energy_model$energy_params_memory_depth,
                                          mapply(function(x, y) {
                                            list(c(unlist(x), y))
                                          }, x=past_sender_business, y=sender_business),
                                          mapply(function(x, y) {
                                            list(c(unlist(x)[1:params(sim)$rc_energy_model$energy_params_memory_depth], y))
                                          }, x=past_sender_business, y=sender_business)
      )]

    # sim$agent_characteristics$energy is not diminished, but the amount of the restoration factor is added
    sim$agent_characteristics <- sim$agent_characteristics %>%
      .[ , energy := energy + params(sim)$rc_energy_model$restoration_factor ]

  }

  return(invisible(sim))

}

rc_energy_modelStep <- function(sim) {

  print(time(sim))
  
  ########################
  #### REBUILD TABLES ####
  ########################

  # tables that are rebuilt at the start of each round: sim$actions_send, sim$actions_overall, sim$messages

  sim$actions_send <- tibble(
    
    agent_id = rep(sim$agent_characteristics$agent_id, each=2),
    actions = rep(c("Unoptimized", "Optimized"), no_agents),
    util_score = rep(0, length(actions))
    
  ) %>%  data.table() %>%
    .[ , agent_id := as.integer(agent_id) ]
  
  sim$actions_overall <- tibble(

    agent_id = rep(sim$agent_characteristics$agent_id, each=4),
    actions = rep(c("Send", "Receive", "Both", "Nothing"), sim$no_agents),
    util_score = rep(0, length(actions))

  ) %>% data.table()

  sim$messages <- sim$environment %>%
    activate(edges) %>%
    as_tibble() %>%
    data.table() %>%
    setnames(old = c("from", "to"), new = c("to", "from"))

  # current opinions are copied from sim$agent_characteristics
  sim$messages <- sim$environment %>%
    activate(edges) %>%
    as_tibble() %>%
    data.table() %>%
    rbind(sim$messages) %>%
    .[copy(sim$agent_characteristics)[, .(agent_id, opinion)], nomatch = 0L, on = c("from" = "agent_id"), allow.cartesian=TRUE] %>%
    setnames("opinion", "opinion_from")

  ######################################## 
  #### CONSTRUCT CHOSEN_ACTIONS TABLE ####
  ########################################

  # make data.table for chosen_actions for notation of subj. optimal actions
  # these are selectors for the actions to be executed by an agent
  # gets manipulated and rebuilt over course of each round 
  # agent_id, action_type, best_action
  sim$chosen_actions <- tibble(

    agent_id = rep(agent_characteristics$agent_id, each=2),
    action_type = rep(c("actions_overall", "actions_send"), sim$no_agents),
    best_action = rep(c("Not assigned"), length(action_type))

  ) %>% data.table() %>%
    .[ , agent_id := as.integer(agent_id) ]
  
  #### sim$chosen_actions table specs at this point:
  # rowlength: sim$no_agents*2
  # columns: agent_id (integer), action_type (character), best_action (character)


  #################################################################
  #### BUILD sim$actions_overall AND FIND BEST OVERALL ACTIONS ####
  #################################################################

  # The code works in X steps
  # Step 1: Get all the necessary records of the discourse from sim$discourse_memory
  # Step 2: Build measures from agent memory
  # Step 3: Compute utilities
  # Step 4: Select max by first casting to wide and then melting back to long
  sim$actions_overall <- copy(sim$discourse_memory) %>%
    .[ , max_send_energy_loss := .N*2, by = from ] %>%
    .[ , max_receive_energy_loss := .N, by = to ] %>%
    .[ , .(from, past_self_incohesion, past_nbh_incohesion, past_receiver_business, past_sender_business, nbh_incohesion, self_incohesion, max_send_energy_loss, max_receive_energy_loss)] %>%
    .[(sim$agent_characteristics[ , .(agent_id, energy)] %>% unique()), on=c("from" = "agent_id") ] %>%
    .[ , psi_mean_index := mapply(function(x) {

      is_na <- sum(unlist(x)) / length(unlist(x))

      ifelse(is.na(is_na), 0, is_na)

    }, x=past_self_incohesion )] %>%
    .[ , pni_mean_index := mapply(function(x) {
 
      is_na <- sum(unlist(x)) / length(unlist(x))
      
      ifelse(is.na(is_na), 0, is_na)

    }, x=past_nbh_incohesion )] %>%
    .[ , rec_business_mean_index := mapply(function(x, y) {

      is_na <- sum(unlist(x)) / y
      
      ifelse(is.na(is_na), 0, is_na)

    }, x=past_receiver_business, y=max_receive_energy_loss )] %>%
    .[ , send_business_mean_index := mapply(function(x, y) {

      is_na <- sum(unlist(x)) / y
      
      ifelse(is.na(is_na), 0, is_na)

    }, x=past_sender_business, y=max_send_energy_loss )] %>%
    .[ , rec_business_mean := mapply(function(x) {

      mean(unlist(x))

    }, x=past_receiver_business )] %>%
    .[ , send_business_mean := mapply(function(x) {

      mean(unlist(x))

    }, x=past_sender_business )] %>%
    .[ , both_business_mean := mapply(function(x, y) {

      sum( mean(unlist(x)), mean(unlist(y))) 

    }, x=past_sender_business, y=past_receiver_business )] %>%
    .[ , both_business_mean_index := mapply(function(x, y, z, a) {

      numerator <- sum( c(unlist(x), unlist(y)) )

      denominator <- z + a

      is_na <- numerator / denominator
      
      ifelse(is.na(is_na), 0, is_na)

    }, x=past_sender_business, y=past_receiver_business, z = max_receive_energy_loss, a = max_send_energy_loss )] %>%
    .[sim$actions_overall[ , -c("best_action")], on=c("from" = "agent_id"), allow.cartesian = TRUE] %>%
    setnames("from", "agent_id") %>%
    # Concept of these utility functions
    # The first part in brackets computes the fraction of the maximum energy an agent can have the agent will have if it chooses that particular strategy; the more energy the agent will have, the better; this is diminished by the second part in brackets: the higher the past percent business for that strategy is, the worse off is the probable amount of energy
    # the third part in brackets represents the possible self incohesion; this is also diminished by a factor constructed in the same way as above
    # the fourth part in brackets represents the possible neighborhood incohesion; this is also diminished by a factor constructed in the same way as above
    # what sign the third and fourth part have depends on the strategy: Sending does help with neighborhood incohesion, but not self incohesion while Receiving helps with high self incohesion (as the agent doesn't say anything this round), but can't help with high neighborhood incohesion
    .[ actions == "Send", util_score :=
         (( energy - send_business_mean) / ( energy + params(sim)$rc_energy_model$restoration_factor )) * (1 - send_business_mean_index) -
         ( self_incohesion * (psi_mean_index) ) +
         ( nbh_incohesion * (pni_mean_index) )] %>%
    .[ actions == "Receive", util_score := 
         (( energy - rec_business_mean) / ( energy + params(sim)$rc_energy_model$restoration_factor )) * (1 - rec_business_mean_index) +
         ( self_incohesion * (psi_mean_index) ) -
         ( nbh_incohesion * (pni_mean_index) )] %>%
    .[ actions == "Both", util_score :=  
         (( energy - both_business_mean) / ( energy + params(sim)$rc_energy_model$restoration_factor )) * (1 - both_business_mean_index) +
         ( self_incohesion * (psi_mean_index) * 0.5 ) +
         ( nbh_incohesion * (pni_mean_index) * 0.5 )] %>%
    .[ actions == "Nothing", util_score := ifelse( energy < vec_min(c(send_business_mean, rec_business_mean)), max(util_score)+100, min(util_score)-100) ] %>%
    .[ , -c("past_self_incohesion", "past_nbh_incohesion", "past_receiver_business", "past_sender_business") ] %>%
    unique() %>%
    dcast(agent_id ~ actions, value.var = "util_score", fun.aggregate = sum) %>%
    .[ , agent_id := as.character(agent_id)] %>%
    .[, best_action :=  names( .[ , -c("agent_id")] )[ apply(.[ , -c("agent_id") ], 1, which.max) ] ] %>%
    melt( id.vars = c("agent_id", "best_action"),
          measure.vars = c("Send", "Receive", "Both", "Nothing"),
          variable.name = "actions",
          value.name = "util_score" ) %>%
    .[ , agent_id := as.integer(agent_id) ]

  ###################################
  #### UPDATE sim$chosen_actions ####
  ###################################

  sim$chosen_actions <- copy(sim$actions_overall)[ actions == best_action , .(agent_id, best_action) ] %>%
    merge(sim$chosen_actions, by=c("agent_id"), all.x=TRUE, all.y=TRUE) %>%
    .[ is.na(action_type) , action_type := "actions_overall" ] %>%
    .[ action_type == "actions_overall" , best_action := ifelse(!is.na(best_action.x), best_action.x,  "NOT ASSIGNED")] %>% 
    .[ , -c("best_action.x", "best_action.y")]

  ##############################################################
  #### UPDATE sim$messages WITH ASSUMPTIONS ABOUT NEIGHBORS ####
  ##############################################################

  # using sim$discourse_memory

  sim$messages <-  sim$discourse_memory[ , .(from, to, message) ] %>%
    unique() %>%
    setnames( old=c("from", "to"), new=c("to", "from") ) %>%
    merge(sim$messages, by.x = c("from", "to"), by.y = c("from", "to") ) %>%
    setnames("message", "assumption_to") # works like a charm until here

  ####################################
  #### PRODUCE OPTIMIZED MESSAGES ####
  ####################################

  # make matrix of all possible optimized messages
  sim$message_matrix <- outer(sim$messages$opinion_from, sim$messages$assumption_to, produce_altered_message) # works
  row.names(sim$message_matrix) <- sim$messages[, from]
  colnames(sim$message_matrix) <- sim$messages[, to]

  sim$messages <- copy(sim$message_matrix) %>%
    data.table() %>%
    .[ , from := as.numeric(row.names(sim$message_matrix))] %>%
    melt( id.vars = c("from"),
          measure.vars = as.character(seq(1, sim$no_agents, 1)),
          variable.name = "to",
          value.name = "opt_message" ) %>%
    .[ to != from] %>%
    setkey("from") %>%
    unique() %>%
    .[ , from := as.integer(from)] %>%
    .[ , to := as.integer(to)] %>%
    .[copy(unique(sim$messages)), on=c("from", "to"), nomatch = 0L, allow.cartesian = TRUE] %>%
    .[ , opt_message := median(opt_message), by = "from" ] 
  
  #######################################################
  #### UPDATE sim$discourse_memory WITH NEW OPINIONS ####
  #######################################################

  # this is inefficient - new opinions were already assigned at the end of init / step
  # the appending to past_opinions could be transported elsewhere; it doesn't hurt to have it here, but it clutters the code

  sim$discourse_memory <- copy(sim$discourse_memory)[ , -c("opinion") ] %>%
    .[sim$agent_characteristics[ , .(agent_id, opinion) ], on=c("from" = "agent_id")] %>%
    .[ , past_opinions := ifelse(lengths(past_opinions) < params(sim)$rc_energy_model$opinion_memory_depth,
                                 mapply(function(x, y) {
                                   list(c(unlist(x), y))
                                 }, x=past_opinions, y=opinion),
                                 mapply(function(x, y) {
                                   list(c(unlist(x)[1:params(sim)$rc_energy_model$opinion_memory_depth], y))
                                 }, x=past_opinions, y=opinion)
    )] %>%
    .[ , -c("assumption_to", "opt_message")]


  if( length(sim$actions_overall[ best_action %in% c("Both", "Send") , best_action ] > 0 ) ) {
   
    ###############################################################
    #### COMPUTE SENDING UTILITIES AND CHOOSE MAX UTIL ACTIONS ####
    ###############################################################

    sim$actions_send <- copy(sim$messages) %>%
      .[copy(sim$actions_overall), on = c("from" = "agent_id"), nomatch = 0L, allow.cartesian = TRUE ] %>%
      .[ best_action == actions ] %>%
      .[ (best_action == "Both" | best_action == "Send") ] %>%
      .[ , .(from , to , opt_message , opinion_from , assumption_to)] %>%
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
      .[ , util_score := 0 - distance_to_past_messages - distance_to_past_opinions - distance_message_opinion - distance_message_assumption] %>% # include distance_to_past_messages in step function
      setnames("from", "agent_id") %>%
      .[, .(agent_id, actions, util_score) ] %>%
      setkey("agent_id") %>%
      unique() %>%
      .[, util_score := sum(util_score), by=c("agent_id", "actions")] %>%
      unique() %>%
      dcast(agent_id ~ actions, value.var = "util_score") %>%
      .[, best_action :=  ifelse(Optimized > Unoptimized, "Optimized", "Unoptimized")] %>%
      melt( id.vars = c("agent_id", "best_action"),
            measure.vars = c("Optimized", "Unoptimized"),
            variable.name = "actions",
            value.name = "util_score" ) %>%
      .[ , agent_id := as.integer(agent_id) ]

    ###################################
    #### UPDATE sim$chosen_actions ####
    ###################################

    sim$chosen_actions <- copy(sim$actions_send)[ actions == best_action , .(agent_id, best_action) ] %>%
      merge(sim$chosen_actions, by="agent_id", all.x=TRUE , all.y=TRUE) %>% 
      .[ best_action.y == "Receive" , action_type := "actions_overall" ] %>%
      .[ best_action.y == "Nothing" , action_type := "actions_overall" ] %>%
      .[ action_type == "actions_send" , best_action := ifelse(!is.na(best_action.x), best_action.x, "NOT ASSIGNED")] %>%
      .[ action_type == "actions_overall" , best_action := ifelse(!is.na(best_action.y), best_action.y, "NOT ASSIGNED")] %>%
      .[ , -c("best_action.x", "best_action.y")]

    print(table(sim$chosen_actions$best_action))

    #####################################
    #### UPDATE sim$discourse_memory ####
    #####################################

    sim$discourse_memory <- copy(sim$actions_send)[ , -c("util_score")] %>%
      .[copy(sim$messages), on = c("agent_id" = "from"), allow.cartesian = TRUE] %>%
      setnames("agent_id", "from") %>%
      setkey("from") %>%
      .[ actions==best_action | is.na(actions) ] %>%
      .[ , -c("actions")] %>% # we need best_action further down
      unique() %>% # do something about business
      .[ , sender_business := .N, by = "from" ] %>% # generate business
      .[ !is.na(best_action) , sender_business := ifelse(best_action == "Optimized", sender_business*2, sender_business) ] %>% # generate business
      .[ !is.na(best_action) , receiver_business := .N, by = "to" ] %>% # generate business
      merge(sim$discourse_memory[ , -c("sender_business", "receiver_business", "assumption_to")], by=c("from", "to"), all=TRUE) %>% # get full row count back, following two lines resolve variable name conflicts
      .[ , sender_business := ifelse(is.na(sender_business), 0, sender_business) ] %>% # generate business
      .[ , receiver_business := ifelse(is.na(receiver_business), 0, receiver_business) ] %>% # generate business
      .[ , past_messages := ifelse( is.na(best_action),
            past_messages,
            ifelse(lengths(past_messages) < params(sim)$rc_energy_model$message_memory_depth,
                                               ifelse(.[ , best_action] == "Unoptimized",
                                          mapply(function(x, y) {
                                            list(c(unlist(x), y))
                                          }, x=past_messages, y=opinion_from),
                                          mapply(function(x, y) {
                                            list(c(unlist(x), y))
                                          }, x=past_messages, y=opt_message)),
                                   ifelse(.[ , best_action] == "Unoptimized",
                                          mapply(function(x, y) {
                                            list(c(unlist(x)[1:params(sim)$rc_energy_model$message_memory_depth], y))
                                          }, x=past_messages, y=opinion_from),
                                          mapply(function(x, y) {
                                            list(c(unlist(x)[1:params(sim)$rc_energy_model$message_memory_depth], y))
                                          }, x=past_messages, y=opt_message)
                                   )
                                 )
      )] %>%
      #.[ , past_opinions := ifelse(lengths(past_opinions) < params(sim)$rc_energy_model$opinion_memory_depth,
      #                             mapply(function(x, y) {
      #                               list(c(unlist(x), y))
      #                             }, x=past_opinions, y=opinion_from),
      #                             mapply(function(x, y) {
      #                               list(c(unlist(x)[1:params(sim)$rc_energy_model$opinion_memory_depth], y))
      #                             }, x=past_opinions, y=opinion_from)
      #)
      #] %>% # past_opinions were already updated above because they are needed for sending action utility computation
      .[ , distance_to_past_opinions := mapply(function(a,b) {
        mean(
          sapply(a, function(x) {
            abs(x - b)
          })
        )
      }, a=past_opinions, b=assumption_to)] %>%
      setnames("opinion_from", "opinion") %>%
      setnames("opt_message", "message") %>%
      .[ , .(from, to, opinion, message, past_messages, past_opinions, distance_to_past_opinions, sender_business, receiver_business, past_sender_business, past_receiver_business, past_nbh_incohesion, past_self_incohesion)] %>%
      .[copy(sim$messages)[ , -c("opt_message", "assumption_to", "opinion_from")], on = c("from", "to"), nomatch = 0L, allow.cartesian = TRUE] %>%
      .[ , past_messages := as.character(past_messages) ] %>%
      .[ , past_opinions := as.character(past_opinions) ] %>%
      .[ , past_receiver_business := as.character(past_receiver_business) ] %>%
      .[ , past_sender_business := as.character(past_sender_business) ] %>%
      unique() %>%
      .[ , past_messages := sapply(past_messages, function(x) list(eval(parse(text = x))))] %>% 
      .[ , past_opinions := sapply(past_opinions, function(x) list(eval(parse(text = x))))] %>% 
      .[ , past_receiver_business := sapply(past_receiver_business, function(x) list(eval(parse(text = x))))] %>% 
      .[ , past_sender_business := sapply(past_sender_business, function(x) list(eval(parse(text = x))))] 

    temp <- copy(sim$discourse_memory)[ , .(to, receiver_business)] %>% 
	    .[ , receiver_business := max(receiver_business), by=to] %>%
	    unique() 
    
    sim$discourse_memory <-  copy( sim$discourse_memory)[ , -c("receiver_business")]  %>%
      merge(temp, by.x = c("from"), by.y = c("to"), all.x = TRUE) %>% # remerge it with receiver_business-less version to correspond to receiver business # remerge it with receiver_business-less version to correspond to receiver business
      .[ , nbh_incohesion := vec_get_nbh_incohesion(from) ] %>%
      .[ , self_incohesion := vec_get_self_incohesion( from ) ] %>%
      .[ , past_receiver_business := ifelse(lengths(past_receiver_business) < params(sim)$rc_energy_model$energy_params_memory_depth,
                                            mapply(function(x, y) {
                                              list(c(unlist(x), y))
                                            }, x=past_receiver_business, y=receiver_business),
                                            mapply(function(x, y) {
                                              list(c(unlist(x)[1:params(sim)$rc_energy_model$energy_params_memory_depth], y))
                                            }, x=past_receiver_business, y=receiver_business)
      )] %>%
      .[ , past_sender_business := ifelse(lengths(past_sender_business) < params(sim)$rc_energy_model$energy_params_memory_depth,
                                          mapply(function(x, y) {
                                            list(c(unlist(x), y))
                                          }, x=past_sender_business, y=sender_business),
                                          mapply(function(x, y) {
                                            list(c(unlist(x)[1:params(sim)$rc_energy_model$energy_params_memory_depth], y))
                                          }, x=past_sender_business, y=sender_business)
      )] %>%
      .[ , past_nbh_incohesion := ifelse(lengths(past_nbh_incohesion) < params(sim)$rc_energy_model$energy_params_memory_depth,
                                         mapply(function(x, y) {
                                           list(c(unlist(x), y))
                                         }, x=past_nbh_incohesion, y=nbh_incohesion),
                                         mapply(function(x, y) {
                                           list(c(unlist(x)[1:params(sim)$rc_energy_model$opinion_memory_depth], y))
                                         }, x=past_nbh_incohesion, y=nbh_incohesion)
      )] %>%
      .[ , past_self_incohesion := ifelse(lengths(past_self_incohesion) < params(sim)$rc_energy_model$energy_params_memory_depth,
                                          mapply(function(x, y) {
                                            list(c(unlist(x), y))
                                          }, x=past_self_incohesion, y=self_incohesion),
                                          mapply(function(x, y) {
                                            list(c(unlist(x)[1:params(sim)$rc_energy_model$opinion_memory_depth], y))
                                          }, x=past_self_incohesion, y=self_incohesion)
      )] %>%
      .[ , past_messages := as.character(past_messages) ] %>%
      .[ , past_opinions := as.character(past_opinions) ] %>%
      .[ , past_receiver_business := as.character(past_receiver_business) ] %>%
      .[ , past_sender_business := as.character(past_sender_business) ] %>%
      .[ , past_nbh_incohesion := as.character(past_nbh_incohesion) ] %>%
      .[ , past_self_incohesion := as.character(past_self_incohesion) ] %>%
      unique() %>%
      .[ , past_messages := sapply(past_messages, function(x) list(eval(parse(text = x))))] %>%
      .[ , past_opinions := sapply(past_opinions, function(x) list(eval(parse(text = x))))] %>%
      .[ , past_receiver_business := sapply(past_receiver_business, function(x) list(eval(parse(text = x))))] %>%
      .[ , past_sender_business := sapply(past_sender_business, function(x) list(eval(parse(text = x))))] %>%
      .[ , past_nbh_incohesion := sapply(past_nbh_incohesion, function(x) list(eval(parse(text = x))))] %>%
      .[ , past_self_incohesion := sapply(past_self_incohesion, function(x) list(eval(parse(text = x))))]
    
    sim$messages <- copy(sim$messages) %>%
      setnames(old=c("opinion_from", "opt_message"), new=c("opinion_from_y", "opt_message_y")) %>%
      unique() %>%
      setnames(old = c("from", "to"), new = c("to", "from")) %>%
      .[ , .(from, to, opinion_from_y, opt_message_y)] %>%
      merge(copy(sim$messages), by=c("from", "to"), all.x=TRUE, all.y=TRUE, allow.cartesian=TRUE) %>%
      merge(copy(sim$actions_send)[ , .(agent_id, best_action)], all=TRUE, by.x="from", by.y = "agent_id", allow.cartesian=TRUE) %>%
      .[ , assumption_to := ifelse( is.na(.[, best_action]), assumption_to, ifelse(.[, best_action] == "Unoptimized", opinion_from_y, opt_message_y))] %>%
      .[ , -c("opinion_from_y", "opt_message_y")] %>%
      setkey("from") %>%
      unique()
    
    # Receiving

# TO DO: CLEAR NOMENCLATURE FOR FROM, TO ETC.!!!!!!!

    sim$opinion_updating <- copy(sim$messages)[ , -c("actions", "best_action")] %>%
      unique() %>%
      .[copy(actions_overall), on = c("to" = "agent_id"), nomatch = 0L, allow.cartesian = TRUE ] %>% # you filter all from-to links out where to is not in actions_receive anymore NO WRONG
      .[ best_action == actions ] %>%
      .[ (best_action == "Both" | best_action == "Receive") ]  %>%
      .[ , .(from, to, opinion_from, assumption_to, opt_message, actions, best_action) ] %>% # as we only
      .[sim$discourse_memory, on=c("to"="from"), nomatch = 0L, allow.cartesian = TRUE] %>% # you need a different distance to past messages, merge on to for that; also, add receiver_business to the first merge
      .[ best_action == actions ] %>%
      .[ , distance_to_past_opinions := mapply(function(a,b) {
        mean(
          sapply(a, function(x) {
            abs(x - b)
          })
        )
      }, a=past_opinions, b=assumption_to)] %>%
      .[ , distance_to_past_messages := mapply(function(a,b) {
        max(
          sapply(a, function(x) {
            abs(x - b)
          })
        )
      }, a=past_messages, b=assumption_to)] %>%
      .[ , within_epsilon := abs(opinion_from - assumption_to) < params(sim)$rc_energy_model$epsilon] %>%
      .[ , self_consistent := distance_to_past_opinions < params(sim)$rc_energy_model$self_incons_tolerance] %>% 
      .[ , trust := distance_to_past_messages < params(sim)$rc_energy_model$other_incons_tolerance] %>%
      .[ within_epsilon == TRUE & self_consistent == TRUE & trust == TRUE ] %>%
      .[ , sum_assumptions := sum(assumption_to), by=to] %>%
      .[ , denominator := .N, by=to ] %>%
      .[ , reception_energy_loss := .N, by=to ] %>%
      .[ , opinion_y := ifelse( denominator != 0, sum_assumptions / denominator, opinion_from )] %>%
      setnames("to", "agent_id") %>%
      .[ , .(agent_id, opinion_y)] %>%
      setkey("agent_id") %>%
      unique()

    if( length(sim$actions_overall[ best_action %in% c("Nothing") , best_action ]) > 0  ) {

	  sim$opinion_updating <- copy(sim$messages)[ , -c("actions", "best_action")] %>%
	    unique() %>%
	    .[ copy(actions_overall), on = c("from" = "agent_id"), nomatch = 0L, allow.cartesian = TRUE ] %>%
	    .[ best_action == actions ] %>%
	    .[ best_action == "Nothing" ] %>%
            .[sim$discourse_memory, on=c("from"), nomatch = 0L, allow.cartesian = TRUE] %>%
            .[ best_action == actions ] %>% 
	    .[ , opinion_y := sapply(past_opinions, function(x) { median(x) }) ] %>%
	    setnames("from", "agent_id") %>%
	    .[ , .(agent_id, opinion_y)] %>%
	    setkey("agent_id") %>%
	    unique() %>%
	    rbind(sim$opinion_updating)

    }



    sim$discourse_memory <- copy(sim$messages)[ , -c("actions", "best_action")] %>%
      unique() %>%
      .[copy(sim$actions_overall), on = c("to" = "agent_id"), nomatch = 0L, allow.cartesian = TRUE ] %>%
      .[ best_action == actions ] %>%
      .[ (best_action == "Both" | best_action == "Receive") ]  %>%
      .[ , .(from, to, opinion_from, assumption_to, opt_message, actions, best_action)] %>%
      .[copy(sim$discourse_memory), on=c("from"), nomatch = 0L, allow.cartesian = TRUE] %>%
      .[ best_action == actions ] %>%
      .[ , distance_to_past_opinions := mapply(function(a,b) {
        mean(
          sapply(a, function(x) {
            abs(x - b)
          })
        )
      }, a=past_opinions, b=assumption_to)] %>%
      .[ , within_epsilon := abs(opinion_from - assumption_to) < params(sim)$rc_energy_model$epsilon] %>%
      .[ within_epsilon == TRUE ] %>%
      .[ , .(from, to) ] %>%
      unique() %>%
      .[ , receiver_business := .N, by=from ] %>%
      .[copy(sim$discourse_memory)[ , -c("receiver_business") ], on=c("from", "to") ] %>%
      .[ , receiver_business := ifelse(is.na(receiver_business), 0, receiver_business) ] 

    sim$agent_characteristics <- merge(sim$agent_characteristics, sim$opinion_updating, by="agent_id", all.x = TRUE) %>% # produces NAs for not repoduced rows
      .[ , opinion := ifelse(is.na(opinion_y), opinion, opinion_y)] %>%
      .[ , -c("opinion_y")]

    sim$agent_characteristics$energy[is.na(sim$agent_characteristics$energy)] <- 0

    sim$agent_characteristics <- copy(sim$actions_overall)[ , best_axn_overall := best_action][ , .(agent_id, best_axn_overall)] %>%
      unique() %>%
      merge(copy(sim$actions_send)[ , best_axn_send := best_action][ , .(agent_id, best_axn_send)], by="agent_id", allow.cartesian = TRUE) %>%
      unique() %>%
      merge(copy(sim$discourse_memory)[ , .(from, receiver_business, sender_business)], by.x = "agent_id", by.y = "from", all = TRUE, allow.cartesian = TRUE) %>%
      unique() %>%
      .[ best_axn_overall == "Send" , energy_loss := sender_business ] %>%
      .[ best_axn_overall == "Receive", energy_loss := receiver_business ] %>%
      .[ best_axn_overall == "Both", energy_loss := sender_business + receiver_business ] %>%
      .[ best_axn_overall == "Nothing", energy_loss := 0 ] %>%
      .[ best_axn_send == "Unoptimized", energy_loss := energy_loss ] %>%
      .[ best_axn_send == "Optimized", energy_loss := energy_loss + sender_business ] %>% 
      .[ , .(agent_id, energy_loss) ] %>%
      unique() %>% 
      .[ , energy_loss := sum(energy_loss), by="agent_id" ] %>% 
      unique() %>%
      .[sim$agent_characteristics, on="agent_id"] %>% 
      .[ , energy_loss := ifelse(is.na(energy_loss), 0, energy_loss) ] %>%
      .[ , energy := energy - energy_loss + params(sim)$rc_energy_model$restoration_factor ] %>%
      .[ , -c("energy_loss") ] # works fine until here

    sim$agent_characteristics$energy[is.na(sim$agent_characteristics$energy)] <- 0

    sim$discourse_memory <- sim$agent_characteristics[ , .(agent_id, opinion) ] %>%
      .[sim$discourse_memory[ , -c("opinion")], on=c("agent_id" = "from") ] %>%
      setnames("agent_id", "from") %>%
      .[ , past_receiver_business := ifelse(lengths(past_receiver_business) < params(sim)$rc_energy_model$energy_params_memory_depth,
                                            mapply(function(x, y) {
                                              list(c(unlist(x), y))
                                            }, x=past_receiver_business, y=receiver_business),
                                            mapply(function(x, y) {
                                              list(c(unlist(x)[1:params(sim)$rc_energy_model$energy_params_memory_depth], y))
                                            }, x=past_receiver_business, y=receiver_business)
      )] %>%
      .[ , past_sender_business := ifelse(lengths(past_sender_business) < params(sim)$rc_energy_model$energy_params_memory_depth,
                                          mapply(function(x, y) {
                                            list(c(unlist(x), y))
                                          }, x=past_sender_business, y=sender_business),
                                          mapply(function(x, y) {
                                            list(c(unlist(x)[1:params(sim)$rc_energy_model$energy_params_memory_depth], y))
                                          }, x=past_sender_business, y=sender_business)
      )] # also fine
    
  } else {

    sim$discourse_memory <- copy(sim$discourse_memory) %>%
      .[ , receiver_business := 0 ] %>%
      .[ , sender_business := 0 ] %>%
      .[ , past_receiver_business := ifelse(lengths(past_receiver_business) < params(sim)$rc_energy_model$energy_params_memory_depth,
                                            mapply(function(x, y) {
                                              list(c(unlist(x), y))
                                            }, x=past_receiver_business, y=receiver_business),
                                            mapply(function(x, y) {
                                              list(c(unlist(x)[1:params(sim)$rc_energy_model$energy_params_memory_depth], y))
                                            }, x=past_receiver_business, y=receiver_business)
      )] %>%
      .[ , past_sender_business := ifelse(lengths(past_sender_business) < params(sim)$rc_energy_model$energy_params_memory_depth,
                                          mapply(function(x, y) {
                                            list(c(unlist(x), y))
                                          }, x=past_sender_business, y=sender_business),
                                          mapply(function(x, y) {
                                            list(c(unlist(x)[1:params(sim)$rc_energy_model$energy_params_memory_depth], y))
                                          }, x=past_sender_business, y=sender_business)
      )]

    sim$agent_characteristics <- sim$agent_characteristics %>%
      .[ , energy := energy + params(sim)$rc_energy_model$restoration_factor ]

  }

  return(invisible(sim))

}

# FUNCTIONS


### produce_altered_message

produce_altered_message <- function(opinion_send, message_receive) {

  # produce altered message without epsilon bound
  altered_message <- ifelse(opinion_send < message_receive,
                            opinion_send + abs(opinion_send - message_receive) / 2,
                            opinion_send - abs(opinion_send - message_receive) / 2)

  return(altered_message)

}

### get_nbh_incohesion

get_nbh_incohesion <- function( id ) {

  nbh_indices <- sim$agent_characteristics[ agent_id == id , neighborhood ] %>% unlist()

  nbh_assumptions <- sim$messages[ , .(from, to, assumption_to) ] %>%
    .[ from == id & to %in% nbh_indices , assumption_to ]

  mean_deviations <- outer(nbh_assumptions, nbh_assumptions, "-") %>%
    abs() %>%
    as.vector() %>%
    sum() %>%
    sapply( "/", ((length(nbh_assumptions)*length(nbh_assumptions)) - length(nbh_assumptions)) ) # mean not appropriate, 0 values of diagonal factor in denominator

  return(mean_deviations)

}

vec_get_nbh_incohesion <- Vectorize(get_nbh_incohesion)

### get_self_incohesion

get_self_incohesion <- function( id ) {

  past <- sim$discourse_memory[ from == id, past_opinions ] %>% unlist()

  mean_deviations <- outer(past, past, "-") %>%
    abs() %>%
    as.vector() %>%
    sum() %>%
    sapply( "/", ((length(past)*length(past)) - length(past)) )

  return(mean_deviations)

}

vec_get_self_incohesion <- Vectorize(get_self_incohesion)

### reverse_sign

reverse_sign <- function ( x ) {

  if (x < 0) {
    return(-x)
  }
  if (x > 0) {
    return(x)
  }

}

### vectorized minimum

vec_min <- Vectorize(min)

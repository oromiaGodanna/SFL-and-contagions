library(truncnorm)
source('./utils.R')

generate_attribute_norm_dist <- function(mean = 0.5, sd = 0.15, lower = 0, upper = 1) {
  rtruncnorm(1, a = lower, b = upper, mean = mean, sd = sd)
}

generate_attribute_random <- function() {
  rand <- runif(1, 0, 1)
  return (rand)
}

clip_item_values <- function(item) {
    lapply(item, function(x) pmax(pmin(x, 1), 0))
}

generate_item <- function(agent_id, num_content_attributes = 0) {
    
    # return list of attributes for an item with all values ideally between 0 and 1
    data.frame(
        attractiveness = generate_attribute_random(),
        popularity = 0.0,
        novelty = 1.0,
        agent_id = agent_id
    )
}

generate_one_item <- function(agent_id) {
  item <- generate_item(agent_id)
  
  return (item)
}

generate_multiple_items <- function(n, agent_id) {
    items_list <- lapply(1:n, function(x) generate_item())
    items_df <- do.call(rbind, items_list)
    return(items_df)
}   

generate_item_from_learned_weights <- function(agent_id, weights, attribute_weights_list){

  # convert to numeric to avoid empty list
  attribute_weights_list <- lapply(attribute_weights_list, function(x) as.numeric(x))
  all_weights_matrix <- do.call(rbind, attribute_weights_list) # convert to matrix

  # calculate the mean for each feature's weight across all time steps and agents
  feature_means <- colMeans(all_weights_matrix)

  # center the weights by all time mean of feature values
  weights <- weights - feature_means
  item <- data.frame(
    attractiveness = standard_sigmoid_transform(weights[1]),
    popularity = 0.0,
    novelty = 1.0,
    agent_id = agent_id
  )
  return(item)

}

generate_items_from_learned_weights_individual_average <- function(agent_id, weights, attribute_weights_list, timestep, N){

  "
  Generate an item based on the learned weights of an agent with individual average mean-centering
  args:
      agent_id: the agent's ID
      weights: the learned weights of the agent
      attribute_weights_list: list of learned weights for each agent from all previous timesteps
      timestep: the current timestep
      N: the number of agents in the simulation(for indexing)
  rerturns:
      item: the generated item based on the learned weights
  "

  # convert to numeric to avoid empty list
  agent_weights_list <- list()
  for (t in 1:(timestep - 1)){
    idx <- (t - 1) * N + agent_id
    if (idx <= length(attribute_weights_list) && !is.null(attribute_weights_list[[idx]])) {
      agent_weights_list[[t]] <- attribute_weights_list[[idx]]
    }
  }
  if (length(agent_weights_list) > 0) {
    agent_weights_matrix <- lapply(agent_weights_list, function(x) as.numeric(x))  
    agent_weights_matrix <- do.call(rbind, agent_weights_matrix)
    feature_means <- colMeans(agent_weights_matrix, na.rm = TRUE)
  } else {
    feature_means <- rep(0, length(weights))
  }

  # mean center the learned weights
  adjusted_weights <- weights - feature_means

  # generate the item using adjusted weights
  item <- data.frame(
    attractiveness = standard_sigmoid_transform(adjusted_weights[1]),
    popularity = 0.0, # new item popularity is initialized to 0
    novelty = 1.0,  # new item novelty is initialized to highest value of 1
    agent_id = agent_id
  )

  return(item)
}
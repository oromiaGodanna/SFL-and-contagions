library(truncnorm)
source('./utils.R')

generate_attribute <- function(mean = 0.5, sd = 0.15, lower = 0, upper = 1) {
  rtruncnorm(1, a = lower, b = upper, mean = mean, sd = sd)
}

generate_attribute_random <- function() {
  rand <- runif(1, 0, 1)
  return (rand)
}


clip_item_values <- function(item) {
    lapply(item, function(x) pmax(pmin(x, 1), 0))
}


generate_item <- function() {
    # generate a random sentiment value between -1 and 1
    base_sentiment <- runif(1, -1, 1)
    
    # normalize sentiment to be between 0 and 1
    # normalized_sentiment = (base_sentiment + 1) / 2
    # sent_transformed <- quadratic_transform(base_sentiment)
    
    # return list of attributes for an item with all values ideally between 0 and 1
    data.frame(
        attractiveness = generate_attribute_random(),
        popularity = generate_attribute_random(),
        novelty = generate_attribute_random(),
        agent_id = runif(1, 1, 5)
        # emotional_trigger = generate_attribute_random()
    )
}

generate_one_item <- function() {
  item <- generate_item()
  
  return (item)
}

generate_multiple_items <- function(n) {
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
    popularity = standard_sigmoid_transform(weights[2]),
    novelty = standard_sigmoid_transform(weights[3]),
    agent_id = agent_id
    # emotional_trigger = standard_sigmoid_transform(weights[4])
  )
  # print(item)
  return(item)

}
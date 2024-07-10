library(truncnorm)

generate_attribute <- function(mean = 0.5, sd = 0.15, lower = 0, upper = 1) {
  rtruncnorm(1, a = lower, b = upper, mean = mean, sd = sd)
}

min_max_normalization <-  function(x) (x - min(x)) / (max(x) - min(x))

clip_item_values <- function(item) {
    lapply(item, function(x) pmax(pmin(x, 1), 0))
}

generate_item <- function() {
    # generate a random sentiment value between -1 and 1
    base_sentiment <- runif(1, -1, 1)
    
    # normalize sentiment to be between 0 and 1
    normalized_sentiment = (base_sentiment + 1) / 2
    
    # return list of attributes for an item with all values ideally between 0 and 1
    data.frame(
        attractiveness = generate_attribute(),
        popularity = generate_attribute(),
        novelty = generate_attribute(),
        sentiment = normalized_sentiment,
        credibility = generate_attribute(),
        emotional_trigger = generate_attribute()
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



normalize_weights <- function(weight_matrix) {
  
  for (i in 1:ncol(weight_matrix)) {
    weight_matrix[, i] <- min_max_normalization(weight_matrix[i, ], min = 0, max = 1)
  }
  return(weight_matrix)
 
}

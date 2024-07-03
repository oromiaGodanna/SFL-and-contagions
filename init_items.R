library(truncnorm)

generate_attribute <- function(mean = 0.5, sd = 0.15, lower = 0, upper = 1) {
  rtruncnorm(1, a = lower, b = upper, mean = mean, sd = sd)
}


clip_item_values <- function (item){
    lapply(item, function(x) pmax(pmin(x, 1), 0))
}

generate_item <- function() {
  base_sentiment <- generate_attribute()
  
  # return a list of attributes for an item - all values between 1 and 0
  list(
    attractiveness = generate_attribute(),
    popularity = generate_attribute(),
    novelty = generate_attribute(),
    positive_sentiment = base_sentiment,
    negative_sentiment = 1 - base_sentiment,
    credibility = generate_attribute(),
    emotional_trigger = generate_attribute()
  )
}

generate_one_item <- function() {
  item <- generate_item()
  clip_item_values(item)
}

generate_multiple_items <- function(n) {
  replicate(n, generate_one_item())
}   

library(truncnorm)

# a function to normalize a vector between 0 and 1
normalize <- function(x) (x - min(x)) / (max(x) - min(x))

normalize_with_fixed_bounds <- function(x, lower_bound, upper_bound) {
    (x - lower_bound) / (upper_bound - lower_bound)
}

normalize_info_attributes <- function(info_df) {
    "Normalize the attributes of information items to be between 0 and 1
    args:
        info_df: data frame with information items and attributes
    returns:
        info_df: data frame with normalized attributes"
    info_df$Attractiveness <- normalize_with_fixed_bounds(info_df$Attractiveness, lower_bound = 0, upper_bound = 8)
    info_df$Popularity <- normalize_with_fixed_bounds(info_df$Popularity,   lower_bound = 0, upper_bound = 10)
    info_df$Novelty <- normalize_with_fixed_bounds(info_df$Novelty, lower_bound = 0, upper_bound = 10)
    return(info_df)
}

majority_type <- function(repertoire, info_df) {
        if (length(repertoire) == 0) {
            return(NA)
        }
        type_counts <- table(info_df$Type[repertoire])
        majority_type <- as.numeric(names(which.max(type_counts)))
        return(majority_type)
    }

calculate_similarity <- function(type1, type2, type_attributes) {
    if (type1 == type2) {
        return(1) #types are the same therefore max similarity score
    } else {
        engagement_diff <- abs(type_attributes$Engagement[type1] - type_attributes$Engagement[type2])
        decay_diff <- abs(type_attributes$DecayRate[type1] - type_attributes$DecayRate[type2])
        similarity_score <- 1 / (1 + engagement_diff + decay_diff) #convert distance to similarity score
        return(similarity_score)
    }
}

initialize_type_attributes <- function(num_types, lower_bound_engagement = 1, upper_bound_engagement = 5, 
                                       lower_bound_decay = 0.01, upper_bound_decay = 0.05) {
    type_attributes <- data.frame(
        Type = 1:num_types,
        Engagement = runif(num_types, lower_bound_engagement, upper_bound_engagement),
        DecayRate = runif(num_types, lower_bound_decay, upper_bound_decay)
    )
    return(type_attributes)
}

initialize_information <- function(num_info, type_attributes, mean_attractiveness = 5, sd_attractiveness = 1.5,
                                   mean_popularity = 5, sd_popularity = 1.5, mean_novelty = 5, sd_novelty = 1.5,
                                   lower_bound_attractiveness = 0, upper_bound_attractiveness = 10,
                                   lower_bound_popularity = 1, upper_bound_popularity = 10,
                                   lower_bound_novelty = 0, upper_bound_novelty = 10) {

    '
    Initialize information items with attributes based on types
    args:
        num_info: number of information items
        num_types: number of types of information
    returns:
        info_df: data frame with information items and attributes
    '
    num_types <- nrow(type_attributes)

    # assign types as numeric values
    types <- sample(1:num_types, num_info, replace = TRUE)

    info_df <- data.frame(
        ID = 1:num_info,
        Type = as.factor(types),
        Attractiveness = numeric(num_info),
        Popularity = numeric(num_info),
        Novelty = numeric(num_info) 
    )

    # assign attributes to informations based on type with probabilistic variation
    for (t in 1:num_types) {
        # get indices of items with type t
        indices <- which(info_df$Type == t)
        base_attractiveness <- rtruncnorm(1, a = lower_bound_attractiveness, b = upper_bound_attractiveness,
                                          mean = mean_attractiveness, sd = sd_attractiveness)
        base_popularity <- round(rnorm(1, mean = mean_popularity, sd = sd_popularity))
        base_popularity <- pmax(pmin(base_popularity, upper_bound_popularity), lower_bound_popularity)

       #base_novelty <- rtruncnorm(1, a = lower_bound_novelty, b = upper_bound_novelty,
         #                         mean = mean_novelty, sd = sd_novelty)

        # probablity of being simmilar to the base within the type with minimum 40% chance
        prob_similar = runif(1, 0.4, 0.8)  

        for (i in indices) {
        
            #novelity is drawn randomly from a truncated normal distribution for each item
            info_df$Novelty[i] <- rtruncnorm(1, a = lower_bound_novelty, b = upper_bound_novelty,
                                             mean = mean_novelty, sd = sd_novelty)
            if (runif(1) < prob_similar) {
                # attributes are similar to the base with some variation
                info_df$Attractiveness[i] <- base_attractiveness + rnorm(1, mean = 0, sd = 0.5)
                info_df$Popularity[i] <- round(base_popularity + rnorm(1, mean = 0, sd = 5))
                info_df$Popularity[i] <- pmax(pmin(info_df$Popularity[i], upper_bound_popularity), lower_bound_popularity)
            } else {

                # attributes are drawn randomly from a normal distribution
                info_df$Attractiveness[i] <- rtruncnorm(1, a = lower_bound_attractiveness, b = upper_bound_attractiveness,
                                                        mean = mean_attractiveness, sd = sd_attractiveness)
                info_df$Popularity[i] <- round(rnorm(1, mean = mean_popularity, sd = sd_popularity))
                info_df$Popularity[i] <- pmax(pmin(info_df$Popularity[i], upper_bound_popularity), lower_bound_popularity)
            }
        }
    }

    # normalize item-level attributes
    # info_df <- normalize_info_attributes(info_df)
    return(info_df)
}

add_new_information <- function(info_df, num_new_info, type_attributes, mean_attractiveness = 5, sd_attractiveness = 1.5,
                                lower_bound_attractiveness = 0, upper_bound_attractiveness = 10,
                                lower_bound_popularity = 1, upper_bound_popularity = 10,
                                lower_bound_novelty = 0, upper_bound_novelty = 10,
                                prob_base_on_existing = 0.7) {  #with probablity of basing attractiveness on existing items

    new_ids <- (max(info_df$ID) + 1):(max(info_df$ID) + num_new_info)
    new_types <- sample(1:nrow(type_attributes), num_new_info, replace = TRUE)

    new_info_df <- data.frame(
        ID = new_ids,
        Type = as.factor(new_types),
        Attractiveness = numeric(num_new_info),
        Popularity = rep(lower_bound_popularity, num_new_info),
        Novelty = rep(upper_bound_novelty, num_new_info)
    )

    for (i in 1:num_new_info) {
        current_type <- new_types[i]
        existing_items <- info_df[info_df$Type == current_type, ]
        
        if (nrow(existing_items) > 0 && runif(1) < prob_base_on_existing) {
            # base attractiveness on an existing item with some variation
            base_item <- existing_items[sample(nrow(existing_items), 1), ]
            new_info_df$Attractiveness[i] <- base_item$Attractiveness + rnorm(1, mean = 0, sd = sd_attractiveness / 2)
        } else {
            #assign random attractiveness
            new_info_df$Attractiveness[i] <- rtruncnorm(1, a = lower_bound_attractiveness, b = upper_bound_attractiveness, 
                                                        mean = mean_attractiveness, sd = sd_attractiveness)
        }
    }

    return(new_info_df)
}

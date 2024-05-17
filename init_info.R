library(truncnorm)

initialize_information <- function(num_info, num_types, mean_attractiveness = 5, sd_attractiveness = 1.5,
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
        # indices of items with type t
        indices <- which(info_df$Type == t)

        # base attribute values for the info type
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

    return(info_df)
}

add_new_information <- function(info_df, num_new_info, num_types, mean_attractiveness = 5, sd_attractiveness = 1.5,
                                lower_bound_attractiveness = 0, upper_bound_attractiveness = 10,
                                lower_bound_popularity = 1, upper_bound_popularity = 10,
                                lower_bound_novelty = 0, upper_bound_novelty = 10) {

    '
    create new information items and append to the existing information dataframe
    Args:
        info_df: dataframe containing existing information items'

    new_ids <- (max(info_df$ID) + 1):(max(info_df$ID) + num_new_info)
    

    new_types <- sample(1:num_types, num_new_info, replace = TRUE)
    

    #maybe the attractiveness here should have more or less similar to the existing ones
    new_attractiveness <- rtruncnorm(num_new_info, a = lower_bound_attractiveness,
                                     b = upper_bound_attractiveness, mean = mean_attractiveness, sd = sd_attractiveness)
    

    new_popularity <- rep(lower_bound_popularity, num_new_info)
    

    new_novelty <- rep(upper_bound_novelty, num_new_info)
    
    new_info_df <- data.frame(
        ID = new_ids,
        Type = as.factor(new_types),
        Attractiveness = new_attractiveness,
        Popularity = new_popularity,
        Novelty = new_novelty
    )
    
    updated_info_df <- rbind(info_df, new_info_df)
    
    return(updated_info_df)
}
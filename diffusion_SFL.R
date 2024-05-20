Info_diffusion_SFL <- function(N, alpha, beta_mu, beta_sd, graph, info_df, timesteps = 40, social_influence = TRUE) {
    '
    create a diffusion process for information adoption based on reinforcement learning
    args:
        N: number of agents
        alpha: learning rate
        beta_mu: mean of beta parameter for softmax
        beta_sd: standard deviation of beta parameter for softmax
        graph: igraph object representing the social network
        info_df: data frame with information items and attributes
        timesteps: number of time steps
        social_influence: boolean indicating whether social influence is considered
    returns:
        output: data frame with agent choices and rewards
        '
    num_info <- nrow(info_df)
    num_attributes <- ncol(info_df) - 2 # exclude ID and Type columns
    num_social_features <- ifelse(social_influence, 1, 0) # for the proportion of adopted neighbors

    total_features_per_info <- num_attributes + num_social_features
    total_columns <- total_features_per_info * num_info

    beta <- pmax(rnorm(N, beta_mu, beta_sd), 0)
    weights <- matrix(0, nrow = N, ncol = total_columns)
    social_value <- matrix(0, nrow = N, ncol = num_info)

    output <- data.frame(
        trial = rep(1:timesteps, each = N),
        agent = rep(1:N, timesteps),
        preferences = I(replicate(N * timesteps, list())), 
        reward = rep(NA, timesteps * N),
        weights = I(replicate(N * timesteps, list())),
        social_value = I(replicate(N * timesteps, list()))
    )

    # Assign one random preference (one information) to each agent
    for (i in 1:N) {
        initial_prefs <- sample(num_info, 1)
        output$preferences[i] <- list(initial_prefs)
    }

    for (t in 1:timesteps) {
        for (i in 1:N) {
            idx <- (t - 1) * N + i
            if (social_influence) {
                neighs_ids <- neighbors(graph, i)

                if (length(neighs_ids) > 0 && t > 1) {
                    neigh_last_choices <- unlist(lapply(neighs_ids, function(nid) {
                        return(output$preferences[(t-2) * N + nid][[1]])
                    }))

                    if (length(neigh_last_choices) > 0) {
                        choice_counts <- table(neigh_last_choices)
                        full_choice_counts <- rep(0, num_info)
                        # fill the counts for the choices
                        full_choice_counts[as.integer(names(choice_counts))] <- choice_counts
                        # calculate the proportion of neighbors who chose each information
                        social_value[i, ] <- full_choice_counts / length(neigh_last_choices)
                    }
                }
            }

            Q_values <- numeric(num_info)
            # calculate Q-values for each information item of the agent
            for (j in 1:num_info) {
                start_index <- (j - 1) * total_features_per_info + 1
                end_index <- start_index + num_attributes - 1
                info_attributes <- c(info_df$Attractiveness[j], info_df$Popularity[j], info_df$Novelty[j])
                features <- if (social_influence) c(info_attributes, social_value[i, j]) else info_attributes
                Q_values[j] <- sum(weights[i, start_index:end_index] * features)
            }

            # normalize Q-values to avoid large exponentials
            Q_values <- Q_values - max(Q_values)
            exp_values <- exp(beta[i] * Q_values)
            probabilities <- exp_values / sum(exp_values)

            # checking NA values in probabilities
            if (any(is.na(probabilities)) || any(is.infinite(probabilities))) {
                probabilities[is.na(probabilities) | is.infinite(probabilities)] <- 0
                probabilities <- probabilities / sum(probabilities)
            }

            choice <- sample(1:num_info, 1, prob = probabilities)

            output$preferences[idx] <- list(choice)

            reward <- rnorm(1, mean = info_df$Attractiveness[choice], sd = 0.1) # Smaller standard deviation to limit reward range
            output$reward[idx] <- reward

            actual_reward <- reward
            Q_adopt <- Q_values[choice]
            delta <- actual_reward - Q_adopt
            # update weights for the chosen information item (index calculation)
            chosen_start_index <- (choice - 1) * total_features_per_info + 1
            chosen_end_index <- chosen_start_index + num_attributes - ifelse(social_influence, 0, 1)
            features_chosen <- if (social_influence) c(info_df$Attractiveness[choice], info_df$Popularity[choice], info_df$Novelty[choice], social_value[i, choice]) else c(info_df$Attractiveness[choice], info_df$Popularity[choice], info_df$Novelty[choice])
            
            # clip delta to prevent excessively large updates
            delta <- max(min(delta, 1), -1)
            weights[i, chosen_start_index:chosen_end_index] <- weights[i, chosen_start_index:chosen_end_index] + alpha * delta * features_chosen

            output$weights[idx] <- list(weights[i, ])
            output$social_value[idx] <- list(social_value[i, ]) 
            
        }
    }
    ##
    return(list(output = output))
}
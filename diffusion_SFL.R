Info_diffusion_SFL <- function(N, alpha, beta_mu, beta_sd, graph, info_df, timesteps = 40, social_influence = TRUE) {
    num_info <- nrow(info_df)
    num_attributes <- ncol(info_df) - 2 # exclude ID and Type columns
    num_social_features <- ifelse(social_influence, 1, 0) # for the proportion of adopted neighbors

    total_columns <- num_attributes * num_info + num_social_features

    beta <- pmax(rnorm(N, beta_mu, beta_sd), 0)
    weights <- matrix(0, nrow = N, ncol = total_columns)
    social_value <- matrix(0, nrow = N, ncol = num_info)

    output <- data.frame(
        trial = rep(1:timesteps, each = N),
        agent = rep(1:N, timesteps),
        preferences = I(replicate(N * timesteps, list())),
        reward = rep(NA, timesteps * N),
        social_weights = rep(NA, timesteps * N) # Store only social weights
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
                start_index <- (j - 1) * num_attributes + 1
                end_index <- start_index + num_attributes - 1
                info_attributes <- c(info_df$Attractiveness[j], info_df$Popularity[j], info_df$Novelty[j])
                features <- info_attributes

                if (social_influence) {
                    social_weight <- weights[i, total_columns]
                    features <- c(features, social_weight * social_value[i, j])
                }

                Q_values[j] <- sum(weights[i, start_index:end_index] * info_attributes) + ifelse(social_influence, social_weight * social_value[i, j], 0)
            }
            # normalise Q-values for softmax - numerical stability
            Q_values <- Q_values - max(Q_values)

            exp_values <- exp(beta[i] * Q_values)
            probabilities <- exp_values / sum(exp_values)
            
            # checking NA values in probabilities
            if (any(is.na(probabilities)) || any(is.infinite(probabilities))) {
                probabilities[is.na(probabilities) | is.infinite(probabilities)] <- 0
            }
            choice <- sample(1:num_info, 1, prob = probabilities)

            output$preferences[idx] <- list(choice)
            reward <- rnorm(1, mean = info_df$Attractiveness[choice], sd = 0.1)
            output$reward[idx] <- reward

            actual_reward <- reward
            Q_adopt <- Q_values[choice]
            delta <- actual_reward - Q_adopt

            # Update weights based on delta and chosen features
            chosen_start_index <- (choice - 1) * num_attributes + 1
            chosen_end_index <- chosen_start_index + num_attributes - 1
            features_chosen <- info_attributes

            if (social_influence) {
                social_weight <- weights[i, total_columns]
                features_chosen <- c(features_chosen, social_value[i, choice])
                weights[i, total_columns] <- social_weight + alpha * delta * social_value[i, choice]
            }

            # Ensure length consistency
            if (length(weights[i, chosen_start_index:chosen_end_index]) != length(info_attributes)) {
                stop("Mismatch in length between weights and info_attributes.")
            }

            weights[i, chosen_start_index:chosen_end_index] <- weights[i, chosen_start_index:chosen_end_index] + alpha * delta * info_attributes

            if (social_influence) {
                output$social_weights[idx] <- weights[i, total_columns]
            }

            # if (t == timesteps) {
            #     print("weights")
            #     print(weights)
            # }
        }
    }
    return(list(output = output))
}

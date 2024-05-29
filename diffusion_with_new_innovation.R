source('./init_info.R')


Info_diffusion_SFL <- function(N, alpha, beta_mu, beta_sd, graph, original_info_df, timesteps = 40, social_influence = TRUE, new_info_prob = 0.01) {
    info_df <- normalize_info_attributes(original_info_df)  # start with a normalized version of the original info_df
    num_info <- nrow(info_df)
    num_attributes <- ncol(info_df) - 2  #exclude ID and Type columns

    beta <- pmax(rnorm(N, beta_mu, beta_sd), 0)
    attribute_weights <- matrix(0, nrow = N, ncol = num_info * num_attributes)
    social_weights <- if (social_influence) rep(0, N) else NULL
    social_value <- matrix(0, nrow = N, ncol = num_info)

    output <- data.frame(
        trial = rep(1:timesteps, each = N),
        agent = rep(1:N, timesteps),
        preferences = I(replicate(N * timesteps, list())),
        reward = rep(NA, timesteps * N),
        social_weights = rep(NA, timesteps * N)
    )

    for (i in 1:N) {
        output$preferences[i] <- list(sample(num_info, 1))
    }

    for (t in 1:timesteps) {
        if (runif(1) < new_info_prob) {
            original_info_df <- add_new_information(original_info_df, 1, length(unique(original_info_df$Type)))
            info_df <- normalize_info_attributes(original_info_df)  
            num_info <- nrow(info_df)  
            new_columns_needed <- num_attributes
            attribute_weights <- cbind(attribute_weights, matrix(0, nrow = N, ncol = new_columns_needed))

            # Ensure that social_value has the correct number of columns
            additional_cols <- num_info - ncol(social_value)
            if (additional_cols > 0) {
                social_value <- cbind(social_value, matrix(0, nrow = N, ncol = additional_cols))
            }
        }

        for (i in 1:N) {
            idx <- (t - 1) * N + i
            if (social_influence) {
                neighs_ids <- neighbors(graph, i)
                if (length(neighs_ids) > 0 && t > 1) {
                    neigh_last_choices <- unlist(lapply(neighs_ids, function(nid) {
                        return(output$preferences[(t-2) * N + nid][[1]])
                    }))

                    if (length(neigh_last_choices) > 0) {
                        choice_counts <- table(factor(neigh_last_choices, levels = 1:num_info))
                        full_choice_counts <- as.numeric(choice_counts)
                        social_value[i, ] <- full_choice_counts / length(neigh_last_choices)
                    }
                }
            }

            Q_values <- numeric(num_info)
            for (j in 1:num_info) {
                start_index <- (j - 1) * num_attributes + 1
                end_index <- start_index + num_attributes - 1

                if (end_index > ncol(attribute_weights)) {
                    stop("Index out of bounds: end_index exceeds the number of columns in attribute_weights")
                }

                info_attributes <- c(info_df$Attractiveness[j], info_df$Popularity[j], info_df$Novelty[j])
                features <- info_attributes

                if (social_influence) {
                    Q_values[j] <- sum(attribute_weights[i, start_index:end_index] * features) + social_weights[i] * social_value[i, j]
                } else {
                    Q_values[j] <- sum(attribute_weights[i, start_index:end_index] * features)
                }
            }

            exp_values <- exp(beta[i] * Q_values)
            probabilities <- exp_values / sum(exp_values)

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

            chosen_start_index <- (choice - 1) * num_attributes + 1
            chosen_end_index <- chosen_start_index + num_attributes - 1
            attribute_weights[i, chosen_start_index:chosen_end_index] <- attribute_weights[i, chosen_start_index:chosen_end_index] + alpha * delta * info_attributes

            if (social_influence) {
                social_weights[i] <- social_weights[i] + alpha * delta * social_value[i, choice]
                output$social_weights[idx] <- social_weights[i]
            }
        }
    }
    return(list(output = output, info_df = info_df))
}

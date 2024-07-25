source("./init_items.R")
source("./reward.R")


Diffusion_with_SFL <- function(N, alpha,
                               beta_mu,
                               beta_sd,
                               graph,
                               timesteps = 40,
                               items_df = NULL,
                               social_influence = TRUE,
                               new_item_prob = 0.12,
                               num_attrib = 4) {
    "
    Simulate the diffusion of information with social learning
    args:
        N: number of agents
        alpha: learning rate
        beta_mu: mean beta value
        beta_sd: standard deviation of beta value
        graph: igraph object representing the social network
        timesteps: number of simulation steps
        items_df: data frame with information items and attributes
        social_influence: boolean indicating whether social influence is enabled
        new_item_prob: probability of generating a new item
        num_attrib: number of attributes for each item
    returns:
        output: data frame with simulation results
        items_df: updated data frame with information items and attributes
    "
    # check if initial item list is provided
    if (is.null(items_df)) {
        num_items <- 0
        num_attributes <- num_attrib # attributes if no items_df is provided
        agent_repertoires <- lapply(1:N, function(i) data.frame(Item = integer(0), Count = integer(0)))
    } else {
        num_items <- nrow(items_df)
        num_attributes <- ncol(items_df) # Exclude the ID column from attributes count
        # Initialize agent repertoires with a subset of items
        agent_repertoires <- lapply(1:N, function(i) {
            items <- sample(num_items, sample(1:num_items, 1), replace = FALSE)
            counts <- sample(1, length(items), replace = TRUE)
            data.frame(Item = items, Count = counts)
        })
    }

    # Initialize parameters for agents
    agent_betas <- pmax(rnorm(N, beta_mu, beta_sd), 0)
    attribute_weights <- matrix(0, nrow = N, ncol = num_attributes)

    social_weights <- if (social_influence) rep(0, N)
    social_value <- matrix(0, nrow = N, ncol = num_items)

    avg_feedback_value_estimates <- numeric(timesteps)
    # Prepare the output data frame
    output <- data.frame(
        trial = rep(1:timesteps, each = N),
        agent = rep(1:N, timesteps),
        last_choice = I(replicate(N * timesteps, list(0))),
        reward = rep(NA, timesteps * N),
        attribute_weights = I(replicate(N * timesteps, list())),
        social_weights = rep(NA, timesteps * N)
    )

    # Set the last_choice for agents if items are available
    if (!is.null(items_df)) {
        output$last_choice <- lapply(agent_repertoires, function(repertoire) {
            if (all(repertoire$Count == 0)) {
                return(NA) # No items to choose from
            } else {
                # Select the item with the highest count
                return(repertoire$Item[which.max(repertoire$Count)])
            }
        })
    }

    normalized_neighbour_counts <- normalize_neighbour_count(graph, N)

    for (t in 1:timesteps) {

        feedback_value_estimates <- numeric()

        for (i in 1:N) {

            idx <- (t - 1) * N + i
            prob <- runif(1)

            neighs_ids <- neighbors(graph, i)
            observed_items <- unique(unlist(lapply(neighs_ids, function(nid) {
                # filter items with Count > 0 just in case
                rep_items <- agent_repertoires[[nid]]
                rep_items$Item[rep_items$Count > 0]
            })))
            available_items <- unique(c(agent_repertoires[[i]]$Item, if (social_influence) observed_items else NULL))

            if (prob < new_item_prob) {

                new_item <- generate_one_item() #
                if(t > 2){ # Generate a new item based on the agent's learned preferences or weights
                    new_item <- generate_item_from_learned_weights(i, attribute_weights[i, ], attribute_weights_list = output$attribute_weights)
                }

                items_df <- rbind(items_df, new_item)
                num_items <- nrow(items_df)

                # Update social value matrix for additional items
                if (num_items > ncol(social_value)) {
                    additional_cols <- num_items - ncol(social_value)
                    social_value <- cbind(social_value, matrix(0, nrow = N, ncol = additional_cols))
                }
                new_item_index <- num_items

                # Update the current agent's repertoire to include the new item with an initial count of 0, count will be updated later
                current_repertoire <- agent_repertoires[[i]]
                new_repertoire_entry <- data.frame(Item = new_item_index, Count = 0)
                agent_repertoires[[i]] <- rbind(current_repertoire, new_repertoire_entry)

                # Agent immediately chooses the new item, introducing it to the network
                features <- c(items_df$attractiveness[new_item_index], items_df$novelty[new_item_index], items_df$popularity[new_item_index], normalized_neighbour_counts[items_df$agent_id[new_item_index]])

                Q_values <- sum(attribute_weights[i, ] * features)
                choice <- new_item_index
                output$last_choice[idx] <- list(choice)

                # Calculate reward based on feedback from the network; could be refined to include novelty bonuses
                average_estimate <- ifelse(t != 1, avg_feedback_value_estimates[t - 1], 0)
                feedback <- calculate_feedback(agent_id = i, chosen_item = choice, graph = g, attribute_weights = attribute_weights, items = items_df, timestep = t, average_estimate = average_estimate, connection_count = normalized_neighbour_counts)
                reward <- feedback$total_feedback_score
                feedback_value_estimates <- c(feedback_value_estimates, feedback$value_estimates)

                Q_adopt <- Q_values

            } else if (length(available_items) > 0) {

                if (social_influence) {

                    if (length(neighs_ids) > 0) {
                        # Retrieve last choices from the previous timestep from neighbors and handel zeros
                        neigh_last_choices <- unlist(lapply(neighs_ids, function(nid) {
                            last_choice <- output$last_choice[(t - 1) * N + nid][[1]]
                            if (last_choice != 0) {
                                last_choice
                            }
                        }))
                        
                        if (length(neigh_last_choices) > 0 ) {
                            choice_counts <- table(factor(neigh_last_choices, levels = 1:num_items))
                            full_choice_counts <- as.numeric(choice_counts)
                            # Updating social value for the agent based on observed items
                            social_value[i, ] <- full_choice_counts / length(neighs_ids) # Normalize by number of neighbors to avoid overestimation
                        }
                    }
                }

                Q_values <- numeric(num_items)
                # avialable_items <- sample_items(available_items, output, t, N)

                for (j in available_items) {
                    # get the number of connections for the item creator agent
                    item_owner_neighbou_count <- normalized_neighbour_counts[items_df$agent_id[j]]
                    features <- c(items_df$attractiveness[j], items_df$popularity[j], items_df$novelty[j], item_owner_neighbou_count)  #, items_df$emotional_trigger[j], items_df$credibility[j], items_df$sentiment[j], 

                    if (social_influence) {
                        Q_values[j] <- sum(attribute_weights[i, ] * features) + social_weights[i] * social_value[i, j]
                    } else {
                        Q_values[j] <- sum(attribute_weights[i, ] * features)
                    }
                }

                valid_indices <- available_items
                invalid_indices <- setdiff(1:num_items, valid_indices)
                Q_values[invalid_indices] <- -Inf # zero out the probability of unobserved items

                exp_values <- exp(agent_betas[i] * Q_values)
                probabilities <- exp_values / sum(exp_values)

                probabilities[probabilities < 0] <- 0
                if (any(is.na(probabilities)) || any(is.infinite(probabilities))) {
                    probabilities[is.na(probabilities) | is.infinite(probabilities)] <- 0
                }
                if (sum(probabilities) == 0) {
                    probabilities <- rep(1, length(probabilities))
                }
                probabilities <- probabilities / sum(probabilities)

                choice <- sample(1:num_items, 1, prob = probabilities)
                output$last_choice[idx] <- list(choice)

                # all the neighbors of the agent give feedback(reward) for the given choice
                average_estimate <- ifelse(t != 1, avg_feedback_value_estimates[t - 1], 0)
                feedback <- calculate_feedback(agent_id = i, chosen_item = choice, graph = g, attribute_weights = attribute_weights, items = items_df, timestep = t, average_estimate = average_estimate, connection_count = normalized_neighbour_counts)
                reward <- feedback$total_feedback_score
                feedback_value_estimates <- c(feedback_value_estimates, feedback$value_estimates)
                Q_adopt <- Q_values[choice]
            }
            if (length(available_items) == 0) {
                next
            }
            output$reward[idx] <- reward
            delta <- reward - Q_adopt

            feature_values_of_choice <- c(items_df$attractiveness[choice], items_df$popularity[choice], items_df$novelty[choice], normalized_neighbour_counts[items_df$agent_id[choice]]) #, items_df$emotional_trigger[choice], items_df$credibility[choice], items_df$sentiment[choice], 
            attribute_weights[i, ] <- attribute_weights[i, ] + alpha * delta * feature_values_of_choice
            output$attribute_weights[idx] <- list(attribute_weights[i, ])
            
            if (social_influence) {
                social_weights[i] <- social_weights[i] + alpha * delta * social_value[i, choice]
                output$social_weights[idx] <- social_weights[i]
            }

            current_repertoire <- agent_repertoires[[i]]
            if (choice %in% current_repertoire$Item) {
                # increase the count for the chosen item
                current_repertoire$Count[current_repertoire$Item == choice] <- current_repertoire$Count[current_repertoire$Item == choice] + 1
            } else {
                # add a new entry for the chosen item
                new_entry <- data.frame(Item = choice, Count = 1)
                current_repertoire <- rbind(current_repertoire, new_entry)
            }
            agent_repertoires[[i]] <- current_repertoire
        }

        avg_feedback_value_estimates[t] <- mean(feedback_value_estimates)

        if (num_items > 0) {
            
            popularity_counts <- rep(0, num_items)
            # Calculate popularity of items based on the last choices of agents
            for (i in 1:N) {
                idx <- (t - 1) * N + i
                last_choice <- output$last_choice[idx][[1]]
                if (last_choice == 0) next
                popularity_counts[last_choice] <- popularity_counts[last_choice] + 1
            }
            popularity <- popularity_counts / N
            items_df$popularity <- popularity

            for (i in 1:nrow(items_df)) {
                decay_rate <- 0.005
                items_df$novelty[i] <- pmax(items_df$novelty[i] - decay_rate, 0)
            }
        }

    }
    return(list(output = output, items_df = items_df))
}


normalize_neighbour_count <- function(graph, N){
    "
    Get the neighbors of all agents and normalize it according to the min-max normalization
    "

    neighbour_counts <- numeric(N)
    for (i in 1:N){
        neighbours <- neighbors(graph, i)
        neighbour_counts[i] <- length(neighbours)
    }
    normalized_neighbour_counts <- min_max_normalization(neighbour_counts)
    return (normalized_neighbour_counts)
    
}


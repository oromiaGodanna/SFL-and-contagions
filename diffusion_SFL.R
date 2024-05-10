Info_diffusion_SFL <- function(N, alpha, beta_mu, beta_sd, graph, timesteps = 40) {
    ideology_mean <- 10
    ideology_sd <- 1.5
    beta <- rnorm(N, beta_mu, beta_sd)
    beta[beta < 0] <- 0

    weights <- matrix(data = 0, nrow = N, ncol = 3) # weights for adopting ideology, not adopting, and social influence

    personal_value <- rnorm(N, 0.5, 0.4) #  affinity towards the ideology
    social_value <- rep(0.0, N) # no social influence to adopt the ideology

    output <- data.frame(
        trial = rep(1:timesteps, each = N),
        agent = rep(1:N, timesteps),
        adopted = rep(FALSE, timesteps * N),
        reward = rep(NA, timesteps * N)
    )

    # Set initial spreaders randomly
    initial_spreaders <- sample(N, size = round(0.05 * N))
    # print(length(initial_spreaders))
    output$adopted[initial_spreaders] <- TRUE

    for (t in 2:timesteps) {
        for (i in 1:N) {

            neighs_ids <- neighbors(graph, i)
            if (!output$adopted[i + N * (t - 2)]) { # check if not already a spreader in the previous timestep
                if (length(neighs_ids) > 0) {
                    # check if any neighbor has at least 4 shared connections and has adopted the ideology
                    meets_shared_connections <- sapply(neighs_ids, function(n) sum(neighs_ids %in% neighbors(graph, n)) > 4 && output$adopted[n + N * (t - 2)])
                    # check if any connections meet the condition
                    if (any(meets_shared_connections)) {
                        # social value 1 or 0 for simplicity
                        social_value[i] <- 1
                    } else {
                        social_value[i] <- 0
                    }
                }
            } else { # if already a spreader, already have adopted the ideology
                output$adopted[i + N * (t - 1)] <- TRUE
                next
            }

            # Q-values for adopting and not adopting
            # update value of adopting the ideology only if agent has social influence
            Q_adopt <- ifelse(social_value[i], weights[i, 1], 0) * personal_value[i] + weights[i, 3] * social_value[i]
            Q_reject <- weights[i, 2] * (1 - personal_value[i]) + weights[i, 3] * (1 - social_value[i]) # Weight for not adopting
            

            # softmax to determine probabilities
            exp_values <- exp(beta[i] * c(Q_adopt, Q_reject))
            probabilities <- exp_values / sum(exp_values)
            adopted <- sample(c(TRUE, FALSE), 1, prob = probabilities)
            output$adopted[i + N * (t - 1)] <- adopted

            # calculate reward based on decision, if doesn't adopt, reward is 7(random value less than ideology_mean)
            reward <- rnorm(1, mean = ifelse(adopted, ideology_mean, 7), sd = ideology_sd)
            output$reward[i + N * (t - 1)] <- reward

            actual_reward <- reward
            delta <- actual_reward - ifelse(adopted, Q_adopt, Q_reject)
            if (adopted == TRUE) {
                features_chosen <- c(1, 0, social_value[i])
            } else {
                features_chosen <- c(0, 1, 1 - social_value[i])
            }
            weights[i, ] <- weights[i, ] + alpha * delta * features_chosen
        }
    }
    return(output)
}

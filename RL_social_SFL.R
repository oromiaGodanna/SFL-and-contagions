RL_social_SFL <- function(N, alpha, beta_mu, beta_sd, graph) {
  arm_means <- c(choice1 = 10, choice2 = 13)
  arm_sd <- 1.5
  beta <- rnorm(N, beta_mu, beta_sd)
  beta[beta < 0] <- 0

  weights <- matrix(data = 0, nrow = N, ncol = 3)

  personal_value <- rnorm(N, 0.5, 0.2) # preference for choice1
  social_value <- rep(0.5, N) # initial social influence

  output <- data.frame(
    trial = rep(1:100, each = N),
    agent = rep(1:N, 100),
    choice = rep(NA, 100 * N),
    reward = rep(NA, 100 * N),
    weights1 = rep(NA, 100 * N),
    weights2 = rep(NA, 100 * N),
    weights3 = rep(NA, 100 * N),
    correct = rep(NA, 100 * N)
  )

  for (t in 1:100) {
    if (t > 1) {
      for (i in 1:N) {
        neighs_ids <- neighbors(graph, i) # get neighbor indices from the graph
        if (length(neighs_ids) > 0) {
          neigh_choices <- output[output$trial == t - 1 & output$agent %in% neighs_ids, "choice"]
          # proportion of neighbors who chose "choice1"
          social_value[i] <- mean(neigh_choices == "choice1")
          # print(paste("Social value:", social_value[i], "Neighs:", neighs_ids, "Choices:", neigh_choices))
        }
      }
    }

    mean_centered_social_value <- social_value - mean(social_value)
    Q_choice1 <- weights[, 1] * personal_value + weights[, 3] * social_value
    Q_choice2 <- weights[, 2] * (1 - personal_value) + weights[, 3] * (1 - social_value)
    Q_values <- cbind(Q_choice1, Q_choice2)

    # compare estimated values of the two choices
    p_softmax <- exp(beta * Q_values) / rowSums(exp(beta * Q_values))
    choice_index <- apply(p_softmax, 1, function(x) sample(1:2, 1, prob = x))
    choices <- ifelse(choice_index == 1, "choice1", "choice2")
    reward <- rnorm(N, mean = ifelse(choices == "choice1", arm_means["choice1"], arm_means["choice2"]), sd = arm_sd)

    for (i in 1:N) {
      actual_Q <- ifelse(choices[i] == "choice1", Q_choice1[i], Q_choice2[i])
      actual_reward <- reward[i]
      delta <- actual_reward - actual_Q
      if (choices[i] == "choice1") {
        features_chosen <- c(1, 0, social_value[i])
      } else {
        features_chosen <- c(0, 1, 1 - social_value[i])
      }
      # update weights for the chosen choice
      weights[i, ] <- weights[i, ] + alpha * delta * features_chosen
    }

    output[((t - 1) * N + 1):(t * N), ] <- data.frame(
      trial = t,
      agent = 1:N,
      choice = choices,
      reward = reward,
      weights1 = weights[, 1],
      weights2 = weights[, 2],
      weights3 = weights[, 3],
      correct = choices == ifelse(arm_means["choice1"] > arm_means["choice2"], "choice1", "choice2")
    )
  }
  return(output)
}

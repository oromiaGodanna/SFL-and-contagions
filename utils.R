source("./diffusion_SFL.R")

run_simulation_diffusion <- function(graph, num_runs, N=100, alpha=0.7, beta_mu=0.3, beta_sd=0.1, timesteps=100) {
  output <- lapply(1:num_runs, function(x) Info_diffusion_SFL(N=N, alpha=alpha, beta_mu=beta_mu, beta_sd=beta_sd, graph=graph, timesteps = timesteps))
  mean_correct <- vector("numeric", length = 100)
  for (t in 1:100) {
    trial_corrects <- sapply(output, function(df) mean(df$adopted[df$trial == t]))
    mean_correct[t] <- mean(trial_corrects)
  }
  return(mean_correct)
}



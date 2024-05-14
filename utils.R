source("./RL_social_SFL.R")
source('./diffusion_SFL.R')
run_simulation_diffusion <- function(graph, num_runs, N=100, alpha=0.7, beta_mu=0.3, beta_sd=0.1, timesteps=100) {
  output <- lapply(1:num_runs, function(x) Info_diffusion_SFL(N=N, alpha=alpha, beta_mu=beta_mu, beta_sd=beta_sd, graph=graph, timesteps = timesteps))
  mean_correct <- vector("numeric", length = 100)
  for (t in 1:100) {
    trial_corrects <- sapply(output, function(df) mean(df$adopted[df$trial == t]))
    mean_correct[t] <- mean(trial_corrects)
  }
  return(mean_correct)
}

compute_graph_metrics <- function(graph) {
  metrics <- list(
    num_nodes = vcount(graph),
    num_links = ecount(graph),
    density = graph.density(graph),
    avg_path_length = average.path.length(graph),
    avg_clustering_coeff = transitivity(graph, type = "average"),
    avg_betweenness = mean(betweenness(graph)),
    avg_closeness = mean(closeness(graph)),
    avg_degree = mean(degree(graph)),
    med_clustering_coeff = median(transitivity(graph, type = "local")),
    med_betweenness = median(betweenness(graph)),
    med_closeness = median(closeness(graph)),
    med_degree = median(degree(graph))
  )
  return(metrics)
}

plot_with_colored_nodes <- function(graph, condition, node_colors){
  plot(graph, layout= layout_in_circle, vertix.color= ifelse(condition, node_colors[1], node_colors[2]))
}

plot_correct <- function(mean_correct, main_title) {
  par(bg = "white")
  plot(x = 1:100,
       y = mean_correct,
       type = 'l',
       ylab = "Frequency Correct",
       xlab = "Timestep",
       ylim = c(0, 1),
       lwd = 2,
       col = "blue", 
       main=main_title)
}
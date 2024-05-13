library(igraph)
library(tidyverse)

create_small_world_network <- function(N, k, p) {
    "
    Create a small world network using the Watts-Strogatz model
    Args:
        N: Number of nodes in the network
        k: Number of nearest neighbors to connect
        p: Probability of rewiring an edge
    Returns:
        igraph object of the small world network
    "
    g <- watts.strogatz.game(1, N, k, p, loops = FALSE, multiple = FALSE)
    return(g)
}

create_weighted_small_world <- function(N, k, p) {
    "
    Create a small world network using the Watts-Strogatz model with weighted edges
    Args:
        N: Number of nodes in the network
        k: Number of nearest neighbors to connect
        p: Probability of rewiring an edge
    Returns:    
        igraph object of the small world network
    "
    g <- watts.strogatz.game(1, N, k, p, loops = FALSE, multiple = FALSE)

    E(g)$weight <- 0.2 # set initial weight for all edges assuming long ties

    # check if an edge should be considered a short tie
    for (e in E(g)) {
        ends <- ends(g, e)
        v1 <- ends[1]
        v2 <- ends[2]

        neighbors_v1 <- neighbors(g, v1)
        neighbors_v2 <- neighbors(g, v2)
        common_neighbors <- intersect(neighbors_v1, neighbors_v2)

        #  if they have atleast three common neighbors then it's a short tie
        if (length(common_neighbors) > 3) {
            E(g)[e]$weight <- 1
        }
    }
    return(g)
}

create_barabasi_albert <- function(N, m) {
    "
    Create a Barabasi-Albert network
    Args:
        N: Number of nodes in the network
        m: Number of edges to attach from a new node to existing nodes
    Returns:
        igraph object of the Barabasi-Albert network
        "
    g <- sample_pa(N, m = m, directed = FALSE, algorithm = "psumtree")
    return(g)
}

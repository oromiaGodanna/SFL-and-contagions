library(igraph)
library(tidyverse)
library(parallel)

create_small_world_network <- function(N, k, p) {
    "
    Create a small world network using the Watts-Strogatz model
    Args:
        N: Number of nodes in the network (integer)
        k: Number of nearest neighbors to connect (integer)
        p: Probability of rewiring an edge (float)
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
        N: Integer - number of nodes in the network
        m: Integer - number of edges to attach from a new node to existing nodes
    Returns:
        igraph object of the Barabasi-Albert network
        "
    g <- sample_pa(N, m = m, directed = FALSE, algorithm = "psumtree")
    return(g)
}


create_social_structure <- function(num_networks, num_connections, network_params) {
    "
    create a social structure by connecting small world networks with random connections
    args:
        num_networks: integer number of small world networks to create
        num_connections: integer number of random connections to add between networks
        network_params: list of parameters for each small world network
    returns: igraph object of the social structure"

    networks <- lapply(seq_along(network_params), function(i) {
        params <- network_params[[i]]
        graph <-  create_small_world_network(N = params$N, k = params$k, p = params$p)
        V(graph)$name <- paste(i, as.character(1:params$N), sep="-")
        return(graph)
    })


    # make sure all vertices are named 
    lapply(networks, function(net) {
        
        if (any(is.na(V(net)$name))) {
            stop("Some vertices are not named in a graph.")
        }
    })

    # Union the networks with named vertices
    all_networks <- Reduce(function(x, y) {
        if (length(unique(c(V(x)$name, V(y)$name))) != length(c(V(x)$name, V(y)$name))) {
            stop("Vertex name clashes detected, ensure unique naming across all networks.")
        }
        graph.union(x, y, byname=TRUE)
    }, networks)
    # add edges between random nodes of the networks
    for (i in 1:num_connections) {
        repeat {
            network1 <- sample(networks, 1)[[1]]
            network2 <- sample(networks, 1)[[1]]
            if (!identical(network1, network2)) {
                node1 <- sample(V(network1)$name, 1)
                node2 <- sample(V(network2)$name, 1)
                all_networks <- add_edges(all_networks, c(node1, node2))
                break
            }
        }
    }

    return(all_networks)
}
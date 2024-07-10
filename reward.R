calculate_feedback <- function(agent_id,
                               chosen_item,
                               graph,
                               attribute_weights,
                               items,
                               num_attrib = 6,
                               timestep) {
    "
    Calculate the feedback score for a chosen item based on the agent\'s neighbors learned weights
    args:
        agent_id: the agent\'s ID
        chosen_item: the item chosen by the agent
        graph: igraph object representing the social network
        attribute_weights: matrix of learned weights for each agent
        items: data frame with information items and attributes
        num_attrib: number of attributes for each item
        timestep: the current timestep
    returns:
        total_feedback_score: the total feedback score from the agent\'s neighbors
    "


    neighbors_ids <- neighbors(graph, agent_id)
    feedback_scores <- numeric(length(neighbors_ids))

    item_features <- items[chosen_item, ] # feature values of the chosen item

    for (j in 1:length(neighbors_ids)) {
        neighbor_id <- neighbors_ids[j]
        neighbor_weights <- attribute_weights[neighbor_id, ]

        Q_value <- sum(neighbor_weights * item_features)
        probability <- sigmoid(Q_value)

        if (timestep == 1) {
            feedback_scores[j] <- ifelse(probability < 0.5, 0, 1)
        } else {
            feedback_scores[j] <- ifelse(probability > 0.5, 1, 0)
        }
    }

    total_feedback_score <- sum(feedback_scores)
    return(total_feedback_score)
}

sigmoid <- function(x, c = 10) {
    "
        Sigmoid function with simple scaling by a constant c
    "
    return(1 / (1 + exp(-x / c)))
}
source ('./utils.R')

calculate_feedback <- function(agent_id,
                               chosen_item,
                               graph,
                               attribute_weights,
                               items,
                               timestep,
                               average_estimate,
                               num_attrib = 6) {
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

    # print(average_estimate)
    neighbors_ids <- neighbors(graph, agent_id)
    feedback_scores <- numeric(length(neighbors_ids))

    item_features <- items[chosen_item, ] # feature values of the chosen item

    value_estimates <- numeric(length(neighbors_ids))
    for (j in 1:length(neighbors_ids)) {
        neighbor_id <- neighbors_ids[j]
        neighbor_weights <- attribute_weights[neighbor_id, ]
        value_estimates[j] <- sum(neighbor_weights * item_features)
    }

    # center the value estimates using the average estimate from the previous timestep
    centered_estimate_values <- value_estimates - average_estimate

    for (j in 1:length(neighbors_ids)) {
        probability <- standard_sigmoid_transform(centered_estimate_values[j])
        if (timestep == 1) {
            feedback_scores[j] <- ifelse(probability < 0.5, 0, 1)
            # feedback_scores[j] <- ifelse(runif(1, 0, 1) > 0.5, 1, 0)

        } else {
            feedback_scores[j] <- ifelse(probability > 0.5, 1, 0)
        }
    }
    total_feedback_score <- sum(feedback_scores)
    return(list(total_feedback_score = total_feedback_score, value_estimates = value_estimates))
}
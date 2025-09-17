#' Dijkstra's shortest path algorithm
#'
#' @param graph A data.frame with three variables (v1, v2, w) that contains the edges
#'              of the graph (from v1 to v2) with the weight of the edge (w).
#' @param init_node A numeric scalar that exists in the graph.
#'
#' @return A vector containing the shortest path distance from \code{init_node}
#'         to every other node in the graph.
#'
#' @description
#' This function implements Dijkstra's algorithm, which is a graph search algorithm
#' that solves the single-source shortest path problem for a graph with non-negative
#' edge weights, producing a shortest path tree.
#'
#' @references
#' \url{https://en.wikipedia.org/wiki/Dijkstra\%27s_algorithm}
#'
#' @examples
#' wiki_graph <- data.frame(
#'   v1 = c(1,1,1,2,2,2,3,3,3,3,4,4,4,5,5,6,6,6),
#'   v2 = c(2,3,6,1,3,4,1,2,4,6,2,3,5,4,6,1,3,5),
#'   w = c(7,9,14,7,10,15,9,10,11,2,15,11,6,6,9,14,2,9)
#' )
#' dijkstra(wiki_graph, 1)
#' dijkstra(wiki_graph, 3)
#'
#' @export
dijkstra <- function(graph, init_node) {
  # Check parameter
  if (!is.data.frame(graph)) {
    stop("Error! The graph parameter must be a data frame.")
  }

  if (!all(c("v1", "v2", "w") %in% colnames(graph))) {
    stop("Error! The graph dataframe must contain three columns: v1, v2, and w.")
  }

  if (!is.numeric(init_node) || length(init_node) != 1) {
    stop("Error! init_node must be a numeric scalar.")
  }

  # Get all nodes
  nodes <- sort(unique(c(graph$v1, graph$v2)))

  if (!(init_node %in% nodes)) {
    stop("Error! init_node does not exist in the graph.")
  }

  # Check if the weights are non-negative numbers.
  if (any(graph$w < 0)) {
    stop("Error! Dijkstra's algorithm requires all edge weights to be non-negative.")
  }

  # Initialize distance vector
  dist <- rep(Inf, length(nodes))
  names(dist) <- as.character(nodes)
  dist[as.character(init_node)] <- 0

  # Unprocessed node collection
  C <- nodes

  # Main Loop
  while (length(C) > 0) {
    # Find the node with the smallest distance in L
    x <- C[which.min(dist[as.character(C)])]
    # Remove x from L
    C <- C[C != x]

    # Obtain all edges of x
    edges <- graph[graph$v1 == x, ]
    if (nrow(edges) > 0) {
      for (i in 1:nrow(edges)) {
        y <- edges$v2[i]
        weight <- edges$w[i]
        # Calculate the new distance from x to y
        alt <- dist[as.character(x)] + weight
        # If the new distance is shorter, update
        if (alt < dist[as.character(y)]) {
          dist[as.character(y)] <- alt
        }
      }
    }
  }

  # Return distance vector (in node order)
  return(as.vector(dist[as.character(nodes)]))
}



# test
wiki_graph <- data.frame(v1 = c(1,1,1,2,2,2,3,3,3,3,4,4,4,5,5,6,6,6),
              v2 = c(2,3,6,1,3,4,1,2,4,6,2,3,5,4,6,1,3,5),
              w = c(7,9,14,7,10,15,9,10,11,2,15,11,6,6,9,14,2,9))

dijkstra(wiki_graph, 1)

dijkstra(wiki_graph, 3)


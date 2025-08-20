#' @title Traverse Nodes and Create Dependency Matrix
#'
#' @description
#' Creates a dependency matrix based on the relationships between nodes provided.
#'
#' @details
#' Given a list of nodes, each represented by a named list with 'x' (inputs) and 'y' (outputs) components,
#' this function generates a dependency matrix. The matrix indicates dependencies between nodes, where a value of
#' 1 indicates that a node depends on another node. Dependencies are determined by checking if any of the inputs
#' of a node are outputs of another node. The resulting matrix provides insights into the order in which nodes
#' should be executed in a data processing or analysis pipeline.
#'
#' @param nodes (list) A list of nodes, where each node is represented by a named list with 'x' (inputs) and 'y' (outputs) components.
#'
#' @return Returns a dependency matrix indicating relationships between nodes, where 1 indicates dependency and
#' 0 indicates no dependency. Each row indicates a different node, and the dependency relationship to the nodes in the columns.
#' @export
#'
traverse_nodes <- function(nodes) {
  ## Create dependency matrix
  nodes_names <- sapply(nodes, function(node) {return(node$name)})
  dependencies <- matrix(
    data = 0,
    nrow = length(nodes),
    ncol = length(nodes),
    dimnames = list(nodes_names, nodes_names)
  )
  ## Traverse all nodes to fill matrix
  for(node in nodes) {
    for(other in nodes) {
      ## Skip itself
      if(node$name == other$name) {
        next
      }
      ## Node A depends on Node B if one of A's inputs is one of B's outputs
      dependencies[node$name, other$name] = ifelse(any(node$x %in% other$y), 1, 0)
    }
  }
  return(dependencies)
}

#' @title Create a Node in a Data Processing Pipeline
#'
#' @description
#' Creates a node that represents a function and its inputs and outputs within a data processing or analysis pipeline.
#'
#' @details
#' This function allows the user to define a node in a pipeline, where a node consists of a function (`f`) along with
#' its inputs (`x`) and outputs (`y`). The function can be any R function, and the inputs and outputs are optional.
#' The name of the node can be specified using the `name` parameter, and if not provided, it defaults to the name of the function.
#' The resulting node is a named list with components `f` (function), `x` (inputs), `y` (outputs), and `name` (node name).
#' Nodes are typically used to represent individual steps or processes in a data workflow.
#'
#' @param f (function) The R function that the node represents in the pipeline.
#' @param x (character, optional) Inputs for the node, represented as character vectors.
#' @param y (character, optional) Outputs for the node, represented as character vectors.
#' @param name (character, optional) The name of the node. If not provided, it defaults to the name of the function.
#'
#' @return Returns a node, which is a named list with components representing the function, inputs, outputs, and name.
#' @export
#'
node <- function(f, x = NULL, y = NULL, name = deparse(substitute(f)), env = parent.frame()) {
  ## Validate inputs for node
  stopifnot(is.function(f))
  stopifnot(is.null(x) || is.character(x))
  stopifnot(is.null(y) || is.character(y))
  stopifnot(is.null(name) || is.character(name))
  ## Create node: function + inputs = outputs
  node <- structure(
    .Data = list(
      f = f,
      x = x,
      y = y,
      name = name
    ),
    class = "node"
  )
  ## Assign node to nodes list
  if(exists("__nodes__", envir = env)) {
    nodes <- get("__nodes__", envir = env)
  } else {
    nodes <- list()
  }
  assign("__nodes__", c(nodes, list(node)), envir = env)
  return(node)
}

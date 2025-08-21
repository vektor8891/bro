#' @title Run Node
#'
#' @description
#' Executes a specified node in the workflow, loading input data, running the node's function, and storing the output.
#'
#' @details
#' This function executes a specified node in the workflow. It first loads input data for the node using the "get_data" function.
#' It then executes the node's function using the loaded inputs and stores the output. If the node produces a single output,
#' the output is saved to a file and in memory using the "save_data" function. If the node produces multiple outputs, each output
#' is saved to a file and in memory separately. The status information is updated using the "update_status" function.
#' Messages are displayed indicating the start and end of node execution, along with the runtime.
#'
#' @param node The node to be executed in the workflow.
#' @param execution The execution environment containing input data, output data, and status information.
#'
#' @return Returns the output of the executed node, stored in the execution environment.
#'
run_node <- function(node, execution) {

  message("(bro) Running node '", node$name, "'")
  start <- Sys.time()

  ## Load data
  inputs <- lapply(x = node$x, fun = get_data, execution)

  ## Execute node
  outputs <- base::do.call(what = node$f, args = inputs)

  ## Store data in memory and in a file if necessary
  if(length(node$y) == 1) {
    save_data(outputs, node$y, execution)
  } else {
    for(i in seq_along(node$y)) {
      save_data(outputs[[i]], node$y[[i]], execution)
    }
  }

  ## Update status on file
  bro:::update_status(node, inputs)

  stop <- Sys.time()
  message("(bro) Node '", node$name, "' Runtime: ", difftime(stop, start, units = "secs"), " seconds")

  ## Return output
  return(execution$data[[node$y]])

}

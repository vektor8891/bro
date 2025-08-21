#' @title Detect Changes in Node and Inputs
#'
#' @description
#' Detects changes in a specified node and its inputs by comparing their current status with the previous status stored in the execution environment.
#'
#' @details
#' This function checks if the node definition has changed by calling the "detect_changes_node" function. If changes are detected in the node,
#' it returns TRUE. Otherwise, it checks each input of the node using the "detect_changes_input" function and determines if any input has changed.
#' If any input has changed, the function returns TRUE; otherwise, it returns FALSE.
#'
#' @param node The node for which changes are to be detected.
#' @param execution The execution environment containing the status information for nodes and input data.
#'
#' @return Returns TRUE if changes are detected in the node or its inputs, FALSE otherwise.
#'
detect_changes <- function(node, execution) {
  ## Check if node definition has changed
  if (detect_changes_node(node, execution)) {
    return(TRUE)
  }

  ## Check if any of the inputs have changed
  changed <- sapply(node$x, detect_changes_input, execution)
  if (any(changed)) {
    return(TRUE)
  }

  return(FALSE)
}

#' @title Update Status Information
#'
#' @description
#' Updates the status information with hash values for a specified node and its inputs.
#'
#' @details
#' This function updates the status information with hash values for a specified node and its inputs. It first loads the current
#' status information from the YAML file, then updates the hash value for the specified node and each input data used by the node.
#' Finally, it writes the updated status information back to the YAML file.
#'
#' @param node The node for which the status information is updated.
#' @param inputs The inputs used by the node, including data for which hash values need to be updated.
#'
#' @return Returns NULL after updating the status information.
#'
update_status <- function(node, inputs) {

  ## Load status file
  status <- load_data_status()

  ## Update node hash
  status$nodes[[node$name]] <- rlang::hash(node)

  ## Update data status
  for(name in node$x) {
    execution$status$data[[name]] <- rlang::hash(inputs[[which(node$x == name)]])
  }

  ## Save status file
  write_status(status)

  return(NULL)
}

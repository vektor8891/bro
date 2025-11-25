#' @title Detect Changes in Input
#'
#' @description
#' Detects changes in a specified input data by comparing its current status with the previous status stored in the execution environment.
#'
#' @details
#' This function checks if the specified input data has undergone changes by comparing its current status with the previous status
#' stored in the execution environment. The update strategy specified in the data registry determines how the comparison is made.
#' If no previous status is available, it assumes that the data has been updated. If the update strategy is based on hashing, the
#' function calculates the hash of the data and compares it with the previous hash. If changes are detected, it returns TRUE; otherwise, FALSE.
#'
#' @param name The name of the input data for which changes are to be detected.
#' @param execution The execution environment containing the status information for input data.
#'
#' @return Returns TRUE if changes are detected, FALSE otherwise.
#'
detect_changes_input <- function(name, execution) {

  previous_status <- execution$status$data[[name]]

  ## If no previous status, assume data has been updated
  if(is.null(previous_status)) {
    message("(bro) no previous status for '", name, "'")
    return(TRUE)
  }

  ## Check if input needs to be loaded to be checked for updates
  needs_load <- c("hash")
  strategy <- tolower(execution$registry[[name]]$update)
  if(strategy %in% needs_load) {
    data <- get_data(name, execution)
  }

  ## Check if data was updated compared to recorded status
  if(strategy == "hash") {
    return(rlang::hash(data) == previous_status)
  }

  stop("undefined update strategy '", strategy, "'")

}

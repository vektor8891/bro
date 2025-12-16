#' @title Close Database Connections
#'
#' @description
#' Closes all active database connections in the execution environment.
#'
#' @details
#' This function iterates through all active database connections
#' stored in the execution environment and closes them properly. It's
#' recommended to call this function when finished with database
#' operations to free up resources.
#'
#' @param execution The execution environment containing active
#'   database connections.
#'
#' @return Returns NULL after closing all connections.
#'
#' @export
close_connections <- function(execution) {
  if (is.null(execution$connections)) {
    message("(bro) No active connections to close")
    return(invisible(NULL))
  }

  conn_names <- ls(execution$connections)

  if (length(conn_names) == 0) {
    message("(bro) No active connections to close")
    return(invisible(NULL))
  }

  for (name in conn_names) {
    conn <- get(name, envir = execution$connections)
    tryCatch(
      {
        if (inherits(conn, "RODBC")) {
          ## RODBC connections use close method
          RODBC::odbcClose(conn)
        } else {
          ## DBI connections use dbDisconnect
          DBI::dbDisconnect(conn)
        }
        message("(bro) Closed connection '", name, "'")
      },
      error = function(e) {
        message(
          "(bro) Warning: Could not close connection '",
          name, "': ", e$message
        )
      }
    )
  }

  ## Clear connections environment
  rm(list = conn_names, envir = execution$connections)

  invisible(NULL)
}

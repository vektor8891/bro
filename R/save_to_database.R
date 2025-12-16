#' @title Save Data to Database
#'
#' @description
#' Saves data to a database table.
#'
#' @details
#' This function writes a data frame to a database table. It supports various
#' write modes including append, overwrite, and fail-if-exists. The database
#' connection is managed through the connection system defined in connections.
#' yaml.
#'
#' @param data The data frame to save.
#' @param connection The name of the database connection (from
#'   connections.yaml).
#' @param table The name of the table to write to.
#' @param execution The execution environment.
#' @param overwrite Logical; if TRUE, overwrite existing table (default:
#'   FALSE).
#' @param append Logical; if TRUE, append to existing table (default:
#'   FALSE).
#' @param ... Additional arguments passed to DBI::dbWriteTable.
#'
#' @return Returns TRUE if successful.
#'
save_to_database <- function(
    data,
    connection,
    table,
    execution,
    overwrite = FALSE,
    append = FALSE,
    ...) {
  ## Validate inputs
  if (is.null(table)) {
    stop("'table' must be specified for database save")
  }

  ## Get database connection
  conn <- bro:::get_connection(connection, execution)

  ## Save data
  message(
    "(bro) Writing to table '", table,
    "' on connection '", connection, "'"
  )
  result <- DBI::dbWriteTable(
    conn,
    table,
    data,
    overwrite = overwrite,
    append = append,
    ...
  )

  result
}

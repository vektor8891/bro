#' @title Load Data from Database
#'
#' @description
#' Loads data from a database using a SQL query or table name.
#'
#' @details
#' This function retrieves data from a database. It can either execute
#' a SQL query or read an entire table. The database connection is
#' managed through the connection system defined in connections.yaml.
#'
#' @param connection The name of the database connection (from
#'   connections.yaml).
#' @param query The SQL query to execute (optional if table is
#'   provided).
#' @param table The name of the table to read (optional if query is
#'   provided).
#' @param execution The execution environment.
#' @param ... Additional arguments passed to DBI::dbGetQuery or
#'   DBI::dbReadTable.
#'
#' @return Returns a data frame containing the query results.
#'
load_from_database <- function(
    connection,
    query = NULL,
    table = NULL,
    execution,
    ...) {
  ## Validate inputs
  if (is.null(query) && is.null(table)) {
    stop("Either 'query' or 'table' must be specified for database load")
  }

  if (!is.null(query) && !is.null(table)) {
    stop("Only one of 'query' or 'table' should be specified, not both")
  }

  ## Get database connection
  conn <- bro:::get_connection(connection, execution)

  ## Load data
  if (!is.null(query)) {
    message(
      "(bro) Executing SQL query on connection '",
      connection, "'"
    )
    data <- DBI::dbGetQuery(conn, query, ...)
  } else {
    message(
      "(bro) Reading table '", table,
      "' from connection '", connection, "'"
    )
    data <- DBI::dbReadTable(conn, table, ...)
  }

  data
}

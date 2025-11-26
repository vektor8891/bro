#' @title Get Database Connection
#'
#' @description
#' Retrieves or creates a database connection based on the specified
#' connection name.
#'
#' @details
#' This function manages database connections in a centralized way. It
#' checks if a connection with the specified name already exists in the
#' execution environment. If it does and is still valid, it returns the
#' existing connection. Otherwise, it creates a new connection using the
#' configuration defined in the connections registry (connections.yaml).
#'
#' The function supports multiple database backends (PostgreSQL, MySQL,
#' SQLite, Redshift, Netezza, etc.) through a pluggable system. Each
#' backend is defined in the connections.yaml file with its specific
#' connection parameters.
#'
#' @param name The name of the connection as defined in
#'   connections.yaml.
#' @param execution The execution environment containing the connections
#'   registry and active connections.
#'
#' @return Returns a database connection object (DBI connection).
#'
#' @export
get_connection <- function(name, execution) {
  ## Check if connection already exists and is valid
  if (!is.null(execution$connections) &&
    base::exists(
      x = name,
      where = execution$connections,
      inherits = FALSE
    )) {
    conn <- base::get(name, envir = as.environment(execution$connections))
    if (DBI::dbIsValid(conn)) {
      message("(bro) Reusing existing connection '", name, "'")
      return(conn)
    } else {
      message(
        "(bro) Existing connection '", name,
        "' is invalid, creating new connection"
      )
    }
  }

  ## Get connection registry
  if (is.null(execution$connections_registry)) {
    stop(
      "No connections registry found. Please ensure ",
      "connections.yaml is loaded."
    )
  }

  registry <- execution$connections_registry

  ## Check if connection exists in registry
  if (!name %in% names(registry)) {
    stop("Connection '", name, "' not found in connections.yaml")
  }

  config <- registry[[name]]

  ## Validate required fields
  if (is.null(config$backend)) {
    stop(
      "Missing 'backend' field for connection '", name,
      "' in connections.yaml"
    )
  }

  ## Create connection based on backend type
  conn <- bro:::create_connection(config, name)

  ## Store connection in execution environment
  if (is.null(execution$connections)) {
    execution$connections <- new.env()
  }
  base::assign(name, conn, pos = execution$connections)

  message(
    "(bro) Created new connection '", name,
    "' using backend '", config$backend, "'"
  )

  conn
}

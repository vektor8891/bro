#' @title Create Database Connection
#'
#' @description
#' Creates a database connection based on the backend type and configuration.
#'
#' @details
#' This internal function creates database connections using the
#' appropriate DBI driver based on the backend type specified in the
#' configuration. It supports multiple database backends including
#' PostgreSQL, MySQL, SQLite, Redshift, Netezza, and others.
#'
#' The function reads credentials from a credentials.yaml file if
#' credential_key is specified, allowing sensitive information to be
#' stored separately from connection configurations.
#'
#' Supported backends:
#' - postgres: PostgreSQL databases using RPostgres
#' - mysql: MySQL/MariaDB databases using RMySQL
#' - sqlite: SQLite databases using RSQLite
#' - redshift: Amazon Redshift using RPostgres
#' - netezza: IBM Netezza using RODBC
#' - odbc: Generic ODBC connections
#'
#' @param config The connection configuration from connections.yaml.
#' @param name The name of the connection (for messaging).
#'
#' @return Returns a DBI connection object.
#'
create_connection <- function(config, name) {
  backend <- tolower(config$backend)

  ## Load credentials if credential_key is specified
  if (!is.null(config$credential_key)) {
    credentials <- bro:::load_credentials(config$credential_key)
    ## Merge credentials into config (credentials override config)
    config <- utils::modifyList(config, credentials)
  }

  ## Create connection based on backend type
  conn <- switch(backend,
    "postgres" = {
      if (!requireNamespace("RPostgres", quietly = TRUE)) {
        stop(
          "Package 'RPostgres' is required for PostgreSQL ",
          "connections. Install it with: ",
          "install.packages('RPostgres')"
        )
      }
      DBI::dbConnect(
        RPostgres::Postgres(),
        host = config$host %||% "localhost",
        port = config$port %||% 5432,
        dbname = config$database %||% config$dbname,
        user = config$user %||% config$username,
        password = config$password
      )
    },
    "mysql" = {
      if (!requireNamespace("RMySQL", quietly = TRUE)) {
        stop(
          "Package 'RMySQL' is required for MySQL connections. ",
          "Install it with: install.packages('RMySQL')"
        )
      }
      DBI::dbConnect(
        RMySQL::MySQL(),
        host = config$host %||% "localhost",
        port = config$port %||% 3306,
        dbname = config$database %||% config$dbname,
        user = config$user %||% config$username,
        password = config$password
      )
    },
    "sqlite" = {
      if (!requireNamespace("RSQLite", quietly = TRUE)) {
        stop(
          "Package 'RSQLite' is required for SQLite connections. ",
          "Install it with: install.packages('RSQLite')"
        )
      }
      DBI::dbConnect(
        RSQLite::SQLite(),
        dbname = config$database %||% config$path %||% ":memory:"
      )
    },
    "redshift" = {
      if (!requireNamespace("RPostgres", quietly = TRUE)) {
        stop(
          "Package 'RPostgres' is required for Redshift ",
          "connections. Install it with: ",
          "install.packages('RPostgres')"
        )
      }
      DBI::dbConnect(
        RPostgres::Postgres(),
        host = config$host,
        port = config$port %||% 5439,
        dbname = config$database %||% config$dbname,
        user = config$user %||% config$username,
        password = config$password
      )
    },
    "netezza" = {
      if (!requireNamespace("RODBC", quietly = TRUE)) {
        stop(
          "Package 'RODBC' is required for Netezza connections. ",
          "Install it with: install.packages('RODBC')"
        )
      }
      ## Build connection string
      conn_str <- sprintf(
        paste0(
          "DRIVER={%s};SERVER=%s;PORT=%s;",
          "DATABASE=%s;UID=%s;PWD=%s"
        ),
        config$driver %||% "NetezzaSQL",
        config$host,
        config$port %||% 5480,
        config$database %||% config$dbname,
        config$user %||% config$username,
        config$password
      )
      RODBC::odbcDriverConnect(conn_str)
    },
    "odbc" = {
      if (!requireNamespace("odbc", quietly = TRUE)) {
        stop(
          "Package 'odbc' is required for ODBC connections. ",
          "Install it with: install.packages('odbc')"
        )
      }
      ## Use dsn if provided, otherwise build connection
      if (!is.null(config$dsn)) {
        DBI::dbConnect(
          odbc::odbc(),
          dsn = config$dsn,
          uid = config$user %||% config$username,
          pwd = config$password
        )
      } else {
        DBI::dbConnect(
          odbc::odbc(),
          driver = config$driver,
          server = config$host %||% config$server,
          database = config$database %||% config$dbname,
          uid = config$user %||% config$username,
          pwd = config$password,
          port = config$port
        )
      }
    },

    ## Default case
    stop(
      "Unsupported backend type '", backend,
      "'. Supported backends: postgres, mysql, sqlite, redshift, ",
      "netezza, odbc"
    )
  )

  conn
}

## Helper function for NULL coalescing
`%||%` <- function(a, b) {
  if (is.null(a)) b else a
}

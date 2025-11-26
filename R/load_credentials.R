#' @title Load Database Credentials
#'
#' @description
#' Loads database credentials from a credentials.yaml file.
#'
#' @details
#' This function loads credentials from a YAML file (default:
#' inst/credentials.yaml). The credentials file should be structured
#' with named credential sets, each containing the necessary
#' authentication information for database connections.
#'
#' The credentials.yaml file is typically excluded from version control
#' (via .gitignore) to protect sensitive information.
#'
#' Example credentials.yaml structure:
#' ```yaml
#' prod_db:
#'   user: myuser
#'   password: mypassword
#'   host: prod-server.example.com
#' ```
#'
#' @param key The name of the credential set to load from
#'   credentials.yaml.
#' @param path The path to the credentials file (default:
#'   inst/credentials.yaml).
#'
#' @return Returns a list containing the credential information.
#'
load_credentials <- function(
    key,
    path = file.path("inst", "credentials.yaml")) {
  if (!file.exists(path)) {
    stop(
      "Credentials file not found at '", path,
      "'. Please create it with your database credentials."
    )
  }

  credentials_file <- yaml::read_yaml(path)

  if (!key %in% names(credentials_file)) {
    stop("Credential key '", key, "' not found in ", path)
  }

  credentials_file[[key]]
}

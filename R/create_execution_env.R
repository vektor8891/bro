#' @title Create Execution Environment
#'
#' @description
#' Creates and initializes an execution environment, loading default
#' parameters, data registry, and data status.
#'
#' @details
#' This function allows the creation of an execution environment, which
#' is essential for managing parameters, data registry, and data status
#' in a data science project. If no environment is provided, a new
#' environment is created. Default file paths for parameters, data
#' registry, and data status are specified and loaded into the
#' environment. The loaded information includes parameters from a YAML
#' file, data registry from another YAML file, and data status using
#' the bro package. Messages are displayed indicating the successful
#' loading of each component.
#'
#' @param env An optional environment to use; if not provided, a new
#'   environment is created.
#'
#' @return Returns the initialized execution environment with loaded
#'   parameters, data registry, and data status.
#'
create_execution_env <- function(env = NULL) {
  if (is.null(env)) {
    env <- new.env()
  }

  defaults <- list(
    parameters = file.path("inst", "parameters.yaml"),
    registry = file.path("inst", "data.yaml"),
    connections = file.path("inst", "connections.yaml"),
    status = file.path("data", "status.yaml")
  )

  env$parameters <- yaml::read_yaml(defaults$parameters)
  message(
    "(bro) Loaded parameters from '", defaults$parameters, "'"
  )

  env$registry <- yaml::read_yaml(defaults$registry)
  message(
    "(bro) Loaded data registry from '", defaults$registry, "'"
  )

  ## Load connections registry if file exists
  if (file.exists(defaults$connections)) {
    env$connections_registry <- yaml::read_yaml(defaults$connections)
    message(
      "(bro) Loaded connections registry from '",
      defaults$connections, "'"
    )
  } else {
    env$connections_registry <- list()
    message(
      "(bro) No connections registry found at '",
      defaults$connections, "'"
    )
  }

  env$status <- bro:::load_data_status()
  message(
    "(bro) Loaded data status from '", defaults$status, "'"
  )

  env$data <- new.env()

  env
}

#' Returns an environment with hooks, data catalog and parameters.
#'
#' @description
#' Creates a 'context' environment with hooks, data catalog and parameters.
#' This environment is used to separate the execution of the pipelines from
#' variables in the global environment. This environment can also be used to
#' debug your code, since it exposes the data catalog and can be used with the
#' other exported functions in the framebar.
#'
#' @details
#' The environment 'context' is created by loading a parameters file, either
#' conf/base/parameters.yml or another one specified by the user.
#'
#' It also adds the following extra values to the list of parameters:
#' - 'current_date_and_time': the current date and time in the format
#'   "%Y%m%d_%H%M%S".
#' - 'parameters_file_name': the name of the parameters file used to create the
#'   context.
#' If either of these parameters are already present in the parameters file, an
#' error is thrown.
#'
#' Then, it loads the data catalog file in conf/base/catalog.yml.
#' Then, it loads the credentials file in conf/local/credentials.yml if it
#' exists.
#' Finally, it sources the hooks file in src/project_name/hooks.R. When hooks
#' are sourced, all of the files containing pipelines, nodes and functions will
#' run, thus making all vailable in the environment. If multiple functions have
#' the same name, make sure that, in the hooks file, they are sourced to
#' different environments.
#'
#' @param parameters (string, optional) Name of a parameters YML file stored in
#' the conf/base folder. File must begin with "parameters" and end with ".yml".
#' If unspecified, the default parameters file named "parameters.yml" is loaded
#' in the environment. If specified, loads the specified parameters file into
#' the environment instead.
#' @param global (boolean, optional) If TRUE, adds parameters, catalog and
#' hooks to the global environment.
#' @param verbose (boolean, optional) If TRUE, prints messages.
#' @return (environment) Environment with hooks, catalog and parameters.
#' @export
get_context <- function(
    parameters = "parameters.yml", global = FALSE,
    verbose = TRUE) {
  context <- if (global) .GlobalEnv else base::new.env()
  ## Parameter file must follow pattern *.yml
  stopifnot(base::endsWith(parameters, ".yml"))
  ## Load parameters and catalog
  if (verbose) {
    print(glue::glue("Parameters file used: {parameters}"))
  }
  context$parameters <- yaml::read_yaml(file.path("conf", "base", parameters),
    eval.expr = TRUE
  )
  ## Save date and time
  if ("current_date_and_time" %in% names(context$parameters)) {
    stop(
      "'current_date_and_time' is a reserved parameter name.",
      " Please avoid using it in your parameters file."
    )
  }
  context$parameters$current_date_and_time <- format(
    Sys.time(),
    "%Y%m%d_%H%M%S"
  )
  ## Save parameters file name
  if ("parameters_file_name" %in% names(context$parameters)) {
    stop(
      "'parameters_file_name' is a reserved parameter name.",
      " Please avoid using it in your parameters file."
    )
  }
  context$parameters$parameters_file_name <- parameters
  context$catalog <- yaml::read_yaml(file.path("conf", "base", "catalog.yml"),
    eval.expr = TRUE
  )
  ## Load credentials if available
  base::tryCatch(
    context$credentials <- yaml::read_yaml(base::file.path(
      "conf", "local", "credentials.yml"
    )),
    warning = invisible,
    error = invisible
  )
  ## Find hooks.R
  path_hooks <- list.files(
    path = "src", pattern = "^hooks.R$",
    recursive = TRUE, include.dirs = TRUE
  )
  source(file.path("src", path_hooks), chdir = TRUE, local = context)
  if (length(path_hooks) > 1) {
    warning(paste0(
      "Loaded multiple hooks.R: ",
      paste0(path_hooks, collapse = ", ")
    ))
  }
  return(context)
}

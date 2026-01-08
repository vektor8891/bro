#' Retrieves a variable with a given name from memory or disk.
#'
#' @description
#' If the variable is not in memory, utilizes the catalog in context to find
#' the data on disk. If the data is not available, stops execution. If the
#' variable is in memory, this function can retrieve the variable itself, or
#' any of its elements (using the `$` operator).
#'
#' @param varname (string) Name of variable or data piece to retrieve.
#' @param parameters (string, optional) Name of a parameters YML file stored in
#' the conf/base folder. File must begin with "parameters" and end with ".yml".
#' If unspecified, the default parameters file named "parameters.yml" is loaded
#' in the environment. If specified, loads the specified parameters file into
#' the environment instead.
#' @param context (environment, optional) An environment containing a data
#' catalog and parameters. If unspecified, will attempt to create such an
#' environment from the current working directory (requires the current working
#' directory to follow framebar's file structure).
#' @param force_reload (boolean, optional) If TRUE, forces a reload of the
#' variable from disk, even if it is already in memory. If FALSE, the function
#' will check if the variable is already in memory and return it without
#' reloading it from disk. Default is FALSE.
#' @param verbose (boolean, optional) If TRUE, prints a message when
#' successfully loaded data.
#' @return (object) Object containing data.
#'
#'
#' @export
retrieve_variable <- function(
    varname, parameters = "parameters.yml",
    context = NULL, force_reload = FALSE, verbose = TRUE) {
  ## If no environment supplied, use .GlobalEnv for loading data
  if (is.null(context)) {
    context <- get_context(parameters = parameters, global = TRUE)
  }
  if (base::exists(
    varname,
    where = context, inherits = FALSE
  ) && !force_reload) {
    # If variable in Memory
    if (verbose) {
      print(paste0("LOAD MEMORY ", varname))
    }
    return(base::get(varname, envir = context))
  } else if (base::grepl(
    pattern = "$", x = varname, fixed = TRUE
  ) && !force_reload) {
    # If variable is value in parameters
    splitted <- base::unlist(base::strsplit(varname, "$", fixed = TRUE))
    extract <- retrieve_variable(splitted[[1]],
      context = context,
      verbose = FALSE
    )
    for (value in splitted[-1]) {
      extract <- extract[[value]]
    }
    if (verbose) {
      print(paste0("LOAD MEMORY ", varname))
    }
    return(extract)
  } else if (varname %in% names(context$catalog)) {
    # If variable in Data Catalog
    return(loading(varname, context, parameters, verbose))
  } else { # If variable cannot be found
    stop(paste0("Could not find: ", varname))
  }
}

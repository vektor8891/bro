#' @title Get Data
#'
#' @description
#' Retrieves and returns a specified dataset from the execution environment or loads it if not already loaded.
#'
#' @details
#' This function checks if the requested dataset is a parameter; if so, it directly retrieves it from the execution environment.
#' If the dataset is not a parameter and is already loaded in the execution environment, it is retrieved from there.
#' If the dataset is neither a parameter nor loaded, the function uses the "load_data" function to load it from the data registry.
#'
#' @param name The name of the dataset to be retrieved or loaded.
#' @param execution The execution environment containing the data registry and loaded datasets.
#'
#' @return Returns the specified dataset, either retrieved from the environment or loaded from the data registry.
#'
get_data <- function(name, execution) {
  ## Check if data requested is a parameter
  if (base::grepl(pattern = "^parameters($|\\$)", x = name)) {
    return(eval(parse(text = paste0("execution$", name))))
  }

  ## Check if data is already loaded in the execution environment
  if (!is.null(execution$data) && base::exists(x = name, where = execution$data, inherits = FALSE)) {
    return(base::get(name, envir = as.environment(execution$data)))
  }

  ## Load from registry
  return(load_data(name = name, execution = execution))
}

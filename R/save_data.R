#' @title Save Data
#'
#' @description
#' Saves a specified dataset to a file based on the information in the data registry in the given execution environment.
#'
#' @details
#' This function saves a specified dataset to a file based on the information in the data registry in the provided execution environment.
#' If the dataset is not in the registry, it is saved in memory only, and a message is displayed. If the dataset is in the registry,
#' the function determines the data type and saves it to a file using the specified data saver method. Messages are displayed indicating
#' the successful saving of the dataset, including its name, type, and saving method.
#'
#' @param data The dataset to be saved.
#' @param name The name under which the dataset is stored in the execution environment and registered in the data registry.
#' @param execution The execution environment containing the data registry.
#'
save_data <- function(data, name, execution) {

  ## Get data registry from execution environment
  registry <- execution$registry

  ## If data not in registry, leave it in memory only
  if(!name %in% names(registry)) {
    message("(bro) Saving In-memory '", name, "'")
  } else {
    ## Save data
    type <- registry[[name]]$type
    path <- do.call(file.path, append(list(), registry[[name]]$path))
    saver <- eval(parse(text = connectors[[type]]$save))
    message("(bro) Saving to File '", name, "' (", type, ", ", connectors[[type]]$save, ")")
    do.call(what = saver, args = append(list(file = path), registry[[name]]$save_args))
  }

  base::assign(name, data, pos = execution$data)
}

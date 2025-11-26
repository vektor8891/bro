#' @title Save Data
#'
#' @description
#' Saves a specified dataset to a file based on the information in the
#' data registry in the given execution environment.
#'
#' @details
#' This function saves a specified dataset to a file based on the
#' information in the data registry in the provided execution environment.
#' If the dataset is not in the registry, it is saved in memory only, and
#' a message is displayed. If the dataset is in the registry, the function
#' determines the data type and saves it to a file using the specified
#' data saver method. Messages are displayed indicating the successful
#' saving of the dataset, including its name, type, and saving method.
#'
#' @param data The dataset to be saved.
#' @param name The name under which the dataset is stored in the
#'   execution environment and registered in the data registry.
#' @param execution The execution environment containing the data
#'   registry.
#'
save_data <- function(data, name, execution) {
  ## Get data registry from execution environment
  registry <- execution$registry

  ## If data not in registry, leave it in memory only
  if (!name %in% names(registry)) {
    message("(bro) Saving In-memory '", name, "'")
  } else {
    ## Save data
    type <- registry[[name]]$type
    saver <- eval(parse(text = bro:::connectors[[type]]$save))
    message(
      "(bro) Saving to File '", name, "' (", type, ", ",
      bro:::connectors[[type]]$save, ")"
    )

    ## For database destinations, pass connection and table parameters
    if (type == "sql") {
      save_args <- list(
        data = data,
        connection = registry[[name]]$connection,
        table = registry[[name]]$table,
        execution = execution
      )
      ## Add any additional save arguments (overwrite, append, etc.)
      if (!is.null(registry[[name]]$save_args)) {
        save_args <- c(save_args, registry[[name]]$save_args)
      }
    } else {
      ## For file-based destinations, use traditional approach
      path <- do.call(file.path, append(list(), registry[[name]]$path))
      save_args <- append(
        list(x = data, file = path),
        registry[[name]]$save_args
      )
    }

    do.call(what = saver, args = save_args)
  }

  base::assign(name, data, pos = execution$data)
}

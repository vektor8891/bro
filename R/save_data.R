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
#' @export
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

    # Check if connector exists
    if (!type %in% names(connectors)) {
      stop(
        "unsupported data type '", type, "'. Available types: ",
        paste(names(connectors), collapse = ", ")
      )
    }

    # Extract package name from saver function
    saver_func <- connectors[[type]]$save
    package_name <- strsplit(saver_func, "::")[[1]][1]

    # Check if package is available for non-base packages
    imported_pkgs <- .get_imported_packages() # nolint: object_usage_linter
    if (package_name != "base" && !(package_name %in% imported_pkgs)) {
      if (
        !.safe_require_namespace(package_name) # nolint: object_usage_linter
      ) {
        stop(
          "Cannot save '", type, "' files without package '", package_name,
          "'"
        )
      }
    }

    saver <- eval(parse(text = saver_func))
    message(
      "(bro) Saving to File '", name, "' (", type, ", ",
      saver_func, ")"
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

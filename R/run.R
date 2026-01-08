#' Executes all nodes listed in a pipeline.
#'
#' @description
#' Pipelines are a sequence of nodes that can be run in sequence.
#' Pipelines are given names in the hooks.R file.
#' Given a name of a hooked pipeline, framebar will execute each of its nodes
#' in a separate environment. The environment is returned at the end of the
#' execution. Each call to run creates and runs on a new environment.
#'
#' @param hook (string, optional) String used to recover the pipeline(s) from
#' the hooks list. Defaults to the 'default' pipeline.
#' @param parameters (string, optional) Name of a parameters YML file stored in
#' the conf/base folder. File must begin with "parameters" and end with ".yml".
#' If unspecified, uses the default parameters file named "parameters.yml". If
#' specified, all references to "parameters" within a node will use the
#' specified parameters file. Any output file that is saved will be stored
#' under a folder with same name as the parameters file. For example, an output
#' named "results.rds" generated with a parameters file named
#' "parameters_2020.yml" would be saved in as "parameters_2020.yml/results.rds".
#' @param cached (boolean, optional) If FALSE, executes each node. If TRUE,
#' only execute a node if its outputs do not exist in memory or in disk (local
#' or remote). Does not support versioned data.
#' @param overwrite (list, optional) List of outputs that will be overwritten
#' even in cached mode (i.e. the corresponding node will be executed even if
#' the output exists).
#' @param rerun_intermediary (boolean, optional) If TRUE, executes intermediary
#' nodes even when their outputs are not required in future nodes with cached
#' outputs.
#' @param clear (boolean, optional) If TRUE, clears variables from memory as
#' they stop being used by nodes. Always keep outputs from last node.
#' @param verbose (boolean, optional) If TRUE, prints information about each
#' node during execution.
#' @return (environment) Environment containing the catalog, parameters,
#' connection and outputs from nodes.
#' @importFrom tictoc tic toc
#' @export
run <- function(hook = "default",
                parameters = "parameters.yml",
                cached = FALSE,
                overwrite = c(),
                rerun_intermediary = FALSE,
                clear = FALSE,
                verbose = TRUE) {
  ## Load parameters, catalog and hooks into environment
  context <- get_context(parameters = parameters)
  context$context <- context # reference iself so it can be used as input
  ## Store runtime (used for versioning outputs)
  runtime <- base::format(base::Sys.time(), format = "%Y%m%d%H%M%S")
  ## Run hooked pipelines
  pipeline <- context$hooks[[hook]]
  tic(hook)
  if (is.null(pipeline)) {
    base::stop(glue::glue("Undefined pipeline '{hook}'. Available pipelines are: {paste0(names(context$hooks), collapse = ', ')}. Visit hooks.R to add a new entry.")) # nolint
  }
  for (i in seq_along(pipeline)) {
    node <- pipeline[[i]]
    tic(node$name)
    if (verbose) {
      print(paste0("==== NODE: ", node$name, " ===="))
    }
    checks <- base::unlist(purrr::map(node$outputs, var_exists,
      context = context, parameters = parameters
    ))
    if (cached && all(checks) && !any(
      node$outputs %in% overwrite
    ) && !is.null(node$outputs)) {
      ## Skip node if
      ## 1) cache mode is ON AND
      ## 2) all outputs exist AND
      ## 3) none of the outputs should be overwritten AND
      ## 4) output is not NULL.
      print(glue::glue("CACHE {node$outputs}"))
    } else {
      ## Load inputs for node
      inputs <- purrr::map(node$inputs, retrieve_variable,
        parameters = parameters, context = context, verbose = verbose
      )
      ## Execute function
      if (verbose) {
        print("EXECUTING FUNCTION")
      }
      results <- base::do.call(node$func, inputs)
      ## Save results in memory or disk
      if (length(node$outputs) == 1) { # avoided map2 due to edge cases
        saving(
          node$outputs, results, context, verbose, runtime,
          parameters
        )
      } else {
        for (j in seq_along(node$outputs)) {
          saving(
            node$outputs[[j]], results[[j]], context, verbose,
            runtime, parameters
          )
        }
      }
    }
    time <- toc(quiet = TRUE)
    print(paste("(INFO)", time$callback_msg))
    ## Clear outputs that are not reused
    if (clear) clear_memory(node, pipeline[-1:-i], context)
  }
  hook_time <- toc(quiet = TRUE)
  print(paste("(INFO)", hook_time$callback_msg))
  if (verbose) {
    print("=== DONE ===")
  }
  return(context)
}

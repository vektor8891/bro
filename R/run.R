#' Run a pipeline
#'
#' @description
#' Executes a pipeline with the given parameters, caching options, and hooks.
#'
#' @param hook (string, optional) Name of the pipeline hook to execute.
#'   Defaults to "default".
#' @param parameters (string, optional) Path to the parameters YAML file.
#'   Defaults to "parameters.yml".
#' @param cached (logical) Whether to use cached results. Defaults to FALSE.
#' @param overwrite (character) Names of outputs to overwrite. Defaults to an
#'   empty character vector.
#' @param verbose (logical) Whether to print detailed execution logs. Defaults
#'   to TRUE.
#' @return (environment) An environment containing the pipeline's outputs,
#'   parameters, and catalog.
#' @export
run <- function(hook = "default",
                parameters = "parameters.yml",
                cached = FALSE,
                overwrite = character(),
                verbose = TRUE) {
  # Load context (parameters, catalog, hooks)
  context <- bro::get_context(parameters = parameters)
  context$context <- context # Self-reference for use as input

  # Retrieve the pipeline from hooks
  pipeline <- context$hooks[[hook]]
  if (is.null(pipeline)) {
    stop(glue::glue(
      "Undefined pipeline '{hook}'. Available pipelines: ",
      "{paste(names(context$hooks), collapse = ', ')}."
    ))
  }

  # Execute each node in the pipeline
  for (node in pipeline) {
    if (verbose) {
      message(glue::glue("==== NODE: {node$name} ===="))
    }

    # Check if node outputs exist and handle caching
    outputs_exist <- all(
      purrr::map_lgl(node$outputs, bro:::var_exists, context = context)
    )
    if (cached && outputs_exist && !any(node$outputs %in% overwrite)) {
      if (verbose) {
        message(glue::glue("CACHE {node$outputs}"))
      }
      next
    }

    # Load inputs for the node
    inputs <- purrr::map(
      node$inputs, bro::retrieve_variable,
      context = context, verbose = verbose
    )

    # Execute the node's function
    results <- do.call(node$func, inputs)

    # Save the node's outputs
    purrr::walk2(
      node$outputs, results, bro:::save_variable,
      context = context, verbose = verbose
    )
  }

  if (verbose) {
    message("=== DONE ===")
  }

  context
}

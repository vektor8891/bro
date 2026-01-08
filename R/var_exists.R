#' Checks if a variable exists in memory or disk.
#'
#' Given the name of a variable, check if it exists in memory or in disk (local
#' or remote).
#' @param output (string) Name of a variable.
#' @param context (environment) Environment containing data catalog.
#' @param parameters (string, optional) Name of a parameters YML file stored in
#' the conf/base folder.
#' @importFrom glue glue
#' @importFrom DBI dbExistsTable Id
#' @importFrom stringr str_split
#' @return (boolean) TRUE if variable exists in memory or disk.
var_exists <- function(output, context, parameters = "parameters.yml") {
  ## In memory
  if (base::exists(output, where = context, inherits = FALSE)) {
    return(TRUE)
  }
  ## In disk (local or remote)
  if (output %in% names(context$catalog)) {
    vartype <- context$catalog[[output]]$type
    filepath_raw <- context$catalog[[output]]$filepath
    filepath <- glue(filepath_raw, .envir = context)
    if (length(filepath) == 0) {
      stop(paste0("Couldn't evaluate filepath for ", output, ". Stop."))
    }
    if (is_remote(vartype)) {
      remote_table <- str_split(filepath, "\\.")[[1]]
      connection <- get_connection(vartype, context)
      return(dbExistsTable(connection, Id(
        schema = remote_table[1],
        table = remote_table[2]
      )))
    } else { # local files are potentially versioned
      versioned <- context$catalog[[output]]$versioned
      if (!is.null(versioned) && versioned == TRUE) {
        filepath <- load_versioned(vartype, filepath, context)
      }
      # include parameters folder if file path is not in 01_raw
      if (!base::grepl(
        "01_raw", filepath,
        fixed = TRUE
      ) && parameters != "parameters.yml") {
        filepath <- base::file.path(
          base::dirname(filepath), parameters,
          base::basename(filepath)
        )
      }
      return(base::file.exists(filepath))
    }
  }
  return(FALSE)
}

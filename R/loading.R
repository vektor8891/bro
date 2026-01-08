#' Loads data with entry in data catalog from disk, using the appropriate
#' method depending on the data type.
#'
#' @description
#' Given a variable name, loads it according to the specifications of the data
#' catalog. The supported data types are NetezzaSQL, Redshift, RDS, CSV, Delim,
#' PNG, Excel, Table, xgb.DMatrix, XML, JSON, lgb.Dataset, lgb.Model, umap.Model. If the
#' variable is not specified in the data catalog, the variable is loaded from
#' memory. If variable cannot be found in memory, interrupts.
#'
#' @details
#' Loading data uses the following functions for each of the available data
#' types:
#' \describe{
#'   \item{NetezzaSQL}{Entry in data catalog must specify a filepath of the
#' format "SCHEMA.TABLE_NAME".
#'     Framebar uses \link[dplyr]{tbl} to load the remote table.}
#'   \item{Redshift}{Entry in data catalog must specify a filepath of the
#' format "SCHEMA.TABLE_NAME".
#'     Framebar uses \link[dplyr]{tbl} to load the remote table.}
#'   \item{RDS}{Uses \link[base]{readRDS}, no optional arguments supported.}
#'   \item{CSV}{Uses \link[readr]{read_csv}. Optional arguments can be
#' specified under "load_args" in the data catalog.}
#'   \item{Delim}{Uses \link[readr]{read_delim}. Optional arguments can be
#' specified under "load_args" in the data catalog.}
#'   \item{xgb.DMatrix}{Uses \link[xgboost]{xgb.DMatrix} to load model data.}
#'   \item{PNG}{Uses \link[png]{readPNG} to load printed figures. Does not
#' support optional arguments.}
#'   \item{Excel}{Uses \link[readxl]{read_excel} to load Excel files.}
#'   \item{XML}{Uses \link[xml2]{read_xml} to load XML files.}
#'   \item{JSON}{Uses \link[jsonlite]{fromJSON} to read JSON files.}
#'   \item{lgb.Dataset}{Uses \link[lightgbm]{lgb.Dataset} to load model data.}
#'   \item{lgb.load}{Uses \link[lightgbm]{lgb.load} to load a model.}
#'   \item{umap.load}{Uses \link[uwot]{load_uwot} to load a model.}
#'   \item{Parquet}{Uses \link[arrow]{read_parquet} to load Parquet files.}
#' }
#' If the data type is not currently supported, an error message is printed.
#'
#' @param parameters  (string, optional) Name of a parameters YML file stored
#' in the conf/base folder. File must begin with "parameters" and end with
#' ".yml". If unspecified, then data is loaded directly from the specified
#' filepath entry in the data catalog. If parameters is specified, data is
#' loaded on a modified version of the filepath entry in the data catalog. The
#' new filepath has the format "filepath/parameters_file.yml/". If the data is
#' versioned, loads the data with the most current timestamp.
#' @inheritParams saving
#' @importFrom dbplyr in_schema
#' @importFrom dplyr tbl
#' @importFrom glue glue
#' @importFrom png readPNG
#' @importFrom readxl read_excel
#' @importFrom xml2 read_xml
#' @importFrom jsonlite fromJSON
#' @importFrom xgboost xgb.DMatrix
#' @importFrom lightgbm lgb.Dataset lgb.load
#' @importFrom uwot load_uwot
#' @importFrom arrow read_parquet
#' @importFrom readr read_csv read_delim
#'
#' @return (object) Object with data.
loading <- function(
    varname, context, parameters = "parameters.yml",
    verbose = TRUE) {
  vartype <- context$catalog[[varname]]$type
  if (verbose) {
    print(paste0("LOAD CATALOG ", vartype, " ", varname))
  }
  ## Get path to file
  filepath_raw <- context$catalog[[varname]]$filepath
  if (is.null(filepath_raw)) {
    stop(paste0("No filepath for ", varname, ". Stop."))
  }
  filepath <- glue(filepath_raw, .envir = context)
  if (length(filepath) == 0) {
    stop(paste0("Couldn't evaluate filepath for ", varname, ". Stop."))
  }
  ## Modify filepath if using a specific parameters file (only local files)
  print(parameters)
  if (
    parameters != "parameters.yml" &&
      !is_remote(vartype) &&
      !base::grepl("01_raw", filepath, fixed = TRUE)
  ) { # do not modify path when loading raw data
    filepath <- base::file.path(
      base::dirname(filepath), parameters,
      base::basename(filepath)
    )
  }
  ## Modify load path if versioned
  versioned <- context$catalog[[varname]]$versioned
  if (!is.null(versioned) && versioned == TRUE) {
    filepath <- load_versioned(vartype, filepath, context)
  }
  load_args <- context$catalog[[varname]]$load_args # possibly NULL
  if (is_remote(vartype)) {
    # creates connection on context
    connection <- get_connection(vartype, context)
    # split to get schema and table name
    schema_name <- base::strsplit(filepath, "\\.")[[1]]
    return(tbl(connection, in_schema(
      schema_name[[1]],
      schema_name[[2]]
    )))
  } else if (vartype == "RDS") {
    return(base::readRDS(filepath))
  } else if (vartype == "CSV") {
    return(do.call(read_csv, append(list(file = filepath), load_args)))
  } else if (tolower(vartype) == "delim") {
    return(do.call(read_delim, append(list(file = filepath), load_args)))
  } else if (vartype == "xgb.DMatrix") {
    return(do.call(xgb.DMatrix, append(
      list(data = filepath),
      load_args
    )))
  } else if (vartype == "PNG") {
    return(readPNG(filepath))
  } else if (vartype == "Excel") {
    return(do.call(read_excel, append(
      list(path = filepath),
      load_args
    )))
  } else if (vartype == "XML") {
    return(do.call(read_xml, append(list(x = filepath), load_args)))
  } else if (vartype == "JSON") {
    return(do.call(fromJSON, append(list(txt = filepath), load_args)))
  } else if (vartype == "lgb.Dataset") {
    return(do.call(lgb.Dataset, append(
      list(data = filepath),
      load_args
    )))
  } else if (vartype == "lgb.Model") {
    return(do.call(lgb.load, list(filename = filepath)))
  } else if (vartype == "umap.Model") {
    return(do.call(load_uwot, list(file = filepath)))
  } else if (vartype == "Parquet") {
    return(do.call(read_parquet, append(list(file = filepath), load_args)))
  } else {
    stop(paste0("Data type (", vartype, ") not supported."))
  }
}

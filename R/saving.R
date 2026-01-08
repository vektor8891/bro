#' Saves data with entry in data catalog to disk, using the appropriate method
#' depending on the data type. Supported data types are NetezzaSQL, Redshift,
#' RDS, CSV, Delim, PNG, Excel, Table, xgb.DMatrix, XML, JSON, lgb.Dataset, lgb.
#' Model, umap.Model. See description for details.
#'
#' @description
#' Given a variable name and data, saves it to disk according to the
#' specifications of the data catalog. The supported data types are NetezzaSQL,
#' Redshift, RDS, CSV, Delim, PNG, Excel, Table, xgb.DMatrix, XML, JSON,
#' lgb.Dataset, lgb.Model, umap.Model. If the variable is not specified in the data
#' catalog, then only keep the variable in memory. The following functions are
#' used to save each of the supported data types:
#' \describe{
#'   \item{NetezzaSQL}{Entry in catalog must contain filepath in the format
#' "SCHEMA.TABLE_NAME". Data is stored using \link{write_table}.}
#'   \item{Redshift}{Entry in catalog must contain filepath in the format
#' "SCHEMA.TABLE_NAME". Data is stored using \link{write_table}.}
#'   \item{RDS}{Data is stored using \link[base]{saveRDS}.}
#'   \item{CSV}{Data is stored using \link[readr]{write_csv}.}
#'   \item{Delim}{Data is stored using \link[readr]{write_delim}.}
#'   \item{PNG}{Data is stored using \link[grDevices]{png}.}
#'   \item{Excel}{Data is stored using \link[writexl]{write_xlsx}.}
#'   \item{Table}{Data is stored using \link[grDevices]{png} and
#' \link[gridExtra]{grid.table}.}
#'   \item{xgb.DMatrix}{Data is stored using \link[xgboost]{xgb.DMatrix}. No
#' optional arguments supported.}
#'   \item{XML}{Data is stored using \link[xml2]{write_xml}.}
#'   \item{JSON}{Data is stored using \link[jsonlite]{write_json}.}
#'   \item{lgb.Dataset}{Data is stored using \link[lightgbm]{lgb.Dataset.save}.}
#'   \item{lgb.Model}{Model is stored using \link[lightgbm]{lgb.save}.}
#'   \item{umap.Model}{Model is stored using \link[uwot]{save_uwot}.}
#'   \item{Parquet}{Data is stored using \link[arrow]{write_parquet}.}
#' }
#' See each of the functions for details.
#'
#' @details
#' This function is usually called in run(), where the runtime variable is
#' clearly defined. This function uses varname to search for an entry in the
#' data catalog. If the entry is found, and contains the fields "type" and
#' "filepath", then the data is saved using an appropriate method. Not all data
#' types are supported. If no entry is found in the data catalog, then the data
#' is just returned and lives in memory.
#'
#' Running the pipeline specifying an alternative parameters file, will
#' slightly modify where data is saved. Entries in the data catalog must list a
#' filepath for data to be saved. When an alternative parameters file is
#' specified, the name of the parameters file is appended to the filepath, so
#' that the data is saved in a subfolder with the same name as the parameters
#' file used for running the pipeline.
#'
#' Entries in the data catalog that specify "versioned: True" will further
#' modify the filepath. A timestamp in the format "%Y%m%d%H%M%S" will be
#' appended to the filepath. Thus, the data will be saved in a subfolder that
#' is named by the runtime timestamp. In case of versioning data and using an
#' alternative parameters file, the filepath will be modified following
#' "filepath/parameters_file.yml/timestamp".
#'
#' If the data catalog entry specifies an "s3_bucket" and "s3_path", the data
#' will also be saved to an S3 bucket. The "s3_path" is optional, and if not
#' specified, the data will be saved to the root of the bucket in a folder named
#' with the current date and time. If "s3_path" is specified, the data will be
#' saved to the specified path in a folder named with the current date and time.
#' The relative path to the file in the S3 bucket will be the same as the
#' filepath in the data catalog. If the user does not have the necessary
#' permissions to write to the S3 bucket, the function will print a message and
#' continue without saving to S3.
#'
#' @param varname (string) Name of the variable associated with data.
#' @param data (object) Object with relevant data.
#' @param context (environment) Environment containing the data catalog.
#' @param verbose (boolean, optional) If TRUE, print messages as data is saved.
#' @param runtime (string) A time stamp string in the format "%Y%m%d%H%M%S".
#'   Used only data that requires versioning.
#' @param parameters (string, optional) Name of a parameters YML file stored in
#' the conf/base folder. File must begin with "parameters" and end with ".yml".
#' If unspecified, then data is saved directly to the specified filepath entry
#' in the data catalog. If parameters is specified, data is saved on a modified
#' version of the filepath entry in the data catalog. The new filepath has the
#' format "filepath/parameters_file.yml/".
#'
#' @return (class(data)) Returns the saved data.
#'
saving <- function(
    varname, data, context, verbose = TRUE, runtime,
    parameters = "parameters.yml") {
  ## If variable in catalog, store in appropriate location
  if (varname %in% names(context$catalog)) {
    ## Type of data file
    vartype <- context$catalog[[varname]]$type
    if (verbose) {
      print(paste0("SAVE CATALOG ", vartype, " ", varname))
    }
    ## Get path to file
    filepath_raw <- context$catalog[[varname]]$filepath
    if (is.null(filepath_raw)) {
      stop(paste0("No filepath for ", varname, ". Stop."))
    }
    filepath <- glue::glue(filepath_raw, .envir = context)
    if (length(filepath) == 0) {
      stop(paste0("Couldn't evaluate filepath for ", varname, ". Stop."))
    }
    ## Modify filepath if using a specific parameters file
    if (parameters != "parameters.yml" && !is_remote(vartype)) {
      # do not modify 01_raw filepath
      if (base::grepl("01_raw", filepath, fixed = TRUE)) {
        base::warning("Saving to data/01_raw is not recommended.")
      } else {
        filepath <- save_parameters(filepath, parameters)
      }
    }
    ## Modify filepath if versioned
    versioned <- context$catalog[[varname]]$versioned
    if (!is.null(versioned) && versioned == TRUE) {
      filepath <- save_versioned(vartype, filepath, runtime)
    }
    ## Additional arguments for saving function
    save_args <- context$catalog[[varname]]$save_args # possibly NULL
    ## Switch for different file types
    if (is_remote(vartype)) {
      data <- save_remote(
        context, data, filepath, vartype,
        save_args
      )
    } else if (vartype == "FastNetezzaSQL") {
      data <- save_fastnetezzasql(
        context, data, filepath, vartype,
        save_args
      )
    } else if (vartype == "RDS") {
      data <- save_rds(context, data, filepath, vartype, save_args)
    } else if (vartype == "CSV") {
      data <- save_csv(context, data, filepath, vartype, save_args)
    } else if (vartype == "Delim") {
      data <- save_delim(context, data, filepath, vartype, save_args)
    } else if (vartype == "xgb.DMatrix") {
      xgboost::xgb.DMatrix.save(data, filepath)
    } else if (vartype == "PNG") {
      data <- save_png(context, data, filepath, vartype, save_args)
    } else if (vartype == "Table") {
      data <- save_table(context, data, filepath, vartype, save_args)
    } else if (vartype == "Excel") {
      data <- save_excel(context, data, filepath, vartype, save_args)
    } else if (vartype == "XML") {
      data <- save_xml(context, data, filepath, vartype, save_args)
    } else if (vartype == "JSON") {
      data <- save_json(context, data, filepath, vartype, save_args)
    } else if (vartype == "lgb.Dataset") {
      data <- save_lightgbm(context, data, filepath, vartype)
    } else if (vartype == "lgb.Model") {
      data <- save_lightgbm_model(context, data, filepath, vartype)
    } else if (vartype == "umap.Model") {
      data <- save_umap_model(context, data, filepath, vartype, save_args)
    } else if (vartype == "Parquet") {
      data <- save_parquet(context, data, filepath, vartype, save_args)
    } else {
      warning(paste0(
        "Unsupported data type: ", vartype,
        ". Continue without saving."
      ))
    }
    # Save to S3 (optional)
    if ("s3_bucket" %in% names(context$catalog[[varname]])) {
      s3_bucket_raw <- context$catalog[[varname]]$s3_bucket
      s3_bucket <- glue::glue(s3_bucket_raw, .envir = context)
      if (is.null(s3_bucket) || s3_bucket == "") {
        message(paste0("No valid 's3_bucket' provided for ", varname))
      } else {
        if (!"s3_path" %in% names(context$catalog[[varname]])) {
          stop("Missing 's3_path' in catalog entry for ", varname)
        }
        s3_path_raw <- context$catalog[[varname]]$s3_path
        s3_path <- glue::glue(s3_path_raw, .envir = context)
        if (is.null(s3_path) || s3_path == "") {
          s3_path_time <- context$parameters$current_date_and_time
        } else {
          s3_path_time <- file.path(
            s3_path,
            context$parameters$current_date_and_time
          )
        }
        tryCatch(
          {
            aws.s3::put_object(
              file = filepath,
              object = file.path(s3_path_time, filepath),
              bucket = s3_bucket
            )
            print(paste0("SAVE S3 ", varname))
          },
          error = function(err) {
            message(paste0(
              "Failed to save '", varname, "' to S3 bucket '", s3_bucket, "'"
            ))
          }
        )
      }
    }
  } else {
    if (verbose) {
      print(paste0("SAVE MEMORY ", varname))
    }
  }
  base::assign(varname, data, pos = context)
}

#' Modifies filepath when saving a file created from an alternative parameters
#' file.
#'
#' When running a pipeline with an alternative parameters file (not
#' 'parameters.yml'), we need to modify the filepath for saving outputs. This
#' function prepends the name of the parameters file to the original filepath,
#' transforming it into a folder. It also creates the appropriate directories
#' when required.
#' @param filepath (string) Original filepath from the data catalog.
#' @param parameters (string) In the format "parameters_*.yml".
#' @return (string) New filepath using the name of parameters as a folder.
save_parameters <- function(filepath, parameters) {
  new_filepath <- base::file.path(
    base::dirname(filepath), parameters,
    base::basename(filepath)
  )
  if (!base::dir.exists(base::dirname(new_filepath))) {
    base::dir.create(base::dirname(new_filepath),
      showWarnings = FALSE,
      recursive = TRUE
    )
  }
  return(new_filepath)
}


#' Modifies filepath for version control.
#'
#' Modifies filepath so that multiple versions of the same data can be saved
#' side by side. Utilizes the time stamp of execution to modify the filepath.
#' The new filepath will create a folder with the same name as the original
#' data, and subfolders named with the timestamp. Also allows for remote data,
#' in which case we simply modify the table name instead of creating subfolders.
#' @param vartype (string) Type of data as specified in the data catalog.
#' @param filepath (string) Filepath where data would be originally stored.
#' @param runtime (string) String containing a timestamp in the format
#' \%Y\%m\%d\%H\%M\%S.
#' @return (string) Filepath to new location.
save_versioned <- function(vartype, filepath, runtime) {
  # avoid versioning on remote for now (space concerns)
  if (is_remote(vartype)) {
    base::warning("Versioning on Remote sources is currently not supported")
    new_filepath <- filepath
  } else { # local data
    # file exists (not folder)
    if (base::file.exists(filepath) && !base::dir.exists(filepath)) {
      base::file.remove(filepath)
    }
    base::dir.create(base::file.path(filepath, runtime),
      showWarnings = FALSE,
      recursive = TRUE
    )
    new_filepath <- base::file.path(filepath, runtime, base::basename(filepath))
  }
  return(new_filepath)
}

#' @title Write Data to Remote Server (Netezza or Redshift)
#' @inheritParams save_netezzasql
#' @author Guilherme Ferreira Pelucio Salome
save_remote <- function(context, data, filepath, vartype, save_args) {
  connection <- framebar::get_connection(vartype, context)
  save_args$temporary <- ifelse(is.null(save_args$temporary), FALSE,
    save_args$temporary
  )
  save_args$overwrite <- ifelse(is.null(save_args$overwrite), TRUE,
    save_args$overwrite
  )
  save_args$append <- ifelse(is.null(save_args$append), FALSE,
    save_args$append
  )
  data <- do.call(
    framebar::write_table,
    append(
      list(
        data = data,
        connection = connection,
        schema_dot_name = filepath
      ),
      save_args
    )
  )
  return(data)
}

#' Saves data to a specific Lilly's Netezza server using
#' \href{./write_table.html}{framebar::write_table} (fast for remote data
#' already in the server).
#'
#' @param filepath (string) Filepath where data would be originally stored.
#' @param vartype (string) Type of data as specified in the data catalog.
#' @param save_args (list, optional) List of optional arguments passed to
#' \href{./write_table.html}{framebar::write_table}.
#' @inheritParams saving
#'
#' @return Variable referencing data.
save_netezzasql <- function(context, data, filepath, vartype, save_args) {
  # creates connection on context
  connection <- get_connection(vartype, context)
  save_args$temporary <- ifelse(is.null(save_args$temporary), FALSE,
    save_args$temporary
  )
  save_args$overwrite <- ifelse(is.null(save_args$overwrite), TRUE,
    save_args$overwrite
  )
  data <- do.call(
    framebar::write_table,
    append(
      list(
        data = data,
        connection = connection,
        schema_dot_name = filepath
      ),
      save_args
    )
  )
  return(data)
}

#' Saves data to a specific Lilly's Netezza server using
#' \href{./write_table.html}{framebar::write_table} (fast for local data not
#' yet in the server).
#'
#' @details
#' Data is first stored in a temporary csv file. Then, an empty table with the
#' same column names and data types as your data is created on Netezza using
#' \href{./write_table.html}{framebar::write_table}. Then, an INSERT INTO
#' statement is executed with \link[DBI]{dbExecute}, which quickly uploads all
#' data from the csv file into the empty remote table.
#'
#' @inheritParams save_netezzasql
save_fastnetezzasql <- function(context, data, filepath, vartype, save_args) {
  ## warning("FastNetezzaSQL is an experimental feature for saving in-memory
  ## data to Netezza.")
  # creates connection on context
  connection <- get_connection(vartype, context)
  ## Table in memory to save in Netezza
  ## Save table to a temporary csv file
  print("Creating temporary csv file")
  temp_dir <- base::tempfile()
  base::dir.create(temp_dir, showWarnings = FALSE)
  temp_csv <- base::file.path(temp_dir, "data.csv")
  readr::write_csv(data, temp_csv)
  ## Create empty table with appropriate data types
  print("Creating empty table with appropriate data types")
  save_args$temporary <- ifelse(is.null(save_args$temporary), FALSE,
    save_args$temporary
  )
  save_args$overwrite <- ifelse(is.null(save_args$overwrite), TRUE,
    save_args$overwrite
  )
  do.call(
    framebar::write_table,
    append(
      list(
        data = dplyr::filter(data, NA), # only write header and data types
        connection = connection,
        schema_dot_name = filepath
      ),
      save_args
    )
  )
  ## Use SQL external table to insert into empty table
  print("Saving to Netezza")
  # split to get schema and table name
  schema_table <- base::strsplit(filepath, "\\.")[[1]]
  query <- dplyr::sql(glue::glue("INSERT INTO {schema_table[[1]]}.{schema_table[[2]]} SELECT * FROM EXTERNAL '{temp_csv}' USING (DELIMITER ',' SKIPROWS 1 REMOTESOURCE 'ODBC')")) # nolint
  DBI::dbExecute(connection, query)
  data <- dplyr::tbl(
    connection,
    dbplyr::in_schema(schema_table[[1]], schema_table[[2]])
  )
  return(data)
}

#' Saves data to an RDS file using \link[base]{saveRDS}.
#'
#' @param save_args (list, optional) List of optional parameters passed to
#' \link[base]{saveRDS}.
#' @inheritParams save_netezzasql
save_rds <- function(context, data, filepath, vartype, save_args) {
  save_args$compress <- if (is.null(save_args$compress)) {
    TRUE
  } else {
    save_args$compress
  }
  do.call(base::saveRDS, append(
    list(object = data, file = filepath),
    save_args
  ))
  return(data)
}

#' Saves data to a CSV file using \link[readr]{write_csv}.
#'
#' @param save_args (list, optional) List of optional parameters passed to
#' \link[readr]{write_csv}.
#' @inheritParams save_netezzasql
save_csv <- function(context, data, filepath, vartype, save_args) {
  do.call(readr::write_csv, append(list(x = data, file = filepath), save_args))
  return(data)
}

#' Prints a figure to a PNG file using \link[grDevices]{png}.
#'
#' @param save_args (list, optional) List of optional parameters passed to
#' \link[grDevices]{png}.
#' @inheritParams save_netezzasql
save_png <- function(context, data, filepath, vartype, save_args) {
  ## Set defaults
  save_args$width <- if (is.null(save_args$width)) 10 else save_args$width
  save_args$height <- if (is.null(save_args$height)) 6 else save_args$height
  save_args$res <- if (is.null(save_args$res)) 600 else save_args$res
  save_args$units <- if (is.null(save_args$units)) "in" else save_args$units
  save_args$bg <- if (is.null(save_args$bg)) "transparent" else save_args$bg
  do.call(
    grDevices::png,
    append(list(filename = filepath), save_args)
  )
  print(data) # necessary for storing figure in png file
  dev.off()
  return(data)
}

#' Saves data to an Excel file using \link[writexl]{write_xlsx}.
#'
#' @param save_args (list, optional) List of optional parameters passed to
#' \link[writexl]{write_xlsx}.
#' @inheritParams save_netezzasql
save_excel <- function(context, data, filepath, vartype, save_args) {
  do.call(writexl::write_xlsx, append(
    list(x = data, path = filepath),
    save_args
  ))
  return(data)
}

#' Prints a table to a PNG file using \link[grDevices]{png} and
#' \link[gridExtra]{grid.table}. Optional arguments are only passed to
#' \link[grDevices]{png}.
#'
#' @param save_args (list, optional) List of optional parameters passed to
#' base::saveRDS().
#' @inheritParams save_netezzasql
save_table <- function(context, data, filepath, vartype, save_args) {
  ## Set defaults
  save_args$width <- if (is.null(save_args$width)) 10 else save_args$width
  save_args$height <- if (is.null(save_args$height)) 6 else save_args$height
  save_args$res <- if (is.null(save_args$res)) 600 else save_args$res
  save_args$units <- if (is.null(save_args$units)) "in" else save_args$units
  save_args$bg <- if (is.null(save_args$bg)) "transparent" else save_args$bg
  do.call(
    grDevices::png,
    append(list(filename = filepath), save_args)
  )
  gridExtra::grid.table(data, rows = NULL)
  grDevices::dev.off()
  return(data)
}

#' Saves data to XML format using \link[xml2]{write_xml}.
#'
#' @param save_args (list, optional) List of optional parameters passed to
#' \link[xml2]{write_xml}.
#' @inheritParams save_netezzasql
save_xml <- function(context, data, filepath, vartype, save_args) {
  do.call(xml2::write_xml, append(list(x = data, file = filepath), save_args))
  return(data)
}

#' Saves data to JSON format using \link[jsonlite]{write_json}.
#'
#' @param save_args (list, optional) List of optional parameters passed to
#' \link[jsonlite]{write_json}.
#' @inheritParams save_netezzasql
save_json <- function(context, data, filepath, vartype, save_args) {
  do.call(jsonlite::write_json, append(
    list(x = data, path = filepath),
    save_args
  ))
  return(data)
}

#' Saves data to a LightGBM Dataset format (binary) using
#' \link[lightgbm]{lgb.Dataset.save}.
#' Additional save arguments are not supported.
#'
#' @inheritParams save_netezzasql
save_lightgbm <- function(context, data, filepath, vartype) {
  if (base::file.exists(filepath)) {
    base::file.remove(filepath)
  }
  do.call(lightgbm::lgb.Dataset.save, list(dataset = data, fname = filepath))
  return(data)
}

#' Saves a LightGBM model using \link[lightgbm]{lgb.save}.
#' Additional save arguments are not supported.
#'
#' @inheritParams save_netezzasql
save_lightgbm_model <- function(context, data, filepath, vartype) {
  if (base::file.exists(filepath)) {
    base::file.remove(filepath)
  }
  do.call(lightgbm::lgb.save, list(booster = data, filename = filepath))
  return(data)
}

#' Saves data to a generic delimited (default is " ") file using
#' \link[readr]{write_delim}.
#'
#' @param save_args (list, optional) List of optional parameters passed to
#' \link[readr]{write_delim}.
#' @inheritParams save_netezzasql
save_delim <- function(context, data, filepath, vartype, save_args) {
  do.call(readr::write_delim, append(
    list(x = data, file = filepath),
    save_args
  ))
  return(data)
}


#' Saves a umap model using \link[uwot]{save_uwot}.
#'
#' @param save_args (list, optional) List of optional parameters passed to
#' \link[uwot]{save_uwot}.
#' Additional save arguments are not supported.
#'
#' @inheritParams save_netezzasql
save_umap_model <- function(context, data, filepath, vartype, save_args) {
  # set default
  save_args$unload <- ifelse(is.null(save_args$unload), FALSE, save_args$unload)
  save_args$verbose <- ifelse(is.null(save_args$verbose), FALSE, save_args$verbose)
  # check if file exists
  if (base::file.exists(filepath)) {
    base::file.remove(filepath)
  }
  do.call(uwot::save_uwot, append(list(model = data, file = filepath), save_args))
  return(data)
}

#' Saves data to a Parquet file using \link[arrow]{write_parquet}.
#'
#' @param save_args (list, optional) List of optional parameters passed to
#' \link[arrow]{write_parquet}.
#' @inheritParams save_netezzasql
save_parquet <- function(context, data, filepath, vartype, save_args = list()) {
  # Default compression to "snappy" if not specified
  save_args$compression <- if (is.null(save_args$compression)) {
    "snappy"
  } else {
    save_args$compression
  }

  do.call(arrow::write_parquet, append(
    list(x = data, sink = filepath),
    save_args
  ))

  return(data)
}

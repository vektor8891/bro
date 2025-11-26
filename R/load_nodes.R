#' @title Load Nodes from a Script File
#'
#' @description
#' Loads nodes and other objects, including functions, from a specified script file into a separate environment.
#'
#' @details
#' This function takes a file path as input, which defaults to "R/nodes.R". It creates a new environment and sources the
#' script file within this environment. All objects, including functions, are stored in this separate environment.
#' The function then retrieves and returns only the objects with the 'node' class from the created environment.
#' These nodes are extracted from a dunder variable named "__nodes__", that is created when the "nodes.R" file
#' is sourced.
#'
#' @param path The file path to the script containing nodes and other objects (default: "R/nodes.R").
#' @param execution An optional execution environment to use; if not provided, a new environment is created.
#'
#' @return Returns the objects with the 'node' class loaded from the specified script file.
#' @export
#'
load_nodes <- function(path = file.path("R", "nodes.R"), execution = new.env()) {
  ## Load nodes and possibly other objects, including functions, into a separate environment
  execution$nodes = new.env()
  source(path, chdir = TRUE, local = execution$nodes)
  ## Select only objects with 'node' class
  return(get("__nodes__", envir = execution$nodes))
}

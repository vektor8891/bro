#' @title List of functions that can be used to load and save different types
#' of data
#' @author Guilherme Salome
#' @export
connectors <- NULL

#' @title Load connectors file
#' @description Loads the connectors file from the package
#' @param libname The library name
#' @param pkgname The package name
#' @importFrom yaml read_yaml
#' @keywords internal
.onLoad <- function(libname, pkgname) {
  # Load connectors file
  connectors <<- read_yaml( # nolint: object_usage_linter
    system.file("connectors.yaml", package = pkgname)
  )
}

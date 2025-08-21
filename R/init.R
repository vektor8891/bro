##' @title List of functions that can be used to load and save different types
##' of data
##' @author Guilherme Salome
##' @export
connectors <- NULL


.onLoad <- function(libname, pkgname) {
  ## Load connectors file
  connectors <<- yaml::read_yaml(system.file("connectors.yaml",
    package = pkgname
  ))
}

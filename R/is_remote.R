#' Is variable type remote.
#'
#' Checks if variable type is associated with a remote database.
#' @param vartype (string) String representing variable type.
#' @return (boolean) TRUE if variable type is remote.
is_remote <- function(vartype) {
  return(
    tolower(vartype) %in% c(
      "netezza", "netezzasql", "fastnetezzasql",
      "redshift", "redshiftsql",
      "redshiftprod", "redshift-prod",
      "redshiftqa", "redshift-qa",
      "redshiftdev", "redshift-dev"
    )
  )
}

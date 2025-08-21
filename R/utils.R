##' @title Safely load external package and provide fallback
##' @description Safely loads an external package using requireNamespace and
##' provides fallback behavior
##' @param package Package name to load
##' @param fallback_function Fallback function to use if package is not
##' available
##' @param error_message Custom error message if package is not available
##' @return TRUE if package is available, FALSE otherwise
##' @export
safe_require_namespace <- function(
    package, fallback_function = NULL,
    error_message = NULL) {
  if (requireNamespace(package, quietly = TRUE)) {
    return(TRUE)
  } else {
    if (!is.null(error_message)) {
      warning(error_message)
    } else {
      warning(
        "Package '", package,
        "' is not available. Install it to use this functionality."
      )
    }
    if (!is.null(fallback_function)) {
      return(fallback_function)
    }
    return(FALSE)
  }
}

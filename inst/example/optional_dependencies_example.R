# Example: Optional Dependencies in bro
# This example shows how bro handles optional dependencies

library(bro)

# Create a simple execution environment
execution <- create_execution_env()

# Example 1: Try to load CSV data without readr installed
# This will fail gracefully with a clear error message
tryCatch({
  # This would work if readr was installed
  # data <- load_data("costs", execution)
  message("Note: To load CSV files, install readr: install.packages('readr')")
}, error = function(e) {
  message("Expected error: ", e$message)
})

# Example 2: Try to load XGBoost model without xgboost installed
# This will fail gracefully with a clear error message
tryCatch({
  # This would work if xgboost was installed
  # model <- load_data("xgboost_model", execution)
  message("Note: To load XGBoost models, install xgboost: install.packages('xgboost')")
}, error = function(e) {
  message("Expected error: ", e$message)
})

# Example 3: Hash function works with rlang (required dependency)
test_data <- list(a = 1, b = "test", c = data.frame(x = 1:3))
hash_result <- rlang::hash(test_data)
message("Hash result: ", hash_result)

# Example 4: Check package availability
message("Checking optional package availability:")
packages <- c("readr", "xgboost", "arrow", "digest")
for (pkg in packages) {
  available <- bro:::safe_require_namespace(pkg)
  message("  ", pkg, ": ", if(available) "Available" else "Not available")
}

message("\nRequired packages (yaml, renv, rlang) are automatically installed with bro")
message("\nTo install optional packages:")
message("install.packages(c('readr', 'xgboost', 'arrow'))")

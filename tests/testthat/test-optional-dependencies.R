# Test optional dependencies functionality

test_that("safe_require_namespace works correctly", {
  # Test with available package (base)
  expect_true(safe_require_namespace("base"))
  
  # Test with unavailable package
  expect_false(safe_require_namespace("nonexistent_package_12345"))
  
  # Test with custom error message
  expect_warning(
    safe_require_namespace("nonexistent_package_12345", 
                         error_message = "Custom error message"),
    "Custom error message"
  )
})

test_that("rlang::hash works correctly", {
  test_data <- list(a = 1, b = "test")
  
  # Should work with rlang (now required)
  hash_result <- rlang::hash(test_data)
  expect_type(hash_result, "character")
  expect_true(length(hash_result) > 0)
  
  # Should be consistent for same input
  hash_result2 <- rlang::hash(test_data)
  expect_equal(hash_result, hash_result2)
})

test_that("load_data handles missing packages gracefully", {
  # Create a minimal execution environment
  execution <- new.env()
  execution$registry <- list(
    test_csv = list(
      type = "csv",
      path = list("test", "data.csv"),
      load_args = list()
    ),
    test_xgboost = list(
      type = "xgboost", 
      path = list("test", "model.xgb"),
      load_args = list()
    )
  )
  
  # Test that appropriate errors are thrown for missing packages
  expect_error(
    bro:::load_data("test_csv", execution),
    "Cannot load 'csv' files without package 'readr'"
  )
  
  expect_error(
    bro:::load_data("test_xgboost", execution),
    "Cannot load 'xgboost' files without package 'xgboost'"
  )
})

test_that("save_data handles missing packages gracefully", {
  # Create a minimal execution environment
  execution <- new.env()
  execution$registry <- list(
    test_csv = list(
      type = "csv",
      path = list("test", "data.csv"),
      save_args = list()
    ),
    test_xgboost = list(
      type = "xgboost",
      path = list("test", "model.xgb"), 
      save_args = list()
    )
  )
  
  test_data <- data.frame(x = 1:3, y = letters[1:3])
  
  # Test that appropriate errors are thrown for missing packages
  expect_error(
    bro:::save_data(test_data, "test_csv", execution),
    "Cannot save 'csv' files without package 'readr'"
  )
  
  expect_error(
    bro:::save_data(test_data, "test_xgboost", execution),
    "Cannot save 'xgboost' files without package 'xgboost'"
  )
})

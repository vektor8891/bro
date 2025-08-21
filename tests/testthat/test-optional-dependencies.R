# Test optional dependencies functionality

test_that("safe_require_namespace works correctly", {
  # Test with available package (base)
  expect_true(safe_require_namespace("base"))

  # Test with unavailable package
  expect_false(safe_require_namespace("nonexistent_package_12345"))

  # Test with custom error message
  expect_warning(
    safe_require_namespace("nonexistent_package_12345",
      error_message = "Custom error message"
    ),
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
    test_nonexistent = list(
      type = "nonexistent_type",
      path = list("test", "data.nonexistent"),
      load_args = list()
    )
  )

  # Test that appropriate errors are thrown for unsupported types
  expect_error(
    bro:::load_data("test_nonexistent", execution),
    "unsupported data type"
  )
})

test_that("save_data handles missing packages gracefully", {
  # Create a minimal execution environment
  execution <- new.env()
  execution$registry <- list(
    test_nonexistent = list(
      type = "nonexistent_type",
      path = list("test", "data.nonexistent"),
      save_args = list()
    )
  )

  test_data <- data.frame(x = 1:3, y = letters[1:3])

  # Test that appropriate errors are thrown for unsupported types
  expect_error(
    bro:::save_data(test_data, "test_nonexistent", execution),
    "unsupported data type"
  )
})

test_that("readr functionality works as required dependency", {
  # Test that readr is available and can be used
  expect_true(requireNamespace("readr", quietly = TRUE))

  # Test basic readr functionality
  test_data <- data.frame(x = 1:3, y = letters[1:3])
  temp_file <- tempfile(fileext = ".csv")

  # Test write_csv
  readr::write_csv(test_data, temp_file)
  expect_true(file.exists(temp_file))

  # Test read_csv
  loaded_data <- readr::read_csv(temp_file, show_col_types = FALSE)
  expect_equal(nrow(loaded_data), 3)
  expect_equal(ncol(loaded_data), 2)

  # Clean up
  unlink(temp_file)
})

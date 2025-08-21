test_that(".safe_require_namespace works correctly", {
  # Test with available package (base)
  expect_true(.safe_require_namespace("base"))

  # Test with unavailable package
  expect_warning(
    expect_false(.safe_require_namespace("nonexistent_package_12345")),
    "Package 'nonexistent_package_12345' is not available"
  )

  # Test with custom error message
  expect_warning(
    .safe_require_namespace("nonexistent_package_12345",
      error_message = "Custom error message"
    ),
    "Custom error message"
  )

  # Test with fallback function
  expect_warning(
    expect_equal(
      .safe_require_namespace("nonexistent_package_12345",
        fallback_function = function() "fallback_value"
      ),
      "fallback_value"
    ),
    "Package 'nonexistent_package_12345' is not available"
  )

  # Test with both custom error message and fallback function
  result <- expect_warning(
    .safe_require_namespace("nonexistent_package_12345",
      error_message = "Custom error message",
      fallback_function = function() "fallback_value"
    ),
    "Custom error message"
  )
  expect_equal(result, "fallback_value")

  # Test with NULL fallback function
  expect_warning(
    expect_false(.safe_require_namespace("nonexistent_package_12345",
      fallback_function = NULL
    )),
    "Package 'nonexistent_package_12345' is not available"
  )

  # Test with NULL error message (should use default)
  expect_warning(
    .safe_require_namespace("nonexistent_package_12345",
      error_message = NULL
    ),
    "Package 'nonexistent_package_12345' is not available"
  )
})

test_that(".get_imported_packages works correctly", {
  # Test that it returns a character vector
  result <- .get_imported_packages()
  expect_type(result, "character")

  # Test that it returns the expected packages from current DESCRIPTION
  expected_packages <- c("renv", "yaml", "rlang", "desc")
  expect_equal(sort(result), sort(expected_packages))

  # Test that all returned packages are non-empty strings
  expect_true(all(nchar(result) > 0))

  # Test that there are no duplicates
  expect_equal(length(result), length(unique(result)))

  # Test that it works when called multiple times (consistency)
  result2 <- .get_imported_packages()
  expect_equal(result, result2)

  # Test that the result is consistent (same order when called multiple times)
  expect_equal(result, result2)
})

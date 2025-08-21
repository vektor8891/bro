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

  # Test with fallback function
  expect_equal(
    safe_require_namespace("nonexistent_package_12345",
      fallback_function = function() "fallback_value"
    ),
    "fallback_value"
  )

  # Test with both custom error message and fallback function
  result <- expect_warning(
    safe_require_namespace("nonexistent_package_12345",
      error_message = "Custom error message",
      fallback_function = function() "fallback_value"
    ),
    "Custom error message"
  )
  expect_equal(result, "fallback_value")

  # Test with NULL fallback function
  expect_false(safe_require_namespace("nonexistent_package_12345",
    fallback_function = NULL
  ))

  # Test with NULL error message (should use default)
  expect_warning(
    safe_require_namespace("nonexistent_package_12345",
      error_message = NULL
    ),
    "Package 'nonexistent_package_12345' is not available"
  )
})

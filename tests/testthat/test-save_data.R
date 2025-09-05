# Unit tests for save_data function
# Test core functionality and error handling

test_that("save_data handles data not in registry", {
  # Mock execution environment
  mock_execution <- new.env()
  mock_execution$registry <- list(
    existing_data = list(
      type = "rds",
      path = list("data", "existing.rds"),
      save_args = list()
    )
  )
  mock_execution$data <- new.env()

  test_data <- data.frame(x = 1:5, y = 6:10)

  expect_message(
    save_data(test_data, "nonexistent_data", mock_execution),
    "\\(bro\\) Saving In-memory 'nonexistent_data'"
  )

  # Check that data was assigned to execution environment
  expect_equal(mock_execution$data$nonexistent_data, test_data)
})

test_that("save_data handles unsupported data type", {
  # Mock execution environment
  mock_execution <- new.env()
  mock_execution$registry <- list(
    test_data = list(
      type = "unsupported_type",
      path = list("data", "test.xyz"),
      save_args = list()
    )
  )
  mock_execution$data <- new.env()

  test_data <- data.frame(x = 1:3)

  # Mock connectors with limited types
  mock_connectors <- list(
    rds = list(save = "base::saveRDS"),
    csv = list(save = "readr::write_csv")
  )

  with_mocked_bindings(
    connectors = mock_connectors,
    {
      expect_error(
        save_data(test_data, "test_data", mock_execution),
        "unsupported data type 'unsupported_type'. Available types: rds, csv"
      )
    }
  )
})

test_that("save_data handles missing package for non-base connector", {
  # Mock execution environment
  mock_execution <- new.env()
  mock_execution$registry <- list(
    test_data = list(
      type = "arrow",
      path = list("data", "test.parquet"),
      save_args = list()
    )
  )
  mock_execution$data <- new.env()

  test_data <- data.frame(x = 1:5)

  # Mock connectors
  mock_connectors <- list(
    arrow = list(save = "arrow::write_parquet")
  )

  # Mock .get_imported_packages to not include arrow
  with_mocked_bindings(
    connectors = mock_connectors,
    .get_imported_packages = function() c("yaml"),
    .safe_require_namespace = function(package) FALSE,
    {
      expect_error(
        save_data(test_data, "test_data", mock_execution),
        "Cannot save 'arrow' files without package 'arrow'"
      )
    }
  )
})

test_that("save_data handles available package for non-base connector", {
  # Mock execution environment
  mock_execution <- new.env()
  mock_execution$registry <- list(
    test_data = list(
      type = "arrow",
      path = list("data", "test.parquet"),
      save_args = list()
    )
  )
  mock_execution$data <- new.env()

  test_data <- data.frame(x = 1:5, y = 6:10)

  # Mock connectors
  mock_connectors <- list(
    arrow = list(save = "arrow::write_parquet")
  )

  # Mock .get_imported_packages to not include arrow initially
  with_mocked_bindings(
    connectors = mock_connectors,
    .get_imported_packages = function() c("yaml"),
    .safe_require_namespace = function(package) TRUE,
    {
      # Mock the parse function to return our test function when parsing
      # "arrow::write_parquet"
      with_mocked_bindings(
        parse = function(text) {
          if (text == "arrow::write_parquet") {
            return(expression(function(x, file) {
              expect_equal(file, file.path("data", "test.parquet"))
              expect_equal(x, data.frame(x = 1:5, y = 6:10))
              invisible(NULL)
            }))
          }
          base::parse(text = text)
        },
        .package = "base",
        {
          expect_message(
            save_data(test_data, "test_data", mock_execution),
            "\\(bro\\) Saving to File 'test_data' \\(arrow, ",
            "arrow::write_parquet\\)"
          )

          # Check that data was assigned to execution environment
          expect_equal(mock_execution$data$test_data, test_data)
        }
      )
    }
  )
})

test_that("save_data handles package already imported", {
  # Mock execution environment
  mock_execution <- new.env()
  mock_execution$registry <- list(
    test_data = list(
      type = "json",
      path = list("data", "test.json"),
      save_args = list()
    )
  )
  mock_execution$data <- new.env()

  test_data <- list(name = "test", value = 42)

  # Mock connectors
  mock_connectors <- list(
    json = list(save = "jsonlite::toJSON")
  )

  # Mock .get_imported_packages to include jsonlite
  with_mocked_bindings(
    connectors = mock_connectors,
    .get_imported_packages = function() c("jsonlite", "yaml"),
    {
      # Mock toJSON to return test data
      with_mocked_bindings(
        toJSON = function(x, file) {
          expect_equal(file, file.path("data", "test.json"))
          expect_equal(x, test_data)
          invisible(NULL)
        },
        .package = "jsonlite",
        {
          expect_message(
            save_data(test_data, "test_data", mock_execution),
            "\\(bro\\) Saving to File 'test_data' \\(json, jsonlite::toJSON\\)"
          )

          # Check that data was assigned to execution environment
          expect_equal(mock_execution$data$test_data, test_data)
        }
      )
    }
  )
})

test_that("save_data handles complex path construction", {
  # Mock execution environment
  mock_execution <- new.env()
  mock_execution$registry <- list(
    test_data = list(
      type = "rds",
      path = list("data", "subfolder", "nested", "test.rds"),
      save_args = list()
    )
  )
  mock_execution$data <- new.env()

  test_data <- matrix(1:9, nrow = 3)

  # Mock connectors
  mock_connectors <- list(
    rds = list(save = "base::saveRDS")
  )

  # Mock .get_imported_packages to return base
  with_mocked_bindings(
    connectors = mock_connectors,
    .get_imported_packages = function() c("base", "yaml"),
    {
      # Mock saveRDS to return test data
      with_mocked_bindings(
        saveRDS = function(object, file) {
          expected_path <- file.path("data", "subfolder", "nested", "test.rds")
          expect_equal(file, expected_path)
          expect_equal(object, test_data)
          invisible(NULL)
        },
        .package = "base",
        {
          expect_message(
            save_data(test_data, "test_data", mock_execution),
            "\\(bro\\) Saving to File 'test_data' \\(rds, base::saveRDS\\)"
          )

          # Check that data was assigned to execution environment
          expect_equal(mock_execution$data$test_data, test_data)
        }
      )
    }
  )
})

test_that("save_data handles empty save_args", {
  # Mock execution environment
  mock_execution <- new.env()
  mock_execution$registry <- list(
    test_data = list(
      type = "rds",
      path = list("data", "test.rds"),
      save_args = NULL
    )
  )
  mock_execution$data <- new.env()

  test_data <- list(a = 1, b = 2)

  # Mock connectors
  mock_connectors <- list(
    rds = list(save = "base::saveRDS")
  )

  # Mock .get_imported_packages to return base
  with_mocked_bindings(
    connectors = mock_connectors,
    .get_imported_packages = function() c("base", "yaml"),
    {
      # Mock saveRDS to return test data
      with_mocked_bindings(
        saveRDS = function(object, file) {
          expect_equal(file, file.path("data", "test.rds"))
          expect_equal(object, test_data)
          invisible(NULL)
        },
        .package = "base",
        {
          expect_message(
            save_data(test_data, "test_data", mock_execution),
            "\\(bro\\) Saving to File 'test_data' \\(rds, base::saveRDS\\)"
          )

          # Check that data was assigned to execution environment
          expect_equal(mock_execution$data$test_data, test_data)
        }
      )
    }
  )
})

test_that("save_data handles base package correctly", {
  # Mock execution environment
  mock_execution <- new.env()
  mock_execution$registry <- list(
    test_data = list(
      type = "rds",
      path = list("data", "test.rds"),
      save_args = list()
    )
  )
  mock_execution$data <- new.env()

  test_data <- c(1, 2, 3, 4, 5)

  # Mock connectors
  mock_connectors <- list(
    rds = list(save = "base::saveRDS")
  )

  # Mock .get_imported_packages to not include base (should still work)
  with_mocked_bindings(
    connectors = mock_connectors,
    .get_imported_packages = function() c("yaml"),
    {
      # Mock saveRDS to return test data
      with_mocked_bindings(
        saveRDS = function(object, file) {
          expect_equal(file, file.path("data", "test.rds"))
          expect_equal(object, test_data)
          invisible(NULL)
        },
        .package = "base",
        {
          expect_message(
            save_data(test_data, "test_data", mock_execution),
            "\\(bro\\) Saving to File 'test_data' \\(rds, base::saveRDS\\)"
          )

          # Check that data was assigned to execution environment
          expect_equal(mock_execution$data$test_data, test_data)
        }
      )
    }
  )
})

test_that("save_data handles function parsing correctly", {
  # Mock execution environment
  mock_execution <- new.env()
  mock_execution$registry <- list(
    test_data = list(
      type = "custom",
      path = list("data", "test.custom"),
      save_args = list()
    )
  )
  mock_execution$data <- new.env()

  test_data <- "custom_data"

  # Mock connectors with custom function
  mock_connectors <- list(
    custom = list(save = "custom::save_function")
  )

  # Mock .get_imported_packages to include custom
  with_mocked_bindings(
    connectors = mock_connectors,
    .get_imported_packages = function() c("custom", "yaml"),
    {
      # Mock the parse function to return our test function when parsing
      # "custom::save_function"
      with_mocked_bindings(
        parse = function(text) {
          if (text == "custom::save_function") {
            return(expression(function(x, file) {
              expect_equal(file, file.path("data", "test.custom"))
              expect_equal(x, "custom_data")
              invisible(NULL)
            }))
          }
          base::parse(text = text)
        },
        .package = "base",
        {
          expect_message(
            save_data(test_data, "test_data", mock_execution),
            "\\(bro\\) Saving to File 'test_data' \\(custom, ",
            "custom::save_function\\)"
          )

          # Check that data was assigned to execution environment
          expect_equal(mock_execution$data$test_data, test_data)
        }
      )
    }
  )
})

test_that("save_data handles multiple data saves in same execution", {
  # Mock execution environment
  mock_execution <- new.env()
  mock_execution$registry <- list(
    data1 = list(
      type = "rds",
      path = list("data", "data1.rds"),
      save_args = list()
    ),
    data2 = list(
      type = "csv",
      path = list("data", "data2.csv"),
      save_args = list()
    )
  )
  mock_execution$data <- new.env()

  test_data1 <- data.frame(x = 1:3)
  test_data2 <- data.frame(y = letters[1:3])

  # Mock connectors
  mock_connectors <- list(
    rds = list(save = "base::saveRDS"),
    csv = list(save = "readr::write_csv")
  )

  # Mock .get_imported_packages
  with_mocked_bindings(
    connectors = mock_connectors,
    .get_imported_packages = function() c("base", "readr", "yaml"),
    {
      # Mock the parse function to return our test functions
      with_mocked_bindings(
        parse = function(text) {
          if (text == "base::saveRDS") {
            return(expression(function(object, file) {
              if (file == file.path("data", "data1.rds")) {
                expect_equal(object, data.frame(x = 1:3))
              }
              invisible(NULL)
            }))
          } else if (text == "readr::write_csv") {
            return(expression(function(x, file) {
              if (file == file.path("data", "data2.csv")) {
                expect_equal(x, data.frame(y = letters[1:3]))
              }
              invisible(NULL)
            }))
          }
          base::parse(text = text)
        },
        .package = "base",
        {
          # Save first dataset
          expect_message(
            save_data(test_data1, "data1", mock_execution),
            "\\(bro\\) Saving to File 'data1' \\(rds, base::saveRDS\\)"
          )
          expect_equal(mock_execution$data$data1, test_data1)

          # Save second dataset
          expect_message(
            save_data(test_data2, "data2", mock_execution),
            "\\(bro\\) Saving to File 'data2' \\(csv, readr::write_csv\\)"
          )
          expect_equal(mock_execution$data$data2, test_data2)

          # Verify both datasets are in execution environment
          expect_equal(mock_execution$data$data1, test_data1)
          expect_equal(mock_execution$data$data2, test_data2)
        }
      )
    }
  )
})

test_that("save_data handles additional arguments correctly", {
  # Mock execution environment
  mock_execution <- new.env()
  mock_execution$registry <- list(
    test_csv = list(
      type = "csv",
      path = list("data", "test.csv"),
      save_args = list(col_names = FALSE, append = TRUE)
    )
  )
  mock_execution$data <- new.env()

  test_data <- data.frame(col1 = letters[1:3], col2 = LETTERS[1:3])

  # Mock connectors
  mock_connectors <- list(
    csv = list(save = "readr::write_csv")
  )

  # Mock .get_imported_packages to include readr
  with_mocked_bindings(
    connectors = mock_connectors,
    .get_imported_packages = function() c("readr", "yaml"),
    {
      # Mock write_csv to return test data
      with_mocked_bindings(
        write_csv = function(x, file, col_names, append) {
          expect_equal(file, file.path("data", "test.csv"))
          expect_equal(x, test_data)
          expect_equal(col_names, FALSE)
          expect_equal(append, TRUE)
          invisible(NULL)
        },
        .package = "readr",
        {
          expect_message(
            save_data(test_data, "test_csv", mock_execution),
            "\\(bro\\) Saving to File 'test_csv' \\(csv, readr::write_csv\\)"
          )

          # Check that data was assigned to execution environment
          expect_equal(mock_execution$data$test_csv, test_data)
        }
      )
    }
  )
})

test_that("save_data handles mixed registry and non-registry data", {
  # Mock execution environment
  mock_execution <- new.env()
  mock_execution$registry <- list(
    registered_data = list(
      type = "rds",
      path = list("data", "registered.rds"),
      save_args = list()
    )
  )
  mock_execution$data <- new.env()

  registered_data <- data.frame(x = 1:3)
  unregistered_data <- data.frame(y = letters[1:3])

  # Mock connectors
  mock_connectors <- list(
    rds = list(save = "base::saveRDS")
  )

  # Mock .get_imported_packages
  with_mocked_bindings(
    connectors = mock_connectors,
    .get_imported_packages = function() c("base", "yaml"),
    {
      # Mock saveRDS
      with_mocked_bindings(
        saveRDS = function(object, file) {
          expect_equal(file, file.path("data", "registered.rds"))
          expect_equal(object, registered_data)
          invisible(NULL)
        },
        .package = "base",
        {
          # Save registered data (should save to file)
          expect_message(
            save_data(registered_data, "registered_data", mock_execution),
            "\\(bro\\) Saving to File 'registered_data' \\(rds, ",
            "base::saveRDS\\)"
          )

          # Save unregistered data (should only save in memory)
          expect_message(
            save_data(unregistered_data, "unregistered_data", mock_execution),
            "\\(bro\\) Saving In-memory 'unregistered_data'"
          )

          # Verify both datasets are in execution environment
          expect_equal(mock_execution$data$registered_data, registered_data)
          expect_equal(mock_execution$data$unregistered_data, unregistered_data)
        }
      )
    }
  )
})

test_that("save_data handles complex data types", {
  # Mock execution environment
  mock_execution <- new.env()
  mock_execution$registry <- list(
    complex_data = list(
      type = "rds",
      path = list("data", "complex.rds"),
      save_args = list()
    )
  )
  mock_execution$data <- new.env()

  # Create complex data structure
  complex_data <- list(
    matrix = matrix(1:12, nrow = 3, ncol = 4),
    data_frame = data.frame(a = 1:5, b = letters[1:5]),
    vector = c(1.1, 2.2, 3.3),
    nested_list = list(
      level1 = list(level2 = "deep_value"),
      another = 42
    )
  )

  # Mock connectors
  mock_connectors <- list(
    rds = list(save = "base::saveRDS")
  )

  # Mock .get_imported_packages
  with_mocked_bindings(
    connectors = mock_connectors,
    .get_imported_packages = function() c("base", "yaml"),
    {
      # Mock saveRDS
      with_mocked_bindings(
        saveRDS = function(object, file) {
          expect_equal(file, file.path("data", "complex.rds"))
          expect_equal(object, complex_data)
          invisible(NULL)
        },
        .package = "base",
        {
          expect_message(
            save_data(complex_data, "complex_data", mock_execution),
            "\\(bro\\) Saving to File 'complex_data' \\(rds, base::saveRDS\\)"
          )

          # Check that data was assigned to execution environment
          expect_equal(mock_execution$data$complex_data, complex_data)
        }
      )
    }
  )
})

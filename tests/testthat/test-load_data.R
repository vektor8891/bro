# Unit tests for load_data function
# Test core functionality and error handling

test_that("load_data handles missing data entry", {
  # Mock execution environment
  mock_execution <- new.env()
  mock_execution$registry <- list(
    existing_data = list(
      type = "rds",
      path = list("data", "existing.rds"),
      load_args = list()
    )
  )
  mock_execution$data <- new.env()

  expect_error(
    load_data("nonexistent_data", mock_execution),
    "no entry 'nonexistent_data' in inst/data.yaml"
  )
})

test_that("load_data handles missing path", {
  # Mock execution environment
  mock_execution <- new.env()
  mock_execution$registry <- list(
    test_data = list(
      type = "rds",
      path = NULL,
      load_args = list()
    )
  )
  mock_execution$data <- new.env()

  expect_error(
    load_data("test_data", mock_execution),
    "missing 'path' in 'test_data' in inst/data.yaml"
  )
})

test_that("load_data handles unsupported data type", {
  # Mock execution environment
  mock_execution <- new.env()
  mock_execution$registry <- list(
    test_data = list(
      type = "unsupported_type",
      path = list("data", "test.xyz"),
      load_args = list()
    )
  )
  mock_execution$data <- new.env()

  # Mock connectors with limited types
  mock_connectors <- list(
    rds = list(load = "base::readRDS"),
    csv = list(load = "readr::read_csv")
  )

  with_mocked_bindings(
    connectors = mock_connectors,
    {
      expect_error(
        load_data("test_data", mock_execution),
        "unsupported data type 'unsupported_type'. Available types: rds, csv"
      )
    }
  )
})

test_that("load_data handles missing package for non-base connector", {
  # Mock execution environment
  mock_execution <- new.env()
  mock_execution$registry <- list(
    test_data = list(
      type = "arrow",
      path = list("data", "test.parquet"),
      load_args = list()
    )
  )
  mock_execution$data <- new.env()

  # Mock connectors
  mock_connectors <- list(
    arrow = list(load = "arrow::read_parquet")
  )

  # Mock .get_imported_packages to not include arrow
  with_mocked_bindings(
    connectors = mock_connectors,
    .get_imported_packages = function() c("yaml"),
    .safe_require_namespace = function(package) FALSE,
    {
      expect_error(
        load_data("test_data", mock_execution),
        "Cannot load 'arrow' files without package 'arrow'"
      )
    }
  )
})

test_that("load_data handles available package for non-base connector", {
  # Mock execution environment
  mock_execution <- new.env()
  mock_execution$registry <- list(
    test_data = list(
      type = "arrow",
      path = list("data", "test.parquet"),
      load_args = list()
    )
  )
  mock_execution$data <- new.env()

  # Mock connectors
  mock_connectors <- list(
    arrow = list(load = "arrow::read_parquet")
  )

  # Mock .get_imported_packages to not include arrow initially
  with_mocked_bindings(
    connectors = mock_connectors,
    .get_imported_packages = function() c("yaml"),
    .safe_require_namespace = function(package) TRUE,
    {
      # Mock the parse function to return our test function when parsing
      # "arrow::read_parquet"
      with_mocked_bindings(
        parse = function(text) {
          if (text == "arrow::read_parquet") {
            return(expression(function(file) {
              expect_equal(file, file.path("data", "test.parquet"))
              data.frame(x = 1:5, y = 6:10)
            }))
          }
          base::parse(text = text)
        },
        .package = "base",
        {
          result <- load_data("test_data", mock_execution)

          expect_equal(result, data.frame(x = 1:5, y = 6:10))
          expect_equal(
            mock_execution$data$test_data,
            data.frame(x = 1:5, y = 6:10)
          )
        }
      )
    }
  )
})

test_that("load_data handles package already imported", {
  # Mock execution environment
  mock_execution <- new.env()
  mock_execution$registry <- list(
    test_data = list(
      type = "json",
      path = list("data", "test.json"),
      load_args = list()
    )
  )
  mock_execution$data <- new.env()

  # Mock connectors
  mock_connectors <- list(
    json = list(load = "jsonlite::fromJSON")
  )

  # Mock .get_imported_packages to include jsonlite
  with_mocked_bindings(
    connectors = mock_connectors,
    .get_imported_packages = function() c("jsonlite", "yaml"),
    {
      # Mock fromJSON to return test data
      with_mocked_bindings(
        fromJSON = function(file) {
          expect_equal(file, file.path("data", "test.json"))
          list(name = "test", value = 42)
        },
        .package = "jsonlite",
        {
          result <- load_data("test_data", mock_execution)

          expect_equal(result, list(name = "test", value = 42))
          expect_equal(
            mock_execution$data$test_data,
            list(name = "test", value = 42)
          )
        }
      )
    }
  )
})

test_that("load_data handles complex path construction", {
  # Mock execution environment
  mock_execution <- new.env()
  mock_execution$registry <- list(
    test_data = list(
      type = "rds",
      path = list("data", "subfolder", "nested", "test.rds"),
      load_args = list()
    )
  )
  mock_execution$data <- new.env()

  # Mock connectors
  mock_connectors <- list(
    rds = list(load = "base::readRDS")
  )

  # Mock .get_imported_packages to return base
  with_mocked_bindings(
    connectors = mock_connectors,
    .get_imported_packages = function() c("base", "yaml"),
    {
      # Mock readRDS to return test data
      with_mocked_bindings(
        readRDS = function(file) {
          expected_path <- file.path("data", "subfolder", "nested", "test.rds")
          expect_equal(file, expected_path)
          matrix(1:9, nrow = 3)
        },
        .package = "base",
        {
          result <- load_data("test_data", mock_execution)

          expect_equal(result, matrix(1:9, nrow = 3))
          expect_equal(mock_execution$data$test_data, matrix(1:9, nrow = 3))
        }
      )
    }
  )
})

test_that("load_data handles empty load_args", {
  # Mock execution environment
  mock_execution <- new.env()
  mock_execution$registry <- list(
    test_data = list(
      type = "rds",
      path = list("data", "test.rds"),
      load_args = NULL
    )
  )
  mock_execution$data <- new.env()

  # Mock connectors
  mock_connectors <- list(
    rds = list(load = "base::readRDS")
  )

  # Mock .get_imported_packages to return base
  with_mocked_bindings(
    connectors = mock_connectors,
    .get_imported_packages = function() c("base", "yaml"),
    {
      # Mock readRDS to return test data
      with_mocked_bindings(
        readRDS = function(file) {
          expect_equal(file, file.path("data", "test.rds"))
          list(a = 1, b = 2)
        },
        .package = "base",
        {
          result <- load_data("test_data", mock_execution)

          expect_equal(result, list(a = 1, b = 2))
          expect_equal(mock_execution$data$test_data, list(a = 1, b = 2))
        }
      )
    }
  )
})

test_that("load_data handles base package correctly", {
  # Mock execution environment
  mock_execution <- new.env()
  mock_execution$registry <- list(
    test_data = list(
      type = "rds",
      path = list("data", "test.rds"),
      load_args = list()
    )
  )
  mock_execution$data <- new.env()

  # Mock connectors
  mock_connectors <- list(
    rds = list(load = "base::readRDS")
  )

  # Mock .get_imported_packages to not include base (should still work)
  with_mocked_bindings(
    connectors = mock_connectors,
    .get_imported_packages = function() c("yaml"),
    {
      # Mock readRDS to return test data
      with_mocked_bindings(
        readRDS = function(file) {
          expect_equal(file, file.path("data", "test.rds"))
          c(1, 2, 3, 4, 5)
        },
        .package = "base",
        {
          result <- load_data("test_data", mock_execution)

          expect_equal(result, c(1, 2, 3, 4, 5))
          expect_equal(mock_execution$data$test_data, c(1, 2, 3, 4, 5))
        }
      )
    }
  )
})

test_that("load_data handles function parsing correctly", {
  # Mock execution environment
  mock_execution <- new.env()
  mock_execution$registry <- list(
    test_data = list(
      type = "custom",
      path = list("data", "test.custom"),
      load_args = list()
    )
  )
  mock_execution$data <- new.env()

  # Mock connectors with custom function
  mock_connectors <- list(
    custom = list(load = "custom::load_function")
  )

  # Mock .get_imported_packages to include custom
  with_mocked_bindings(
    connectors = mock_connectors,
    .get_imported_packages = function() c("custom", "yaml"),
    {
      # Mock the parse function to return our test function when parsing
      # "custom::load_function"
      with_mocked_bindings(
        parse = function(text) {
          if (text == "custom::load_function") {
            return(expression(function(file) {
              expect_equal(file, file.path("data", "test.custom"))
              "custom_data"
            }))
          }
          base::parse(text = text)
        },
        .package = "base",
        {
          result <- load_data("test_data", mock_execution)

          expect_equal(result, "custom_data")
          expect_equal(mock_execution$data$test_data, "custom_data")
        }
      )
    }
  )
})

test_that("load_data handles multiple data loads in same execution", {
  # Mock execution environment
  mock_execution <- new.env()
  mock_execution$registry <- list(
    data1 = list(
      type = "rds",
      path = list("data", "data1.rds"),
      load_args = list()
    ),
    data2 = list(
      type = "csv",
      path = list("data", "data2.csv"),
      load_args = list()
    )
  )
  mock_execution$data <- new.env()

  # Mock connectors
  mock_connectors <- list(
    rds = list(load = "base::readRDS"),
    csv = list(load = "readr::read_csv")
  )

  # Mock .get_imported_packages
  with_mocked_bindings(
    connectors = mock_connectors,
    .get_imported_packages = function() c("base", "readr", "yaml"),
    {
      # Mock the parse function to return our test functions
      with_mocked_bindings(
        parse = function(text) {
          if (text == "base::readRDS") {
            return(expression(function(file) {
              if (file == file.path("data", "data1.rds")) {
                data.frame(x = 1:3)
              }
            }))
          } else if (text == "readr::read_csv") {
            return(expression(function(file) {
              if (file == file.path("data", "data2.csv")) {
                data.frame(y = letters[1:3])
              }
            }))
          }
          base::parse(text = text)
        },
        .package = "base",
        {
          # Load first dataset
          result1 <- load_data("data1", mock_execution)
          expect_equal(result1, data.frame(x = 1:3))
          expect_equal(mock_execution$data$data1, data.frame(x = 1:3))

          # Load second dataset
          result2 <- load_data("data2", mock_execution)
          expect_equal(result2, data.frame(y = letters[1:3]))
          expect_equal(mock_execution$data$data2, data.frame(y = letters[1:3]))

          # Verify both datasets are in execution environment
          expect_equal(mock_execution$data$data1, data.frame(x = 1:3))
          expect_equal(mock_execution$data$data2, data.frame(y = letters[1:3]))
        }
      )
    }
  )
})

test_that("load_data handles additional arguments correctly", {
  # Mock execution environment
  mock_execution <- new.env()
  mock_execution$registry <- list(
    test_csv = list(
      type = "csv",
      path = list("data", "test.csv"),
      load_args = list(col_types = "cc", skip = 1)
    )
  )
  mock_execution$data <- new.env()

  # Mock connectors
  mock_connectors <- list(
    csv = list(load = "readr::read_csv")
  )

  # Mock .get_imported_packages to include readr
  with_mocked_bindings(
    connectors = mock_connectors,
    .get_imported_packages = function() c("readr", "yaml"),
    {
      # Mock read_csv to return test data
      with_mocked_bindings(
        read_csv = function(file, col_types, skip) {
          expect_equal(file, file.path("data", "test.csv"))
          expect_equal(col_types, "cc")
          expect_equal(skip, 1)
          data.frame(col1 = letters[1:3], col2 = LETTERS[1:3])
        },
        .package = "readr",
        {
          result <- load_data("test_csv", mock_execution)

          expect_equal(
            result,
            data.frame(col1 = letters[1:3], col2 = LETTERS[1:3])
          )
          expect_equal(
            mock_execution$data$test_csv,
            data.frame(col1 = letters[1:3], col2 = LETTERS[1:3])
          )
        }
      )
    }
  )
})

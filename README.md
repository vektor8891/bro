# bro: Framework for Business Analytics and Data Science Projects

A framework for developing projects that are well-organized and easy to reproduce.

## Installation

```r
# Install from GitHub
remotes::install_github("Salompas/bro")
```

## Dependencies

The `bro` package is designed to be lightweight and only installs the packages you actually need. Many data formats and model types are supported through optional dependencies.

### Core Dependencies (Required)

These packages are automatically installed with `bro`:

- **yaml**: For reading/writing YAML configuration files
- **renv**: For virtual environment management
- **rlang**: For hashing and advanced R programming utilities

### Optional Dependencies

The following packages are optional and only loaded when needed:

#### Data I/O

- **readr**: For CSV file operations (`csv` type)
- **data.table**: For fast data.table operations (`data.table` type)
- **arrow**: For Parquet and Feather files (`arrow`, `feather` types)
- **jsonlite**: For JSON files (`json` type)
- **xml2**: For XML files (`xml` type)

#### Model Formats

- **xgboost**: For XGBoost model files (`xgboost` type)
- **lightgbm**: For LightGBM model files (`lightgbm` type)

#### Network/API

- **httr**: For HTTP requests (`http` type)
- **curl**: For curl operations (`curl` type)

#### Utilities

- **digest**: Alternative hashing (optional)

### How It Works

When you try to load or save a file type that requires an optional package:

1. The system checks if the required package is available using `requireNamespace()`
2. If available, the operation proceeds normally
3. If not available, you get a clear error message telling you which package to install

### Examples

See the example script for a demonstration of how optional dependencies work:

```r
# Run the example script
source(system.file("example", "optional_dependencies_example.R", package = "bro"))
```

This example shows:

- How to handle missing optional packages gracefully
- How hashing works with the required `rlang` dependency
- How to check package availability
- Best practices for working with optional dependencies

# bro

**bro** is an R framework for developing business analytics and data science projects that are well-organized, reproducible, and easy to maintain. It provides a structured approach to building data pipelines with support for multiple data sources including files (CSV, RDS, YAML) and databases (PostgreSQL, MySQL, SQLite, Redshift, Netezza).

## Key Features

- 📦 **Project scaffolding** - Initialize projects with a standardized folder structure
- 🔄 **Pipeline orchestration** - Define and run data processing workflows as directed acyclic graphs (DAGs)
- 🗄️ **Multi-source data catalog** - Unified interface for files and databases
- 🔌 **Pluggable database backends** - Support for PostgreSQL, MySQL, SQLite, Redshift, Netezza, and ODBC
- 🔐 **Secure credential management** - Separate credentials from configuration
- 💾 **Automatic caching** - Smart detection of data and pipeline changes
- 🌲 **Dependency resolution** - Automatically runs nodes in the correct order
- 📊 **Status tracking** - Track execution state and data lineage

## Installation

```r
# Install from GitHub
remotes::install_github("guilherme-salome/bro")
```

## Quick Start

### 1. Create a New Project

```r
# Create a new project with example files
bro::create("my_project", example = TRUE, renv = TRUE, git = TRUE)

# Restart R in the new project directory
setwd("my_project")
```

This creates a project structure:

```
my_project/
├── R/                    # Your R code and pipeline definitions
│   └── nodes.R          # Define your data processing nodes here
├── inst/                 # Configuration files
│   ├── data.yaml        # Data catalog (sources and destinations)
│   ├── connections.yaml # Database connection definitions
│   ├── connectors.yaml  # Data type handlers (CSV, RDS, SQL, etc.)
│   └── parameters.yaml  # Project parameters
├── data/                 # Data storage
│   ├── base/            # Raw/source data (tracked in git)
│   ├── inputs/          # Input data (not tracked)
│   ├── models/          # Model artifacts (not tracked)
│   ├── outputs/         # Results (not tracked)
│   └── reporting/       # Reports (not tracked)
└── logs/                # Execution logs
```

### 2. Configure Your Data Sources

Edit `inst/data.yaml` to define your data sources and destinations:

```yaml
# File-based data
raw_sales:
  type: csv
  path: ['data', 'base', 'sales.csv']

# Database table
customer_data:
  type: sql
  connection: prod_db        # References connection in connections.yaml
  table: customers

# Database query
recent_orders:
  type: sql
  connection: prod_db
  query: |
    SELECT * FROM orders 
    WHERE order_date >= CURRENT_DATE - INTERVAL '30 days'

# Output destination
sales_summary:
  type: csv
  path: ['data', 'outputs', 'sales_summary.csv']
```

### 3. Configure Database Connections (if needed)

Edit `inst/connections.yaml`:

```yaml
prod_db:
  backend: postgres
  host: db.example.com
  port: 5432
  database: analytics
  credential_key: prod_creds

data_warehouse:
  backend: redshift
  host: warehouse.us-east-1.redshift.amazonaws.com
  port: 5439
  database: dwh
  credential_key: redshift_creds
```

Create `inst/credentials.yaml` (this file is git-ignored):

```yaml
prod_creds:
  user: myuser
  password: mypassword

redshift_creds:
  user: analytics_user
  password: supersecret
```

### 4. Define Your Pipeline

Create `R/nodes.R` with your data processing functions:

```r
# Define a data processing function
calculate_sales_summary <- function(raw_sales, customer_data) {
  merged <- merge(raw_sales, customer_data, by = "customer_id")
  summary <- aggregate(amount ~ region, data = merged, FUN = sum)
  return(summary)
}

# Register it as a node in the pipeline
bro::node(
  f = calculate_sales_summary,
  x = c("raw_sales", "customer_data"),  # Input data (from data.yaml)
  y = "sales_summary"                    # Output data (to data.yaml)
)

# Add more nodes as needed
enrich_data <- function(raw_sales, recent_orders) {
  # Your enrichment logic
  enriched <- merge(raw_sales, recent_orders, by = "order_id", all.x = TRUE)
  return(enriched)
}

bro::node(
  f = enrich_data,
  x = c("raw_sales", "recent_orders"),
  y = "enriched_sales"
)
```

### 5. Run Your Pipeline

```r
# Create execution environment (loads all configurations)
execution <- bro::create_execution_env()

# Load your pipeline nodes
nodes <- bro::load_nodes()

# Run all nodes (automatically resolves dependencies)
for(node in nodes) {
  bro::run_node(node, execution)
}

# Close database connections when done
bro::close_connections(execution)
```

## How It Works

### The Execution Flow

1. **Project Initialization** (`create()`)
   - Creates folder structure
   - Copies configuration templates
   - Sets up git and renv (optional)

2. **Execution Environment** (`create_execution_env()`)
   - Loads `data.yaml` → data registry (catalog of all data sources)
   - Loads `connections.yaml` → database connection definitions
   - Loads `connectors.yaml` → handlers for different data types (CSV, SQL, etc.)
   - Loads `parameters.yaml` → project parameters
   - Loads `status.yaml` → execution state (for caching)

3. **Node Definition** (`node()`)
   - Wraps your function with metadata (inputs, outputs, name)
   - Registers in the pipeline DAG

4. **Pipeline Execution** (`run_node()`)
   - **Data Loading**: When a node runs, it requests its inputs by name
   - **Smart Loading**: `get_data()` checks:
     - Is it a parameter? → Return from `execution$parameters`
     - Is it already loaded? → Return from `execution$data` (cache)
     - Otherwise → Load it via `load_data()`
   - **Dynamic Connection**: For SQL sources, `load_data()`:
     - Calls `load_from_database()` with connection name
     - `get_connection()` checks if connection exists and is valid
     - If not, creates it using config from `connections.yaml` and `credentials.yaml`
     - Executes the query/reads the table
     - Caches the connection for reuse
   - **Function Execution**: Runs your function with loaded inputs
   - **Data Saving**: Saves outputs according to `data.yaml`
   - **Status Update**: Updates execution state for change detection

### Database Connection Flow

```
┌─────────────────────────────────────────────────────────┐
│ 1. Node requests data: "customer_data"                 │
└────────────────────┬────────────────────────────────────┘
                     │
                     ▼
┌─────────────────────────────────────────────────────────┐
│ 2. get_data() looks up "customer_data" in data.yaml    │
│    → type: sql                                          │
│    → connection: prod_db                                │
│    → table: customers                                   │
└────────────────────┬────────────────────────────────────┘
                     │
                     ▼
┌─────────────────────────────────────────────────────────┐
│ 3. load_from_database() called with connection="prod_db"│
└────────────────────┬────────────────────────────────────┘
                     │
                     ▼
┌─────────────────────────────────────────────────────────┐
│ 4. get_connection("prod_db") checks:                   │
│    - Does connection exist? Yes → reuse it             │
│                              No  → create it            │
└────────────────────┬────────────────────────────────────┘
                     │ (if creating new)
                     ▼
┌─────────────────────────────────────────────────────────┐
│ 5. Look up "prod_db" in connections.yaml               │
│    → backend: postgres                                  │
│    → host: db.example.com                              │
│    → credential_key: prod_creds                        │
└────────────────────┬────────────────────────────────────┘
                     │
                     ▼
┌─────────────────────────────────────────────────────────┐
│ 6. Load credentials from credentials.yaml              │
│    → prod_creds: {user: myuser, password: mypassword}  │
└────────────────────┬────────────────────────────────────┘
                     │
                     ▼
┌─────────────────────────────────────────────────────────┐
│ 7. create_connection() uses DBI to connect             │
│    → DBI::dbConnect(RPostgres::Postgres(), ...)        │
│    → Stores connection in execution$connections        │
└────────────────────┬────────────────────────────────────┘
                     │
                     ▼
┌─────────────────────────────────────────────────────────┐
│ 8. Execute query: DBI::dbReadTable(conn, "customers")  │
└────────────────────┬────────────────────────────────────┘
                     │
                     ▼
┌─────────────────────────────────────────────────────────┐
│ 9. Return data to node function                        │
└─────────────────────────────────────────────────────────┘
```

### The Connector System

The `connectors.yaml` file defines how different data types are loaded and saved:

```yaml
csv:
  load: readr::read_csv
  save: readr::write_csv

rds:
  load: base::readRDS
  save: base::saveRDS

sql:
  load: bro:::load_from_database
  save: bro:::save_to_database
```

When `load_data()` processes a data entry:

1. Looks up the `type` field in `data.yaml`
2. Finds the corresponding loader in `connectors.yaml`
3. For `type: csv` → calls `readr::read_csv`
4. For `type: sql` → calls `bro:::load_from_database` (which handles connections)

This makes the system extensible - you can add new data types by adding entries to `connectors.yaml`.

## Supported Database Backends

- **PostgreSQL** - `backend: postgres` (requires RPostgres)
- **MySQL/MariaDB** - `backend: mysql` (requires RMySQL)
- **SQLite** - `backend: sqlite` (requires RSQLite)
- **Amazon Redshift** - `backend: redshift` (requires RPostgres)
- **IBM Netezza** - `backend: netezza` (requires RODBC)
- **Generic ODBC** - `backend: odbc` (requires odbc)

Install drivers as needed:

```r
install.packages(c("RPostgres", "RMySQL", "RSQLite", "RODBC", "odbc"))
```

## Advanced Features

### Dependency Resolution

bro automatically determines the execution order based on data dependencies:

```r
# Node A produces "data_a"
node(f = function_a, x = NULL, y = "data_a")

# Node B needs "data_a" to produce "data_b"
node(f = function_b, x = "data_a", y = "data_b")

# Node C needs both "data_a" and "data_b"
node(f = function_c, x = c("data_a", "data_b"), y = "data_c")

# Execution order is automatically: A → B → C
```

### Change Detection

bro tracks hashes of:

- Node definitions (function code)
- Input data

A node only re-runs if:

- The function code changed
- Any input data changed
- It hasn't been run before

This enables efficient incremental computation.

### Parameters

Store configuration in `inst/parameters.yaml`:

```yaml
analysis:
  start_date: '2024-01-01'
  threshold: 0.05

database:
  batch_size: 1000
```

Access in your nodes:

```r
my_function <- function(raw_data, parameters) {
  start_date <- parameters$analysis$start_date
  # Use parameters
}

node(
  f = my_function,
  x = c("raw_data", "parameters$analysis"),
  y = "filtered_data"
)
```

### Running Specific Nodes

Run a single node with its dependencies:

```r
execution <- bro::create_execution_env()
nodes <- bro::load_nodes()

# Find the specific node
target_node <- nodes[[which(sapply(nodes, function(n) n$name == "calculate_sales_summary"))]]

# Get its dependency path
dependencies <- bro::traverse_nodes(nodes)
path <- bro::find_path(target_node, nodes, dependencies)

# Run nodes in dependency order
for(node_name in path) {
  node <- nodes[[which(sapply(nodes, function(n) n$name == node_name))]]
  bro::run_node(node, execution)
}
```

## Best Practices

1. **Separate concerns**: Keep configuration (YAML) separate from code (R)
2. **Version control**: Commit code and configs, but NOT credentials or generated data
3. **Use renv**: Lock package versions for reproducibility
4. **Document nodes**: Add comments explaining what each node does
5. **Small functions**: Keep node functions focused and testable
6. **Close connections**: Always call `bro::close_connections(execution)` when done
7. **Test connections**: Verify database configs work before building pipelines

## Security Notes

- `credentials.yaml` is automatically git-ignored
- Never hardcode passwords in code or YAML (except credentials.yaml)
- Use environment-specific credentials files for dev/staging/prod
- Consider using environment variables or secret managers for production

## Documentation

- 📖 Full database connection guide: `docs/DATABASE_CONNECTIONS.md`
- 📖 Implementation details: `docs/IMPLEMENTATION_SUMMARY.md`
- 📖 Vignette: `vignette("database-connections")`

## Example Projects

See working examples:

- File-based pipeline: Example included with `bro::create(example = TRUE)`
- Database pipeline: See `vignettes/database-connections.Rmd`

## Contributing

Contributions are welcome! Please feel free to submit a Pull Request.

## License

MIT License - see LICENSE file for details.

## Authors

- Guilherme Salome ([@guilherme-salome](https://github.com/guilherme-salome))

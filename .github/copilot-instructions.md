# GitHub Copilot Instructions for bro Package

## Code Generation Rules

### Always Use Namespace Notation

**CRITICAL**: When generating code that uses the `bro` package, ALWAYS use namespace notation (`bro::`) instead of `library(bro)`.

✅ **CORRECT - Always generate code like this:**

```r
execution <- bro::create_execution_env()
nodes <- bro::load_nodes()
bro::run_node(node, execution)
bro::close_connections(execution)
```

❌ **NEVER generate code like this:**

```r
library(bro)
execution <- create_execution_env()
nodes <- load_nodes()
run_node(node, execution)
```

### Exception: Internal Package Code

When creating code inside the `bro` package itself (in `R/` directory), use internal functions without namespace prefix as this is standard R package development practice.

### Rationale

- **Safety**: Prevents namespace conflicts and ensures predictable behavior
- **Clarity**: Makes dependencies explicit in the code
- **Production-ready**: Follows R best practices for package development
- **Debugging**: Makes it immediately clear which package provides each function

## Code Formatting Rules

### Line Width Limit

**IMPORTANT**: Always respect a maximum line width of 80 characters when generating code.

- Break long function calls across multiple lines
- Use proper indentation for continuation lines
- Apply this rule to both code and comments
- Follow R style guidelines for line breaks

✅ **CORRECT - Properly formatted within 80 characters:**

```r
result <- bro::load_data(
  data_name = "my_dataset",
  execution = execution,
  force_reload = TRUE
)
```

❌ **INCORRECT - Exceeds 80 character limit:**

```r
result <- bro::load_data(data_name = "my_dataset", execution = execution, force_reload = TRUE)
```

### Linting and Code Quality

**IMPORTANT**: Whenever you edit or create a file, always fix any existing linting issues in that file.

- Use **implicit returns** instead of explicit `return()` statements where appropriate (last expression in function)
- Remove unnecessary explicit returns
- Fix any style violations flagged by linters
- Ensure consistent code formatting throughout the file
- Apply tidyverse/lintr style guidelines

✅ **CORRECT - Implicit return:**

```r
add_numbers <- function(a, b) {
  a + b
}
```

❌ **INCORRECT - Unnecessary explicit return:**

```r
add_numbers <- function(a, b) {
  return(a + b)
}
```

**Note**: Use explicit `return()` only for early returns or when needed for clarity in complex control flow.

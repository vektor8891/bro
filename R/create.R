#' @title Create Project
#'
#' @description
#' Initializes a new project with a predefined folder structure and optional examples, virtual environment, and Git repository.
#'
#' @details
#' The function creates a new project directory, including the 'R' source code folder, 'inst' folder for parameters,
#' connections, and data catalog, 'data' folder with subdirectories for different data types, and a 'logs' folder.
#' It copies necessary files from the 'bro' package for configuration and data, and sets up a '.gitignore' file in
#' relevant data folders. If requested, it also copies example files ('costs.csv' and 'data.yaml'). Additionally,
#' it initializes a virtual environment using 'renv' and installs the 'bro' package from the specified GitHub repository.
#' Lastly, it initializes a Git repository and adds an initial commit.
#'
#' @param name (string, optional) Name of the new project or relative/absolute path to a folder.
#'   If unspecified, uses the current working directory as the project folder and name.
#' @param example (logical) If TRUE, copies example files to the project.
#' @param renv (logical) If TRUE, initializes a virtual environment using 'renv'.
#' @param git (logical) If TRUE, initializes a Git repository and adds an initial commit.
#' @param replace (logical) If FALSE and the specified folder already exists, stops and displays an error.
#'   If TRUE, replaces an existing folder with the new project.
#' @export
#'
create <- function(name = getwd(), example = TRUE, renv = TRUE, git = TRUE, replace = FALSE) {

  ## Create project directory
  path <- path.expand(name)
  if(dir.exists(path) & !replace) {
    stop("Project could *not* be created because the folder already exists. Please set 'replace = TRUE' to create a project on an existing folder.")
  }
  dir.create(path, showWarnings = FALSE, recursive = TRUE)
  message("Project:\n", path)

  ## Create folder structure
  folders <- list(
    source = file.path(path, "R"), # source code
    configurations = file.path(path, "inst"), # parameters, connections, and data catalog
    data = lapply(
      c("", "base", "inputs", "models", "outputs", "reporting"),
      function(x) {file.path(path, "data", x)}
    ),
    logs = file.path(path, "logs")
  )
  created <- lapply(
    unlist(folders), dir.create, showWarnings = FALSE, recursive = TRUE
  )
  message("Folders:\n", paste0(unlist(folders), collapse = "\n"))

  ## Copy files to 'inst'
  inst <- list.files(system.file(file.path("create", "inst"), package = "bro"), all.files = TRUE, recursive = TRUE, full.names = TRUE)
  copied <- file.copy(
    from = inst,
    to = folders$configurations,
    overwrite = TRUE,
    recursive = TRUE
  )
  message("Files:\n", paste0(file.path(folders$configurations, basename(inst)), collapse = "\n"))

  ## Copy .gitignore to some data folders
  copied <- file.copy(
    from = system.file(file.path("create", "gitignore_data"), package = "bro"),
    ## track data folders and data in base, but not data in other folders
    to = file.path(folders$data[-1:-2], ".gitignore"),
    overwrite = TRUE
  )
  message(paste0(file.path(folders$data[-1:-2], ".gitignore"), collapse = "\n"))

  ## Copy example files if requested
  if(example) {
    file.copy(
      from = system.file(file.path("example", "costs.csv"), package = "bro"),
      to = folders$data[[2]],
      overwrite = TRUE
    )
    message("Examples:\n", file.path(folders$data[[2]], "costs.csv"))
    file.copy(
      from = system.file(file.path("example", "data.yaml"), package = "bro"),
      to = folders$configurations,
      overwrite = TRUE
    )
    message(file.path(folders$configurations, "data.yaml"))
  }

  ## Set up virtual environment using 'renv'
  if(renv) {
    message("Virtual Environment:\n")
    renv::init(
            project = path, # initialize renv in the project folder
            bare = TRUE,    # do not install any package
            settings = list(snapshot.type = "all"), # capture all packages that will be installed
            restart = FALSE # more works needs to be done after renv, will ask user to restart later
          )
    tryCatch({
      ## Try installing 'bro' in the virtual environment
      renv::install("https://github.com/Salompas/bro", prompt = FALSE) # needs to be replaced by CRAN version
      message("Installed 'bro' to virtual environment (renv)")
    },
    error = function(e) {
      message("Error when installing 'bro' in virtual environment: ", e$message)
    })
  }

  ## Set up git repository and first commit
  if(git) {
    message("Git:\n")
    tryCatch({
      ## Create new git repository and add a commit with all new files
      system2("git", "init")
      system2("git", "add -A")
      system2("git", "commit -m 'Project created'")
      message("Tracking project with Git")
    },
    error = function(e) {
      message("Unable to initialize Git repository: ", e$message)
    })
  }

  ## Ask user to start R in the project directory
  message("NOTICE: Project successfully created. Please restart R on the working directory '", path, "'")

}

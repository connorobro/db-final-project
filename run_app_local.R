required_packages <- c(
  "shiny",
  "tidyverse",
  "plotly",
  "DT",
  "jsonlite",
  "DBI",
  "RSQLite"
)

r_version <- paste(R.version[["major"]], sub("\\..*", "", R.version[["minor"]]), sep = ".")
personal_lib <- file.path(Sys.getenv("USERPROFILE"), "Documents", "R", "win-library", r_version)

dir.create(personal_lib, recursive = TRUE, showWarnings = FALSE)
.libPaths(c(personal_lib, .libPaths()))

for (pkg in required_packages) {
  if (!requireNamespace(pkg, quietly = TRUE)) {
    install.packages(pkg, lib = personal_lib, repos = "https://cloud.r-project.org")
  }
}

status <- sapply(required_packages, requireNamespace, quietly = TRUE)
print(status)

if (!all(status)) {
  stop("Missing required packages: ", paste(names(status)[!status], collapse = ", "))
}

shiny::runApp(
  appDir = ".",
  host = "127.0.0.1",
  port = 3838,
  launch.browser = FALSE
)

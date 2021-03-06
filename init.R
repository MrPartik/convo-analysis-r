packages <- c("shiny", "devtools", "plotly", "dplyr", "readr", "jsonlite", "shinycssloaders")
install_if_missing <- function(p) {
  if (!p %in% rownames(installed.packages())) {
    install.packages(p)
  }
}
invisible(sapply(packages, install_if_missing))


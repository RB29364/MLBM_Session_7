# dependencies.R

# List of required packages
required_packages <- c("shiny", "quantmod", "DT", "lubridate", "dplyr", "ggplot2", "scales")

# Check if packages are installed, and install if missing
for (pkg in required_packages) {
  if (!requireNamespace(pkg, quietly = TRUE)) {
    install.packages(pkg)
  }
}

# Load the required packages
lapply(required_packages, library, character.only = TRUE)

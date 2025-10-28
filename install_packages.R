# Quality Control App - Package Installation
# Run this once to install all required packages

packages <- c(
  "shiny", "shinydashboard", "dplyr", "ggplot2", "writexl",
  "readxl", "DT", "lubridate")

# Install packages
for(pkg in packages) {
  if(!require(pkg, character.only = TRUE)) {
    install.packages(pkg, dependencies = TRUE)
  }
}

cat("All packages installed successfully!\n")
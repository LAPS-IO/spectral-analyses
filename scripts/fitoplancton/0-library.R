# Package names
packages <- c("data.table", "tidyverse", "here", "pbapply", "stringr",
              "plotly", "astsa", "dplR", "lubridate", "parallel",
              "ggpubr", "patchwork", "vegan", "hms")

# Install packages not yet installed
installed_packages <- packages %in% rownames(installed.packages())
if (any(installed_packages == FALSE)) {
  install.packages(packages[!installed_packages])
}

# Load packages
invisible(lapply(packages, library, character.only = TRUE))

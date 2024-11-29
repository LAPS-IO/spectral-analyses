# Package names
packages <- c("data.table", "tidyverse", "here", 
              "plotly", "astsa", "dplR", "lubridate",
              "ggpubr", "patchwork", "vegan")

# Install packages not yet installed
installed_packages <- packages %in% rownames(installed.packages())
if (any(installed_packages == FALSE)) {
  install.packages(packages[!installed_packages])
}

# Load packages
invisible(lapply(packages, library, character.only = TRUE))
# Package names
packages <- c("data.table", "tidyverse", "here", "pbapply", "parallel", "fst",
              "plotly", "astsa", "dplR", "lubridate", "purrr", "e1071", "progressr",
              "ggpubr", "patchwork", "vegan", "hms", "FactoMineR", "factoextra")

# Install packages not yet installed
installed_packages <- packages %in% rownames(installed.packages())
if (any(installed_packages == FALSE)) {
  install.packages(packages[!installed_packages])
}

# Load packages
invisible(lapply(packages, library, character.only = TRUE))
# Read image files and save as *.csv

# Packages
# Loading packages used run the analysis
library(here)
source(here("scripts", "general", "0-library.R"))

# Path image files of interest - roi's classified as some class of zooplankton
path <- here("data", "raw", "zooplancton", "imgs", "penilia")

# Listando os arquivos .png no diretório
files <- list.files(path, pattern = "\\.png$", full.names = FALSE)

## 2. Tibble completo
data <- tibble(names = files,
               year_month = str_replace(str_sub(files, 1, 7), "-", "_"),
               pred = "penilia")

## 3. Divide pelo rótulo e grava CSVs
data_split <- split(data, data$year_month)

purrr::iwalk(data_split,
             ~ fwrite(.x %>% select(names, pred),
                      here("data", "raw", "zooplancton",
                           paste0(.y, ".csv"))))

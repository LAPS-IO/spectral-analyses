# RAW Time-Series plot

# Ler *.csv's de fitoplâncton para séries temporais

# Packages
# Loading packages used run the analysis
library(here)
source(here("scripts", "general", "0-library.R"))

# Path of files of interest - roi's classified as some class of zooplankton
path <- here("data", "raw", "zooplancton", "chaetognatha")
# Path of files of interest - all roi's and their cycle
path2 <- here("data", "raw", "rois_list")
# Path of files of interest - all raw's and their cycle
path3 <- here("data", "raw", "raw_csv_list")


# Função para ler todos os arquivos CSV uma vez
read_all_files <- function(path) {
  
  files <- list.files(path)
  
  data <- lapply(files, function(f) {
    
    df <- fread(here(path, f))
    df <- df |> 
      dplyr::mutate(pred = as.factor(pred),
                    date_time = stringr::str_sub(names, 1, 23) %>%
                      lubridate::ymd_hms())
    cat("file:", f, "- N:", dim(df)[1], "\n")
    return(df)
    
  })
  
  data <- dplyr::bind_rows(data)
  return(data)
}

# Lê todos os arquivos CSV uma vez
all_data <- read_all_files(path)

# Função para filtrar e processar dados por classe
process_data <- function(data, classe, rounddate = "30 min") {
  
  df <- data |> 
    dplyr::filter(pred %in% classe) |> 
    droplevels() |> 
    dplyr::transmute(names = names,
                     class = as.factor(pred),
                     date_time = date_time,
                     cycle_rounded = lubridate::round_date(date_time, rounddate))
  
  return(df)
  
}

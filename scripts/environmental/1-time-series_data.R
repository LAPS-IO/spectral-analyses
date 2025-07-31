# RAW Time-Series plot

# Ler *.csv's de fitoplâncton para séries temporais

# Packages
# Loading packages used run the analysis
library(here)
source(here("scripts", "general", "0-library.R"))

# Path of files of interest - roi's classified as some class of phytoplankton
data_path <- here("data", "raw", "environ")

# Configurar paralelismo com 7 núcleos
num_cores <- 7
cl <- makeCluster(num_cores)
clusterEvalQ(cl, {
  library(here)
  library(data.table)
  library(pbapply)
})

# Passo 1: listar arquivos e criar lista vazia
environ_data <- list.files(data_path, pattern = "\\.csv$", full.names = TRUE)
environ_data_list <- list()

# Passo 2: processa cada arquivo e o salva na lista
for (arquivo in environ_data) {
  
  # Lê o arquivo CSV
  dados <- fread(arquivo)
  
  # Processa cada coluna (da segunda em diante)
  colunas <- names(dados)[-1]
  
  for (col in colunas) {
    
    # Cria o data.table com as duas colunas
    dt <- dados[, .SD, .SDcols = c(names(dados)[1], col)]
    
    # Adiciona à lista principal
    environ_data_list[[col]] <- dt
    
  }
}

# Passo 3: Save environmental data by type
path_to_save <- here("data", "processed", "environ", "1 - data raw")
for(s in names(environ_data_list)) {
  
  fwrite(environ_data_list[[s]], here(path_to_save, paste0(s, ".csv")))
  
}
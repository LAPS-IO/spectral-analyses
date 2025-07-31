# RAW Time-Series plot

# Ler *.csv's de fitoplâncton para séries temporais

# Packages
# Loading packages used run the analysis
library(here)
source(here("scripts", "general", "0-library.R"))

# Path of files of interest - roi's classified as some class of phytoplankton
path <- here("data", "raw", "fitoplancton")
# Path of files of interest - all roi's and their cycle
path2 <- here("data", "raw", "rois_list")
# Path of files of interest - all raw's and their cycle
path3 <- here("data", "raw", "raw_csv_list")

# Configurar paralelismo com 7 núcleos
num_cores <- 7
cl <- makeCluster(num_cores)
clusterEvalQ(cl, {
  library(here)
  library(data.table)
  library(pbapply)
})

# Função para ler todos os arquivos classificados como fitoplâncton e todos os rois existentes, obtendo o ciclo de origem do roi
read_all_files <- function(path, path2) {
  
  files <- list.files(path, full.names = TRUE)
  files2 <- list.files(path2, full.names = TRUE)
  
  data <- pblapply(files, function(f) {
    
    file_name <- basename(f)
    matching_file <- file.path(path2, file_name)
    
    if (!file.exists(matching_file)) {
      
      warning(paste("Arquivo correspondente não encontrado para:", file_name))
      return(NULL)
      
    }
    
    df <- fread(f)
    df[, pred := as.factor(pred)]
    setnames(df, old = "names", new = "roi")
    
    dg <- fread(matching_file)
    
    # Merge dos data.tables pelos 'roi'
    setkey(df, roi)
    setkey(dg, roi)
    merged <- merge(df, dg, by = "roi", all.x = TRUE)[, .(roi, pred, cycle = as.character(cycle))]
    
    return(merged)
  }, cl = cl)
  
  return(data)
}

# Executa a função criada no passo anterior
all_data <- read_all_files(path, path2)

# Agrupar os dados obtidos anteriormente (por classe e ciclo) e contabilizar a quantidade de rois da referida classe por ciclo
all_data2 <- pblapply(all_data, function(x) {
  # Converte o dataframe em data.table
  dt <- as.data.table(x)
  
  # Agrupa por 'pred' e 'cycle' e calcula a soma
  dt <- dt[, .(N_rois = .N), by = .(pred, cycle)]
  
  return(dt)
})

# Save phytoplankton Nrois list by cycle and class
path_to_save <- here("data", "processed", "fitoplancton")
fwrite(rbindlist(all_data2), here(path_to_save, "class_cycle_Nrois.csv"))

# Terminar o cluster
stopCluster(cl)

# 
all_data2 <- fread("data/processed/fitoplancton/class_cycle_Nrois.csv", 
                   colClasses = list(character = "cycle"))
all_data2 <- rbindlist(all_data2)
setnames(all_data2, old = "cycle", new = "ciclo")
cycles[, ciclo := gsub("_", ":", ciclo)]
# cycles <- fread("data/processed/general/cycles.csv")
all_data2[, ciclo := ifelse(nchar(ciclo) == 25, paste0(ciclo, "0"), ciclo)]
setkey(all_data2, ciclo)
setkey(cycles, ciclo)

df <- merge(all_data2, cycles, by = "ciclo", all = T, all.y = F)
df <- na.omit(df)
# Separando por 'pred' e criando uma lista de data.tables
split_df <- split(df, by = "pred")

split_df <- lapply(split_df, function(x) {
  df <- na.omit(x)
  df[, vol_sampled_L := raw*(0.101767)
     ][, density := N_rois/vol_sampled_L]
})
#####


# Função para filtrar e processar dados por classe
process_data <- function(data, classe, rounddate = "1 hour") {
  
  df <- data |> 
    dplyr::filter(pred %in% classe) |> 
    droplevels() |> 
    dplyr::transmute(names = names,
                     class = as.factor(pred),
                     date_time = date_time,
                     cycle_rounded = lubridate::round_date(date_time, rounddate))
  
  return(df)
  
}

# classes <- c("doliolidae", "penilia", "radiozoa")
classes <- c("coscinodiscus", "guinardia_striata", "rhizosolenia_robusta",
             "compl_rhizosolenia_proboscia", "compl_guinardia_dactyliosolen",
             "hemidiscus", "chaetoceros")

df <- list()
pb <- txtProgressBar(min = 0, max = length(classes), style = 3)
for (i in seq_along(classes)) {
  
  df[[classes[i]]] <- process_data(all_data, classes[i], rounddate = "1 hour")
  setTxtProgressBar(pb, i)
  
}

# Ajusta dataframe para número de imagens por hora
df2 <- NULL
pb <- txtProgressBar(min = 0, max = length(df), style = 3)
for(d in names(df)) {
  df2[[d]] <- df[[d]] |>
    group_by(class, 
             cycle_rounded = lubridate::round_date(cycle_rounded, "1 hour")) %>%
    dplyr::summarise(n = dplyr::n() %>% as.integer()) |>
    ungroup()
  
  setTxtProgressBar(pb, d)
  
}

# Pivota os dados, para que a classe de interesse sejam colunas
df3 <- NULL
pb <- txtProgressBar(min = 0, max = length(df2), style = 3)
for(i in names(df2)) {
  df3[[i]] <- df2[[i]] |>
    tidyr::pivot_wider(names_from = class, 
                       values_from = n)
  
  setTxtProgressBar(pb, i)
}

# IDEAL: Necessário listar os ciclos existentes dos arquivos raw e posteriomente adicionar os 0's
path_raw_cycles <- here("data", "raw")

cycles <- fread(here(path_raw_cycles, "raw_cycles.csv"), 
                select = c(2, 3))

cycles <- cycles |> 
  dplyr::filter(as.numeric(size) <= 181) |>
  mutate(cycle = lubridate::ymd_hms(cycle),
         cycle_rounded = lubridate::round_date(cycle, "1 hour"),
         volume_L = size * 0.101767)

cycles_hour <- cycles |> 
  group_by(cycle_rounded) |>
  summarise(size = sum(size, na.rm = T),
            volume_L = sum(volume_L, na.rm = T))

# Se há horários que não aparece imagens adicionar 0
df4 <- NULL
pb <- txtProgressBar(min = 0, max = length(df3), style = 3)
for(i in names(df3)) {
  df4[[i]] <- dplyr::full_join(cycles_hour, df3[[i]])
  df4[[i]][, 4][is.na(df4[[i]][, 4])] <- 0
  
  setTxtProgressBar(pb, i)
}

# Completando com ciclos que não existem (manutenção, falha na aquisição...)
cycles_complete <- tibble(cycle_rounded = seq(ymd_hm("2020-11-04 00:00"),
                                              ymd_hm("2022-06-10 23:59"),
                                              by = "1 hour"))

df5 <- NULL
pb <- txtProgressBar(min = 0, max = length(df4), style = 3)
for(i in names(df4)) {
  df5[[i]] <- dplyr::full_join(df4[[i]], cycles_complete)
  
  setTxtProgressBar(pb, i)
}

# Criando a coluna densidade N/L e filtrando para o período de interesse
df6 <- NULL
pb <- txtProgressBar(min = 0, max = 100, style = 3)
for(i in names(df5)) {
  df6[[i]] <- df5[[i]] |>
    dplyr::mutate(density = as.numeric(unlist(df5[[i]][i])) / as.numeric(unlist(volume_L))) |>
    dplyr::filter(dplyr::between(cycle_rounded,
                                 lubridate::ymd_hms("2020-11-04 00:00:00"),
                                 lubridate::ymd_hms("2021-12-31 23:59:59"))) |>
    droplevels()

  setTxtProgressBar(pb, i)
  
}

# Save data frames
path_to_save <- here("data", "processed", "fitoplancton")
for(s in names(split_df)) {
  fwrite(split_df[[s]], here(path_to_save, paste0(s, ".csv")))
  
}


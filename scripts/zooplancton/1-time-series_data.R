# RAW Time-Series plot

# Ler *.csv's de fitoplâncton para séries temporais

# Packages
# Loading packages used run the analysis
library(here)
source(here("scripts", "general", "0-library.R"))

# Path of files of interest - roi's classified as some class of phytoplankton
path <- here("data", "raw", "zooplancton", "penilia")
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
  
  files <- list.files(path, full.names = TRUE, recursive = F)
  files2 <- list.files(path2, full.names = TRUE, recursive = F, pattern = ".csv$")
  
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

##################################################

# Agrupar os dados obtidos anteriormente (por classe e ciclo) e contabilizar a quantidade de rois da referida classe por ciclo
all_data2 <- pblapply(all_data, function(x) {
  # Converte o dataframe em data.table
  dt <- as.data.table(x)
  
  # Agrupa por 'pred' e 'cycle' e calcula a soma
  dt <- dt[, .(N_rois = .N), by = .(pred, cycle)]
  
  return(dt)
})

# Save zooplankton Nrois list by cycle and class
path_to_save <- here("data", "processed", "zooplancton")
fwrite(all_data2, here(path_to_save, "class_cycle_Nrois.csv"))

# Terminar o cluster
stopCluster(cl)

# arquivos <- list.files(here("data", "processed", "all_data"), pattern = "\\.csv$", full.names = TRUE)
# all_data  <- lapply(arquivos, fread)
all_data2 <- fread(here("data", "processed", "zooplancton", "class_cycle_Nrois.csv"), 
                   colClasses = list(character = "cycle", factor = "pred"))

# all_data2 <- rbindlist(all_data2)
# setnames(all_data2, old = "cycle", new = "ciclo")
cycles <- fread("data/processed/general/cycles.csv")
cycles[, ciclo := gsub("_", ":", ciclo)]
all_data2[, ciclo := str_pad(ciclo, width = 26, side = "right", pad = "0")]
setkey(all_data2, ciclo)
setkey(cycles, ciclo)

df <- merge(all_data2, cycles, by = "ciclo", all = T, all.y = F)
df <- na.omit(df)

split_df <- split(df, by = "pred")

split_df <- lapply(split_df, function(x) {
  df <- na.omit(x)
  df[, vol_sampled_L := raw*(0.101767)
  ][, density := N_rois/vol_sampled_L]
})
#####

# 
# # Função para filtrar e processar dados por classe
# process_data <- function(data, classe, rounddate = "1 hour") {
#   
#   df <- data |> 
#     dplyr::filter(pred %in% classe) |> 
#     droplevels() |> 
#     dplyr::transmute(roi = roi,
#                      class = as.factor(pred),
#                      date_time = cycle,
#                      cycle_rounded = lubridate::round_date(date_time, rounddate))
#   
#   return(df)
#   
# }
# 
# # classes <- c("doliolidae", "penilia", "radiozoa")
# classes <- names(split_df)
# 
# df <- list()
# pb <- txtProgressBar(min = 0, max = length(classes), style = 3)
# for (i in seq_along(all_data)) {
#   
#   df[[classes[i]]] <- process_data(all_data[i], classes[i], rounddate = "1 hour")
#   setTxtProgressBar(pb, i)
#   
# }
# 
# ###################################
# n_cores <- 7
# cl <- parallel::makeCluster(n_cores)
# parallel::clusterEvalQ(cl, { library(data.table) })
# 
# ############################################################
# # 1. Caminhos dos dados brutos
# ############################################################
# path_penilia <- here("data", "raw", "zooplancton", "penilia")
# path_rois    <- here("data", "raw", "rois_list")
# 
# ############################################################
# # 2. Função: lê cada CSV de penilia e anexa o ciclo respectivo
# ############################################################
# read_penilia <- function(path_penilia, path_rois) {
#   
#   arquivos_penilia <- list.files(path_penilia, full.names = TRUE)
#   
#   pblapply(arquivos_penilia, function(f) {
#     
#     nome_base   <- basename(f)
#     arq_ciclo   <- file.path(path_rois, nome_base)
#     
#     ## se não existir par correspondente, pula
#     if (!file.exists(arq_ciclo)) {
#       warning("Arquivo de ciclo não encontrado para: ", nome_base)
#       return(NULL)
#     }
#     
#     # dados classificados
#     dt_pred <- fread(f)
#     dt_pred[, pred := factor(pred)]
#     setnames(dt_pred, "names", "roi")         # garante consistência
#     
#     # dados com ciclo
#     dt_cyc  <- fread(arq_ciclo)               # tem colunas roi + cycle
#     
#     setkey(dt_pred, roi)
#     setkey(dt_cyc , roi)
#     
#     merge(dt_pred, dt_cyc, by = "roi", all.x = TRUE)[
#       , .(roi, pred, cycle = as.character(cycle))]
#     
#   }, cl = cl)
# }
# 
# lista_penilia <- read_penilia(path_penilia, path_rois)
# parallel::stopCluster(cl)
# 
# ############################################################
# # 3. N. de ROI por classe & ciclo  --------------------------
# ############################################################
# n_roi_por_ciclo <- rbindlist(
#   pblapply(lista_penilia, \(dt) dt[, .(N_rois = .N), by = .(pred, cycle)])
# )
# 
# dir_out <- here("data", "processed", "zooplancton")
# dir.create(dir_out, recursive = TRUE, showWarnings = FALSE)
# 
# fwrite(n_roi_por_ciclo, file = file.path(dir_out, "class_cycle_Nrois.csv"))
# 
# ############################################################
# # 4. Anexar info de ciclos gerais + densidade  -------------
# ############################################################
# cycles <- fread(here("data", "processed", "general", "cycles.csv"))
# cycles[, ciclo := gsub("_", ":", ciclo)]
# 
# ## prepara tabela de contagens
# n_roi_por_ciclo[, ciclo := cycle]   # renomeia (mantém string)
# n_roi_por_ciclo[, cycle := NULL]
# 
# ## se ciclos com 25 dígitos > acrescenta zero ao final
# n_roi_por_ciclo[, ciclo := ifelse(
#   nchar(ciclo) == 25, paste0(ciclo, "0"), ciclo)]
# 
# setkey(cycles,    ciclo)
# setkey(n_roi_por_ciclo, ciclo)
# 
# df_full <- merge(n_roi_por_ciclo, cycles, by = "ciclo", all.x = TRUE)
# df_full <- na.omit(df_full)
# 
# # volume amostrado e densidade (ajuste se 'raw' mudar de nome)
# df_full[, vol_sampled_L := raw * 0.101767]
# df_full[, density       := N_rois / vol_sampled_L]
# 
# ############################################################
# # 5. Split por classe (pred) -------------------------------
# ############################################################
# split_df <- split(df_full, by = "pred")
# 
# ############################################################
# # 6. Função de processamento final -------------------------
# ############################################################
# process_data <- function(dt, round_unit = "1 hour") {
#   dt[, .(
#     class         = pred,
#     N_rois        = N_rois,
#     density       = density,
#     date_time     = cycle_rounded,                              # já é POSIXct
#     cycle_rounded = round_date(cycle_rounded, round_unit)
#   )]
# }
# 
# ############################################################
# # 7. Aplicar a todas as classes ----------------------------
# ############################################################
# classes   <- names(split_df)
# resultado <- vector("list", length(classes)); names(resultado) <- classes
# 
# pb <- txtProgressBar(min = 0, max = length(classes), style = 3)
# for (i in seq_along(classes)) {
#   resultado[[i]] <- process_data(split_df[[classes[i]]])
#   setTxtProgressBar(pb, i)
# }
# close(pb)
# 
# 
# 
# 
# 
# 
# ###################################
# 
# 
# # Ajusta dataframe para número de imagens por hora
# df2 <- NULL
# pb <- txtProgressBar(min = 0, max = length(df), style = 3)
# for(d in names(df)) {
#   df2[[d]] <- df[[d]] |>
#     group_by(class, 
#              cycle_rounded = lubridate::round_date(cycle_rounded, "1 hour")) %>%
#     dplyr::summarise(n = dplyr::n() %>% as.integer()) |>
#     ungroup()
#   
#   setTxtProgressBar(pb, d)
#   
# }
# 
# # Pivota os dados, para que a classe de interesse sejam colunas
# df3 <- NULL
# pb <- txtProgressBar(min = 0, max = length(df2), style = 3)
# for(i in names(df2)) {
#   df3[[i]] <- df2[[i]] |>
#     tidyr::pivot_wider(names_from = class, 
#                        values_from = n)
#   
#   setTxtProgressBar(pb, i)
# }
# 
# # IDEAL: Necessário listar os ciclos existentes dos arquivos raw e posteriomente adicionar os 0's
# path_raw_cycles <- here("data", "raw")
# 
# cycles <- fread(here(path_raw_cycles, "raw_cycles.csv"), 
#                 select = c(2, 3))
# 
# cycles <- cycles |> 
#   dplyr::filter(as.numeric(size) <= 181) |>
#   mutate(cycle = lubridate::ymd_hms(cycle),
#          cycle_rounded = lubridate::round_date(cycle, "1 hour"),
#          volume_L = size * 0.101767)
# 
# cycles_hour <- cycles |> 
#   group_by(cycle_rounded) |>
#   summarise(size = sum(size, na.rm = T),
#             volume_L = sum(volume_L, na.rm = T))
# 
# # Se há horários que não aparece imagens adicionar 0
# df4 <- NULL
# pb <- txtProgressBar(min = 0, max = length(df3), style = 3)
# for(i in names(df3)) {
#   df4[[i]] <- dplyr::full_join(cycles_hour, df3[[i]])
#   df4[[i]][, 4][is.na(df4[[i]][, 4])] <- 0
#   
#   setTxtProgressBar(pb, i)
# }
# 
# Completando com ciclos que não existem (manutenção, falha na aquisição...)
cycles_complete <- tibble(cycle_rounded = seq(ymd_hm("2020-11-04 00:00"),
                                              ymd_hm("2022-06-10 23:59"),
                                              by = "30 min"))

df5 <- NULL
pb <- txtProgressBar(min = 0, max = length(split_df), style = 3)
for(i in names(split_df)) {
  df5[[i]] <- dplyr::full_join(split_df[[i]], cycles_complete)

  setTxtProgressBar(pb, i)
}


# Save data frames
path_to_save <- here("data", "processed", "zooplancton")
for(s in names(df5)) {
  fwrite(df5[[s]], here(path_to_save, paste0(s, ".csv")))
  
}


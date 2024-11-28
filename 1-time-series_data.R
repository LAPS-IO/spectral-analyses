# RAW Time-Series plot

# Ler *.csv's de fitoplâncton para séries temporais

# Packages
# Loading packages used run the analysis
library(here)
source(here("0-library.R"))

# path of files of interest
path <- here("data", "raw", "fitoplancton")

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
             cycle_rounded = round_date(cycle_rounded, "1 hour")) %>%
    summarize(n = n() %>% as.integer()) |>
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
    dplyr::filter(between(cycle_rounded,
                          ymd_hms("2020-11-04 00:00:00"),
                          ymd_hms("2021-12-31 23:59:59"))) |>
    droplevels()

  setTxtProgressBar(pb, i)
  
}

# Save data frames
path_to_save <- here("data", "processed", "fitoplancton")
for(s in names(df6)) {
  fwrite(df6[[s]], here(path_to_save, paste0(s, ".csv")))
  
}


# PACKAGES E CONFIG
source(here("scripts", "general", "0-library.R"))
source(here("scripts", "environmental", "2-time-series_plot.R"))
rm(list = ls()); gc()

# PARAMETRO: escolha da janela
# Opções de janela espectral disponíveis:
# - "none"    → sem janela
# - "hanning" → janela Hanning (comum para suavização moderada)
# - "hamming" → janela Hamming (melhor atenuação de lobos laterais)
# - "blackman" → janela Blackman (máxima suavização, menor resolução)
# - "tukey"   → janela Tukey (requer parâmetro alpha, opcional)
# Mais janelas podem ser adicionadas conforme necessário
apply_window <- FALSE
window_type <- "none"

# Reading and adjusting data
path <- here("data", "processed", "environ", "1 - data raw")
files <- list.files(path, pattern = ".csv", full.names = TRUE)
datetime_col <- function(dt) {
  nome1 <- names(dt)[1]
  dt[, (nome1) := lubridate::dmy_hm(get(nome1), tz = "America/Bahia")]
  return(dt)
}
files_list <- lapply(files, function(x) datetime_col(data.table::fread(x)))
names(files_list) <- sapply(files_list, function(dt) names(dt)[2])

<<<<<<< HEAD
# Loop por todos os dataframes da lista
for (nome in names(files_list)) {
  
  df <- as.data.table(files_list[[nome]])     # garante data.table
  var_nome <- names(df)[2]                    # nome da 2ª coluna
  
  # agrupa por hora mantendo o nome original da variável
  df_agrupado <- df[
    , setNames(list(mean(get(var_nome), na.rm = TRUE)), var_nome),
    by = .(datetime = round_date(datetime, "1 hour"))
  ]
  
  # substitui o objeto dentro da própria lista
  files_list[[nome]] <- df_agrupado
}

#####
=======
>>>>>>> fc9c083e9a513e94ce0c624be017bc851b2432d9
# Lists
data_fft <- list()
data_filtered <- list()
monthly_data_filtered <- list()
hourly_data_filtered <- list()

# Band filter function
filter_band <- function(complex_vector, period, min_p = -Inf, max_p = Inf) {
<<<<<<< HEAD
  mask <- period > min_p & period <= max_p
=======
  mask <- period >= min_p & period < max_p
>>>>>>> fc9c083e9a513e94ce0c624be017bc851b2432d9
  complex_vector[!mask] <- 0 + 0i
  return(complex_vector)
}

# Main loop
for (f in names(files_list)) {
  df <- files_list[[f]]
  
  # Imputation
  df[[2]] <- zoo::na.approx(df[[2]], x = df$datetime, rule = 2, na.rm = FALSE)
  df[[2]] <- zoo::na.locf(df[[2]], fromLast = FALSE, na.rm = FALSE)
  df[[2]] <- zoo::na.locf(df[[2]], fromLast = TRUE, na.rm = FALSE)
  
  a0 <- mean(df[[2]], na.rm = TRUE)
  N <- length(df[[2]])
  Zt <- df[[2]] - a0
  
  # Apply window
  if (apply_window && window_type == "hanning") {
    window_vec <- 0.5 - 0.5 * cos(2 * pi * (0:(N-1)) / (N-1))
    Zt <- Zt * window_vec
  }
  
  delta_t <- as.numeric(difftime(df$datetime[2], df$datetime[1], units = "hours"))
  if (is.na(delta_t) || delta_t <= 0) stop("Delta_t inválido")
  
  spectrum_complex <- fft(Zt) / sqrt(N)
  spectrum_power <- Mod(spectrum_complex)^2
  freq_hz <- (0:(N - 1)) / (N * delta_t)
  period_h <- ifelse(freq_hz < 1e-10, Inf, 1 / freq_hz)
  
  data_fft_temp <- tibble(index = 1:N,
                          frequency_hz = freq_hz,
                          period_hours = period_h,
                          spectrum_power = spectrum_power,
                          spectrum_complex = spectrum_complex)
  
  # Frequency band filters
  BF1_complex <- filter_band(spectrum_complex, period_h, max_p = 53)
  BF2_complex <- filter_band(spectrum_complex, period_h, min_p = 53, max_p = 13*24)
  BF3_complex <- filter_band(spectrum_complex, period_h, min_p = 13*24, max_p = 15*24)
  BF4_complex <- filter_band(spectrum_complex, period_h, min_p = 15*24)
  
  # Rebuild time-series by frequency band
  BF1_series <- Re(fft(BF1_complex, inverse = TRUE)) / sqrt(N)
  BF2_series <- Re(fft(BF2_complex, inverse = TRUE)) / sqrt(N)
  BF3_series <- Re(fft(BF3_complex, inverse = TRUE)) / sqrt(N)
  BF4_series <- Re(fft(BF4_complex, inverse = TRUE)) / sqrt(N)
  
  data_fft_temp <- data_fft_temp %>%
    mutate(BF1_power = Mod(BF1_complex)^2,
           BF2_power = Mod(BF2_complex)^2,
           BF3_power = Mod(BF3_complex)^2,
           BF4_power = Mod(BF4_complex)^2)
  
  data_fft[[f]] <- data_fft_temp
  
  df_bands <- tibble(cycle = df$datetime,
                     mes = month(df$datetime),
                     data = df[[2]],
                     BF1 = BF1_series,
                     BF2 = BF2_series,
                     BF3 = BF3_series,
                     BF4 = BF4_series)
  data_filtered[[f]] <- df_bands
  
  monthly_data_filtered[[f]] <- df_bands %>%
    group_by(mes) %>%
    summarise(across(.cols = c(data, BF1, BF2, BF3, BF4),
                     .fns = list(m = ~mean(.x, na.rm = TRUE),
                                 dp = ~ifelse(length(.x) >= 2, sd(.x, na.rm = TRUE), NA_real_)),
                     .names = "{.col}_{.fn}"), .groups = "drop")
  
  hourly_data_filtered[[f]] <- df_bands %>%
    mutate(hora = hour(cycle)) %>%
    group_by(mes, hora) %>%
    summarise(across(.cols = c(data, BF1, BF2, BF3, BF4),
                     .fns = list(m = ~mean(.x, na.rm = TRUE),
                                 dp = ~ifelse(length(.x) >= 2, sd(.x, na.rm = TRUE), NA_real_)),
                     .names = "{.col}_{.fn}"), .groups = "drop")
  
  cat(paste0("Concluído: ", f, "\n"))
}

# Saving data
path_fft <- here("data", "processed", "environ", "2 - data fft")
dir.create(path_fft, recursive = TRUE, showWarnings = FALSE)
walk2(data_fft, names(data_fft), ~fwrite(.x, file.path(path_fft, paste0(.y, ".csv"))))

path_filt <- here("data", "processed", "environ", "3 - data rebuilded")
dir.create(path_filt, recursive = TRUE, showWarnings = FALSE)
walk2(data_filtered, names(data_filtered), ~fwrite(.x, file.path(path_filt, paste0(.y, ".csv"))))

# PLOTS
<<<<<<< HEAD
rotulos <- c(
  airtemp.C_inmet = "Air temperature (°C)",
  CDOM.ppb_simcosta = "CDOM (ppb)",
  chl.ugL_simcosta = "Chlorophyll (µg L⁻¹)",
  DO.mlL_simcosta = "DO (mL L⁻¹)",
  eastwestcurrentvelocity.ms_simcosta = "E-W current (m s⁻¹)",
  eastwestwindvelocity.ms_simcosta = "E-W wind (m s⁻¹)",
  northsouthcurrentvelocity.ms_simcosta = "N-S current (m s⁻¹)",
  northsouthwindvelocity.ms_simcosta = "N-S wind (m s⁻¹)",
  rain.mm_inmet = "Rainfall (mm\u00B3)",
  riverdischarge.m3s_votorantim = "River discharge (m³ s⁻¹)",
  sal_simcosta = "Salinity",
  solrad.Kjm2_inmet = "Solar radiation (kJ m⁻²)",
  subtidalelevation.m_marinha   = "Subtidal elevation (m)",
  tidalelevation.m_marinha = "Tidal elevation (m)",
  totsealev.m_marinha = "Total sea level (m)",
  turb.ntu_simcosta = "Turbidity (NTU)",
  wattemp.C_simcosta = "Water temperature (°C)")
# Rebuideld plot V1
rebuilded_plot <- function(df_list = data_filtered, 
                           titles = names(df_list), 
                           output_dir = here("images", 
                                             "environ", 
                                             "3 - rebuilded_series_by_BF")) {
=======
rebuilded_plot <- function(df_list = data_filtered, 
                           titles = names(df_list), 
                           output_dir = here("images", "environ", "3 - rebuilded_series_by_BF")) {
>>>>>>> fc9c083e9a513e94ce0c624be017bc851b2432d9
  
  dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
  
  for (p in seq_along(df_list)) {
    
    data_long <- df_list[[p]][-1, ] %>%
      pivot_longer(cols = -c(cycle, mes), 
                   names_to = "bands_series", 
                   values_to = "bands_values")
    
    data_rib <- data_long %>%
      group_by(bands_series) %>%
      summarise(q25 = quantile(bands_values, 0.25, na.rm = TRUE),
                q75 = quantile(bands_values, 0.75, na.rm = TRUE),
                desv_min = q25 - 1.5 * IQR(bands_values, na.rm = TRUE),
                desv_max = q75 + 1.5 * IQR(bands_values, na.rm = TRUE), .groups = "drop")
    
    media_df <- data_long %>%
      group_by(bands_series) %>%
      summarise(media = mean(bands_values, na.rm = TRUE), .groups = "drop")
    
    plot_data <- left_join(data_long, data_rib, by = "bands_series")
    
    fig <- ggplot(plot_data, aes(x = cycle)) +
      geom_line(aes(y = bands_values), color = "darkblue") +
      geom_ribbon(aes(ymin = desv_min, ymax = desv_max), 
                  fill = "grey90", alpha = 0.5) +
      geom_hline(data = media_df, aes(yintercept = media), 
                 color = "red", linewidth = 0.2, inherit.aes = FALSE) +
      facet_wrap(~ factor(bands_series,
                          levels = c("data", "BF1", "BF2", "BF3", "BF4"),
                          labels = c("Original time-series",
<<<<<<< HEAD
                                     "frequency <= 53 hours",
                                     "53 hours < frequency <= 13 days",
                                     "13 days < frequency <= 15 days",
                                     "15 days < frequency")),
=======
                                     "BF1: frequency <= 53h",
                                     "BF2: 53h < frequency <= 13 dias",
                                     "BF3: 13 dias < frequency <= 15 dias",
                                     "BF4: frequency > 15 dias")),
>>>>>>> fc9c083e9a513e94ce0c624be017bc851b2432d9
                 scales = "free_y", nrow = 5) +
      scale_x_datetime(breaks = "1 month", date_labels = "%Y-%m") +
      labs(x = "Date", y = "Value", title = titles[p]) +
      theme_light() +
      theme(axis.text.x = element_text(size = 13, face = "bold", 
                                       angle = 90, color = "black", 
                                       family = "Times New Roman"),
            axis.text.y = element_text(size = 13, face = "bold", 
                                       color = "black", 
                                       family = "Times New Roman"),
            axis.title.x = element_text(size = 22, face = "bold", 
                                        family = "Times New Roman"),
            axis.title.y = element_text(size = 20, face = "bold", 
                                        family = "Times New Roman"),
            plot.title = element_text(size = 20, face = "bold", 
                                      family = "Times New Roman"),
            strip.text = element_text(size = 11, face = "bold", 
                                      color = "black", 
                                      family = "Times New Roman"))
    
<<<<<<< HEAD
    ggsave(filename = here(output_dir, 
                           paste0("rebuild_byband_", 
                                  names(df_list)[p], ".png")), 
=======
    ggsave(filename = here(output_dir, paste0("rebuild_byband_", names(df_list)[p], ".png")), 
>>>>>>> fc9c083e9a513e94ce0c624be017bc851b2432d9
           plot = fig, width = 15, height = 7, dpi = "retina")
    print(fig)
    
  }
}
<<<<<<< HEAD
rebuilded_plot()

# Rebuideld plot V2
rebuilded_plot <- function(
    df_list,                                # sem valor-padrão recursivo
    rotulos = rotulos_default,
    output_dir = here("images", "environ", "3 - rebuilded_series_by_BF")) {
  
  # exige que o usuário passe algo em df_list
  if (missing(df_list)) {
    stop("Você deve fornecer 'df_list', por exemplo: rebuilded_plot(my_list_of_dfs)")
  }
  
  dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
  df_names <- names(df_list)
  
  for (p in seq_along(df_list)) {
    df_name     <- df_names[p]
    # procura o rótulo correspondente; se não achar, usa o próprio nome
    title_label <- if (!is.na(rotulos[df_name])) rotulos[df_name] else df_name
    
    # organiza os dados em formato long, calcula quartis, média etc.
    data_long <- df_list[[p]][-1, ] %>%
      pivot_longer(
        cols = -c(cycle, mes),
        names_to  = "bands_series",
        values_to = "bands_values"
      )
    
    data_rib <- data_long %>%
      group_by(bands_series) %>%
      summarise(
        q25      = quantile(bands_values, 0.25, na.rm = TRUE),
        q75      = quantile(bands_values, 0.75, na.rm = TRUE),
        desv_min = q25 - 1.5 * IQR(bands_values, na.rm = TRUE),
        desv_max = q75 + 1.5 * IQR(bands_values, na.rm = TRUE),
        .groups  = "drop"
      )
    
    media_df <- data_long %>%
      group_by(bands_series) %>%
      summarise(media = mean(bands_values, na.rm = TRUE), .groups = "drop")
    
    plot_data <- left_join(data_long, data_rib, by = "bands_series")
    
    fig <- ggplot(plot_data, aes(x = cycle)) +
      geom_line(aes(y = bands_values), color = "darkblue") +
      geom_ribbon(aes(ymin = desv_min, ymax = desv_max),
                  fill = "grey90", alpha = 0.5) +
      geom_hline(data = media_df, aes(yintercept = media),
                 color = "red", linewidth = 0.2, inherit.aes = FALSE) +
      facet_wrap(~ factor(bands_series,
                          levels = c("data","BF1","BF2","BF3","BF4"),
                          labels = c("Original time-series",
                                     "frequency <= 53 hours",
                                     "53 hours < frequency <= 13 days",
                                     "13 days < frequency <= 15 days",
                                     "15 days < frequency")),
                 scales = "free_y", nrow = 5) +
      scale_x_datetime(breaks = "1 month", date_labels = "%Y-%m") +
      labs(x = "Date", y = "Value", title = title_label) +
      theme_light() +
      theme(
        axis.text.x  = element_text(size = 13, face = "bold", angle = 90,
                                    color = "black", family = "Times New Roman"),
        axis.text.y  = element_text(size = 13, face = "bold",
                                    color = "black", family = "Times New Roman"),
        axis.title.x = element_text(size = 22, face = "bold",
                                    family = "Times New Roman"),
        axis.title.y = element_text(size = 20, face = "bold",
                                    family = "Times New Roman"),
        plot.title   = element_text(size = 20, face = "bold",
                                    family = "Times New Roman"),
        strip.text   = element_text(size = 11, face = "bold",
                                    color = "black",
                                    family = "Times New Roman")
      )
    
    ggsave(
      filename = here(output_dir,
                      paste0("rebuild_byband_", df_name, ".png")),
      plot     = fig,
      width    = 15, height = 7, dpi = "retina"
    )
    print(fig)
  }
}
rebuilded_plot(data_filtered,
               rotulos = rotulos,
               output_dir = here("images", "environ", "3 - rebuilded_series_by_BF"))

# Spectral plot V1
spectral_plot <- function(df_list = data_fft, 
                          titles = names(df_list), 
                          output_dir = here("images", 
                                            "environ", 
                                            "2 - freq_filters")) {
=======

spectral_plot <- function(df_list = data_fft, 
                          titles = names(df_list), 
                          output_dir = here("images", "environ", "2 - freq_filters")) {
>>>>>>> fc9c083e9a513e94ce0c624be017bc851b2432d9
  
  dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
  
  for (p in seq_along(df_list)) {
    
    df <- df_list[[p]][-1, ]
    N <- nrow(df)
    max_pos <- floor(N / 2) + 1
    df <- df[1:max_pos, ]
    
    data_long <- df %>%
      select(frequency_hz, period_hours, 
             BF1_power, BF2_power, 
             BF3_power, BF4_power) %>%
      pivot_longer(cols = starts_with("BF"), 
                   names_to = "bands_series", 
                   values_to = "bands_values") %>%
<<<<<<< HEAD
      filter((bands_series == "BF1_power" & period_hours <= 53) |
               (bands_series == "BF2_power" & period_hours > 53 & period_hours <= 13*24) |
               (bands_series == "BF3_power" & period_hours > 13*24 & period_hours <= 15*24) |
               (bands_series == "BF4_power" & period_hours > 15*24))
=======
      filter((bands_series == "BF1_power" & period_hours < 53) |
               (bands_series == "BF2_power" & period_hours >= 53 & period_hours < 13*24) |
               (bands_series == "BF3_power" & period_hours >= 13*24 & period_hours < 15*24) |
               (bands_series == "BF4_power" & period_hours >= 15*24))
>>>>>>> fc9c083e9a513e94ce0c624be017bc851b2432d9
    
    fig <- ggplot(data_long) +
      geom_line(aes(x = period_hours, y = bands_values), color = "darkblue") +
      scale_x_continuous(n.breaks = 15) +
      facet_wrap(~ factor(bands_series,
<<<<<<< HEAD
                          levels = c("BF1_power", 
                                     "BF2_power", 
                                     "BF3_power", 
                                     "BF4_power"),
                          labels = c("frequency <= 53 hours",
                                     "53 hours < frequency <= 13 days",
                                     "13 days < frequency <= 15 days",
                                     "15 days < frequency")),
=======
                          levels = c("BF1_power", "BF2_power", "BF3_power", "BF4_power"),
                          labels = c("BF1: frequency <= 53h",
                                     "BF2: 53h < frequency <= 13 dias",
                                     "BF3: 13 dias < frequency <= 15 dias",
                                     "BF4: frequency > 15 dias")),
>>>>>>> fc9c083e9a513e94ce0c624be017bc851b2432d9
                 scales = "free", nrow = 5) +
      labs(x = "Hour", y = "Spectrum", title = titles[p]) +
      theme_light() +
      theme(axis.text.x = element_text(size = 13, face = "bold", 
                                       family = "Times New Roman", 
                                       color = "black"),
            axis.text.y = element_text(size = 13, face = "bold", 
                                       family = "Times New Roman", 
                                       color = "black"),
            axis.title.x = element_text(size = 22, face = "bold", 
                                        family = "Times New Roman", 
                                        color = "black"),
            axis.title.y = element_text(size = 20, face = "bold", 
                                        family = "Times New Roman", 
                                        color = "black"),
            title = element_text(size = 20, face = "bold", 
                                 family = "Times New Roman", 
                                 color = "black"),
            strip.text = element_text(size = 11, face = "bold", 
                                      family = "Times New Roman", 
                                      color = "black"))
    
<<<<<<< HEAD
    ggsave(filename = here(output_dir, 
                           paste0("spectrum_byband_", 
                                  names(df_list)[p], ".png")), 
=======
    ggsave(filename = here(output_dir, paste0("spectrum_byband_", names(df_list)[p], ".png")), 
>>>>>>> fc9c083e9a513e94ce0c624be017bc851b2432d9
           plot = fig, width = 15, height = 7, dpi = "retina")
    print(fig)
    
  }
}
<<<<<<< HEAD
spectral_plot()

# Spectral plot V2
spectral_plot <- function(
    df_list,                               # sem dependência recursiva
    rotulos    = rotulos_default,
    output_dir = here("images", "environ", "2 - freq_filters")
) {
  if (missing(df_list)) {
    stop("Forneça a lista de data.frames em 'df_list'.")
  }
  
  dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
  df_names <- names(df_list)
  
  for (p in seq_along(df_list)) {
    
    df_name     <- df_names[p]
    title_label <- if (!is.na(rotulos[df_name])) rotulos[df_name] else df_name
    
    df  <- df_list[[p]][-1, ]             # descarta primeira linha
    N   <- nrow(df)
    max_pos <- floor(N / 2) + 1           # metade + 1 (Nyquist)
    df  <- df[1:max_pos, ]
    
    # reorganiza para long + filtra as bandas válidas
    data_long <- df %>% 
      select(frequency_hz, period_hours,
             BF1_power, BF2_power, BF3_power, BF4_power) %>% 
      pivot_longer(cols  = starts_with("BF"),
                   names_to  = "bands_series",
                   values_to = "bands_values") %>% 
      filter((bands_series == "BF1_power" & period_hours <= 53) |
               (bands_series == "BF2_power" & period_hours > 53    & period_hours <= 13*24) |
               (bands_series == "BF3_power" & period_hours > 13*24 & period_hours <= 15*24) |
               (bands_series == "BF4_power" & period_hours > 15*24))
    
    # gráfico
    fig <- ggplot(data_long) +
      geom_line(aes(period_hours, bands_values), color = "darkblue") +
      scale_x_continuous(n.breaks = 15) +
      facet_wrap(~ factor(bands_series,
                          levels = c("BF1_power","BF2_power","BF3_power","BF4_power"),
                          labels = c("frequency <= 53 hours",
                                     "53 hours < frequency <= 13 days",
                                     "13 days < frequency <= 15 days",
                                     "15 days < frequency")),
                 scales = "free", nrow = 5) +
      labs(x = "Hour", y = "Spectrum", title = title_label) +
      theme_light() +
      theme(
        axis.text.x  = element_text(size = 13, face = "bold",
                                    family = "Times New Roman", color = "black"),
        axis.text.y  = element_text(size = 13, face = "bold",
                                    family = "Times New Roman", color = "black"),
        axis.title.x = element_text(size = 22, face = "bold",
                                    family = "Times New Roman", color = "black"),
        axis.title.y = element_text(size = 20, face = "bold",
                                    family = "Times New Roman", color = "black"),
        plot.title   = element_text(size = 20, face = "bold",
                                    family = "Times New Roman", color = "black"),
        strip.text   = element_text(size = 11, face = "bold",
                                    family = "Times New Roman", color = "black")
      )
    
    ggsave(
      filename = here(output_dir,
                      paste0("spectrum_byband_", df_name, ".png")),
      plot   = fig,
      width  = 15, height = 7, dpi = "retina"
    )
    print(fig)
  }
}
spectral_plot(data_fft, 
              rotulos = rotulos, 
              output_dir = here("images", 
                                "environ", 
                                "2 - freq_filters"))


=======

# EXECUTA PLOTS
rebuilded_plot()
spectral_plot()
>>>>>>> fc9c083e9a513e94ce0c624be017bc851b2432d9

# PACKAGES E CONFIG
source(here("scripts", "general", "0-library.R"))
source(here("scripts", "zooplancton", "2-time-series_plot.R"))
rm(list = ls()); gc()

# PARAMETRO: escolha da janela
# Opções de janela espectral disponíveis:
# - "none"    → sem janela
# - "hanning" → janela Hanning (comum para suavização moderada)
# - "hamming" → janela Hamming (melhor atenuação de lobos laterais)
# - "blackman" → janela Blackman (máxima suavização, menor resolução)
# - "tukey"   → janela Tukey (requer parâmetro alpha, opcional)
# Mais janelas podem ser adicionadas conforme necessário
apply_window <- TRUE
window_type <- "none"

# Reading and adjusting data
library(here)
path <- here("data", "processed", "zooplancton")
<<<<<<< HEAD
# path <- here("data", "processed", "environ", "3 - data rebuilded")
=======
>>>>>>> fc9c083e9a513e94ce0c624be017bc851b2432d9
files <- list.files(path, pattern = ".csv", full.names = TRUE)[3]
datetime_col <- function(dt) {
  nome1 <- names(dt)[1]
  dt[, (nome1) := lubridate::ymd_hms(get(nome1), tz = "America/Bahia")]
  return(dt)
}
files_list <- lapply(files, function(x) datetime_col(data.table::fread(x)))
names(files_list) <- sapply(files_list, function(dt) dt[1, 2])
<<<<<<< HEAD
# names(files_list) <- "tot"
# files_list$tot <- rename(files_list$tot, "cycle_rounded" = cycle)
=======

>>>>>>> fc9c083e9a513e94ce0c624be017bc851b2432d9
#####
densidade_por_hora <- list()

# Loop por todos os dataframes da lista
for (nome in names(files_list)) {
  
  df <- files_list[[nome]]
  
  # Agrupamento por hora, somando N_rois e vol_sampled_L
  df_agrupado <- df[, .(
    total_rois = sum(N_rois, na.rm = TRUE),
    total_volume = sum(vol_sampled_L, na.rm = TRUE)
<<<<<<< HEAD
  ), by = .(cycle_rounded = lubridate::round_date(cycle_rounded, "1 hour")) ]
=======
  ), by = cycle_rounded]
>>>>>>> fc9c083e9a513e94ce0c624be017bc851b2432d9
  
  # Calcula densidade
  df_agrupado[, density := total_rois / total_volume]
  
  # Mantém apenas as colunas requisitadas
  df_resultado <- df_agrupado[, .(cycle_rounded, density)]
  
  # Armazena o resultado na lista com o mesmo nome
  densidade_por_hora[[nome]] <- df_resultado
}
<<<<<<< HEAD

=======
>>>>>>> fc9c083e9a513e94ce0c624be017bc851b2432d9
#####
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
for (f in names(densidade_por_hora)) {
  df <- densidade_por_hora[[f]]
  df <- df %>% mutate(across(2, ~ ifelse(is.nan(.), NA, .)))
  
  # Imputation
  df[[2]] <- zoo::na.approx(df[[2]], x = df$cycle_rounded, rule = 2, na.rm = FALSE)
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
  
  delta_t <- as.numeric(difftime(df$cycle_rounded[2], 
                                 df$cycle_rounded[1], units = "hours"))
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
  BF1_complex <- filter_band(spectrum_complex, 
                             period_h, max_p = 53)
  BF2_complex <- filter_band(spectrum_complex, 
                             period_h, min_p = 53, max_p = 13*24)
  BF3_complex <- filter_band(spectrum_complex, 
                             period_h, min_p = 13*24, max_p = 15*24)
  BF4_complex <- filter_band(spectrum_complex, 
                             period_h, min_p = 15*24)
  
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
  
  df_bands <- tibble(cycle = df$cycle_rounded,
                     mes = month(df$cycle_rounded),
                     data = df[[2]],
                     BF1 = BF1_series,
                     BF2 = BF2_series,
                     BF3 = BF3_series,
                     BF4 = BF4_series)
  data_filtered[[f]] <- df_bands
  
  monthly_data_filtered[[f]] <- df_bands %>%
<<<<<<< HEAD
    # mutate(ano = year(cycle)) %>%
=======
>>>>>>> fc9c083e9a513e94ce0c624be017bc851b2432d9
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
path_fft <- here("data", "processed", "zooplancton", "2 - data fft")
dir.create(path_fft, recursive = TRUE, showWarnings = FALSE)
walk2(data_fft, names(data_fft), ~fwrite(.x, file.path(path_fft, paste0(.y, ".csv"))))

path_filt <- here("data", "processed", "zooplancton", "3 - data rebuilded")
dir.create(path_filt, recursive = TRUE, showWarnings = FALSE)
<<<<<<< HEAD
walk2(data_filtered, names(data_filtered), 
      ~fwrite(.x, file.path(path_filt, paste0(.y, ".csv"))))

# PLOTS
# Rebuilded plot V1
=======
walk2(data_filtered, names(data_filtered), ~fwrite(.x, file.path(path_filt, paste0(.y, ".csv"))))

# PLOTS
>>>>>>> fc9c083e9a513e94ce0c624be017bc851b2432d9
rebuilded_plot <- function(df_list = data_filtered, 
                           titles = names(df_list), 
                           output_dir = here("images", "zooplancton", "3 - rebuilded_series_by_BF")) {
  
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
                 scales = "free_y", nrow = 5) +
      scale_x_datetime(breaks = "1 month", date_labels = "%Y-%m") +
      labs(x = "Date", y = "Density (ind./L)", title = titles[p]) +
=======
                                     "BF1: frequency <= 53h",
                                     "BF2: 53h < frequency <= 13 dias",
                                     "BF3: 13 dias < frequency <= 15 dias",
                                     "BF4: frequency > 15 dias")),
                 scales = "free_y", nrow = 5) +
      scale_x_datetime(breaks = "1 month", date_labels = "%Y-%m") +
      labs(x = "Date", y = "Value", title = titles[p]) +
>>>>>>> fc9c083e9a513e94ce0c624be017bc851b2432d9
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
    
    ggsave(filename = here(output_dir, paste0("rebuild_byband_", names(df_list)[p], ".png")), 
           plot = fig, width = 15, height = 7, dpi = "retina")
    print(fig)
    
  }
}
<<<<<<< HEAD
rebuilded_plot()

# Spectral plot V1
spectral_plot <- function(df_list = data_fft, 
                          titles = names(data_fft), 
                          output_dir = here("images", 
                                            "zooplancton", 
                                            "2 - freq_filters")) {
=======

spectral_plot <- function(df_list = data_fft, 
                          titles = names(df_list), 
                          output_dir = here("images", "zooplancton", "2 - freq_filters")) {
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
                          levels = c("BF1_power", "BF2_power", "BF3_power", "BF4_power"),
<<<<<<< HEAD
                          labels = c("frequency <= 53 hours",
                                     "53 hours < frequency <= 13 days",
                                     "13 days < frequency <= 15 days",
                                     "15 days < frequency")),
=======
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
    ggsave(filename = here(output_dir, paste0("spectrum_byband_",
                                              names(df_list)[p], ".png")),
=======
    ggsave(filename = here(output_dir, paste0("spectrum_byband_", 
                                              names(df_list)[p], ".png")), 
>>>>>>> fc9c083e9a513e94ce0c624be017bc851b2432d9
           plot = fig, width = 15, height = 7, dpi = "retina")
    print(fig)
    
  }
}
<<<<<<< HEAD
spectral_plot()

# Spectral plot V2
spectral_plot <- function(
    df_list,                               # lista de data.frames (um por série)
    titles      = names(df_list),          # títulos p/ cada gráfico
    smooth_kern = list(                    # <- edite aqui seu kernel/BF
      BF1 = list(type = "daniell",          m = 2),
      BF2 = list(type = "modified.daniell", m = 3),
      BF3 = list(type = "hamming",          m = 10),
      BF4 = list(type = "daniell",          m = 20)
    ),
    output_dir  = here::here("images", "zooplancton", "2 - freq_filters")) {
  # ─── pacotes ──────────────────────────────────────────────────
  requireNamespace("dplyr")
  requireNamespace("tidyr")
  requireNamespace("ggplot2")
  requireNamespace("stats")
  requireNamespace("here")
  
  dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
  
  # helper → devolve vetor de pesos normalizado
  build_kernel <- function(type, m) stats::kernel(type, m)$coef
  
  # ─── loop sobre cada periodograma da lista ───────────────────
  for (p in seq_along(df_list)) {
    
    df <- df_list[[p]]
    
    # 1. remove 0 Hz (primeira linha) se existir
    if (df$frequency_hz[1] == 0) df <- df[-1, ]
    
    # 2. mantém só a metade positiva do espectro
    df <- df[seq_len(floor(nrow(df)/2) + 1), ]
    
    # 3. aplica kernel específico de cada banda -----------------
    pow_cols <- grep("^BF\\d+_power$", names(df), value = TRUE)
    
    for (col in pow_cols) {
      band  <- sub("_power$", "", col)               # eg. "BF3"
      kinfo <- smooth_kern[[band]]
      
      if (is.null(kinfo))
        stop("Não há kernel definido para a banda ", band, ".")
      
      w <- build_kernel(kinfo$type, kinfo$m)
      
      # convolução centrada; bordas ficam NA (stats::filter)
      df[[paste0(col, "_sm")]] <- stats::filter(df[[col]],
                                                filter   = w,
                                                sides    = 2,
                                                circular = FALSE)
    }
    
    # 4. organiza para o ggplot ---------------------------------
    data_long <- df |>
      dplyr::select(frequency_hz, period_hours,
                    tidyselect::ends_with("_sm")) |>
      tidyr::pivot_longer(cols      = tidyselect::ends_with("_sm"),
                          names_to  = "bands_series",
                          values_to = "bands_values") |>
      dplyr::mutate(bands_series = sub("_sm$", "", bands_series)) |>
      dplyr::filter(
        (bands_series == "BF1_power" & period_hours <= 53) |
          (bands_series == "BF2_power" & period_hours > 53    & period_hours <= 13*24) |
          (bands_series == "BF3_power" & period_hours > 13*24 & period_hours <= 15*24) |
          (bands_series == "BF4_power" & period_hours > 15*24)
      )
    
    # 5. plota ---------------------------------------------------
    fig <- ggplot2::ggplot(data_long) +
      ggplot2::geom_line(ggplot2::aes(period_hours, bands_values),
                         colour = "darkblue") +
      ggplot2::scale_x_continuous(n.breaks = 15) +
      ggplot2::facet_wrap(
        ~ factor(bands_series,
                 levels = c("BF1_power", "BF2_power", "BF3_power", "BF4_power"),
                 labels = c("frequency ≤ 53 hours",
                            "53 hours < frequency ≤ 13 days",
                            "13 days < frequency ≤ 15 days",
                            "15 days < frequency")),
        scales = "free", nrow = 5
      ) +
      ggplot2::labs(x = "Hour",
                    y = "Smoothed spectrum",
                    title = titles[p]) +
      ggplot2::theme_light() +
      ggplot2::theme(
        axis.text   = ggplot2::element_text(size = 13, face = "bold",
                                            family = "Times New Roman",
                                            colour = "black"),
        axis.title  = ggplot2::element_text(size = 21, face = "bold",
                                            family = "Times New Roman",
                                            colour = "black"),
        title       = ggplot2::element_text(size = 20, face = "bold",
                                            family = "Times New Roman",
                                            colour = "black"),
        strip.text  = ggplot2::element_text(size = 11, face = "bold",
                                            family = "Times New Roman",
                                            colour = "black")
      )
    
    # 6. salva opcionalmente em disco ---------------------------
    ggplot2::ggsave(
      here::here(output_dir,
                 paste0("spectrum_byband_", names(df_list)[p], ".png")),
      fig, width = 15, height = 7, dpi = 300
    )
    
    print(fig)
  }
}
spectral_plot(df_list = data_fft,
              smooth_kern = list(
                BF1 = list(type = "daniell", m = 60),
                BF2 = list(type = "daniell", m = 5),
                BF3 = list(type = "daniell", m = 2),
                BF4 = list(type = "daniell", m = 5)))


=======

# EXECUTA PLOTS
rebuilded_plot()
spectral_plot()
>>>>>>> fc9c083e9a513e94ce0c624be017bc851b2432d9

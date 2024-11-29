#################################
# Packages
# Loading packages used run the analysis
source(here("scripts", "fitoplancton", "0-library.R"))
source(here("scripts", "fitoplancton", "2-time-series_plot.R"))
rm(list = ls())
gc()

# Get data to model
# Get data
path <- here("data", "processed", "fitoplancton")
files <- list.files(path, pattern = ".csv", 
                    full.names = T)

files_list <- lapply(files, fread)
names(files_list) <- files %>% 
  basename() %>% 
  sub(pattern = ".csv", replacement = "")

# Fill NA's
## With 0
list_fill <- lapply(files_list, function(e) {
  e[is.na(e)] <- 0
  e
})
## With the mean of the series
# list_fill <- lapply(files_list, function(e) {
#   e <- e %>% 
#     mutate(across(everything(), ~ ifelse(is.na(.), mean(., na.rm = TRUE), .)))
#   e
# })

monthly_data_filtered <- NULL
hourly_data_filtered <- NULL
data_fft <- NULL
HF_series_list <- NULL
LF_series_list <- NULL

for(f in names(files_list)) {
  
  # Select df
  df <- list_fill[[f]]
  
  # Get series mean, length and series detrended
  a0 <- mean(df$density, na.rm = T)
  N <- length(df$density)
  Zt <- df$density - a0
  
  # Get Series spectrum by FFT
  spectrum_complex <- fft(Zt) / sqrt(N) # valores do espectro na forma complexa
  spectrum <- abs(spectrum_complex)^2 # sem parte complexa
  
  # Get angular frequency
  omega <- 0:(N - 1) / N 
  
  # Build tibble with frequency, period and the real and complex spectrum
  data_fft_temp <- tibble(frequency = omega,
                          period = 1/omega,
                          spectrum = spectrum,
                          spectrum_complex = spectrum_complex)
  
  # Filter by frequency 
  # Turn 0, periods > 26h == drop low frequencies
  N_freq_cut <- length(data_fft_temp$period) - sum(data_fft_temp$period < 26, na.rm = T)
  data_fft_temp$HF <- ifelse(data_fft_temp$period > 26, 0, data_fft_temp$spectrum)
  data_fft_temp$HF[(N - N_freq_cut + 1):N] <- 0 # Turn as 0 symmetric frequencies
  data_fft_temp$HF_complex <- ifelse(data_fft_temp$period > 26, 0,
                                     data_fft_temp$spectrum_complex)
  data_fft_temp$HF_complex[(N - N_freq_cut + 1):N] <- 0 # Turn as 0 symmetric frequencies
  
  # Turn 0, periods < 26h == drop high frequencies
  data_fft_temp <- data_fft_temp %>%
    mutate(LF = dplyr::if_else(HF == 0, spectrum, 0),
           LF_complex = dplyr::if_else(HF == 0, spectrum_complex, 0))
  
  data_fft[[f]] <- data_fft_temp
  
  # Rebuild high frequencies series after filter
  HF_series <- Re(fft(data_fft_temp$HF_complex, inverse = T))
  HF_series <- HF_series / sqrt(N)
  N_HF_series <- length(HF_series)
  
  # Rebuild low frequencies series after filter
  LF_series <- Re(fft(data_fft_temp$LF_complex, inverse = T))
  LF_series <- LF_series / sqrt(N)
  N_LF_series <- length(LF_series)
  
  # Add mean to rebuilded series
  HF_series <- HF_series + a0
  mean(HF_series, na.rm = T)
  HF_series_list[[f]] <- HF_series
  
  LF_series <- LF_series + a0
  mean(LF_series, na.rm = T)
  LF_series_list[[f]] <- LF_series
  
  # Monthly Data for Time plot filtered
  monthly_data_filtered[[f]] <- tibble(cycle = df$cycle_rounded,
                                       mes = month(df$cycle_rounded),
                                       density = df$density,
                                       HF = HF_series,
                                       LF = LF_series) %>%
    dplyr::reframe(dplyr::across(c(density, HF, LF), 
                                 list(m = mean, dp = sd),
                                 .names = "{.col}_{.fn}"),
                   .by = mes)
  
  # Hourly Data for Time plot filtered
  hourly_data_filtered[[f]] <- tibble(cycle = df$cycle_rounded,
                                      mes = month(df$cycle_rounded),
                                      hora = hour(df$cycle_rounded),
                                      density = df$density,
                                      HF = HF_series,
                                      LF = LF_series) %>%
    dplyr::reframe(dplyr::across(c(density, HF, LF),
                                 list(m = mean, dp = sd),
                                 .names = "{.col}_{.fn}"),
                   .by = c(mes, hora))
  
  cat(paste(f, "\n"))
}

# LOOP to PLOT Spectrum filtered in 26h
# classes <- c("Doliolidae",
#              expression(paste(italic("Penilia avirostris"))),
# "Radiozoa")
classes <- c("Chaetoceros",
             expression(paste(italic("Guinardia dactyliosolen"), " Complex")),
             expression(paste(italic("Rhizosolenia proboscia"), " Complex")),
             expression(paste(italic("Coscinodiscus wailesii"))),
             expression(paste(italic("Guinardia striata"))),
             "Hemidiscus",
             expression(paste(italic("Rhizosolenia robusta"))))

output_dir_figs <- here("images", "fitoplancton", "2-freq_filters")

spectral_plot <- function(df_list = data_fft, titles = classes) {
  
  for(p in seq_along(names(df_list))) {
    
    data <- df_list[[p]][-1, ]
    
    fig <- ggplot(data) +
      geom_line(aes(x = frequency, 
                    y = spectrum, 
                    color = ifelse(period < 26, 
                                   "High Frequency", 
                                   "Low Frequency"))) +
      xlim(0, .5) +
      labs(x = "Frequency (cycles/hour)", 
           y = "Spectrum", 
           title = titles[p]) +
      scale_color_manual(name = "Filters", 
                         values = c("Low Frequency" = "darkblue",
                                    "High Frequency" = "red")) +
      theme_light() +
      theme(axis.text.x = element_text(size = 16, face = "bold", 
                                       family = "Times New Roman",
                                       color = "black"),
            axis.text.y = element_text(size = 16, face = "bold",
                                       family = "Times New Roman",
                                       color = "black"),
            legend.text = element_text(size = 20, face = "bold",
                                       family = "Times New Roman",
                                       color = "black"),
            axis.title.x = element_text(size = 22, face = "bold",
                                        family = "Times New Roman",
                                        color = "black"),
            axis.title.y = element_text(size = 20, face = "bold",
                                        family = "Times New Roman",
                                        color = "black"),
            legend.title = element_text(size = 22, face = "bold",
                                        family = "Times New Roman",
                                        color = "black"),
            title = element_text(size = 20, face = "bold", 
                                 family = "Times New Roman", 
                                 color = "black"))
    
    ggsave(filename = here(output_dir_figs, 
                           paste0("freqfilter_", names(df_list)[p], 
                                  ".png")), 
           plot = fig,
           width = 15, height = 7, 
           dpi = "retina")
    
    print(fig)
    
    cat(paste(names(df_list)[p], "\n"))
    
  }
  
}
spectral_plot()

# Plot densities of low and high frequencies by month
output_dir_figs <- here("images", "fitoplancton", "3-freq_monthly_filtered_plots")

for(p in seq_along(names(monthly_data_filtered))) {
  
  data <- monthly_data_filtered[[p]]
  
  fig <- ggplot(data, aes(x = mes)) +
    geom_point(aes(y = density_m, color = "Density")) +
    geom_line(aes(y = density_m, color = "Density")) +
    geom_point(aes(y = HF_m, color = "High Frequency")) +
    geom_line(aes(y = HF_m, color = "High Frequency")) +
    geom_point(aes(y = LF_m, color = "Low Frequency")) +
    geom_line(aes(y = LF_m, color = "Low Frequency")) +
    labs(title = classes[p],
         x = "Month", y = "Mean of Density (ind./L)") +
    scale_color_manual(name = "Filters", 
                       values = c("Density" = "black", 
                                  "High Frequency" = "red", 
                                  "Low Frequency" = "blue")) +
    scale_x_continuous(n.breaks = 12, minor_breaks = NULL) +
    theme_light() +
    theme(axis.text.x = element_text(size = 16, face = "bold", 
                                     family = "Times New Roman",
                                     color = "black"),
          axis.text.y = element_text(size = 16, face = "bold",
                                     family = "Times New Roman",
                                     color = "black"),
          legend.text = element_text(size = 20, face = "bold",
                                     family = "Times New Roman",
                                     color = "black"),
          axis.title.x = element_text(size = 22, face = "bold",
                                      family = "Times New Roman",
                                      color = "black"),
          axis.title.y = element_text(size = 20, face = "bold",
                                      family = "Times New Roman",
                                      color = "black"),
          legend.title = element_text(size = 22, face = "bold",
                                      family = "Times New Roman",
                                      color = "black"),
          title = element_text(size = 20, face = "bold", 
                               family = "Times New Roman", 
                               color = "black"))
  
  ggsave(filename = here(output_dir_figs,
                         paste0("monthly_filtered_", 
                                names(monthly_data_filtered)[p],
                                ".png")),
         plot = fig,
         width = 15, height = 7,
         dpi = "retina")
  
  print(fig)
  cat(paste(names(monthly_data_filtered)[p], "\n"))
  
}

# Get months where are low and high densities by month in monthly data filtered
low_dens_month <- NULL
high_dens_month <- NULL

for(n in names(monthly_data_filtered)) {
  
  data_m <- monthly_data_filtered[[n]]
  
  low_dens_month[[n]] <- data_m$mes[data_m$LF_m < data_m$HF_m]
  high_dens_month[[n]] <- data_m$mes[data_m$LF_m >= data_m$HF_m]
  
  cat(paste("classe:", n, 
            "\n low months:", low_dens_month[n], 
            "\n high months:", high_dens_month[n], "\n"))
}

# Plot hourly densities of low and high frequencies, separated by months where the mean are above or under monthly mean in HF and save them
output_dir_figs <- here("images", "fitoplancton", "4-freq_24_filtered_plots")

for(d in seq_along(names(hourly_data_filtered))) {
  
  data_low <- hourly_data_filtered[[d]] %>% 
    dplyr::filter(factor(hourly_data_filtered[[d]]$mes) %in% 
                    factor(low_dens_month[[d]])) %>% 
    droplevels() %>% 
    dplyr::reframe(dplyr::across(c(density_m, HF_m, LF_m),
                          list(mean),
                          .names = "{.col}"),
                   .by = hora)
  
  data_high <- hourly_data_filtered[[d]] %>%  
    dplyr::filter(factor(hourly_data_filtered[[d]]$mes) %in% 
                    factor(high_dens_month[[d]])) %>% 
    droplevels() %>% 
    dplyr::reframe(dplyr::across(c(density_m, HF_m, LF_m),
                                 list(mean),
                                 .names = "{.col}"),
                   .by = hora)
  
  p_low <- ggplot(data_low, aes(x = hora)) +
    geom_point(aes(y = density_m, color = "Density")) +
    geom_line(aes(y = density_m, color = "Density")) +
    geom_point(aes(y = HF_m, color = "HF")) +
    geom_line(aes(y = HF_m, color = "HF")) +
    geom_point(aes(y = LF_m, color = "LF")) +
    geom_line(aes(y = LF_m, color = "LF")) +
    labs(subtitle = "Low Densities",
         x = "Hour", y = "Mean (ind./L)") +
    scale_color_manual(name = "Filters", 
                       values = c("Density" = "black", 
                                  "HF" = "red", 
                                  "LF" = "blue")) +
    scale_x_continuous(breaks = seq(0, 23, by = 1), 
                       limits = c(0, 23), 
                       minor_breaks = NULL) +
    theme_light() +
    theme(axis.text.x = element_text(size = 14, face = "bold", 
                                     family = "Times New Roman",
                                     color = "black"),
          axis.text.y = element_text(size = 16, face = "bold",
                                     family = "Times New Roman",
                                     color = "black"),
          legend.text = element_text(size = 20, face = "bold",
                                     family = "Times New Roman",
                                     color = "black"),
          axis.title.x = element_text(size = 22, face = "bold",
                                      family = "Times New Roman",
                                      color = "black"),
          axis.title.y = element_text(size = 20, face = "bold",
                                      family = "Times New Roman",
                                      color = "black"),
          legend.position = "bottom",
          title = element_text(size = 20, face = "bold", 
                               family = "Times New Roman", 
                               color = "black"))
  
  p_high <- ggplot(data_high, aes(x = hora)) +
    geom_point(aes(y = density_m, color = "Density")) +
    geom_line(aes(y = density_m, color = "Density")) +
    geom_point(aes(y = HF_m, color = "HF")) +
    geom_line(aes(y = HF_m, color = "HF")) +
    geom_point(aes(y = LF_m, color = "LF")) +
    geom_line(aes(y = LF_m, color = "LF")) +
    labs(title = classes[d], subtitle = "High Densities",
         x = "Hour", y = "Mean (ind./L)") +
    scale_color_manual(name = "Filters", 
                       values = c("Density" = "black", 
                                  "HF" = "red", 
                                  "LF" = "blue")) +
    scale_x_continuous(breaks = seq(0, 23, by = 1), 
                       limits = c(0, 23), 
                       minor_breaks = NULL) +
    theme_light() +
    theme(axis.text.x = element_text(size = 14, face = "bold", 
                                     family = "Times New Roman",
                                     color = "black"),
          axis.text.y = element_text(size = 14, face = "bold", 
                                     family = "Times New Roman",
                                     color = "black"),
          legend.text = element_text(size = 20, face = "bold",
                                     family = "Times New Roman",
                                     color = "black"),
          axis.title.x = element_text(size = 22, face = "bold",
                                      family = "Times New Roman",
                                      color = "black"),
          axis.title.y = element_text(size = 20, face = "bold",
                                      family = "Times New Roman",
                                      color = "black"),
          legend.position = "none",
          title = element_text(size = 20, face = "bold", 
                               family = "Times New Roman", 
                               color = "black"))
  
  final_plot <- p_high / p_low
  ggsave(filename = here(output_dir_figs, 
                         paste0("hour_", names(hourly_data_filtered)[d], 
                                ".png")), 
         plot = final_plot,
         width = 15, height = 7, 
         dpi = "retina")
  
  print(final_plot)
  
  cat(paste0(d, ":"), names(hourly_data_filtered)[d], "\n")
  
}


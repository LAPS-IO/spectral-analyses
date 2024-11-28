#################################
library(dplR)
library(astsa)
# pegando o dataframe com a classe de interesse
df <- list_fill[[4]]
# # Spectral analysis
# spec <- mvspec(df$density, detrend = T, log = "n")
# spec_data <- spec[["details"]] %>% as_tibble()
# plot(spec_data$spectrum ~ spec_data$frequency, type = "l", col = "black")
# spec_data$spectrum[spec_data$period > 24.3] <- 0.0000000001
# lines(spec_data$spectrum ~ spec_data$frequency, type = "l", col = "red")
# plot(spec_data$spectrum ~ spec_data$frequency, type = "l", col = "red", ylim = c(0, 600000))

# par(mfrow = c(3, 2))
# Plot da série original
plot(df$density, type = "l", 
     main = "série original - rhizosolenia_proboscia", 
     ylab = "density ind./L")

# extração da média da série e comprimento da série e remoção da média da série
a0 <- mean(df$density, na.rm = T)
N <- length(df$density)
Zt <- df$density - a0
plot(Zt, type = "l", 
     main = "série original sem média - rhizosolenia_proboscia",
     ylab = "density ind./L")

# Obtenção da FFT - espectros da série
spectrum_complex <- fft(Zt) / sqrt(N) # valores do espectro na forma complexa
spectrum <- abs(spectrum_complex)^2 # sem parte complexa
plot(spectrum, type = "l", 
     main = "Espectro da série - rhizosolenia_proboscia", 
     ylab = "spectrum",
     xlim = c(0, N/2))

# Reconstrução da FFT - Prova real
inv <- Re(fft(spectrum_complex, inverse = T))
inv <- inv / sqrt(length(inv))
plot(inv, type = "l", 
     main = "Série reconstruída a partir das frequências - Prova Real - rhizosolenia_proboscia")

# Obtenção da frequência angular
omega <- 0:(N - 1) / N 

# Construção do tibble com os dados de frequência, período, espectro e o espectro na forma complexa
data_fft <- tibble(frequency = omega, period = 1/omega, 
                   spectrum = spectrum, spectrum_complex = spectrum_complex)

# Filtro para eliminação das frequências baixas (períodos > 24h)
N_freq_cut <- length(data_fft$period) - sum(data_fft$period < 26, na.rm = T)
data_fft$HF <- ifelse(data_fft$period > 26, 0, data_fft$spectrum)
data_fft$HF[(N - N_freq_cut + 1):N] <- 0 # zerar as frequências simétricas
data_fft$HF_complex <- ifelse(data_fft$period > 26, 0, data_fft$spectrum_complex)
data_fft$HF_complex[(N - N_freq_cut + 1):N] <- 0 # zerar as frequências simétricas

# Filtro para eliminação frequências altas, complementar a frequências baixas (períodos < 24h)
data_fft <- data_fft %>% 
  mutate(LF = if_else(HF == 0, spectrum, 0),
         LF_complex = if_else(HF == 0, spectrum_complex, 0))

# Reconstrução da série de alta frequência após a filtragem
HF_series <- Re(fft(data_fft$HF_complex, inverse = T))
HF_series <- HF_series / sqrt(N)
plot(HF_series, type = "l", 
     main = " série reconstruída com períodos inferiores a 26h - Altas frequências")
N_HF_series <- length(HF_series)
astsa::mvspec(HF_series, detrend = T, log = "no")

# Reconstrução da série de baixa frequência após a filtragem
LF_series <- Re(fft(data_fft$LF_complex, inverse = T))
LF_series <- LF_series / sqrt(N)
plot(LF_series, type = "l", 
     main = " série reconstruída com períodos superiores a 26h - Baixas frequências")
N_LF_series <- length(LF_series)
astsa::mvspec(LF_series, detrend = T, log = "no")

# Adicionar a média de volta à série reconstruída
HF_series <- HF_series + a0
mean(HF_series, na.rm = T)

LF_series <- LF_series + a0
mean(LF_series, na.rm = T)

# Obtendo os espectros da série reconstruída e filtrada para confirmar que a reconstrução removeu as frequências desejadas
dft_HF_series <- fft(HF_series) / sqrt(N_HF_series)
dj_HF_series <- abs(dft_HF_series)^2
lines(dj_HF_series[-1], type = "l", col = "red",
      main = "espectro da série reconstruída com as altas frequências", 
      xlim = c(0, 5076))
dft_LF_series <- fft(LF_series) / sqrt(N_LF_series)
dj_LF_series <- abs(dft_LF_series)^2
plot(dj_LF_series[-1], type = "l", col = "darkblue",
     main = "espectro da série reconstruída com as baixas frequências", 
     xlim = c(0, 5076))

ggplot() +
  geom_line(aes(x = data_fft[-1,]$frequency[data_fft$period < 26], 
                y = data_fft[-1,]$spectrum[data_fft$period < 26], 
                color = "High Frequency")) +
  geom_line(aes(x = data_fft[-1,]$frequency[data_fft$period > 26], 
                y = data_fft[-1,]$spectrum[data_fft$period > 26], 
                color = "Low Frequency")) +
  xlim(0, .5) +
  labs(x = "Frequency (cycles/hour)", 
       y = "Spectrum", 
       title = "rhizosolenia_proboscia") +
  scale_color_manual(name = "Filters", 
                     values = c("Low Frequency" = "darkblue",
                                "High Frequency" = "red")) +
  theme_bw()

# Monthly Data for Time plot filtered
test <- tibble(cycle = df$cycle_rounded, 
               mes = month(df$cycle_rounded), 
               density = df$density, 
               HF = HF_series, 
               LF = LF_series) %>%
  reframe(across(c(density, HF, LF), 
                 list(m = mean, dp = sd), 
                 .names = "{.col}_{.fn}"),
          .by = mes)

# Monthly Time plot filtered by low frequency
p <- ggplot(test, aes(x = mes)) +
  geom_point(aes(y = density_m, color = "Density")) +
  geom_line(aes(y = density_m, color = "Density")) +
  geom_point(aes(y = HF_m, color = "HF")) +
  geom_line(aes(y = HF_m, color = "HF")) +
  geom_point(aes(y = LF_m, color = "LF")) +
  geom_line(aes(y = LF_m, color = "LF")) +
  labs(title = "Rhizosolenia proboscia Complex",
       x = "Month", y = "Mean (ind./L)") +
  # ylim(0.5, 2) +
  scale_color_manual(name = "Filters", 
                     values = c("Density" = "black", 
                                "HF" = "red", 
                                "LF" = "blue")) +
  scale_x_continuous(breaks = seq(1, 12, by = 1), 
                     limits = c(1, 12), 
                     minor_breaks = NULL) +
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
p
ch_low <- c(4, 5, 6, 7, 8, 9, 10, 11)
ch_hig <- c(1, 2, 3, 12)
gd_low <- c(1, 2, 4, 6, 8, 9, 10, 11)
gd_hig <- c(3, 5, 7, 12)
rp_low <- c(4, 5, 7, 8, 9, 10, 11, 12)
rp_hig <- c(1, 2, 3, 6)
co_low <- c(1, 2, 3, 8, 9, 10, 11)
co_hig <- c(4, 5, 6, 7, 12)
gs_low <- c(1, 2, 5, 6, 7, 8, 9, 10, 11)
gs_hig <- c(3, 4, 12)
he_low <- c(1, 2, 3, 7, 11, 12)
he_hig <- c(4, 5, 6, 8, 9, 10)
rr_low <- c(1, 2, 5, 7, 8, 12)
rr_hig <- c(3, 4, 6, 9, 10, 11)

# Hourly Data for Time plot filtered by low month densities
low_dens <- tibble(cycle = df$cycle_rounded, 
                   mes = month(df$cycle_rounded), 
                   hora = hour(df$cycle_rounded),
                   density = df$density, 
                   HF = HF_series, 
                   LF = LF_series) %>%
  filter(mes %in% rr_low) %>% 
  droplevels() %>% 
  reframe(across(c(density, HF, LF), 
                 list(m = mean, dp = sd), 
                 .names = "{.col}_{.fn}"),
          .by = hora)

high_dens <- tibble(cycle = df$cycle_rounded, 
                    mes = month(df$cycle_rounded), 
                    hora = hour(df$cycle_rounded),
                    density = df$density, 
                    HF = HF_series, 
                    LF = LF_series) %>%
  filter(mes %in% rr_hig) %>% 
  droplevels() %>% 
  reframe(across(c(density, HF, LF), 
                 list(m = mean, dp = sd), 
                 .names = "{.col}_{.fn}"),
          .by = hora)

# Hourly Time plot filtered by low frequency
p_low <- ggplot(low_dens, aes(x = hora)) +
  geom_point(aes(y = density_m, color = "Density")) +
  geom_line(aes(y = density_m, color = "Density")) +
  geom_point(aes(y = HF_m, color = "HF")) +
  geom_line(aes(y = HF_m, color = "HF")) +
  geom_point(aes(y = LF_m, color = "LF")) +
  geom_line(aes(y = LF_m, color = "LF")) +
  labs(title = "Low Densities",
       x = "Hour", y = "Mean (ind./L)") +
  ylim(0, 3) +
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
        legend.position = "none",
        title = element_text(size = 20, face = "bold", 
                             family = "Times New Roman", 
                             color = "black"))

p_high <- ggplot(high_dens, aes(x = hora)) +
  geom_point(aes(y = density_m, color = "Density")) +
  geom_line(aes(y = density_m, color = "Density")) +
  geom_point(aes(y = HF_m, color = "HF")) +
  geom_line(aes(y = HF_m, color = "HF")) +
  geom_point(aes(y = LF_m, color = "LF")) +
  geom_line(aes(y = LF_m, color = "LF")) +
  labs(title = "High Densities",
       x = "Hour", y = "Mean (ind./L)") +
  ylim(0, 3) +
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
        axis.text.y = element_blank(),
        legend.text = element_text(size = 20, face = "bold",
                                   family = "Times New Roman",
                                   color = "black"),
        axis.title.x = element_text(size = 22, face = "bold",
                                    family = "Times New Roman",
                                    color = "black"),
        axis.title.y = element_blank(),
        legend.position = "none",
        title = element_text(size = 20, face = "bold", 
                             family = "Times New Roman", 
                             color = "black"))

# library(patchwork)
final_plot <- p/(p_low + p_high)
final_plot
# Save plot
library(here)
output_dir_figs <- here("images", "freq_24_filtered_plots")

ggsave(filename = here(output_dir_figs, "rhi_pro.png"), 
       plot = final_plot,
       width = 15, height = 7, 
       dpi = "retina")

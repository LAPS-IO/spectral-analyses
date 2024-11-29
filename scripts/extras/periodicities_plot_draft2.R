#################################
library(dplR)
library(astsa)
# pegando o dataframe com a classe de interesse
df <- list_fill[[1]]
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
     main = "série original - Chaetoceros", 
     ylab = "density ind./L")

# extração da média da série e comprimento da série e remoção da média da série
a0 <- mean(df$density, na.rm = T)
N <- length(df$density)
Zt <- df$density - a0
plot(Zt, type = "l", 
     main = "série original sem média - Chaetoceros",
     ylab = "density ind./L")

# Obtenção da FFT - espectros da série
spectrum_complex <- fft(Zt) / sqrt(N) # valores do espectro na forma complexa
spectrum <- abs(spectrum_complex)^2 # sem parte complexa
plot(spectrum, type = "l", 
     main = "Espectro da série - Chaetoceros", 
     ylab = "spectrum",
     xlim = c(0, N/2))

# Reconstrução da FFT - Prova real
inv <- Re(fft(spectrum_complex, inverse = T))
inv <- inv / sqrt(length(inv))
plot(inv, type = "l", 
     main = "Série reconstruída a partir das frequências - Prova Real - Chaetoceros")

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
     main = " série reconstruída com períodos inferiores a 24h - Altas frequências")
N_HF_series <- length(HF_series)
astsa::mvspec(HF_series, detrend = T, log = "no")

# Reconstrução da série de baixa frequência após a filtragem
LF_series <- Re(fft(data_fft$LF_complex, inverse = T))
LF_series <- LF_series / sqrt(N)
plot(LF_series, type = "l", 
     main = " série reconstruída com períodos superiores a 24h - Baixas frequências")
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
       title = "Chaetoceros") +
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
ggplot(test, aes(x = mes)) +
  geom_point(aes(y = density_m, color = "Density")) +
  geom_line(aes(y = density_m, color = "Density")) +
  geom_point(aes(y = HF_m, color = "HF")) +
  geom_line(aes(y = HF_m, color = "HF")) +
  geom_point(aes(y = LF_m, color = "LF")) +
  geom_line(aes(y = LF_m, color = "LF")) +
  labs(x = "Month", y = "Mean of Density (ind./L)") +
  scale_color_manual(name = "Filters", 
                     values = c("Density" = "black", 
                                "HF" = "red", 
                                "LF" = "blue")) +
  scale_x_continuous(n.breaks = 12, minor_breaks = NULL) +
  theme_minimal()

# Hourly Data for Time plot filtered by low month densities
low_dens <- tibble(cycle = df$cycle_rounded, 
              mes = month(df$cycle_rounded), 
              hora = hour(df$cycle_rounded),
              density = df$density, 
              HF = HF_series, 
              LF = LF_series) %>%
  filter(mes %in% c(1,2,5,7,8,12)) %>% 
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
  filter(mes %in% c(3,4,6,9,10,11)) %>% 
  droplevels() %>% 
  reframe(across(c(density, HF, LF), 
                 list(m = mean, dp = sd), 
                 .names = "{.col}_{.fn}"),
          .by = hora)

# Hourly Time plot filtered by low frequency
ggplot(low_dens, aes(x = hora)) +
  geom_point(aes(y = density_m, color = "Density")) +
  geom_line(aes(y = density_m, color = "Density")) +
  geom_point(aes(y = HF_m, color = "HF")) +
  geom_line(aes(y = HF_m, color = "HF")) +
  geom_point(aes(y = LF_m, color = "LF")) +
  geom_line(aes(y = LF_m, color = "LF")) +
  labs(x = "Hour", y = "Mean of Density (ind./L)") +
  scale_color_manual(name = "Filters", 
                     values = c("Density" = "black", 
                                "HF" = "red", 
                                "LF" = "blue")) +
  scale_x_continuous(n.breaks = 24, minor_breaks = NULL) +
  theme_minimal()

  
##############################################################






# Aplicando filtros de baixa e alta frequencia - pacote dplR
# mantém periodos menores que 25h
HPF <- pass.filt(df$density, W = 24.4, n = 3,
                 type = "high", method = "Butterworth")
plot(HPF, main = "HPF signal", col = "red", type = "l")
# mantém periodos maiores que 48h
LPF <- pass.filt(df$density, W = 0.0207, 
                 type = "low", method = "Butterworth")
plot(LPF, main = "LPF signal", col = "red", type = "l")
# mantém periodos de 15dias 360h
# BPF <- pass.filt(df$density, W = c(100, 200), 
#                  type = "pass", method = "Butterworth")
# 
# 
# # Plots basicos para visualizar o HPF e LPF
# plot(df$density, type="l", col="green")
# lines(HPF, col="red")
# plot(df$density, type="l", col="green")
# lines(LPF, col="blue")
# plot(df$density, type="l", col="green")
# lines(BPF, col="black")
# 
# # Para visualizar as frequências com aplicação do mvspec
# a <- mvspec(HPF, detrend = T, log = "n")
# # a[["details"]] %>% view()
# 
# plot(a$spec ~a$freq, type = "l")
# dat = data.frame(x= a$freq, y = a$spec)
# p = ggplot(dat, aes(x, y)) +
#   geom_line() + theme_bw()
# ggplotly(p)

# dataframe para produzir gráficos de médias de densidade horárias, diárias e mensais
test <- tibble(cycle = df$cycle_rounded, tempo = month(df$cycle_rounded), 
               density = df$density, HPF = HPF, LPF = LPF, BPF = BPF,
               hand_FFT = HFout_series) %>%
  reframe(across(c(density, HPF, LPF, BPF, hand_FFT), 
                 list(m = mean, dp = sd), 
                 .names = "{.col}_{.fn}"),
          .by = tempo)


# plot de densidade média horária# plot de decount.fields()nsidade média horária
ggplot(test, aes(x = tempo)) +
  # geom_point(aes(y = density_m, color = "Density")) +
  geom_line(aes(y = density_m, color = "Density")) +
  geom_line(aes(y = hand_FFT_m, color = "hand_FFT")) +
  # geom_point(aes(y = HPF_m, color = "HPF")) +
  # geom_errorbar(aes(ymin = HPF_m - HPF_dp, ymax = HPF_m + HPF_dp,
  # colour = "HPF"), width = 0.2, position = "dodge") +
  geom_line(aes(y = HPF_m, color = "HPF")) +
  # geom_point(aes(y = LPF_m, color = "LPF")) +
  geom_line(aes(y = LPF_m, color = "LPF")) +
  # geom_point(aes(y = BPF_m, color = "BPF")) +
  # geom_smooth(aes(y = BPF_m, color = "BPF")) +
  # ylim(100, 200) +
  scale_color_manual(name = "Data with/without Filters", 
                     values = c("Density" = "forestgreen", 
                                "HPF" = "red", 
                                "LPF" = "blue", 
                                "hand_FFT" = "black")) +
  scale_x_continuous(n.breaks = 12, minor_breaks = NULL) +
  theme_minimal()



###
q <- tibble(cycle = df$cycle_rounded, tempo = month(df$cycle_rounded), 
            density = df$density)
q

q$density1 <- ifelse(q$tempo %in% c(4,5,6,7,8,9,10,11), 0, q$density)
q$density2 <- ifelse(q$tempo %in% c(1,2,3,12), 0, q$density)
plot(x = q$cycle, y = q$density1, col = "black", type = "l")
plot(x = q$cycle, y = q$density2, col = "black", type = "l")

astsa::mvspec(q$density1, detrend = T, log = "no")
astsa::mvspec(q$density2, detrend = T, log = "no")





###

















































# plot de densidade média diária
ggplot() +
  geom_smooth(aes(x = day(df$cycle_rounded),
                  y = test$density,
                  color = "Density")) +
  geom_smooth(aes(x = day(df$cycle_rounded),
                  y = test$HPF,
                  color = "HPF")) +
  geom_smooth(aes(x = day(test$cycle),
                  y = test$LPF,
                  color = "LPF")) +
  geom_smooth(aes(x = day(test$cycle),
                  y = test$BPF,
                  color = "BPF")) +
  scale_color_manual(name = "Data with/without Filters", 
                     values = c("Density" = "green", 
                                "HPF" = "red", 
                                "LPF" = "blue", 
                                "BPF" = "black"))
# plot de densidade média mensal
ggplot() +
  geom_smooth(aes(x = month(df$cycle_rounded), 
                  y = test$density, 
                  color = "Density")) +
  geom_smooth(aes(x = month(df$cycle_rounded), 
                  y = test$HPF, 
                  color = "HPF")) +
  geom_smooth(aes(x = month(test$cycle),
                  y = test$LPF,
                  color = "LPF")) +
  geom_smooth(aes(x = month(test$cycle),
                  y = test$BPF,
                  color = "BPF")) +
  scale_color_manual(name = "Data with/without Filters", 
                     values = c("Density" = "green", 
                                "HPF" = "red", 
                                "LPF" = "blue", 
                                "BPF" = "black"))


###############################################################
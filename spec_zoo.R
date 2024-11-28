# Ler imagens na pasta

# Pacotes
library(tidyverse)
library(plotly)

# Listando das imagens
path <- "C:/Users/souza/Documents/PhD/all/doliolidae"
files <- list.files(path)

# Cria dataframe com nome das imagens
df <- files %>%
  as_tibble() %>% 
  transmute(class = "doliolidae", 
            date_time = str_sub(value, 1, 23) %>% ymd_hms()) %>%
  transmute(class, cycle_rounded = round_date(date_time, "1 day")) 

# Ajusta dataframe para número de imagens por hora
df <- df %>%
  group_by(class,
           cycle_rounded = round_date(cycle_rounded,
                                      "1 day")) %>%
  summarize(n = n()) %>%
  ungroup()

# Pivota os dados, para que a classe de interesse seja uma coluna
df <- df %>% 
  tidyr::pivot_wider(names_from = class, 
                     values_from = n)
# Se há horários que não aparece imagens adicionar 0
df[is.na(df)] <- 0

# Cria dataframe com coluna referente a tods os ciclos que deveriam existir
# IDEAL: Necessário listar os ciclos existentes dos arquivos raw e posteriomente adicionar os 0's
cycles <- tibble(cycle_rounded = seq(ymd_hm("2020-11-04 00:00"),
                                     ymd_hm("2022-06-10 23:59"),
                                     by = "1 day"))

# Merge do dataframe dos ciclos existentes com os dos organismos
df_full <- dplyr::full_join(cycles, df)


# lims <- as.POSIXct(strptime(c("2021-12-01 00:00","2021-12-31 23:59"), format = "%Y-%m-%d %H:%M"))
# df_full_lim <- df_full[c(9409:10152), ]
# PLOT da série temporal de interesse 
p <- ggplot(data = df_full) +
  geom_point(aes(x = cycle_rounded, y = doliolidae),
             col = "red", size = .1) +
  geom_line(aes(x = cycle_rounded, y = doliolidae),
            col = "black", linewidth = .5) +
  scale_x_datetime(breaks = "10 days") +
  labs(x = "Date", y = "Total doliolidae/hour", 
       title = toupper("doliolidae")) +
  theme_test() +
  theme(axis.text.x = element_text(size = 15, face = "bold", 
                                   family = "Times New Roman",
                                   angle = 45, hjust = 1,
                                   vjust = 1, color = "black"),
        axis.text.y = element_text(size = 15, face = "bold",
                                   family = "Times New Roman",
                                   color = "black"),
        axis.title.x = element_text(size = 16, face = "bold",
                                    family = "Times New Roman",
                                    color = "black"),
        axis.title.y = element_text(size = 16, face = "bold",
                                    family = "Times New Roman",
                                    color = "black"))

ggplotly(p)

##########################################################################

# Obter a média da série temporal de interesse
mean(df_full$doliolidae, na.rm = T)

# INTERPOLAÇÃO: Preenche os NA's com a média da série temporal de interesse
# Verificar e criar script para outras formas de interpolação
df_dol <- df_full %>% 
  mutate(across(everything(), ~ ifelse(is.na(.), mean(., na.rm = TRUE), .)))
acf(df_dol$doliolidae, lag.max = 50)
pacf(df_dol$doliolidae, lag.max = 50)
lowess()
# aplicar a media ovel na série temporal
# Análise espectral com diferentes janelas de suavização
## Sem estacionarizar a série
a1 <- spectrum(df_dol$doliolidae, log = c("no"))
a1 <- spectrum(df_dol$doliolidae, log = c("no"))
a2 <- spectrum(df_dol$doliolidae, span = 3, log = c("no"), detrend = T)
a3 <- spectrum(df_dol$doliolidae, span = 20, log = c("no"), detrend = T)
#a4 <- spectrum(df_dol$doliolidae, log = c("no"), method = c("ar"))
# ## Estacionarizando a série
# b1 <- spectrum(I(df_dol$doliolidae - mean(df_dol$doliolidae)), 
#                  span = c(3, 3, 3, 3), log = c("no"))
# b2 <- spectrum(I(df_dol$doliolidae - mean(df_dol$doliolidae)), 
#                  span = c(5, 5, 5, 5), log = c("no"))
# b3 <- spectrum(I(df_dol$doliolidae - mean(df_dol$doliolidae)),
#                  span = c(3, 5, 5, 3), log = c("no"))
# b4 <- spectrum(I(df_dol$doliolidae - mean(df_dol$doliolidae)), 
#                  span = c(3, 3, 3, 3, 3), log = c("no"))
# b5 <- spectrum(I(df_dol$doliolidae - mean(df_dol$doliolidae)), 
#                  span = c(5, 5, 5), log = c("no"))
# b6 <- spectrum(I(df_dol$doliolidae - mean(df_dol$doliolidae)),
#                  span = c(3, 7, 3), log = c("no"))
# b7 <- spectrum(I(df_dol$doliolidae - mean(df_dol$doliolidae)), 
#                  span = c(5, 10, 15, 10, 5), log = c("no"))


#####
x <- a4$freq / (3.6/1000)
y <- 2 * a4$spec

df_spectral <- tibble(x, 
                      doliolidae = y, 
                      period = uHz_hour(x))


df_spectral <- df_spectral %>% 
  tidyr::pivot_longer(!c(x, period), 
                      names_to = "species", 
                      values_to = "spectrum")

period_labels <- function(x) {
  sapply(x, function(x_val) {
    unique(df_spectral$period[df_spectral$x == x_val])
  })
}

p <- ggplot(df_spectral) +
  geom_line(aes(x = period, y = spectrum, col = species), linewidth = 1) +
  geom_point(aes(x = period, y = spectrum), col = "black", size = .03) +
  # facet_wrap(~species, scales = "free") +
  scale_x_continuous(n.breaks = 10, trans = "reverse", limits = c(50,0)) +
  labs(x = "Hora") +
  theme_test() +
  theme(axis.text.x = element_text(size = 12, face = "bold", 
                                   family = "Times New Roman",
                                   color = "black"),
        axis.text.y = element_text(size = 12, face = "bold", 
                                   family = "Times New Roman",
                                   color = "black"),
        axis.title.x = element_text(size = 16, face = "bold",
                                    family = "Times New Roman",
                                    color = "black"),
        axis.title.y = element_text(size = 16, face = "bold",
                                    family = "Times New Roman",
                                    color = "black"), 
        strip.text = element_text(size = 14, face = "bold", 
                                  family = "Times New Roman",
                                  color = "black"), 
        legend.position = "none")

p

#####

spx <- a3$freq / (3.6/1000)
spy <- 2 * a3$spec
plot(spx, spy, 
     xlab = paste0("uHz (", "x", "10^6)"), 
     ylab = "variance/Hz", 
     type = "l", xlim = c(0, 4))

df <- tibble(spx, spy) %>% 
  view()

# mHz_hour <- function(frequencymHz) { 1/(frequencymHz/1000)/3600 }
uHz_hour <- function(frequencyuHz) { 
  1/(frequencyuHz/1000)/3.6 
}
uHz_hour(0.11574074)


####################################################################


periodo1 <- 12  # primeira periodicidade em horas
periodo2 <- 6  # segunda periodicidade em horas
amplitude1 <- 1 # Amplitude da primeira senoide
amplitude2 <- 1 # Amplitude da segunda senoide
fase1 <- 0      # Fase da primeira senoide
fase2 <- 0      # Fase da segunda senoide

df_full_lim$month_year <- floor_date(df_full_lim$cycle_rounded, "month")


library(lubridate)
inicio <- min(df_full_lim$cycle_rounded)  # Data e hora de início
tempo <- as.numeric(difftime(df_full_lim$cycle_rounded, inicio, units = "hours"))

criar_senoide <- function(tempo, 
                          periodo1, periodo2, 
                          amplitude1, amplitude2, 
                          fase1, fase2) {
  senoide1 <- amplitude1 * sin((2 * pi / periodo1) * tempo + fase1)
  senoide2 <- amplitude2 * sin((2 * pi / periodo2) * tempo + fase2)
  return(senoide1 + senoide2)
}

df_full_lim$senoide <- criar_senoide(tempo,
                                 periodo1, periodo2, 
                                 amplitude1, amplitude2, 
                                 fase1, fase2)
# library(scales)
lims <- as.POSIXct(strptime(c("2021-12-01 00:00","2021-12-31 00:00"), format = "%Y-%m-%d %H:%M"))
# Plotar os dados originais e a senoide
p <- ggplot(df_full, aes(x = cycle_rounded)) +
  geom_point(aes(y = log10(I(doliolidae - mean(doliolidae, na.rm = T))+1), color = "doliolidae"), size = .1) +
  geom_line(aes(y = senoide, color = "Senoide"), size = .3) +
  # facet_wrap(~ month_year, scales = "free_x") + # Facetar por mês e ano
  scale_x_datetime(date_labels = "%H:%M", date_breaks = "24 hour", 
                   limits = lims) +
  # ylim(c(0, 10)) +
  labs(title = "Dados Temporais e Senoide com Duas Periodicidades",
       x = "Tempo (horas)",
       y = "Valor",
       color = "Legenda") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

ggplotly(p)




########################
library(ggplot2)
library(lubridate)
library(plotly)
library(stats)
# Função para calcular o erro (MSE) entre a senoide ajustada e os dados reais
erro_senoide <- function(params, tempo, dados) {
  periodo1 <- params[1]
  periodo2 <- params[2]
  fase1 <- params[3]
  fase2 <- params[4]
  
  senoide_ajustada <- 
    amplitude1 * sin((2 * pi / periodo1) * tempo + fase1) +
    amplitude2 * sin((2 * pi / periodo2) * tempo + fase2)
  
  erro <- mean((dados - senoide_ajustada)^2, na.rm =T)
  return(erro)
}

# Definir os valores iniciais para a otimização
valores_iniciais <- c(periodo1, periodo2, fase1, fase2)

# Realizar a otimização para minimizar o erro
resultado <- optim(valores_iniciais, erro_senoide, 
                   tempo = tempo, 
                   dados = df_full_lim$doliolidae, 
                   method = "L-BFGS-B", 
                   lower = c(1, 1, -pi/2, -pi/2), 
                   upper = c(100, 100, pi/2, pi/2))

# Extrair os parâmetros otimizados
periodo1_otimizado <- resultado$par[1]
periodo2_otimizado <- resultado$par[2]
fase1_otimizado <- resultado$par[3]
fase2_otimizado <- resultado$par[4]

# Criar senoide ajustada com os parâmetros otimizados
df_full_lim$senoide_ajustada <- criar_senoide(tempo,
                                          periodo1, periodo2, 
                                          amplitude1, amplitude2, 
                                          fase1_otimizado, fase2_otimizado)

# Plotar os dados originais e a senoide ajustada
p <- ggplot(df_full_lim, aes(x = cycle_rounded)) +
  geom_point(aes(y = log10(doliolidae), color = "doliolidae"), size = .1) +
  geom_line(aes(y = senoide_ajustada, color = "Senoide Ajustada"), size = .3) +
  # facet_wrap(~ month_year, scales = "free_x") + # Facetar por mês e ano
  scale_x_datetime(date_labels = "%H:%M", date_breaks = "12 hours", 
                   limits = lims) +
  # ylim(c(-5, 30)) +
  labs(title = "Dados Temporais e Senoide Ajustada",
       x = "Tempo (horas)",
       y = "Valor",
       color = "Legenda") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

ggplotly(p)



############################


upper <- df_full$doliolidae
upper[is.na(upper)] <- mean(upper, na.rm = T)
time <- as.numeric(difftime(df_full$cycle_rounded, inicio, units = "hours"))
model <- lm(upper ~ sin(time * 2 * pi) + cos(time * 2 * pi))
plot(time, upper, pch = "o", ylim = c(0, 50), xlim = c(-8000, -8050))
lines(time, predict(model), col = "red", lwd = 2)
plot(model)

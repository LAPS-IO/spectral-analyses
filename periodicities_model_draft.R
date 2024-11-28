N = 180
tempo = 1:N
erro = rnorm(N, sd= 3)

Xt = 5 + 3 * cos(2 * pi * 1/12 * tempo) + 4 * sin(2 * pi * 1/12 * tempo) + 
  2.5 * cos(2 * pi * 1/18 * tempo) + 1.5 * sin(2 * pi * 1/18 * tempo) + erro

plot(y = Xt, x = tempo, type = "l")
# plot(Xt ~ tempo)

a0 = mean(Xt)
Xt_2 = Xt - a0


# Fw = fft(Xt_2)
# Ij = Mod(Fw)^2
# 
# Ij_2 = Ij[1:(N/2)]
# 
# plot(Ij_2, type = "l")
# which(Ij_2 > 2000)


omega = 0:(N-1)/N
dj = c()
tempo = 1:N
dcos = c()
dsin = c()

for (j in 0:(N-1)) {
  dj[j+1] = sum(Xt *exp(-2 * pi * 1i * omega[j+1] * tempo)) / sqrt(N)
  dcos[j+1] = sum(Xt * cos(2 * pi *  omega[j+1] * tempo)) / sqrt(N)
  dsin[j+1] = sum(Xt * sin(2 * pi *  omega[j+1] * tempo)) / sqrt(N)
}

Ij = tempo*abs(dj)^2

round(Ij,4) == round(dcos^2 + dsin^2, 4)

Ij_2 = Ij[1:(N/2)]
plot(Ij_2, type = "l")
idx = which(Ij_2>30)

1 / omega[idx]


a12 = (2/sqrt(N)) * dcos[11]
b12 = (2/sqrt(N)) * dsin[11] 
a18 = (2/sqrt(N)) * dcos[16] 
b18 = (2/sqrt(N)) * dsin[16] 


Xt_est = a0 + a12*cos(2*pi*omega[11]*tempo) + b12*sin(2*pi*omega[11]*tempo) +
  a18*cos(2*pi*omega[16]*tempo) + b18*sin(2*pi*omega[16]*tempo)

plot(Xt, type = "l")
lines(Xt_est, col = 2)

xcos12 = cos(2*pi*omega[11]*tempo)
xsin12 = sin(2*pi*omega[11]*tempo)
xcos18 = cos(2*pi*omega[16]*tempo)
xsin18 = sin(2*pi*omega[16]*tempo)

xcos20 = cos(2*pi*omega[10]*tempo)
xsin20 = sin(2*pi*omega[10]*tempo)


modelo = lm(Xt ~ xcos12 + xsin12 + xcos18 + xsin18 + xcos20 + xsin20)
summary(modelo)

resid = Xt- Xt_est

plot(resid, type = "l")
acf(resid)






## Analise de série real




dados = read.table("https://www.ime.usp.br/~pam/m-cananeia.76.85")
serie = ts(dados$V1)
N = length(serie)

plot(serie)
Zt = serie-mean(serie)
# 
# a <- mvspec(serie, detrend = T)
# b <- as_tibble(a[["details"]])

dft = fft(Zt)/sqrt(N)
dj =  abs(dft)^2

plot(dj)
# plot(spec_data$spectrum, type = "l")
which(dj>100)

temp = 1:N
omega = 0:(N-1)/N # frequência angular
dcos = c()
dsin = c()
Ij = c()

for (j in 1:N) {
  Ij[j] = sum(Zt*exp(-2 * pi * 1i * omega[j] * temp)) / sqrt(N)
  dcos[j] = sum(Zt*cos(2*pi*omega[j]*temp)) / sqrt(N)
  dsin[j] = sum(Zt*sin(2*pi*omega[j]*temp)) / sqrt(N)
}


round(dcos^2+dsin^2, 4) == round(dj, 4) 
round(dcos^2+dsin^2, 4) == round(abs(Ij)^2, 4)

a_0 = mean(serie)
a12 = (2/sqrt(N)) * dcos[11]
b12 = (2/sqrt(N)) * dsin[11]
dcos[11]

########### Hemidiscus
df <- list_fill$hemidiscus
Zt <- df$density - mean(df$density)
a_0 <- mean(df$density)
dc1 <- sum(Zt * cos(2 * pi * 0.0006 * 1:length(df$density))) / sqrt(length(df$density))
dc2 <- sum(Zt * cos(2 * pi * 0.0011 * 1:length(df$density))) / sqrt(length(df$density))
dc3 <- sum(Zt * cos(2 * pi * 0.0416 * 1:length(df$density))) / sqrt(length(df$density))
dc4 <- sum(Zt * cos(2 * pi * 0.0801 * 1:length(df$density))) / sqrt(length(df$density))
a1 <- (2 / sqrt(length(df$density))) * dc1
a2 <- (2 / sqrt(length(df$density))) * dc2
a3 <- (2 / sqrt(length(df$density))) * dc3
a4 <- (2 / sqrt(length(df$density))) * dc4

ds1 <- sum(Zt * sin(2 * pi * 0.0006 * 1:length(df$density))) / sqrt(length(df$density))
ds2 <- sum(Zt * sin(2 * pi * 0.0011 * 1:length(df$density))) / sqrt(length(df$density))
ds3 <- sum(Zt * sin(2 * pi * 0.0416 * 1:length(df$density))) / sqrt(length(df$density))
ds4 <- sum(Zt * sin(2 * pi * 0.0801 * 1:length(df$density))) / sqrt(length(df$density))
b1 <- (2 / sqrt(length(df$density))) * ds1
b2 <- (2 / sqrt(length(df$density))) * ds2
b3 <- (2 / sqrt(length(df$density))) * ds3
b4 <- (2 / sqrt(length(df$density))) * ds4

xcos1 <- cos(2 * pi * 0.0006 * 1:length(df$density))
xcos2 <- cos(2 * pi * 0.0011 * 1:length(df$density))
xcos3 <- cos(2 * pi * 0.0416 * 1:length(df$density))
xcos4 <- cos(2 * pi * 0.0801 * 1:length(df$density))

xsin1 <- sin(2 * pi * 0.0006 * 1:length(df$density))
xsin2 <- sin(2 * pi * 0.0011 * 1:length(df$density))
xsin3 <- sin(2 * pi * 0.0416 * 1:length(df$density))
xsin4 <- sin(2 * pi * 0.0801 * 1:length(df$density))

Zt_est <- a_0 + 
  a1 * xcos1 + b1 * xsin1 +
  a2 * xcos2 + b2 * xsin2 +
  a3 * xcos3 + b3 * xsin3 +
  a4 * xcos4 + b4 * xsin4

plot(df$density)
lines(Zt_est, col = 2)

modelo <- lm(df$density ~ xcos1 + xsin1 + 
               xcos2 + xsin2 + 
               xcos3 + xsin3 + 
               xcos4 + xsin4)
summary(modelo)

plot(residuals(modelo), type = "l")

astsa::acf2(residuals(modelo))

test <- tibble(cycle = df$cycle_rounded, serie = Zt_est)
  
ggplot() +
  geom_smooth(aes(x = hour(test$cycle), 
                  y = test$serie), 
              color = "blue") +
  geom_smooth(aes(x = hour(df$cycle_rounded), 
                  y = df$density), 
              color = "red")


########### Coscinodiscus
df <- list_fill$coscinodiscus
Zt <- df$density - mean(df$density)
a_0 <- mean(df$density)
dc1 <- sum(Zt * cos(2 * pi * 0.0028 * 1:length(df$density))) / sqrt(length(df$density))
dc2 <- sum(Zt * cos(2 * pi * 0.0416 * 1:length(df$density))) / sqrt(length(df$density))
dc3 <- sum(Zt * cos(2 * pi * 0.0834 * 1:length(df$density))) / sqrt(length(df$density))
dc4 <- sum(Zt * cos(2 * pi * 0.1250 * 1:length(df$density))) / sqrt(length(df$density))
dc5 <- sum(Zt * cos(2 * pi * 0.1610 * 1:length(df$density))) / sqrt(length(df$density))
dc6 <- sum(Zt * cos(2 * pi * 0.2420 * 1:length(df$density))) / sqrt(length(df$density))
a1 <- (2 / sqrt(length(df$density))) * dc1
a2 <- (2 / sqrt(length(df$density))) * dc2
a3 <- (2 / sqrt(length(df$density))) * dc3
a4 <- (2 / sqrt(length(df$density))) * dc4
a5 <- (2 / sqrt(length(df$density))) * dc5
a6 <- (2 / sqrt(length(df$density))) * dc6

ds1 <- sum(Zt * sin(2 * pi * 0.0028 * 1:length(df$density))) / sqrt(length(df$density))
ds2 <- sum(Zt * sin(2 * pi * 0.0416 * 1:length(df$density))) / sqrt(length(df$density))
ds3 <- sum(Zt * sin(2 * pi * 0.0834 * 1:length(df$density))) / sqrt(length(df$density))
ds4 <- sum(Zt * sin(2 * pi * 0.1250 * 1:length(df$density))) / sqrt(length(df$density))
ds5 <- sum(Zt * sin(2 * pi * 0.1610 * 1:length(df$density))) / sqrt(length(df$density))
ds6 <- sum(Zt * sin(2 * pi * 0.2420 * 1:length(df$density))) / sqrt(length(df$density))
b1 <- (2 / sqrt(length(df$density))) * ds1
b2 <- (2 / sqrt(length(df$density))) * ds2
b3 <- (2 / sqrt(length(df$density))) * ds3
b4 <- (2 / sqrt(length(df$density))) * ds4
b5 <- (2 / sqrt(length(df$density))) * ds5
b6 <- (2 / sqrt(length(df$density))) * ds6

xcos1 <- cos(2 * pi * 0.0028 * 1:length(df$density))
xcos2 <- cos(2 * pi * 0.0416 * 1:length(df$density))
xcos3 <- cos(2 * pi * 0.0834 * 1:length(df$density))
xcos4 <- cos(2 * pi * 0.1250 * 1:length(df$density))
xcos5 <- cos(2 * pi * 0.1610 * 1:length(df$density))
xcos6 <- cos(2 * pi * 0.2420 * 1:length(df$density))

xsin1 <- sin(2 * pi * 0.0028 * 1:length(df$density))
xsin2 <- sin(2 * pi * 0.0416 * 1:length(df$density))
xsin3 <- sin(2 * pi * 0.0834 * 1:length(df$density))
xsin4 <- sin(2 * pi * 0.1250 * 1:length(df$density))
xsin5 <- sin(2 * pi * 0.1610 * 1:length(df$density))
xsin6 <- sin(2 * pi * 0.2420 * 1:length(df$density))

Zt_est <- a_0 + 
  a1 * xcos1 + b1 * xsin1 +
  a2 * xcos2 + b2 * xsin2 +
  a3 * xcos3 + b3 * xsin3 +
  a4 * xcos4 + b4 * xsin4 +
  a5 * xcos5 + b5 * xsin5 +
  a6 * xcos5 + b6 * xsin6

plot(df$density)
lines(Zt_est, col = 2)

modelo <- lm(df$density ~ xcos1 + xsin1 + 
               xcos2 + xsin2 +
               xcos3 + xsin3 + 
               xcos4 + xsin4 + 
               xcos5 + xsin5 + 
               xcos6 + xsin6
             )
summary(modelo)

plot(residuals(modelo), type = "l")

astsa::acf2(residuals(modelo))
shapiro.test(residuals(modelo))

test <- tibble(cycle = df$cycle_rounded, serie = Zt_est)

ggplot() +
  geom_smooth(aes(x = hour(test$cycle), 
                  y = test$serie), 
              color = "blue") +
  geom_smooth(aes(x = hour(df$cycle_rounded), 
                  y = df$density), 
              color = "red")




########### guinardia dactyliosolen complex
df <- list_fill$compl_guinardia_dactyliosolen
Zt <- df$density - mean(df$density)
a_0 <- mean(df$density)
dc1 <- sum(Zt * cos(2 * pi * 0.0007 * 1:length(df$density))) / sqrt(length(df$density))
dc2 <- sum(Zt * cos(2 * pi * 0.0415 * 1:length(df$density))) / sqrt(length(df$density))
dc3 <- sum(Zt * cos(2 * pi * 0.0817 * 1:length(df$density))) / sqrt(length(df$density))
dc4 <- sum(Zt * cos(2 * pi * 0.118 * 1:length(df$density))) / sqrt(length(df$density))
dc5 <- sum(Zt * cos(2 * pi * 0.197 * 1:length(df$density))) / sqrt(length(df$density))
a1 <- (2 / sqrt(length(df$density))) * dc1
a2 <- (2 / sqrt(length(df$density))) * dc2
a3 <- (2 / sqrt(length(df$density))) * dc3
a4 <- (2 / sqrt(length(df$density))) * dc4
a5 <- (2 / sqrt(length(df$density))) * dc5

ds1 <- sum(Zt * sin(2 * pi * 0.0007 * 1:length(df$density))) / sqrt(length(df$density))
ds2 <- sum(Zt * sin(2 * pi * 0.0415 * 1:length(df$density))) / sqrt(length(df$density))
ds3 <- sum(Zt * sin(2 * pi * 0.0817 * 1:length(df$density))) / sqrt(length(df$density))
ds4 <- sum(Zt * sin(2 * pi * 0.118 * 1:length(df$density))) / sqrt(length(df$density))
ds5 <- sum(Zt * sin(2 * pi * 0.197 * 1:length(df$density))) / sqrt(length(df$density))
b1 <- (2 / sqrt(length(df$density))) * ds1
b2 <- (2 / sqrt(length(df$density))) * ds2
b3 <- (2 / sqrt(length(df$density))) * ds3
b4 <- (2 / sqrt(length(df$density))) * ds4
b5 <- (2 / sqrt(length(df$density))) * ds5

xcos1 <- cos(2 * pi * 0.0007 * 1:length(df$density))
xcos2 <- cos(2 * pi * 0.0415 * 1:length(df$density))
xcos3 <- cos(2 * pi * 0.0817 * 1:length(df$density))
xcos4 <- cos(2 * pi * 0.118 * 1:length(df$density))
xcos5 <- cos(2 * pi * 0.197 * 1:length(df$density))
xsin1 <- sin(2 * pi * 0.0007 * 1:length(df$density))
xsin2 <- sin(2 * pi * 0.0415 * 1:length(df$density))
xsin3 <- sin(2 * pi * 0.0817 * 1:length(df$density))
xsin4 <- sin(2 * pi * 0.118 * 1:length(df$density))
xsin5 <- sin(2 * pi * 0.197 * 1:length(df$density))

Zt_est <- a_0 + 
  a1 * xcos1 + b1 * xsin1 +
  a2 * xcos2 + b2 * xsin2 +
  a3 * xcos3 + b3 * xsin3 +
  a4 * xcos4 + b4 * xsin4 +
  a5 * xcos5 + b5 * xsin5

plot(df$density)
lines(Zt_est, col = 2)

modelo <- lm(df$density ~ xcos1 + xsin1 + 
               xcos2 + xsin2 +
               xcos3 + xsin3 + 
               xcos4 + xsin4 +
               xcos5 + xsin5
)
summary(modelo)

plot(residuals(modelo), type = "l")

astsa::acf2(residuals(modelo))
astsa::acf2(residuals(modelo2))
shapiro.test(residuals(modelo2))

test <- tibble(cycle = df$cycle_rounded, serie = Zt_est)

ggplot() +
  geom_smooth(aes(x = hour(test$cycle), 
                  y = test$serie), 
              color = "blue") +
  geom_smooth(aes(x = hour(df$cycle_rounded), 
                  y = df$density), 
              color = "red")



########### compl_rhizosolenia_proboscia
df <- list_fill$compl_rhizosolenia_proboscia
Zt <- df$density - mean(df$density)
a_0 <- mean(df$density)
dc1 <- sum(Zt * cos(2 * pi * 0.0005 * 1:length(df$density))) / sqrt(length(df$density))
dc2 <- sum(Zt * cos(2 * pi * 0.0109 * 1:length(df$density))) / sqrt(length(df$density))
dc3 <- sum(Zt * cos(2 * pi * 0.041 * 1:length(df$density))) / sqrt(length(df$density))
# dc4 <- sum(Zt * cos(2 * pi * 0.200 * 1:length(df$density))) / sqrt(length(df$density))
a1 <- (2 / sqrt(length(df$density))) * dc1
a2 <- (2 / sqrt(length(df$density))) * dc2
a3 <- (2 / sqrt(length(df$density))) * dc3
# a4 <- (2 / sqrt(length(df$density))) * dc4

ds1 <- sum(Zt * sin(2 * pi * 0.0005 * 1:length(df$density))) / sqrt(length(df$density))
ds2 <- sum(Zt * sin(2 * pi * 0.0109 * 1:length(df$density))) / sqrt(length(df$density))
ds3 <- sum(Zt * sin(2 * pi * 0.041 * 1:length(df$density))) / sqrt(length(df$density))
# ds4 <- sum(Zt * sin(2 * pi * 0.200 * 1:length(df$density))) / sqrt(length(df$density))
b1 <- (2 / sqrt(length(df$density))) * ds1
b2 <- (2 / sqrt(length(df$density))) * ds2
b3 <- (2 / sqrt(length(df$density))) * ds3
# b4 <- (2 / sqrt(length(df$density))) * ds4

xcos1 <- cos(2 * pi * 0.0005 * 1:length(df$density))
xcos2 <- cos(2 * pi * 0.0109 * 1:length(df$density))
xcos3 <- cos(2 * pi * 0.041 * 1:length(df$density))
# xcos4 <- cos(2 * pi * 0.200 * 1:length(df$density))

xsin1 <- sin(2 * pi * 0.0005 * 1:length(df$density))
xsin2 <- sin(2 * pi * 0.0109 * 1:length(df$density))
xsin3 <- sin(2 * pi * 0.041 * 1:length(df$density))
# xsin4 <- sin(2 * pi * 0.200 * 1:length(df$density))

Zt_est <- a_0 + 
  a1 * xcos1 + b1 * xsin1 +
  a2 * xcos2 + b2 * xsin2 +
  a3 * xcos3 + b3 * xsin3# +
  # a4 * xcos4 + b4 * xsin4

plot(df$density, ylim = c(0, 6), xlim = c(3000, 5000))
lines(Zt_est, col = 2, lwd = 5)

modelo <- lm(df$density ~ xcos1 + xsin1 + 
               xcos2 + xsin2 +
               xcos3 + xsin3 #+ 
               # xcos4 + xsin4
)
summary(modelo)

plot(residuals(modelo), type = "l")

astsa::acf2(residuals(modelo))
astsa::acf2(residuals(modelo2))
shapiro.test(residuals(modelo2))


#################################
# pegando o dataframe com a classe de interesse
df <- list_fill$coscinodiscus
library(dplR)
# Aplicando filtros de baixa e alta frequencia - pacote dplR
# mantém periodos menores que 25h
HPF <- pass.filt(df$density, W = 25, 
                 type = "high", method = "Butterworth")
# mantém periodos maiores que 48h
LPF <- pass.filt(df$density, W = 0.0210, 
                 type = "low", method = "Butterworth")
# mantém periodos de 15dias 360h
BPF <- pass.filt(df$density, W = c(100, 200), 
                 type = "pass", method = "Butterworth")

BPF <- pass.filt(df$density, W = c(0.0025, 0.0035), n = 3,
                 type = "pass", method = "Butterworth")

# Plots basicos para visualizar o HPF e LPF
plot(df$density, type="l", col="green")
lines(HPF, col="red")
plot(df$density, type="l", col="green")
lines(LPF, col="blue")
plot(df$density, type="l", col="green")
lines(BPF, col="black")

# Para visualizar as frequências com aplicação do mvspec
a <- mvspec(HPF, detrend = T, log = "n")
# a[["details"]] %>% view()

plot(a$spec ~a$freq, type = "l")
dat = data.frame(x= a$freq, y = a$spec)
p = ggplot(dat, aes(x, y)) +
  geom_line() + theme_bw()
ggplotly(p)

# dataframe para produzir gráficos de médias de densidade horárias, diárias e mensais
test <- tibble(cycle = df$cycle_rounded, tempo = month(df$cycle_rounded), 
               density = df$density, HPF = HPF, LPF = LPF, BPF = BPF) %>%
  reframe(across(c(density, HPF, LPF, BPF), 
                 list(m = mean, dp = sd), 
                 .names = "{.col}_{.fn}"),
          .by = tempo)


# plot de densidade média horária# plot de decount.fields()nsidade média horária
ggplot(test, aes(x = tempo)) +
  # geom_point(aes(y = density_m, color = "Density")) +
  geom_smooth(aes(y = density_m, color = "Density")) +
  # geom_point(aes(y = HPF_m, color = "HPF")) +
  # geom_errorbar(aes(ymin = HPF_m - HPF_dp, ymax = HPF_m + HPF_dp,
                    # colour = "HPF"), width = 0.2, position = "dodge") +
  geom_smooth(aes(y = HPF_m, color = "HPF"), ) +
  # geom_point(aes(y = LPF_m, color = "LPF")) +
  # geom_smooth(aes(y = LPF_m, color = "LPF")) +
  # geom_point(aes(y = BPF_m, color = "BPF")) +
  geom_smooth(aes(y = BPF_m, color = "BPF")) +
  scale_color_manual(name = "Data with/without Filters", 
                     values = c("Density" = "forestgreen", 
                                "HPF" = "red", 
                                "LPF" = "blue", 
                                "BPF" = "black")) +
  scale_x_continuous(n.breaks = 12, minor_breaks = NULL) +
  theme_minimal()

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






k <- kernel("daniell", c(3,3,3))
spec <- mvspec(bSm, 
               detrend = T, log = c("no"), 
               # xlim = c(0.01, .25), ylim = c(0, 2000000),
               kernel = k
               ) # periodicity between 4h - 40days

# get data to spec plot
spec_data <- as_tibble(spec[["details"]])
p1 <- ggplot(spec_data) +
  geom_line(aes(x = frequency, y = spectrum), 
            color = "black", linewidth = 1) +
  # ylim(0, 300) +
  lims(x = c(0, 0.05)) +
  theme_test() +
  labs(x = "Frequency (cycles/hour)", y = "Spectrum",
       title = "test ") +
  theme(axis.text.x = element_text(size = 20, face = "bold", 
                                   family = "Times New Roman",
                                   hjust = 0.5, color = "black"),
        axis.text.y = element_text(size = 20, face = "bold",
                                   family = "Times New Roman",
                                   color = "black"),
        axis.title.x = element_text(size = 22, face = "bold",
                                    family = "Times New Roman",
                                    color = "black"),
        axis.title.y = element_text(size = 22, face = "bold",
                                    family = "Times New Roman",
                                    color = "black"),
        title = element_text(size = 24, face = "bold", color = "black",
                             family = "Times New Roman"))

ggplotly(p1)














temp = 1:N
Zt_est = a_0 + a12*cos(2*pi*omega[11]*temp) + b12*sin(2*pi*omega[11]*temp)


par(mfrow = c(1,1))
plot(serie)
lines(Zt_est, col = 2)


xcos = cos(2*pi*omega[11]*temp)
xsin = sin(2*pi*omega[11]*temp)

modelo = lm(serie ~ xcos + xsin)
summary(modelo)

plot(residuals(modelo), type = "l")
astsa::acf2(residuals(modelo))
shapiro.test(residuals(modelo)) # teste para verificar se os residuos são normais




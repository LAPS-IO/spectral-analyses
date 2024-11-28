# Spectral analyses

# Packages
library(astsa)
library(data.table)
library(ggpubr)
library(tidyverse)
library(here)
library(plotly)

# Get data
path <- here("data", "processed")
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
list_fill <- lapply(files_list, function(e) {
                      e <- e %>% 
                        mutate(across(everything(), ~ ifelse(is.na(.), mean(., na.rm = TRUE), .)))
                      e
                    })

# Spectral analysis - astsa
k <- kernel("daniell", c(1))
plot(k)
spec <- mvspec(list_fill$guinardia_striata$density, 
               detrend = T, log = c("no"), #demean = T,
               # xlim = c(0.01, .25), ylim = c(0, 2000000),
               #kernel = #k
                 ) # periodicity between 4h - 40days

                 
x = 1:1024
y = cos(2*x*pi/24)
y[512:1024] = y[512:1024] +  cos(2*x[512:1024]*pi/12)
signal::specgram(y, n =30, Fs = 2)
signal::specgram(list_fill$guinardia_striata$density, n = 24, Fs = 2)
# get data to spec plot
spec_data <- as_tibble(spec[["details"]])

# get main frequencies
spec_data %>% 
  filter(frequency > 0) %>% 
  slice_max(spectrum)

# Plot Periodogram
p1 <- ggplot(spec_data) +
  geom_line(aes(x = frequency, y = spectrum), 
            color = "black", linewidth = 1) +
  # ylim(0, 300) + 
  xlim(0, 0.05) +
  theme_test() +
  labs(x = "Frequency (cycles/hour)", y = "Spectrum",
       title = "Coscinodiscus sp.") +
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

p2 <- ggplot(spec_data) +
  geom_line(aes(x = frequency, y = spectrum), 
            color = "black", linewidth = 1) +
  labs(x = " ", y = " ") +
  xlim(0, 0.1) + 
  ylim(0, 1000000) +
  geom_point(aes(x = spec_data %>%
                   filter(frequency > 0.002) %>%
                   slice_max(spectrum) %>% .[[1]],
                 y = spec_data %>%
                   filter(frequency > 0.002) %>%
                   slice_max(spectrum) %>% .[[3]]),
             color = "red") +
  geom_text(aes(x = spec_data %>%
                  filter(frequency > 0.002) %>%
                  slice_max(spectrum) %>% .[[1]] + 0.013,
                y = spec_data %>%
                  filter(frequency > 0.002) %>%
                  slice_max(spectrum) %>% .[[3]] + 65000,
                label = paste0(spec_data %>%
                                 filter(frequency > 0.002) %>%
                                 slice_max(spectrum) %>%
                                 .[[2]] %>%
                                 round(0),
                "h")),
            color = "black", size = 8) +
  geom_point(aes(x = spec_data %>%
                   filter(frequency > 0.007) %>%
                   slice_max(spectrum) %>% .[[1]],
                 y = spec_data %>%
                   filter(frequency > 0.007) %>%
                   slice_max(spectrum) %>% .[[3]]),
             color = "red") +
  geom_text(aes(x = spec_data %>%
                  filter(frequency > 0.007) %>%
                  slice_max(spectrum) %>% .[[1]],
                y = spec_data %>%
                  filter(frequency > 0.007) %>%
                  slice_max(spectrum) %>% .[[3]] + 65000,
                label = paste0(spec_data %>%
                                 filter(frequency > 0.007) %>%
                                 slice_max(spectrum) %>%
                                 .[[2]] %>%
                                 round(0),
                               "h")),
            color = "black", size = 8) +
  geom_point(aes(x = spec_data %>%
                   filter(frequency > 0.05) %>%
                   slice_max(spectrum) %>% .[[1]],
                 y = spec_data %>%
                   filter(frequency > 0.05) %>%
                   slice_max(spectrum) %>% .[[3]]),
             color = "red") +
  geom_text(aes(x = spec_data %>%
                  filter(frequency > 0.05) %>%
                  slice_max(spectrum) %>% .[[1]],
                y = spec_data %>%
                  filter(frequency > 0.05) %>%
                  slice_max(spectrum) %>% .[[3]] + 65000,
                label = paste0(spec_data %>%
                                 filter(frequency > 0.05) %>%
                                 slice_max(spectrum) %>%
                                 .[[2]] %>%
                                 round(0),
                               "h")),
            color = "black", size = 8) +
  theme_test() +
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
                                    color = "black"))

p3 <- ggplot(spec_data) +
  geom_line(aes(x = frequency, y = spectrum), 
            color = "black", linewidth = 1) +
  labs(x = " ", y = " ") +
  xlim(0.1, 0.4) + 
  ylim(0, 40000) +
  geom_point(aes(x = spec_data %>%
                   filter(frequency > 0.1) %>%
                   slice_max(spectrum) %>% .[[1]],
                 y = spec_data %>%
                   filter(frequency > 0.1) %>%
                   slice_max(spectrum) %>% .[[3]]),
             color = "red") +
  geom_text(aes(x = spec_data %>%
                  filter(frequency > 0.1) %>%
                  slice_max(spectrum) %>% .[[1]],
                y = spec_data %>%
                  filter(frequency > 0.1) %>%
                  slice_max(spectrum) %>% .[[3]] + 2000,
                label = paste0(spec_data %>%
                                 filter(frequency > 0.1) %>%
                                 slice_max(spectrum) %>%
                                 .[[2]] %>%
                                 round(0),
                               "h")),
            color = "black", size = 8) +
  geom_point(aes(x = spec_data %>%
                   filter(frequency > 0.14) %>%
                   slice_max(spectrum) %>% .[[1]],
                 y = spec_data %>%
                   filter(frequency > 0.14) %>%
                   slice_max(spectrum) %>% .[[3]]),
             color = "red") +
  geom_text(aes(x = spec_data %>%
                  filter(frequency > 0.14) %>%
                  slice_max(spectrum) %>% .[[1]],
                y = spec_data %>%
                  filter(frequency > 0.14) %>%
                  slice_max(spectrum) %>% .[[3]] + 2000,
                label = paste0(spec_data %>%
                                 filter(frequency > 0.14) %>%
                                 slice_max(spectrum) %>%
                                 .[[2]] %>%
                                 round(0),
                               "h")),
            color = "black", size = 8) +
  geom_point(aes(x = spec_data %>%
                   filter(frequency > 0.18) %>%
                   slice_max(spectrum) %>% .[[1]],
                 y = spec_data %>%
                   filter(frequency > 0.18) %>%
                   slice_max(spectrum) %>% .[[3]]),
             color = "red") +
  geom_text(aes(x = spec_data %>%
                  filter(frequency > 0.18) %>%
                  slice_max(spectrum) %>% .[[1]],
                y = spec_data %>%
                  filter(frequency > 0.18) %>%
                  slice_max(spectrum) %>% .[[3]] + 2000,
                label = paste0(spec_data %>%
                                 filter(frequency > 0.18) %>%
                                 slice_max(spectrum) %>%
                                 .[[2]] %>%
                                 round(0),
                               "h")),
            color = "black", size = 8) +
  theme_test() +
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
                                    color = "black"))

final_plot <- p1 + 
  annotation_custom(ggplotGrob(p2), 
                    xmin = 0.05, xmax = 0.25, 
                    ymin = max(spec_data$spectrum)/4, 
                    ymax = max(spec_data$spectrum)) + 
  annotation_custom(ggplotGrob(p3), 
                    xmin = 0.3, xmax = 0.5, 
                    ymin = max(spec_data$spectrum)/4, 
                    ymax = max(spec_data$spectrum))
final_plot


# Salvando plots
output_dir_figs <- here("images", "spectral_analysis_plot")

ggsave(filename = here(output_dir_figs, "spec_plot_cha.png"), 
       plot = final_plot,
       width = 16, height = 7, 
       dpi = "retina")

ggsave(filename = here(output_dir_figs, "spec_plot_gui_dac.png"), 
       plot = final_plot,
       width = 16, height = 7, 
       dpi = "retina")

ggsave(filename = here(output_dir_figs, "spec_plot_rhi_pro.png"), 
       plot = final_plot,
       width = 16, height = 7, 
       dpi = "retina")

ggsave(filename = here(output_dir_figs, "spec_plot_cos.png"), 
       plot = final_plot,
       width = 16, height = 7, 
       dpi = "retina")

ggsave(filename = here(output_dir_figs, "spec_plot_gui_str.png"), 
       plot = final_plot,
       width = 16, height = 7, 
       dpi = "retina")

ggsave(filename = here(output_dir_figs, "spec_plot_rhi_rob.png"), 
       plot = final_plot,
       width = 16, height = 7, 
       dpi = "retina")

ggsave(filename = here(output_dir_figs, "spec_plot_hem.png"), 
       plot = final_plot,
       width = 16, height = 7, 
       dpi = "retina")



# Get main frequencies/periodicities




























# Get main periodicities
n <- length(df$density)
Freq <- (1:n - 1) / n

which.min(spec_data$spectrum[0:500])
Freq[243]
which.max(spec_data$spectrum[245:n])
Freq[245+138-1]
1/0.00058






# n <- length(df$density)
# plot(df$density, ylab = "Densidade", xlab = "Hora")
# Per <- Mod(fft(df$density - mean(df$density)))^2 / n
# Freq <- (1:n - 1) / n
# plot(Freq[350:5000], Per[350:5000], 
#      type = 'h', lwd = 3, ylab = "Periodogram",
#      xlab = "Frequency", ylim = c(0, 4000000))
# u <- which.max(Per[350:2000]) # 22 freq=21/600=.035 cycles/day
# uu <- which.max(Per[350:2000][-u]) # 25 freq=25/600=.041 cycles/day
# 1/Freq[22]; 1/Freq[26] # period = days/cycle
# text(.05, 7000, "24 day cycle"); text(.027, 9000, "29 day cycle")
# ### another way to find the two peaks is to order on Per
# y <- cbind(350:2000, Freq[350:2000], Per[350:2000]); y[order(y[, 3]), ]



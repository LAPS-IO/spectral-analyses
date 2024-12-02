# listar ciclos RAW

library(tidyverse)
library(lubridate)

setwd("/backup/Salvador_raw_imgs_frames/raw/")

rawdirs <- basename(list.dirs()) %>% 
  tibble()

colnames(rawdirs) <- "raw_dir"

rawdirs2 <- rawdirs %>%
  filter(str_detect(raw_dir, "Basler")) %>% 
  transmute(cycles = str_sub(raw_dir, 8, 33),
            cycles_round = cycles %>%
              ymd_hms() %>%
              round_date("30 min"),
            same_as_above = cycles_round == lag(cycles_round))

################################################################################
# listar ciclos LPD

setwd("/backup/Salvador_raw_imgs_frames/LPD/")

lpddirs <- basename(list.dirs()) %>% 
  tibble()

colnames(lpddirs) <- "lpd_dir"

lpddirs2 <- lpddirs %>%
  filter(str_detect(lpd_dir, "Basler")) %>% 
  mutate(lpd_dir = str_sub(lpd_dir, 8, 33) %>% 
           ymd_hms() %>% 
           round_date("1 hour"))

################################################################################
# listar ciclos LLS

setwd("/backup/Salvador_raw_imgs_frames/LLS/")

llsdirs <- basename(list.dirs()) %>% 
  tibble()

colnames(llsdirs) <- "lls_dir"

llsdirs2 <- llsdirs %>%
  filter(str_detect(lls_dir, "Basler")) %>% 
  mutate(lls_dir = str_sub(lls_dir, 8, 33) %>% 
           ymd_hms() %>% 
           round_date("30 min"))

################################################################################
# Encontrando ciclos em que não houve coleta e classifica-los com NA

cycles <- tibble(cycles = seq(ymd_hm("2020-11-04 13:00"),
                              ymd_hm("2022-06-10 12:30"),
                              by = "30 min"))

cycles <- cycles %>%
  mutate(coleta = if_else(cycles %in% rawdirs2$cycles_round, 0, NA))

################################################################################
# Juntando os dados
cycles <- cycles %>% 
  mutate(data = str_sub(cycles, 1, 10) %>% ymd(),
         hora = hms::as_hms(cycles),
         descricao = if_else(coleta %in% 0, "dados coletados", "sem dados")) %>% 
  dplyr::rename(cycles_round = cycles)

teste <- full_join(cycles, rawdirs2)

################################################################################
# Plot dos dados
teste <- full_join(cycles, rawdirs2) %>% filter(descricao != "sem dados")

teste <- teste %>% 
  mutate(xmin = data, 
         xmax = data + months(1) - days(3),
         ymin = day(data), 
         ymax = day(data) + 1)

ggplot() +
  # geom_tile(data = teste,
  #           mapping = aes(x = str_sub(data, 1, 7) %>% ym(),
  #                         y = day(data),
  #                         fill = factor(descricao)),
  #           color = "black", linewidth = .2, width = 31) +
  geom_rect(data = teste,
            mapping = aes(xmin = str_sub(data, 1, 7) %>% ym(),
                          xmax = str_sub(data, 1, 7) %>% ym() + months(1),
                          ymin = day(data),
                          ymax = day(data) + 1,
                          fill = factor(descricao)),
            color = "black", size = 0.2) +
  geom_text(data = aggregate(descricao ~ data,
                             FUN = length,
                             data = teste),
            mapping = aes(x = str_sub(data, 1, 7) %>% ym() + 15,
                          y = day(data) + .5,
                          label = descricao)) +
  scale_x_date(date_breaks = "1 month", date_labels = "%m", expand = c(0, 0)) +
  facet_grid(.~ lubridate::year(data), 
             space = "free_x", scales = "free_x", 
             switch = "x", drop = T) +
  scale_y_continuous(n.breaks = 7) +
  labs(x = "DATA", y = "DIA", fill = "", 
       title = "Número de ciclos obtidos pelo equipamento") +
  scale_fill_manual(values = c("lightblue")) +
  theme_classic() +
  theme(axis.text = element_text(size = 16, colour = "black"),
        axis.title = element_text(size = 18, face = "bold"), 
        axis.text.x = element_text(size = 16, hjust = -.5),
        axis.text.y = element_text(size = 16, colour = "black"), 
        legend.position = "none",
        strip.placement = "outside", 
        strip.text.x = element_text(size = 16, face = "bold"),
        strip.background = element_rect(fill = NA, colour = "grey50"),
        panel.spacing = unit(0, "mm"))


# Packages
# Loading packages used run the analysis
library(here)
source(here("scripts", "fitoplancton", "0-library.R"))

# Carregando o script
path <- here("data", "raw")
df <- fread(here(path, "meta.csv"))

# Ajuste do datetime do objeto com os ciclos completos
df <- df %>% 
  mutate(cycles = ymd_hms(paste(date, time)),
         cycle_rounded = floor_date(cycles, "30 min"))

df <- df %>%
  dplyr::group_by(date) %>%
  dplyr::summarise(N_cycle = n(),
                   N_raw = sum(n_raws),
                   N_rois = sum(n_rois),
                   roi_cycle = round(N_rois/N_cycle, 0)) %>%
  dplyr::ungroup()

# Plot dos ciclos por dia
p_cycle <- ggplot() +
  geom_rect(data = df %>% 
              mutate(descricao = "dados coletados"),
            mapping = aes(xmin = str_sub(date, 1, 7) %>% ym(),
                          xmax = str_sub(date, 1, 7) %>% ym() + months(1),
                          ymin = day(date),
                          ymax = day(date) + 1,
                          fill = factor(descricao)),
            color = "black", size = 0.2) +
  geom_text(data = df,
            mapping = aes(x = str_sub(date, 1, 7) %>% ym() + 15,
                          y = day(date) + 0.5,
                          label = N_cycle)) +
  scale_x_date(date_breaks = "1 month", date_labels = "%m", expand = c(0, 0)) +
  facet_grid(.~ lubridate::year(date), 
             space = "free_x", scales = "free_x", 
             switch = "x", drop = T) +
  scale_y_continuous(n.breaks = 7) +
  labs(x = "DATE", y = "DAY", fill = "", 
       title = "Number of cycles per day") +
  scale_fill_manual(values = c("lightblue", "white")) +
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

p_raw <- ggplot() +
  geom_rect(data = df %>% 
              mutate(descricao = "dados coletados"),
            mapping = aes(xmin = str_sub(date, 1, 7) %>% ym(),
                          xmax = str_sub(date, 1, 7) %>% ym() + months(1),
                          ymin = day(date),
                          ymax = day(date) + 1,
                          fill = factor(descricao)),
            color = "black", size = 0.2) +
  geom_text(data = df,
            mapping = aes(x = str_sub(date, 1, 7) %>% ym() + 15,
                          y = day(date) + 0.5,
                          label = N_raw)) +
  scale_x_date(date_breaks = "1 month", date_labels = "%m", expand = c(0, 0)) +
  facet_grid(.~ lubridate::year(date), 
             space = "free_x", scales = "free_x", 
             switch = "x", drop = T) +
  scale_y_continuous(n.breaks = 7) +
  labs(x = "DATE", y = "DAY", fill = "", 
       title = "Number of raws per day") +
  scale_fill_manual(values = c("lightblue", "white")) +
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

p_roi <- ggplot() +
  geom_rect(data = df %>% 
              mutate(descricao = "dados coletados"),
            mapping = aes(xmin = str_sub(date, 1, 7) %>% ym(),
                          xmax = str_sub(date, 1, 7) %>% ym() + months(1),
                          ymin = day(date),
                          ymax = day(date) + 1,
                          fill = factor(descricao)),
            color = "black", size = 0.2) +
  geom_text(data = df,
            mapping = aes(x = str_sub(date, 1, 7) %>% ym() + 15,
                          y = day(date) + 0.5,
                          label = N_rois)) +
  scale_x_date(date_breaks = "1 month", date_labels = "%m", expand = c(0, 0)) +
  facet_grid(.~ lubridate::year(date), 
             space = "free_x", scales = "free_x", 
             switch = "x", drop = T) +
  scale_y_continuous(n.breaks = 7) +
  labs(x = "DATE", y = "DAY", fill = "", 
       title = "Number of rois per day") +
  scale_fill_manual(values = c("lightblue", "white")) +
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

# Salvando os plots
output_dir_figs <- here("images", "general", "1-time_plot")

ggsave(filename = here(output_dir_figs, "cycle.png"), 
       plot = p_cycle, width = 16, height = 7, dpi = "retina")
ggsave(filename = here(output_dir_figs, "raw.png"), 
       plot = p_raw, width = 16, height = 7, dpi = "retina")
ggsave(filename = here(output_dir_figs, "roi.png"), 
       plot = p_roi, width = 16, height = 7, dpi = "retina")

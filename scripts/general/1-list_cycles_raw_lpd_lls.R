# Packages
# Loading packages used run the analysis
library(here)
source(here("scripts", "general", "0-library.R"))

# Carregando o script
path <- here("data", "raw", "raw_csv_lists")

# Listar arquivos
arquivos_csv <- list.files(path = path, pattern = "\\.csv$", full.names = TRUE)

# Função para adicionar o ciclo como variável
ler_e_adicionar_ciclo <- function(arquivo) {
  
  dt <- fread(arquivo)
  dt[, ciclo := tools::file_path_sans_ext(basename(arquivo))]
  return(dt)
  
}

# Ler os arquivos listados e adicionar o ciclo como variável
lista_arquivos <- pblapply(arquivos_csv, ler_e_adicionar_ciclo)
names(lista_arquivos) <- tools::file_path_sans_ext(basename(arquivos_csv))

# Função para construir lista de ciclos arredondados a cada 30min
arredondar_ciclo <- function(df) { 
  df[, cycle_rounded := floor_date(ymd_hms(ciclo), "30 minutes")] 
}

# Cria variável com ciclo arredondado (defined in last step)
lista_arquivos <- pblapply(lista_arquivos, arredondar_ciclo)

# Obter o nome de todos os ciclos existentes, arredondado-os e extraindo o N de raws
cycle_list <- lapply(lista_arquivos, function(df) {
  df[, (ncol(df)-1):ncol(df)] %>% 
    dplyr::group_by(cycle_rounded) %>% 
    dplyr::mutate(raw = n()) %>% 
    ungroup() %>% 
    unique(by = "ciclo")
})
cycles <- rbindlist(cycle_list)

# Save cycle list
path_to_save <- here("data", "processed", "general")
fwrite(cycles, here(path_to_save, "cycles.csv"))

# Get N_raw and N_roi by timestamp (defined in last step)
df <- pblapply(lista_arquivos, function(x) {
  my_data <- x %>%
    dplyr::group_by(cycle_rounded) %>% 
    dplyr::summarise(raw = n(),
                     rois = sum(n_rois))
  
})

# Grouping dataframes from list
df <- rbindlist(df)

# Get N_cycles as variable
df <- df[,
         .(cycles = .N,
           raw = sum(na.omit(raw)),
           rois = sum(na.omit(rois))),
         by = cycle_rounded]

# If has cycle rounded duplicated, grouping and summing them by ymd
df <- df[,
         .(cycles = sum(cycles),
           raw = sum(na.omit(raw)),
           rois = sum(na.omit(rois))),
         by = .(cycle_rounded = as.Date(cycle_rounded))]

# Save data table final
path_to_save <- here("data", "processed", "general")
fwrite(df, here(path_to_save, "N_cycle_raw_roi.csv"))

# Plot dos ciclos, raws and rois por dia
p_cycle <- ggplot() +
  geom_rect(data = df %>% 
              mutate(descricao = "dados coletados"),
            mapping = aes(xmin = str_sub(cycle_rounded, 1, 7) %>% ym(),
                          xmax = str_sub(cycle_rounded, 1, 7) %>% ym() + months(1),
                          ymin = day(cycle_rounded),
                          ymax = day(cycle_rounded) + 1,
                          fill = factor(descricao)),
            color = "black", linewidth = 0.2) +
  geom_text(data = df,
            mapping = aes(x = str_sub(cycle_rounded, 1, 7) %>% ym() + 15,
                          y = day(cycle_rounded) + 0.5,
                          label = cycles)) +
  scale_x_date(date_breaks = "1 month", 
               labels = date_format(format = "%b", locale = 'en_US'), 
               expand = c(0, 0)) +
  facet_grid(.~ lubridate::year(cycle_rounded), 
             space = "free_x", scales = "free_x", 
             switch = "x", drop = T) +
  scale_y_continuous(n.breaks = 7) +
  labs(x = "Months and years", y = "DAYS", fill = "", 
       title = "Number of cycles per day") +
  scale_fill_manual(values = c("lightblue", "white")) +
  theme_classic() +
  theme(axis.text = element_text(size = 16, colour = "black"),
        axis.title = element_text(size = 18, face = "bold"), 
        axis.text.x = element_text(size = 16, hjust = -.5),
        axis.text.y = element_text(size = 16, colour = "black"), 
        plot.title = element_text(hjust = 0.5, size = 20, face = "bold"),
        legend.position = "none",
        strip.placement = "outside", 
        strip.text.x = element_text(size = 16, face = "bold"),
        strip.background = element_rect(fill = NA, colour = "grey50"),
        panel.spacing = unit(0, "mm"))

p_raw <- ggplot(data = df %>% 
                  dplyr::mutate(descricao = "dados coletados")) +
  geom_rect(mapping = aes(xmin = str_sub(cycle_rounded, 1, 7) %>% ym(),
                          xmax = str_sub(cycle_rounded, 1, 7) %>% ym() + months(1),
                          ymin = day(cycle_rounded),
                          ymax = day(cycle_rounded) + 1,
                          fill = factor(descricao)),
            color = "black", linewidth = 0.2) +
  geom_text(mapping = aes(x = str_sub(cycle_rounded, 1, 7) %>% ym() + 15,
                          y = day(cycle_rounded) + 0.5,
                          label = format(round(raw/1000, 2), nsmall = 2)),
            size = 4) +
  scale_x_date(date_breaks = "1 month", 
               labels = date_format(format = "%b", locale = 'en_US'), 
               expand = c(0, 0)) +
  facet_grid(.~ lubridate::year(cycle_rounded), 
             space = "free_x", scales = "free_x", 
             switch = "x", drop = T) +
  scale_y_continuous(n.breaks = 7) +
  labs(x = "Months and years", y = "DAYS", fill = "", 
       title = bquote("Number of raws (\U00D7"*10^3*") per day")) +
  scale_fill_manual(values = c("lightblue", "white")) +
  theme_classic() +
  theme(axis.text = element_text(size = 16, colour = "black"),
        axis.title = element_text(size = 18, face = "bold"), 
        axis.text.x = element_text(size = 16, hjust = -.5),
        axis.text.y = element_text(size = 16, colour = "black"), 
        plot.title = element_text(hjust = 0.5, size = 20, face = "bold"),
        legend.position = "none",
        strip.placement = "outside", 
        strip.text.x = element_text(size = 16, face = "bold"),
        strip.background = element_rect(fill = NA, colour = "grey50"),
        panel.spacing = unit(0, "mm"))

p_roi <- ggplot() +
  geom_rect(data = df %>% 
              mutate(descricao = "dados coletados"),
            mapping = aes(xmin = str_sub(cycle_rounded, 1, 7) %>% ym(),
                          xmax = str_sub(cycle_rounded, 1, 7) %>% ym() + months(1),
                          ymin = day(cycle_rounded),
                          ymax = day(cycle_rounded) + 1,
                          fill = factor(descricao)),
            color = "black", linewidth = 0.2) +
  geom_text(data = df,
            mapping = aes(x = str_sub(cycle_rounded, 1, 7) %>% ym() + 15,
                          y = day(cycle_rounded) + 0.5,
                          label = format(round(rois/1000, 2), nsmall = 2)),
            size = 4) +
  scale_x_date(date_breaks = "1 month", 
               labels = date_format(format = "%b", locale = 'en_US'), 
               expand = c(0, 0)) +
  facet_grid(.~ lubridate::year(cycle_rounded), 
             space = "free_x", scales = "free_x", 
             switch = "x", drop = T) +
  scale_y_continuous(n.breaks = 7) +
  labs(x = "Months and years", y = "DAYS", fill = "", 
       title = bquote("Number of rois (\U00D7"*10^3*") per day")) +
  scale_fill_manual(values = c("lightblue", "white")) +
  theme_classic() +
  theme(axis.text = element_text(size = 16, colour = "black"),
        axis.title = element_text(size = 18, face = "bold"), 
        axis.text.x = element_text(size = 16, hjust = -.5),
        axis.text.y = element_text(size = 16, colour = "black"), 
        plot.title = element_text(hjust = 0.5, size = 20, face = "bold"),
        legend.position = "none",
        strip.placement = "outside", 
        strip.text.x = element_text(size = 16, face = "bold"),
        strip.background = element_rect(fill = NA, colour = "grey50"),
        panel.spacing = unit(0, "mm"))

p_vol <- ggplot(data = df %>% 
                  dplyr::mutate(descricao = "dados coletados")) +
  geom_rect(mapping = aes(xmin = str_sub(cycle_rounded, 1, 7) %>% ym(),
                          xmax = str_sub(cycle_rounded, 1, 7) %>% ym() + months(1),
                          ymin = day(cycle_rounded),
                          ymax = day(cycle_rounded) + 1,
                          fill = factor(descricao)),
            color = "black", linewidth = 0.2) +
  geom_text(mapping = aes(x = str_sub(cycle_rounded, 1, 7) %>% ym() + 15,
                          y = day(cycle_rounded) + 0.5,
                          label = format(round(raw*(0.101767*0.001), 2), 
                                         nsmall = 2)),
            size = 4) +
  scale_x_date(date_breaks = "1 month", 
               labels = date_format(format = "%b", locale = 'en_US'), 
               expand = c(0, 0)) +
  facet_grid(.~ lubridate::year(cycle_rounded), 
             space = "free_x", scales = "free_x", 
             switch = "x", drop = T) +
  scale_y_continuous(n.breaks = 7) +
  labs(x = "Months and years", y = "DAYS", fill = "", 
       title = bquote("Volume ("*m^3*") sampled per day")) +
  scale_fill_manual(values = c("lightblue", "white")) +
  theme_classic() +
  theme(axis.text = element_text(size = 16, colour = "black"),
        axis.title = element_text(size = 18, face = "bold"), 
        axis.text.x = element_text(size = 16, hjust = -.5),
        axis.text.y = element_text(size = 16, colour = "black"), 
        plot.title = element_text(hjust = 0.5, size = 20, face = "bold"),
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
ggsave(filename = here(output_dir_figs, "volume.png"), 
       plot = p_vol, width = 16, height = 7, dpi = "retina")

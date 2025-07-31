# Packages
# Loading packages used run the analysis - run if is the first time
source(here("scripts", "general", "0-library.R"))
source(here("scripts", "zooplancton", "1-time-series_data.R"))
rm(list = ls())
gc()

# Lendo os dados
path <- here("data", "processed", "zooplancton")
files <- list.files(path, pattern = ".csv",
                    full.names = T)

files_list <- lapply(files[3], fread)
names(files_list) <- files[3] %>% 
  basename() %>% 
  sub(pattern = ".csv", replacement = "")

# função para o plot
plot_func <- function(data, ylab) {
  
  ggplot(data = data) +
    geom_point(aes(x = cycle_rounded, y = density),
               col = "red", size = .1) +
    geom_line(aes(x = cycle_rounded, y = density),
              col = "black", linewidth = .5) +
    scale_x_datetime(breaks = "1 month", date_labels = "%Y-%m") +
    labs(x = "Date", y = ylab) +
    theme_test() +
    theme(axis.text.x = element_text(size = 20, face = "bold", 
                                     family = "Times New Roman",
                                     angle = 90, hjust = 1,
                                     vjust = .5, color = "black"),
          axis.text.y = element_text(size = 20, face = "bold",
                                     family = "Times New Roman",
                                     color = "black"),
          axis.title.x = element_text(size = 22, face = "bold",
                                      family = "Times New Roman",
                                      color = "black"),
          axis.title.y = element_text(size = 22, face = "bold",
                                      family = "Times New Roman",
                                      color = "black"))
  
}

# Gerando os plots
p2 <- plot_func(data = files_list$penilia,
                ylab = expression(paste(italic("penilia avirostris"), "(ind./L)")))
p2

output_dir_figs <- here("images", "zooplancton", "1-time_series_plot")
ggsave(filename = here(output_dir_figs, paste0(names(p2[["data"]])[4], ".png")), 
       plot = p2, width = 16, height = 7, dpi = "retina")

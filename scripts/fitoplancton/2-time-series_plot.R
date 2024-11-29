# Packages
# Loading packages used run the analysis
source(here("scripts", "fitoplancton", "0-library.R"))
source(here("scripts", "fitoplancton", "1-time-series_data.R"))
rm(list = ls())
gc()

# Lendo os dados
path <- here("data", "processed", "fitoplancton")
files <- list.files(path, pattern = ".csv",
                    full.names = T)

files_list <- lapply(files, fread)
names(files_list) <- files %>% 
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
p1 <- plot_func(data = files_list$chaetoceros, 
                ylab = paste("Chaetceros sp. (ind./L)"))
p2 <- plot_func(data = files_list$compl_guinardia_dactyliosolen,
                ylab = expression(paste(italic("Guinardia dactyliosolen"), "Complex",
                                        " (ind./L)")))
p3 <- plot_func(data = files_list$compl_rhizosolenia_proboscia,
                ylab = expression(paste(italic("Rhizosolenia proboscia"), "Complex",
                                        " (ind./L)")))
p4 <- plot_func(data = files_list$coscinodiscus, 
                ylab = paste("Coscinodiscus (ind./L)"))
p5 <- plot_func(data = files_list$guinardia_striata,
                ylab = expression(paste(italic("Guinardia striata"),
                                        " (ind./L)")))
p6 <- plot_func(data = files_list$hemidiscus, 
                ylab = paste("Hemidisus (ind./L)"))
p7 <- plot_func(data = files_list$rhizosolenia_robusta,
                ylab = expression(paste(italic("Rhizosolenia robusta"),
                                        " (ind./L)")))

# Salvando os plots
output_dir_figs <- here("images", "fitoplancton", "1-time_series_plot")

ggsave(filename = here(output_dir_figs, paste0(names(p1[["data"]])[4], ".png")),
       plot = p1, width = 16, height = 7, dpi = "retina")

ggsave(filename = here(output_dir_figs, paste0(names(p2[["data"]])[4], ".png")), 
       plot = p2, width = 16, height = 7, dpi = "retina")

ggsave(filename = here(output_dir_figs, paste0(names(p3[["data"]])[4], ".png")), 
       plot = p3, width = 16, height = 7, dpi = "retina")

ggsave(filename = here(output_dir_figs, paste0(names(p4[["data"]])[4], ".png")), 
       plot = p4, width = 16, height = 7, dpi = "retina")

ggsave(filename = here(output_dir_figs, paste0(names(p5[["data"]])[4], ".png")), 
       plot = p5, width = 16, height = 7, dpi = "retina")

ggsave(filename = here(output_dir_figs, paste0(names(p6[["data"]])[4], ".png")), 
       plot = p6, width = 16, height = 7, dpi = "retina")

ggsave(filename = here(output_dir_figs, paste0(names(p7[["data"]])[4], ".png")), 
       plot = p7, width = 16, height = 7, dpi = "retina")

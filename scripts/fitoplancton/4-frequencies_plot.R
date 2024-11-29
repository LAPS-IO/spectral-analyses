# Packages
# Loading packages used run the analysis
library(here)
source(here("scripts", "fitoplancton", "3-periodicities_plot.R"))
gc()

# Hourly filtered densities of normalized data, filtered by low and high month densities
df_high <- NULL
for(i in names(hourly_data_filtered)) {
  
  df <- hourly_data_filtered[[i]]
  month_selected <- high_dens_month[[i]]
  
  df_high[[i]] <- df %>% 
    dplyr::filter(mes %in% month_selected) %>% 
    droplevels() %>% 
    dplyr::reframe(HF_m = mean(HF_m), .by = hora) %>% 
    dplyr::mutate(HF_norm = decostand(HF_m,
                                      method = "range") %>%
                    as.numeric)
    
    
}

df_low <- NULL
for(i in names(hourly_data_filtered)) {
  
  df <- hourly_data_filtered[[i]]
  month_selected <- low_dens_month[[i]]
  
  df_low[[i]] <- df %>% 
    dplyr::filter(mes %in% month_selected) %>% 
    droplevels() %>% 
    dplyr::reframe(HF_m = mean(HF_m), .by = hora) %>% 
    dplyr::mutate(HF_norm = decostand(HF_m,
                                      method = "range") %>%
                    as.numeric)
  
  
}

df_low_combined <- do.call(rbind, lapply(seq_along(df_low), function(i) {
                                     
                                     df <- df_low[[i]]
                                     df$class <- names(df_low)[i]
                                     
                                     return(df)
                                     
                                   }))

df_high_combined <- do.call(rbind, lapply(seq_along(df_high), function(i) {
                                     
                                     df <- df_high[[i]]
                                     df$class <- names(df_high)[i]
                                     
                                     return(df)
                                     
                                   }))

# classes <- c("Doliolidae", 
#              expression(paste(italic("Penilia avirostris"))),
#              "Radiozoa")
classes <- c("Chaetoceros",
             expression(paste(italic("Guinardia dactyliosolen"), " Complex")),
             expression(paste(italic("Rhizosolenia proboscia"), " Complex")),
             expression(paste(italic("Coscinodiscus wailesii"))),
             expression(paste(italic("Guinardia striata"))),
             "Hemidiscus",
             expression(paste(italic("Rhizosolenia robusta"))))

final_plot <- ggplot(df_low_combined,
                    aes(x = hora, y = HF_norm,
                        group = factor(class), color = factor(class))) +
  geom_line(linewidth = 1) +
  geom_point(size = 1, color = "black", alpha = .3) +
  labs(x = "Hour", 
       y = "Normalized Densities", 
       title = "Months of Low Densities") +
  scale_color_manual(name = "Classes",
                     values = c("chaetoceros" = "black",
                                "compl_guinardia_dactyliosolen" = "red",
                                "compl_rhizosolenia_proboscia" = "forestgreen",
                                "coscinodiscus" = "blue",
                                "guinardia_striata" = "brown",
                                "hemidiscus" = "yellow",
                                "rhizosolenia_robusta" = "gray"),
                     labels = c("chaetoceros" = classes[1],
                                "compl_guinardia_dactyliosolen" = classes[2],
                                "compl_rhizosolenia_proboscia" = classes[3],
                                "coscinodiscus" = classes[4],
                                "guinardia_striata" = classes[5],
                                "hemidiscus" = classes[6],
                                "rhizosolenia_robusta" = classes[7])) +
  scale_x_continuous(breaks = c(0,2,4,6,8,10,12,14,16,18,20,22)) +
  theme_light() +
  theme(axis.text.x = element_text(size = 14, face = "bold", 
                                   family = "Times New Roman",
                                   color = "black"),
        axis.text.y = element_text(size = 16, face = "bold",
                                   family = "Times New Roman",
                                   color = "black"),
        legend.text = element_text(size = 20,
                                   family = "Times New Roman",
                                   color = "black"),
        axis.title.x = element_text(size = 22, face = "bold",
                                    family = "Times New Roman",
                                    color = "black"),
        axis.title.y = element_text(size = 20, face = "bold",
                                    family = "Times New Roman",
                                    color = "black"),
        title = element_text(size = 20, face = "bold", 
                             family = "Times New Roman", 
                             color = "black"))

output_dir_figs <- here("images", "fitoplancton", "4-freq_24_filtered_plots")

ggsave(filename = here(output_dir_figs, "LD_hourly peaks normalized.png"), 
       plot = final_plot,
       width = 15, height = 7, 
       dpi = "retina")

final_plot <- ggplot(df_high_combined,
                     aes(x = hora, y = HF_norm,
                         group = factor(class), color = factor(class))) +
  geom_line(linewidth = 1) +
  geom_point(size = 1, color = "black", alpha = .3) +
  labs(x = "Hour", 
       y = "Normalized Densities", 
       title = "Months of High Densities") +
  scale_color_manual(name = "Classes",
                     values = c("chaetoceros" = "black",
                                "compl_guinardia_dactyliosolen" = "red",
                                "compl_rhizosolenia_proboscia" = "forestgreen",
                                "coscinodiscus" = "blue",
                                "guinardia_striata" = "brown",
                                "hemidiscus" = "yellow",
                                "rhizosolenia_robusta" = "gray"),
                     labels = c("chaetoceros" = classes[1],
                                "compl_guinardia_dactyliosolen" = classes[2],
                                "compl_rhizosolenia_proboscia" = classes[3],
                                "coscinodiscus" = classes[4],
                                "guinardia_striata" = classes[5],
                                "hemidiscus" = classes[6],
                                "rhizosolenia_robusta" = classes[7])) +
  scale_x_continuous(breaks = c(0,2,4,6,8,10,12,14,16,18,20,22)) +
  theme_light() +
  theme(axis.text.x = element_text(size = 14, face = "bold", 
                                   family = "Times New Roman",
                                   color = "black"),
        axis.text.y = element_text(size = 16, face = "bold",
                                   family = "Times New Roman",
                                   color = "black"),
        legend.text = element_text(size = 20,
                                   family = "Times New Roman",
                                   color = "black"),
        axis.title.x = element_text(size = 22, face = "bold",
                                    family = "Times New Roman",
                                    color = "black"),
        axis.title.y = element_text(size = 20, face = "bold",
                                    family = "Times New Roman",
                                    color = "black"),
        title = element_text(size = 20, face = "bold", 
                             family = "Times New Roman", 
                             color = "black"))

output_dir_figs <- here("images", "fitoplancton", "4-freq_24_filtered_plots")

ggsave(filename = here(output_dir_figs, "HD_hourly peaks normalized.png"), 
       plot = final_plot,
       width = 15, height = 7, 
       dpi = "retina")

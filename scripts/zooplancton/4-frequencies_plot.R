<<<<<<< HEAD
# Script test
# Packages
# Loading packages used run the analysis
library(here)
source(here("scripts", "zooplancton", "3-periodicities_plot.R"))
gc()

# Get months where are low and high densities by month in monthly data filtered
low_dens_month <- NULL
high_dens_month <- NULL

for(n in names(monthly_data_filtered)) {
  
  df <- monthly_data_filtered[[n]][,1:3]
  
  low_dens_month[[n]] <- df$mes[df$data_m < mean(df$data_m)]
  high_dens_month[[n]] <- df$mes[df$data_m >= mean(df$data_m)]
  
  cat(paste("classe:", n, 
            "\n low months:", low_dens_month[n], 
            "\n high months:", high_dens_month[n], "\n"))
}
#######
for(d in seq_along(names(hourly_data_filtered))) {
  
  data_low <- hourly_data_filtered[[d]] %>% 
    dplyr::filter(factor(hourly_data_filtered[[d]]$mes) %in% 
                    factor(low_dens_month[[d]])) %>% 
    droplevels() %>% 
    dplyr::reframe(dplyr::across(c(density_m, HF_m, LF_m),
                                 list(mean),
                                 .names = "{.col}"),
                   .by = hora)
  
  data_high <- hourly_data_filtered[[d]] %>%  
    dplyr::filter(factor(hourly_data_filtered[[d]]$mes) %in% 
                    factor(high_dens_month[[d]])) %>% 
    droplevels() %>% 
    dplyr::reframe(dplyr::across(c(density_m, HF_m, LF_m),
                                 list(mean),
                                 .names = "{.col}"),
                   .by = hora)
  
  p_low <- ggplot(data_low, aes(x = hora)) +
    geom_point(aes(y = density_m, color = "Density")) +
    geom_line(aes(y = density_m, color = "Density")) +
    geom_point(aes(y = HF_m, color = "HF")) +
    geom_line(aes(y = HF_m, color = "HF")) +
    geom_point(aes(y = LF_m, color = "LF")) +
    geom_line(aes(y = LF_m, color = "LF")) +
    labs(subtitle = "Low Densities",
         x = "Hour", y = "Mean (ind./L)") +
    scale_color_manual(name = "Filters", 
                       values = c("Density" = "black", 
                                  "HF" = "red", 
                                  "LF" = "blue")) +
    scale_x_continuous(breaks = seq(0, 23, by = 1), 
                       limits = c(0, 23), 
                       minor_breaks = NULL) +
    theme_light() +
    theme(axis.text.x = element_text(size = 14, face = "bold", 
                                     family = "Times New Roman",
                                     color = "black"),
          axis.text.y = element_text(size = 16, face = "bold",
                                     family = "Times New Roman",
                                     color = "black"),
          legend.text = element_text(size = 20, face = "bold",
                                     family = "Times New Roman",
                                     color = "black"),
          axis.title.x = element_text(size = 22, face = "bold",
                                      family = "Times New Roman",
                                      color = "black"),
          axis.title.y = element_text(size = 20, face = "bold",
                                      family = "Times New Roman",
                                      color = "black"),
          legend.position = "bottom",
          title = element_text(size = 20, face = "bold", 
                               family = "Times New Roman", 
                               color = "black"))
  
  p_high <- ggplot(data_high, aes(x = hora)) +
    geom_point(aes(y = density_m, color = "Density")) +
    geom_line(aes(y = density_m, color = "Density")) +
    geom_point(aes(y = HF_m, color = "HF")) +
    geom_line(aes(y = HF_m, color = "HF")) +
    geom_point(aes(y = LF_m, color = "LF")) +
    geom_line(aes(y = LF_m, color = "LF")) +
    labs(title = classes[d], subtitle = "High Densities",
         x = "Hour", y = "Mean (ind./L)") +
    scale_color_manual(name = "Filters", 
                       values = c("Density" = "black", 
                                  "HF" = "red", 
                                  "LF" = "blue")) +
    scale_x_continuous(breaks = seq(0, 23, by = 1), 
                       limits = c(0, 23), 
                       minor_breaks = NULL) +
    theme_light() +
    theme(axis.text.x = element_text(size = 14, face = "bold", 
                                     family = "Times New Roman",
                                     color = "black"),
          axis.text.y = element_text(size = 14, face = "bold", 
                                     family = "Times New Roman",
                                     color = "black"),
          legend.text = element_text(size = 20, face = "bold",
                                     family = "Times New Roman",
                                     color = "black"),
          axis.title.x = element_text(size = 22, face = "bold",
                                      family = "Times New Roman",
                                      color = "black"),
          axis.title.y = element_text(size = 20, face = "bold",
                                      family = "Times New Roman",
                                      color = "black"),
          legend.position = "none",
          title = element_text(size = 20, face = "bold", 
                               family = "Times New Roman", 
                               color = "black"))
  
  final_plot <- p_high / p_low
  # ggsave(filename = here(output_dir_figs, 
  #                        paste0("hour_", names(hourly_data_filtered)[d], 
  #                               ".png")), 
  #        plot = final_plot,
  #        width = 15, height = 7, 
  #        dpi = "retina")
  
  print(final_plot)
  
  cat(paste0(d, ":"), names(hourly_data_filtered)[d], "\n")
  
}

#######




=======
# Packages
# Loading packages used run the analysis
library(here)
source(here("scripts", "fitoplancton", "3-periodicities_plot.R"))
gc()

# Hourly filtered densities of normalized data, filtered by low and high month densities
>>>>>>> fc9c083e9a513e94ce0c624be017bc851b2432d9
df_high <- NULL
for(i in names(hourly_data_filtered)) {
  
  df <- hourly_data_filtered[[i]]
<<<<<<< HEAD
  month_selected <- high_dens_month[[i]][, "data_m"]
=======
  month_selected <- high_dens_month[[i]]
>>>>>>> fc9c083e9a513e94ce0c624be017bc851b2432d9
  
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
  
<<<<<<< HEAD
  df <- hourly_data_filtered[[i]][, 1:3]
=======
  df <- hourly_data_filtered[[i]]
>>>>>>> fc9c083e9a513e94ce0c624be017bc851b2432d9
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

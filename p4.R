mod <- high_dens_comparison[, c(1, 3, 5, 7, 9, 11, 13, 15)] %>% 
  dplyr::arrange(hora)
mod <- mod %>% 
  transmute(hora = hora,
            g1 = rowMeans(select(., HF_norm_hem,HF_norm_cos,HF_norm_guistr,HF_norm_rhipro)) %>% 
              decostand(., method = "range") %>%
              as.numeric,
            g2 = rowMeans(select(., HF_norm_guidac, HF_norm_cha)) %>% 
              decostand(., method = "range") %>%
              as.numeric,
            g3 = rowMeans(select(., HF_norm_rhirob)) %>% 
              decostand(., method = "range") %>%
              as.numeric)

mod_high <- mod %>% 
  pivot_longer(!hora, names_to = "class", values_to = "HF_norm")
p2_high <- ggplot(mod_high, aes(x = hora)) +
  # geom_point(aes(y = density_m, color = "Density")) +
  # geom_line(aes(y = density_m, color = "Density")) +
  # geom_point(aes(y = HF_norm, color = class)) +
  geom_smooth(aes(y = HF_norm, color = class), linewidth = 2, se = F) +
  # geom_point(aes(y = LF_m, color = "LF")) +
  # geom_line(aes(y = LF_m, color = "LF")) +
  # labs(title = "Low Densities",
  #      x = "Hour", y = "Mean (ind./L)") +
  # ylim(0, 1) +
  # scale_color_manual(name = "Filters", 
  #                    values = c("Density" = "black", 
  #                               "HF" = "red", 
  #                               "LF" = "blue")) +
  labs(x = "Hour", y = "Normalized HF",
       title = "High densities") +
  scale_x_continuous(breaks = seq(0, 23, by = 1), 
                     limits = c(0, 23), 
                     minor_breaks = NULL) +
  scale_color_manual(name = "Groups", 
                     values = c("g1" = "black", 
                                "g2" = "red",
                                "g3" = "forestgreen"),
                     labels = c("g1" = "g1", 
                                "g2" = "g2",
                                "g3" = "g3")) +
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
        title = element_text(size = 20, face = "bold", 
                             family = "Times New Roman", 
                             color = "black"))

library(patchwork)
p_high/p2_high/eq

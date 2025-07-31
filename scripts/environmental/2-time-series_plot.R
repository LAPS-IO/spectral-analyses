# Loadingpackages and scripts
source(here("scripts", "general", "0-library.R"))
source(here("scripts", "environmental", "1-time-series_data.R"))
rm(list = ls())
gc()

# Reading and adjusting files
path <- here("data", "processed", "environ", "1 - data raw")
files <- list.files(path, pattern = ".csv", full.names = TRUE)

files_list <- lapply(files, function(x) {
  dt <- fread(x)
  nome1 <- names(dt)[1]
  dt[, (nome1) := dmy_hm(get(nome1), tz = "America/Bahia")]
  return(dt)
})

names(files_list) <- sapply(files_list, function(dt) names(dt)[2])

# Defining directories
output_dir_figs <- here("images", "environ", "1-time_series_plot")
dir.create(output_dir_figs, recursive = TRUE, showWarnings = FALSE)

# Metadata to plot
plot_specs <- tibble(nome = c("airtemp.C_inmet", "CDOM.ppb_simcosta", "chl.ugL_simcosta",
                              "DO.mlL_simcosta", "eastwestcurrentvelocity.ms_simcosta",
                              "eastwestwindvelocity.ms_simcosta", 
                              "northsouthcurrentvelocity.ms_simcosta", 
                              "northsouthwindvelocity.ms_simcosta", "rain.mm_inmet", 
                              "riverdischarge.m3s_votorantim", "sal_laps", "sal_simcosta",
                              "solrad.Kjm2_inmet", "subtidalelevation.m_marinha", 
                              "tidalelevation.m_marinha", "totsealev.m_marinha", 
                              "turb.ntu_simcosta", "wattemp.C_laps", "wattemp.C_simcosta"),
<<<<<<< HEAD
                     ylab = c("Air temperature °C - INMET", "CDOM ppb/QSDE - SiMCosta", 
                              "Clorofila µg/L - SiMCosta", "DO ml/L - SiMCosta", 
                              "East-West cur vel m/s - SiMCosta", 
                              "East-West wind vel m/s - SiMCosta",
                              "North-South cur vel m/s - SiMCosta", 
                              "North-South wind vel m/s - SiMCosta", "Rainfall mm\u00B3 - INMET", 
                              "River Discharge mm\u00B3/s - Votorantim", "Salinidade - LAPS", 
                              "Salinidade - SiMCosta", "Solar Radiation Kj/m² - INMET", 
=======
                     ylab = c("Air temperature °C - INMET", "CDOM ppp/QSDE - SiMCosta", 
                              "Clorofila ug/L - SiMCosta", "DO ml/L - SiMCosta", 
                              "East-West cur vel m/s - SiMCosta", 
                              "East-West wind vel m/s - SiMCosta",
                              "North-South cur vel m/s - SiMCosta", 
                              "North-South wind vel m/s - SiMCosta", "Rainfall mm3 - INMET", 
                              "River Discharge m3/s - Votorantim", "Salinidade - LAPS", 
                              "Salinidade - SiMCosta", "Solar Radiation Kj/M2 - INMET", 
>>>>>>> fc9c083e9a513e94ce0c624be017bc851b2432d9
                              "Subtidal Elevation m - Marinha", "Tidal Elevation m - Marinha", 
                              "Total Sea Elevation m - Marinha", "Turbidity NTU - SiMCosta",
                              "Water Temperature °C - LAPS", "Water Temperature °C - SiMCosta"),
                     filename = c("Air temperature °C - INMET.png", "CDOM ppp-QSDE - SiMCosta.png",
                                  "Clorofila ug-L - SiMCosta.png", "DO ml-L - SiMCosta.png", 
                                  "East-West cur vel m-s - SiMCosta.png", 
                                  "East-West wind vel m-s - SiMCosta.png",
                                  "North-South cur vel m-s - SiMCosta.png", 
                                  "North-South wind vel m-s - SiMCosta.png", 
                                  "Rainfall mm3 - INMET.png", 
                                  "River Discharge m3-s - Votorantim.png", 
                                  "Salinidade - LAPS.png", "Salinidade - SiMCosta.png",
                                  "Solar Radiation Kj-M2 - INMET.png", 
                                  "Subtidal Elevation m - Marinha.png", 
                                  "Tidal Elevation m - Marinha.png", 
                                  "Total Sea Elevation m - Marinha.png", 
                                  "Turbidity NTU - SiMCosta.png", "Water Temperature °C - LAPS.png",
                                  "Water Temperature °C - SiMCosta.png"))


# Plot general parameters
default_axis_t_size <- 22
default_axis_d_size <- 20

# Plot function
plot_func <- function(data, ylab, axis_t_size = default_axis_t_size, 
                      axis_d_size = default_axis_d_size) {
  
  ggplot(data = data) +
    geom_point(aes(x = data[[1]], y = data[[2]]), 
               col = "red", size = .1) +
    geom_line(aes(x = data[[1]], y = data[[2]]), 
              col = "black", linewidth = .5) +
    scale_x_datetime(breaks = "1 month", date_labels = "%Y-%m") +
    labs(x = "Date", y = ylab) +
    theme_test() +
    theme(axis.text.x = element_text(size = axis_d_size, face = "bold", 
                                     family = "Times New Roman", 
                                     angle = 90, hjust = 1, vjust = .5, 
                                     color = "black"),
          axis.text.y = element_text(size = axis_d_size, face = "bold", 
                                     family = "Times New Roman", color = "black"),
          axis.title.x = element_text(size = axis_t_size, face = "bold", 
                                      family = "Times New Roman", color = "black"),
          axis.title.y = element_text(size = axis_t_size, face = "bold", 
                                      family = "Times New Roman", color = "black"))
  
}

# Loop for plot
for (i in seq_len(nrow(plot_specs))) {
  
  nome_var <- plot_specs$nome[i]
  
  if (!nome_var %in% names(files_list)) {
    
    warning(paste("Variável não encontrada:", nome_var))
    next
    
  }
  
  df <- files_list[[nome_var]]
  
  fig <- plot_func(df, ylab = plot_specs$ylab[i])
  
  ggsave(filename = here(output_dir_figs, plot_specs$filename[i]),
         plot = fig, width = 16, height = 7, dpi = "retina")
  
  print(fig)
  
} 
<<<<<<< HEAD

#############################################
#############################################
#############################################

# Plot série temporal com plotly
to_plot <- NULL
for(i in names(files_list_env)) {
  
  to_plot[[i]] <- files_list_env[[i]] |> 
    dplyr::select(cycle, data) |> 
    dplyr::rename(!!i := data)
  print(i)
  
}
df <- reduce(to_plot, full_join, by = "cycle")
rotulos <- c(
  airtemp.C_inmet = "Air temperature (°C)",
  CDOM.ppb_simcosta = "CDOM (ppb)",
  chl.ugL_simcosta   = "Chlorophyll (µg L⁻¹)",
  DO.mlL_simcosta = "DO (mL L⁻¹)",
  eastwestcurrentvelocity.ms_simcosta   = "E-W current (m s⁻¹)",
  eastwestwindvelocity.ms_simcosta = "E-W wind (m s⁻¹)",
  northsouthcurrentvelocity.ms_simcosta = "N-S current (m s⁻¹)",
  northsouthwindvelocity.ms_simcosta   = "N-S wind (m s⁻¹)",
  rain.mm_inmet = "Rainfall (mm\u00B3)",
  riverdischarge.m3s_votorantim   = "River discharge (m³ s⁻¹)",
  sal_simcosta = "Salinity",
  solrad.Kjm2_inmet = "Solar radiation (kJ m⁻²)",
  subtidalelevation.m_marinha   = "Subtidal elevation (m)",
  tidalelevation.m_marinha = "Tidal elevation (m)",
  totsealev.m_marinha = "Total sea level (m)",
  turb.ntu_simcosta = "Turbidity (NTU)",
  wattemp.C_simcosta = "Water temperature (°C)")

head(df)
library(plotly)

ann <- list(list(x = 0.92, y = 0.5,
                 xref = "paper", yref = "paper",
                 text = "E-W wind (m s⁻¹)",
                 showarrow = FALSE, textangle = -90,
                 xanchor = "left", yanchor = "middle",
                 font = list(color = "red", size = 25))#,
            # list(x = 0.99, y = 0.5,
            #      xref = "paper", yref = "paper",
            #      text = "N-S wind (m s⁻¹)",
            #      showarrow = FALSE, textangle = -90,
            #      xanchor = "left", yanchor = "middle",
            #      font = list(color = "blue", size = 25))
)

#––– Começa o plot com ciclo no eixo x –––
fig <- plot_ly(data = df, x = ~cycle)

#––– 1º eixo (yaxis), à esquerda –––
fig <- fig %>%
  add_trace(y = ~northsouthcurrentvelocity.ms_simcosta,
            name = "N-S wind (m s⁻¹)",
            type = "scatter",
            mode = "lines",
            line = list(color = "black")) %>%
  layout(xaxis = list(tickfont = list(color = "black",
                                      size = 20)),
         yaxis = list(title = "N-S wind (m s⁻¹)",
                      linecolor = "black",
                      zeroline = FALSE,
                      showgrid = FALSE,
                      tickfont = list(color = "black",
                                      size = 25),
                      titlefont = list(color = "black", 
                                       size = 25)))

#––– 2º eixo (yaxis2), à direita – posição 0.75 –––
y2 <- list(overlaying = "y",
           side = "right",
           anchor = "free",
           position = 0.90,
           title = "",
           linecolor = "red",
           tickfont = list(color = "red", 
                           size = 25),
           titlefont = list(color = "red"),
           zeroline = FALSE,
           showgrid = FALSE)
fig <- fig %>%
  add_trace(y = ~eastwestwindvelocity.ms_simcosta,
            name = "E-W wind (m s⁻¹)",
            yaxis = "y2",
            type ="scatter",
            mode = "lines",
            line = list(color = "red")) %>%
  layout(yaxis2 = y2)
# 
# #––– 3º eixo (yaxis3), à direita – posição 0.85 –––
# y3 <- list(overlaying = "y",
#            side = "right",
#            anchor = "free",
#            position = 0.95,
#            title = "",
#            linecolor = "blue",
#            tickfont = list(color = "blue",
#                            size = 25),
#            titlefont = list(color = "blue"),
#            zeroline = FALSE,
#            showgrid = FALSE)
# fig <- fig %>%
#   add_trace(y = ~riverdischarge.m3s_votorantim,
#             name = "River discharge (m³ s⁻¹)",
#             yaxis = "y3",
#             type = "scatter",
#             mode = "lines",
#             line = list(color = "blue")) %>%
#   layout(yaxis3 = y3)

# Final adjustments
fig <- fig %>% 
  layout(title = "multiple y-axes example",
         yaxis2 = y2, #yaxis3 = y3,
         xaxis = list(title = '',
                      domain = c(0, .90))) %>%
  layout(plot_bgcolor = 'White',
         autosize = T,
         showlegend = FALSE,
         plot_bgcolor = "white",
         autosize = TRUE,
         margin = list(r = 30),
         annotations = ann,
         xaxis = list(zeroline = FALSE,
                      showgrid = FALSE, 
                      dtick = "M1", 
                      tickformat="%b<br>%Y"))

fig


=======
>>>>>>> fc9c083e9a513e94ce0c624be017bc851b2432d9

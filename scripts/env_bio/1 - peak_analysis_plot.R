library(here)
source(here("scripts", "general", "0-library.R"))

# Leitura dos dados
path_dados <- here("data", "processed", "zooplancton", "3 - data rebuilded")
files <- list.files(path_dados, pattern = ".csv$", full.names = TRUE)
files_list <- lapply(files, fread)
names(files_list) <- tools::file_path_sans_ext(basename(files))


#####
# Leitura dos dados
path_dados <- here("data", "processed", "environ", "3 - data rebuilded")
files <- list.files(path_dados, pattern = ".csv$", full.names = TRUE)
files_list_env <- lapply(files, fread)
names(files_list_env) <- tools::file_path_sans_ext(basename(files))

files_list <- c(files_list, files_list_env)
files_list <- files_list[-c(10, 13, 20)]

df_env <- files_list$solrad.Kjm2_inmet[, c(1, 4:7)] |> 
  dplyr::rename(BF1_env = BF1)
df_pen <- files_list$penilia[, c(1, 4)] |> 
  dplyr::rename(BF1_penilia = BF1)

df <- full_join(df_pen, df_env, by = "cycle") |> 
  drop_na()
df <- df |> 
  mutate(env_anomaly = if_else(BF1_env <= 0, "low", "high") %>% 
           as.factor())

rotulos_var <- c(
  airtemp.C_inmet = "Air temperature (°C)",
  CDOM.ppb_simcosta = "CDOM (ppb)",
  chl.ugL_simcosta   = "Chlorophyll (µg L⁻¹)",
  DO.mlL_simcosta = "DO (mL L⁻¹)",
  eastwestcurrentvelocity.ms_simcosta   = "E‑W current (m s⁻¹)",
  eastwestwindvelocity.ms_simcosta = "E‑W wind (m s⁻¹)",
  northsouthcurrentvelocity.ms_simcosta = "N‑S current (m s⁻¹)",
  northsouthwindvelocity.ms_simcosta   = "N‑S wind (m s⁻¹)",
  rain.mm_inmet = "Rainfall (mm\u00B3)",
  riverdischarge.m3s_votorantim   = "River discharge (m³ s⁻¹)",
  sal_simcosta = "Salinity",
  solrad.Kjm2_inmet = "Solar radiation (kJ m⁻²)",
  subtidalelevation.m_marinha   = "Subtidal elevation (m)",
  tidalelevation.m_marinha = "Tidal elevation (m)",
  totsealev.m_marinha = "Total sea level (m)",
  turb.ntu_simcosta = "Turbidity (NTU)",
  wattemp.C_simcosta = "Water temperature (°C)"
)

periodos <- list(
  list(lab = "18–24 Jan",
       ini = ymd_hms("2021-01-18 02:00:00"),
       fim = ymd_hms("2021-01-24 16:00:00")),
  list(lab = "30 May–03 Jun",
       ini = ymd_hms("2021-05-30 14:00:00"),
       fim = ymd_hms("2021-06-03 23:00:00")),
  list(lab = "24–26 Dec",
       ini = ymd_hms("2021-12-24 04:00:00"),
       fim = ymd_hms("2021-12-26 08:00:00"))
)

## ─────────────────── 2. resposta Penilia  ────────────────────────────────────
df_pen <- as.data.table(files_list$penilia)[, .(cycle, BF1_penilia = BF1)]

## ─────────────────── 3. função geradora  ─────────────────────────────────────
graf_por_pred <- function(pred_dt, pred_nome) {
  
  pred_dt <- as.data.table(pred_dt)[, .(cycle, BF1_env = BF1)]
  dt <- merge(df_pen, pred_dt, by = "cycle", all = FALSE)[
    , env_anomaly := factor(fifelse(BF1_env <= 0, "low", "high"),
                            levels = c("low", "high"))]
  
  xlab_global <- paste(rotulos_var[[pred_nome]], "anomaly")
  
  plots <- lapply(seq_along(periodos), function(i) {
    per  <- periodos[[i]]
    dt_p <- dt[cycle %between% c(per$ini, per$fim)]
    
    ggplot(dt_p,
           aes(x = BF1_env,
               y = BF1_penilia,
               colour = env_anomaly,
               fill   = env_anomaly)) +
      geom_violin(alpha = 0.10, show.legend = TRUE) +
      geom_point (alpha = 0.20, show.legend = TRUE) +
      scale_fill_manual(
        breaks = c("low", "high"),
        values = c(low = "blue", high = "red"),
        name   = "Class",
        drop   = FALSE) +                      # força manter ambos
      scale_colour_manual(values = c(low = "blue", high = "red"),
                          guide  = "none") +
      labs(
        title = per$lab,
        x = if (i == 2) xlab_global else NULL, # X só no painel central
        y = if (i == 1) "Penilia anomaly" else NULL
      ) +
      theme_light(base_size = 11) +
      theme(
        axis.text   = element_text(size = 11, face = "bold", color = "black",
                                   family = "Times New Roman"),
        axis.title  = element_text(size = 18, face = "bold", color = "black",
                                   family = "Times New Roman"),
        plot.title  = element_text(size = 18, face = "bold", color = "black",
                                   family = "Times New Roman"),
        strip.text  = element_text(size = 12, family = "Times New Roman")
      )
  })
  
  wrap_plots(plots, ncol = 3, guides = "collect") +    # junta e deduplica
    plot_annotation(
      title = rotulos_var[[pred_nome]],
      theme = theme(
        plot.title      = element_text(size = 14, face = "bold"),
        legend.position = "right"                      # legenda única, à direita
      )
    )
}
# v2
graf_por_pred <- function(pred_dt, pred_nome) {
  pred_dt <- as.data.table(pred_dt)[, .(cycle, BF1_env = BF1)]
  dt <- merge(df_pen, pred_dt, by = "cycle", all = FALSE)[
    , env_anomaly := factor(fifelse(BF1_env <= 0, "low", "high"),
                            levels = c("low", "high"))]
  
  xlab_global <- paste(rotulos_var[[pred_nome]], "anomaly")
  
  plots <- lapply(seq_along(periodos), function(i) {
    per  <- periodos[[i]]
    dt_p <- dt[cycle %between% c(per$ini - hours(336), 
                                 per$ini)]
    
    dt_p[, pen_anomaly := factor(fifelse(BF1_penilia <= 0, "low", "high"),
                                 levels = c("low", "high"))]
    
    tab <- table(Penilia = dt_p$pen_anomaly,
                 Env     = dt_p$env_anomaly)
    
    tab_completo <- matrix(0, nrow = 2, ncol = 2,
                           dimnames = list(Penilia = c("low", "high"),
                                           Env     = c("low", "high")))
    tab_completo[rownames(tab), colnames(tab)] <- tab
    
    if (any(tab_completo < 5)) {
      test_type <- "Fisher"
      p_valor <- fisher.test(tab_completo)$p.value
    } else {
      test_type <- "\u03C7\u00B2"  # símbolo χ²
      p_valor <- chisq.test(tab_completo, correct = F)$p.value
    }
    
    p_valor_txt <- paste0(" (", test_type, ", p = ",
                          formatC(p_valor, format = "f", digits = 3), ")")
    
    ggplot(dt_p,
           aes(x = BF1_env,
               y = BF1_penilia,
               colour = env_anomaly,
               fill   = env_anomaly)) +
      geom_violin(alpha = 0.10, show.legend = TRUE) +
      geom_point (alpha = 0.20, show.legend = TRUE) +
      scale_fill_manual(
        breaks = c("low", "high"),
        values = c(low = "blue", high = "red"),
        name   = "Class",
        drop   = FALSE) +
      scale_colour_manual(values = c(low = "blue", high = "red"),
                          guide  = "none") +
      labs(
        title = paste0(per$lab, p_valor_txt),
        x = if (i == 2) xlab_global else NULL,
        y = if (i == 1) "Penilia anomaly" else NULL
      ) +
      theme_light(base_size = 11) +
      theme(
        axis.text   = element_text(size = 11, face = "bold", color = "black",
                                   family = "Times New Roman"),
        axis.title  = element_text(size = 14, face = "bold", color = "black",
                                   family = "Times New Roman"),
        plot.title  = element_text(size = 12, face = "bold", color = "black",
                                   family = "Times New Roman"),
        strip.text  = element_text(size = 10, family = "Times New Roman")
      )
  })
  
  wrap_plots(plots, ncol = 3, guides = "collect") +
    plot_annotation(
      title = rotulos_var[[pred_nome]],
      theme = theme(
        plot.title      = element_text(size = 14, face = "bold"),
        legend.position = "right"
      )
    )
}
# V3
graf_por_pred <- function(pred_dt, pred_nome) {
  pred_dt <- as.data.table(pred_dt)[, .(cycle, BF1_env = BF1)]
  dt <- merge(df_pen, pred_dt, by = "cycle", all = FALSE)[
    , env_anomaly := factor(fifelse(BF1_env <= 0, "low", "high"),
                            levels = c("low", "high"))]
  
  rotulo_var_anomalia <- paste(rotulos_var[[pred_nome]], "anomaly")
  
  plots <- lapply(seq_along(periodos), function(i) {
    per <- periodos[[i]]
    
    # Estende o período para o gráfico temporal (14 dias antes)
    dt_p <- dt[cycle %between% c(per$ini - hours(336), 
                                 per$ini)]
    
    # Dados do período principal para violino e teste
    dt_violin <- dt[cycle %between% c(per$ini - hours(336), 
                                      per$ini)]
    dt_violin[, pen_anomaly := factor(fifelse(BF1_penilia <= 0, "low", "high"),
                                      levels = c("low", "high"))]
    
    # Matriz de contingência
    tab <- table(Penilia = dt_violin$pen_anomaly,
                 Env     = dt_violin$env_anomaly)
    
    tab_completo <- matrix(0, nrow = 2, ncol = 2,
                           dimnames = list(Penilia = c("low", "high"),
                                           Env     = c("low", "high")))
    tab_completo[rownames(tab), colnames(tab)] <- tab
    
    # Teste estatístico
    if (any(tab_completo < 5)) {
      test_type <- "Fisher"
      p_valor <- fisher.test(tab_completo)$p.value
    } else {
      test_type <- "\u03C7\u00B2"
      p_valor <- chisq.test(tab_completo, correct = FALSE)$p.value
    }
    
    p_valor_txt <- paste0(" (", test_type, ", p = ",
                          formatC(p_valor, format = "f", digits = 3), ")")
    
    ## Gráfico 1: violino
    g1 <- ggplot(dt_violin,
                 aes(x = BF1_env,
                     y = BF1_penilia,
                     colour = env_anomaly,
                     fill   = env_anomaly)) +
      geom_violin(alpha = 0.10, show.legend = TRUE) +
      geom_point(alpha = 0.20, show.legend = TRUE) +
      scale_fill_manual(
        breaks = c("low", "high"),
        values = c(low = "blue", high = "red"),
        name   = "Class",
        drop   = FALSE) +
      scale_colour_manual(values = c(low = "blue", high = "red"),
                          guide = "none") +
      labs(
        title = paste0(per$lab, p_valor_txt),
        x = if (i == 2) rotulo_var_anomalia else NULL,
        y = if (i == 1) "Penilia anomaly" else NULL
      ) +
      theme_light(base_size = 11) +
      theme(
        axis.text   = element_text(size = 11, face = "bold", color = "black",
                                   family = "Times New Roman"),
        axis.title  = element_text(size = 14, face = "bold", color = "black",
                                   family = "Times New Roman"),
        plot.title  = element_text(size = 12, face = "bold", color = "black",
                                   family = "Times New Roman"),
        strip.text  = element_text(size = 10, family = "Times New Roman")
      )
    
    ## Gráfico 2: linha temporal
    g2 <- ggplot(dt_p, aes(x = cycle, y = BF1_env)) +
      geom_smooth(size = 0.6, method = "loess", n = 500) +
      geom_hline(yintercept = 0, color = "red", linetype = "dashed") +
      labs(
        x = NULL,
        y = if (i == 1) rotulo_var_anomalia else NULL
      ) +
      theme_light(base_size = 11) +
      theme(
        axis.text   = element_text(size = 10, face = "bold", color = "black",
                                   family = "Times New Roman"),
        axis.title  = element_text(size = 12, face = "bold", color = "black",
                                   family = "Times New Roman"),
        plot.title  = element_text(size = 10, face = "bold", color = "black",
                                   family = "Times New Roman")
      )
    
    g1 / g2  # empilha verticalmente
  })
  
  wrap_plots(plots, ncol = 3, guides = "collect") +
    plot_annotation(
      theme = theme(
        plot.title      = element_text(size = 14, face = "bold"),
        legend.position = "right"
      )
    )
}
# V4
graf_por_pred <- function(pred_dt, pred_nome) {
  pred_dt <- as.data.table(pred_dt)[, .(cycle, BF1_env = BF1)]
  dt <- merge(df_pen, pred_dt, by = "cycle", all = FALSE)[
    , env_anomaly := factor(fifelse(BF1_env <= 0, "low", "high"),
                            levels = c("low", "high"))]
  
  rotulo_var_anomalia <- paste(rotulos_var[[pred_nome]], "anomaly")
  
  plots <- lapply(seq_along(periodos), function(i) {
    per <- periodos[[i]]
    
    ## ─── dados temporais ──────────────────────────────────────────────
    dt_p      <- dt[cycle %between% c(per$ini - hours(360), per$ini)]
    dt_violin <- dt[cycle %between% c(per$ini - hours(360), per$ini)]
    dt_violin[, pen_anomaly := factor(fifelse(BF1_penilia <= 0, "low", "high"),
                                      levels = c("low", "high"))]
    
    ## ─── teste estatístico ────────────────────────────────────────────
    tab <- table(dt_violin$pen_anomaly, dt_violin$env_anomaly)
    tab_full <- matrix(0, 2, 2,
                       dimnames = list(Penilia = c("low", "high"),
                                       Env     = c("low", "high")))
    tab_full[rownames(tab), colnames(tab)] <- tab
    if (any(tab_full < 5)) {
      test_type <- "Fisher"
      p_valor   <- fisher.test(tab_full)$p.value
    } else {
      test_type <- "\u03C7\u00B2"
      p_valor   <- chisq.test(tab_full, correct = FALSE)$p.value
    }
    p_txt <- paste0(" (", test_type, ", p = ",
                    formatC(p_valor, format = "f", digits = 3), ")")
    
    ## ─── gráfico 1: violino ───────────────────────────────────────────
    g1 <- ggplot(dt_violin,
                 aes(x = BF1_env, y = BF1_penilia,
                     colour = env_anomaly, fill = env_anomaly)) +
      geom_violin(alpha = .10, show.legend = TRUE) +
      geom_point (alpha = .20, show.legend = TRUE) +
      scale_fill_manual(breaks = c("low","high"),
                        values = c(low = "blue", high = "red"),
                        name   = "Class", drop = FALSE) +
      scale_colour_manual(values = c(low = "blue", high = "red"),
                          guide = "none") +
      labs(
        title = paste0(per$lab, p_txt),
        x = if (i == 2) rotulo_var_anomalia else NULL,
        y = if (i == 1) "Penilia anomaly" else NULL
      ) +
      theme_light(base_size = 11) +
      theme(
        axis.text  = element_text(size = 11, face = "bold", colour = "black",
                                  family = "Times New Roman"),
        axis.title = element_text(size = 14, face = "bold", colour = "black",
                                  family = "Times New Roman"),
        plot.title = element_text(size = 12, face = "bold", colour = "black",
                                  family = "Times New Roman")
      )
    
    ## ─── gráfico 2: tendência temporal ────────────────────────────────
    esc_env  <- sd(dt_p$BF1_env,      na.rm = TRUE)
    esc_pen  <- sd(dt_p$BF1_penilia,  na.rm = TRUE)
    f_esc    <- esc_env/esc_pen       # fator de escala
    
    g2 <- ggplot(dt_p, aes(x = cycle)) +
      geom_smooth(aes(y = BF1_env),
                  method = "loess", se = TRUE, n = 100,
                  colour = "black", size = .9) +
      geom_smooth(aes(y = BF1_penilia * f_esc),
                  method = "loess", se = TRUE, n = 100,
                  colour = "black", linetype = "dashed", size = .9) +
      geom_hline(yintercept = 0, colour = "red", linetype = "dashed") +
      scale_y_continuous(
        name     = if (i == 1) rotulo_var_anomalia else NULL,
        sec.axis = sec_axis(~ . / f_esc,
                            name = if (i == 3) "Penilia anomaly" else NULL)
      ) +
      labs(x = NULL) +
      theme_light(base_size = 11) +
      theme(
        axis.text          = element_text(size = 10, face = "bold", colour = "black",
                                          family = "Times New Roman"),
        axis.title.y.left  = element_text(size = 12, face = "bold", colour = "black",
                                          family = "Times New Roman"),
        axis.title.y.right = element_text(size = 12, face = "bold", colour = "black",
                                          family = "Times New Roman")
      )
    
    g1 / g2
  })
  
  wrap_plots(plots, ncol = 3, guides = "collect") +
    plot_annotation(
      theme = theme(plot.title = element_text(size = 14, face = "bold"),
                    legend.position = "right")
    )
}
# V5
graf_por_pred <- function(pred_dt, pred_nome) {
  
  ## 0. Pré-processamento --------------------------------------------------------
  pred_dt <- as.data.table(pred_dt)[, .(cycle, BF1_env = BF1)]
  dt <- merge(df_pen, pred_dt, by = "cycle", all = FALSE)[
    , env_anomaly := factor(fifelse(BF1_env <= 0, "low", "high"),
                            levels = c("low", "high"))]
  
  rotulo_var_anomalia <- paste(rotulos_var[[pred_nome]], "anomaly")
  
  ## 1. Gráficos por período -----------------------------------------------------
  plots <- lapply(seq_along(periodos), function(i) {
    
    per <- periodos[[i]]
    
    # 1.1 Janela temporal (-360 h até o instante ‘ini’)
    dt_p      <- dt[cycle %between% c(per$ini - hours(360), per$ini)]
    dt_violin <- copy(dt_p)
    
    # 1.2 Variáveis auxiliares
    dt_violin[, pen_anomaly := factor(fifelse(BF1_penilia <= 0, "low", "high"),
                                      levels = c("low", "high"))]
    
    # ---- AQUI: posição x central (média) por grupo ------------
    dt_violin[, x_center := mean(BF1_env, na.rm = TRUE), by = env_anomaly]
    
    # 1.3 Teste de independência
    tab <- table(dt_violin$pen_anomaly, dt_violin$env_anomaly)
    tab_full <- matrix(0, 2, 2,
                       dimnames = list(Penilia = c("low", "high"),
                                       Env     = c("low", "high")))
    tab_full[rownames(tab), colnames(tab)] <- tab
    if (any(tab_full < 5)) {
      test_type <- "Fisher"
      p_valor   <- fisher.test(tab_full)$p.value
    } else {
      test_type <- "\u03C7\u00B2"
      p_valor   <- chisq.test(tab_full, correct = FALSE)$p.value
    }
    p_txt <- paste0(" (", test_type, ", p = ",
                    formatC(p_valor, format = "f", digits = 3), ")")
    
    # 1.4 GRÁFICO 1 ─ Violin centralizado / pontos em x contínuo
    g1 <- ggplot(dt_violin) +
      ## violino centralizado
      geom_violin(
        aes(x = x_center,
            y = BF1_penilia,
            colour = env_anomaly,
            fill   = env_anomaly),
        alpha = .10, show.legend = TRUE
      ) +
      ## pontos na posição real de BF1_env
      geom_point(
        aes(x = BF1_env,
            y = BF1_penilia,
            colour = env_anomaly),
        alpha = .20, show.legend = FALSE
      ) +
      scale_fill_manual(
        breaks = c("low","high"),
        values = c(low = "blue", high = "red"),
        name   = "Class", drop = FALSE
      ) +
      scale_colour_manual(
        values = c(low = "blue", high = "red"),
        guide  = "none"
      ) +
      labs(
        title = paste0(per$lab, p_txt),
        x = if (i == 2) rotulo_var_anomalia else NULL,
        y = if (i == 1) "Penilia anomaly"  else NULL
      ) +
      theme_light(base_size = 11) +
      theme(
        axis.text  = element_text(size = 11, face = "bold", colour = "black",
                                  family = "Times New Roman"),
        axis.title = element_text(size = 14, face = "bold", colour = "black",
                                  family = "Times New Roman"),
        plot.title = element_text(size = 12, face = "bold", colour = "black",
                                  family = "Times New Roman")
      )
    
    # 1.5 GRÁFICO 2 ─ Tendência temporal
    esc_env <- sd(dt_p$BF1_env,     na.rm = TRUE)
    esc_pen <- sd(dt_p$BF1_penilia, na.rm = TRUE)
    f_esc   <- esc_env / esc_pen      # fator de escala
    
    g2 <- ggplot(dt_p, aes(x = cycle)) +
      geom_smooth(aes(y = BF1_env),
                  method = "loess", se = TRUE, n = 100,
                  colour = "black", size = .9) +
      geom_smooth(aes(y = BF1_penilia * f_esc),
                  method = "loess", se = TRUE, n = 100,
                  colour = "black", linetype = "dashed", size = .9) +
      geom_hline(yintercept = 0, colour = "red", linetype = "dashed") +
      scale_y_continuous(
        name     = if (i == 1) rotulo_var_anomalia else NULL,
        sec.axis = sec_axis(~ . / f_esc,
                            name = if (i == 3) "Penilia anomaly" else NULL)
      ) +
      labs(x = NULL) +
      theme_light(base_size = 11) +
      theme(
        axis.text          = element_text(size = 10, face = "bold", colour = "black",
                                          family = "Times New Roman"),
        axis.title.y.left  = element_text(size = 12, face = "bold", colour = "black",
                                          family = "Times New Roman"),
        axis.title.y.right = element_text(size = 12, face = "bold", colour = "black",
                                          family = "Times New Roman")
      )
    
    # 1.6 Combina os dois gráficos do período
    g1 / g2
  })
  
  ## 2. Painel final ------------------------------------------------------------
  wrap_plots(plots, ncol = 3, guides = "collect") +
    plot_annotation(
      theme = theme(plot.title  = element_text(size = 14, face = "bold"),
                    legend.position = "right")
    )
}
#V6
graf_por_pred <- function(pred_dt, pred_nome) {
  
  ## 0. Pré-processamento --------------------------------------------------------
  pred_dt <- as.data.table(pred_dt)[, .(cycle, BF1_env = BF1)]
  dt <- merge(df_pen, pred_dt, by = "cycle", all = FALSE)[
    , env_anomaly := factor(fifelse(BF1_env <= 0, "low", "high"),
                            levels = c("low", "high"))]
  
  rotulo_var_anomalia <- paste(rotulos_var[[pred_nome]], "anomaly")
  
  ## 1. Gráficos por período -----------------------------------------------------
  plots <- lapply(seq_along(periodos), function(i) {
    
    per <- periodos[[i]]
    
    # 1.1 Janela temporal (-360 h até o instante ‘ini’)
    dt_p      <- dt[cycle %between% c(per$ini - hours(360), per$ini)]
    # dt_p      <- dt[cycle %between% c(per$ini, per$fim)]
    dt_period <- copy(dt_p)
    
    # 1.2 Classificação de Penilia
    dt_period[, pen_anomaly := factor(fifelse(BF1_penilia <= 0, "low", "high"),
                                      levels = c("low", "high"))]
    
    # 1.3 Estatísticas resumidas por combinação Env × Pen
    dt_sum <- dt_period[, .(
      mean_env = mean(BF1_env,      na.rm = TRUE),
      se_env   = sd(  BF1_env,      na.rm = TRUE) / sqrt(.N),
      mean_pen = mean(BF1_penilia,  na.rm = TRUE),
      se_pen   = sd(  BF1_penilia,  na.rm = TRUE) / sqrt(.N),
      N        = .N
    ), by = .(env_anomaly, pen_anomaly)]
    
    # 1.4 Teste de independência
    tab <- table(dt_period$pen_anomaly, dt_period$env_anomaly)
    tab_full <- matrix(0, 2, 2,
                       dimnames = list(Penilia = c("low", "high"),
                                       Env     = c("low", "high")))
    tab_full[rownames(tab), colnames(tab)] <- tab
    if (any(tab_full < 5)) {
      test_type <- "Fisher"
      p_valor   <- fisher.test(tab_full)$p.value
    } else {
      test_type <- "\u03C7\u00B2"
      p_valor   <- chisq.test(tab_full, correct = FALSE)$p.value
    }
    p_txt <- paste0(" (", test_type, ", p = ",
                    formatC(p_valor, format = "f", digits = 3), ")")
    
    # 1.5 GRÁFICO 1 ─ Pontos de média ± SE e N
    g1 <- ggplot(dt_sum,
                 aes(x = mean_env,
                     y = mean_pen,
                     colour = env_anomaly,
                     shape  = pen_anomaly)) +
      
      # Erro padrão vertical (Penilia)
      geom_errorbar(aes(ymin = mean_pen - se_pen,
                        ymax = mean_pen + se_pen),
                    width = 0) +
      
      # Erro padrão horizontal (variável ambiental)
      ggstance::geom_errorbarh(aes(xmin = mean_env - se_env,
                                   xmax = mean_env + se_env),
                               height = 0) +
      
      # Pontos da média
      geom_point(size = 3) +
      
      # Rótulo com N acima do ponto
      geom_text(aes(label = N),
                vjust = -1.2, hjust = 0.5,
                colour = "black", size = 3, show.legend = FALSE) +
      
      scale_colour_manual(values = c(low = "blue", high = "red"),
                          name = "Env anomaly",
                          labels = c("≤ 0", "> 0")) +
      scale_shape_manual(values = c(low = 16, high = 17),
                         name = "Penilia anomaly",
                         labels = c("≤ 0", "> 0")) +
      
      labs(
        title = paste0(per$lab, p_txt),
        x = if (i == 2) rotulo_var_anomalia else NULL,
        y = if (i == 1) "Penilia anomaly" else NULL
      ) +
      
      theme_light(base_size = 11) +
      theme(
        axis.text  = element_text(size = 11, face = "bold", colour = "black",
                                  family = "Times New Roman"),
        axis.title = element_text(size = 14, face = "bold", colour = "black",
                                  family = "Times New Roman"),
        plot.title = element_text(size = 12, face = "bold", colour = "black",
                                  family = "Times New Roman")
      )
    
    # 1.6 GRÁFICO 2 ─ Tendência temporal (inalterado)
    esc_env <- sd(dt_p$BF1_env,     na.rm = TRUE)
    esc_pen <- sd(dt_p$BF1_penilia, na.rm = TRUE)
    f_esc   <- esc_env / esc_pen      # fator de escala
    
    g2 <- ggplot(dt_p, aes(x = cycle)) +
      geom_smooth(aes(y = BF1_env),
                  method = "loess", se = TRUE, n = 100,
                  colour = "black", size = .9) +
      geom_smooth(aes(y = BF1_penilia * f_esc),
                  method = "loess", se = TRUE, n = 100,
                  colour = "black", linetype = "dashed", size = .9) +
      geom_hline(yintercept = 0, colour = "red", linetype = "dashed") +
      scale_y_continuous(
        name     = if (i == 1) rotulo_var_anomalia else NULL,
        sec.axis = sec_axis(~ . / f_esc,
                            name = if (i == 3) "Penilia anomaly" else NULL)
      ) +
      labs(x = NULL) +
      theme_light(base_size = 11) +
      theme(
        axis.text          = element_text(size = 10, face = "bold", colour = "black",
                                          family = "Times New Roman"),
        axis.title.y.left  = element_text(size = 12, face = "bold", colour = "black",
                                          family = "Times New Roman"),
        axis.title.y.right = element_text(size = 12, face = "bold", colour = "black",
                                          family = "Times New Roman")
      )
    
    # 1.7 Combina os dois gráficos do período
    g1 / g2
  })
  
  ## 2. Painel final ------------------------------------------------------------
  wrap_plots(plots, ncol = 3, guides = "collect") +
    plot_annotation(
      theme = theme(plot.title     = element_text(size = 14, face = "bold"),
                    legend.position = "right")
    )
}
## ─────────────────── 4. loop sobre os 17 preditores  ─────────────
preditores   <- files_list[-1]          # remove penilia
nomes_pred   <- names(preditores)

lista_plots <- mapply(graf_por_pred,
                      pred_dt   = preditores,
                      pred_nome = names(preditores),
                      SIMPLIFY  = FALSE)

## ─────────────────── 5. exemplo de visualização  ────────────
lista_plots[[17]]

path_to_save <- here("images", 
                     "env_bio", 
                     "2 - low_high_anomaly_BF1")
walk2(lista_plots, names(lista_plots),
      \(plt, nm) {
        ggsave(
          filename = file.path(path_to_save, paste0(nm, ".png")),
          plot = plt,
          width = 12, height = 6, dpi = 300)})


##########################################################
# matriz_contagem_por_pred <- function(pred_dt, 
#                                      pred_nome, 
#                                      usar_mediana = FALSE, 
#                                      diagnostico = FALSE) {
#   
#   pred_dt <- as.data.table(pred_dt)[, .(cycle, BF1_env = BF1)]
#   dt <- merge(df_pen, pred_dt, by = "cycle", all = FALSE)
#   
#   if (diagnostico) {
#     message("Número de observações após merge: ", nrow(dt))
#   }
#   
#   # Classificação em low/high
#   if (usar_mediana) {
#     med_pen <- median(dt$BF1_penilia, na.rm = TRUE)
#     med_env <- median(dt$BF1_env, na.rm = TRUE)
#     
#     dt[, pen_anomaly := factor(fifelse(BF1_penilia <= med_pen, "low", "high"),
#                                levels = c("low", "high"))]
#     dt[, env_anomaly := factor(fifelse(BF1_env <= med_env, "low", "high"),
#                                levels = c("low", "high"))]
#     
#     if (diagnostico) {
#       message("Mediana Penilia: ", round(med_pen, 3),
#               " | Mediana Env (", pred_nome, "): ", round(med_env, 3))
#     }
#   } else {
#     dt[, pen_anomaly := factor(fifelse(BF1_penilia <= 0, "low", "high"),
#                                levels = c("low", "high"))]
#     dt[, env_anomaly := factor(fifelse(BF1_env <= 0, "low", "high"),
#                                levels = c("low", "high"))]
#   }
#   
#   # Nome mais bonito da variável ambiental (ou usa o código, se não achar)
#   nome_coluna_env <- if (pred_nome %in% names(rotulos_var)) {
#     rotulos_var[[pred_nome]]
#   } else {
#     pred_nome
#   }
#   
#   # Loop por período
#   lista_matrizes <- lapply(periodos, function(per) {
#     dt_p <- dt[cycle %between% c(per$ini - hours(360), 
#                                  per$ini)]
#     
#     if (diagnostico) {
#       message("Período: ", per$lab, " | N = ", nrow(dt_p))
#     }
#     
#     # Tabela de contingência
#     tab <- table(
#       Penilia = dt_p$pen_anomaly,
#       Env     = dt_p$env_anomaly
#     )
#     
#     # Cria estrutura 2x2 vazia se faltar níveis
#     tab_completo <- matrix(0, nrow = 2, ncol = 2,
#                            dimnames = list(Penilia = c("low", "high"),
#                                            Temp = c("low", "high")))
#     tab_completo[rownames(tab), colnames(tab)] <- tab
#     
#     # Corrige o nome da segunda dimensão (coluna)
#     dimnames(tab_completo)[[2]] <- c("low", "high")
#     names(dimnames(tab_completo))[2] <- nome_coluna_env
#     
#     return(tab_completo)
#   })
#   
#   names(lista_matrizes) <- sapply(periodos, function(p) p$lab)
#   return(lista_matrizes)
# }
# matrizes_resultado <- matriz_contagem_por_pred(
#   pred_dt = files_list$solrad.Kjm2_inmet,
#   pred_nome = "solrad.Kjm2_inmet",
#   diagnostico = FALSE
# )
# 
# # Visualiza as três matrizes
# matrizes_resultado[[2]]
# round(prop.table(matrizes_resultado[[1]]), 2)
# chisq.test(x = matrizes_resultado[[1]], correct = F)

########################################################
matriz_media_por_pred <- function(pred_dt, 
                                  pred_nome, 
                                  usar_mediana = FALSE, 
                                  media_de = c("penilia", "ambiental"),
                                  tipo_estatistica = c("media", "sd", "se"),
                                  diagnostico = FALSE) {
  
  media_de <- match.arg(media_de)
  tipo_estatistica <- match.arg(tipo_estatistica)
  
  pred_dt <- as.data.table(pred_dt)[, .(cycle, BF1_env = BF1)]
  dt <- merge(df_pen, pred_dt, by = "cycle", all = FALSE)
  
  if (diagnostico) {
    message("Número de observações após merge: ", nrow(dt))
  }
  
  # Classificação em low/high
  if (usar_mediana) {
    med_pen <- median(dt$BF1_penilia, na.rm = TRUE)
    med_env <- median(dt$BF1_env, na.rm = TRUE)
    
    dt[, pen_anomaly := factor(fifelse(BF1_penilia <= med_pen, "low", "high"),
                               levels = c("low", "high"))]
    dt[, env_anomaly := factor(fifelse(BF1_env <= med_env, "low", "high"),
                               levels = c("low", "high"))]
    
    if (diagnostico) {
      message("Mediana Penilia: ", round(med_pen, 3),
              " | Mediana Env (", pred_nome, "): ", round(med_env, 3))
    }
  } else {
    dt[, pen_anomaly := factor(fifelse(BF1_penilia <= 0, "low", "high"),
                               levels = c("low", "high"))]
    dt[, env_anomaly := factor(fifelse(BF1_env <= 0, "low", "high"),
                               levels = c("low", "high"))]
  }
  
  nome_coluna_env <- if (pred_nome %in% names(rotulos_var)) {
    rotulos_var[[pred_nome]]
  } else {
    pred_nome
  }
  
  lista_matrizes <- lapply(periodos, function(per) {
    dt_p <- dt[cycle %between% c(per$ini - hours(360), per$ini)]
    
    if (diagnostico) {
      message("Período: ", per$lab, " | N = ", nrow(dt_p))
    }
    
    # Define valor base
    valor_para_media <- switch(media_de,
                               "penilia"   = dt_p$BF1_penilia,
                               "ambiental" = dt_p$BF1_env)
    dt_p[, valor_media := valor_para_media]
    
    # Cálculo estatístico
    tab_estat <- dt_p[, .(
      valor = switch(tipo_estatistica,
                     "media" = mean(valor_media, na.rm = TRUE),
                     "sd"    = sd(valor_media, na.rm = TRUE),
                     "se"    = sd(valor_media, na.rm = TRUE) / sqrt(.N))
    ), by = .(pen_anomaly, env_anomaly)]
    
    matriz_resultado <- matrix(NA_real_, nrow = 2, ncol = 2,
                               dimnames = list(Penilia = c("low", "high"),
                                               Temp = c("low", "high")))
    
    for (i in seq_len(nrow(tab_estat))) {
      linha <- tab_estat[i]
      matriz_resultado[linha$pen_anomaly, linha$env_anomaly] <- linha$valor
    }
    
    dimnames(matriz_resultado)[[2]] <- c("low", "high")
    names(dimnames(matriz_resultado))[2] <- nome_coluna_env
    
    return(matriz_resultado)
  })
  
  names(lista_matrizes) <- sapply(periodos, function(p) p$lab)
  return(lista_matrizes)
}

# Para a média (comportamento padrão anterior)
matrizes_media <- matriz_media_por_pred(
  pred_dt = files_list$solrad.Kjm2_inmet,
  pred_nome = "solrad.Kjm2_inmet",
  media_de = "penilia",
  tipo_estatistica = "media"
)
matrizes_se <- matriz_media_por_pred(
  pred_dt = files_list$solrad.Kjm2_inmet,
  pred_nome = "solrad.Kjm2_inmet",
  media_de = "penilia",
  tipo_estatistica = "se"
)
matrizes_media[[1]]
matrizes_se[[1]]

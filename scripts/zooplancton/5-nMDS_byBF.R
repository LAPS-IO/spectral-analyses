# Packages
# Loading packages used run the analysis
library(here)
source(here("scripts", "general", "0-library.R"))
source(here("scripts", "environmental", "3-periodicities_plot.R"))
gc()
theme_set(theme_classic(base_family = "Times New Roman"))

# Leitura dos dados
path_dados <- here("data", "processed", "zooplancton", "3 - data rebuilded")
files <- list.files(path_dados, pattern = ".csv$", full.names = TRUE)
files_list <- lapply(files, fread)
names(files_list) <- tools::file_path_sans_ext(basename(files))

# Função para extrair bandas
extrair_bandas <- function(files_list, 
                           bandas = c("BF1", "BF2", "BF3", "BF4")) {
  
  resultado <- list()
  
  for (banda in bandas) {
    
    banda_list <- list()
    for (nome in names(files_list)) {
      
      df <- as_tibble(files_list[[nome]])
      if (all(c("cycle", banda) %in% names(df))) {
        banda_list[[nome]] <- df %>%
          select(cycle, !!sym(banda)) %>%
          rename(!!nome := !!sym(banda))
        
      }
      
    }
    
    resultado[[banda]] <- reduce(banda_list, left_join, by = "cycle")
    
  }
  
  return(resultado)
  
}

# Aplicar extração
bandas_extraidas <- extrair_bandas(files_list)

p5 <- ggplot() +
  geom_smooth(data = bandas_extraidas$BF1,method="loess", span=0.3,
              aes(x = hour(cycle), y = penilia)) +
  labs(title = "todo período") +
  theme_light()
p6 <- ggplot(data = bandas_extraidas$BF1 |> 
         group_by(cycle = hour(cycle)) |> 
         summarise(media = mean(penilia, na.rm = T),
                   desvio = sd(penilia, na.rm = T))) +
  geom_ribbon(aes(x = cycle, 
                  ymin = media-desvio, 
                  ymax = media+desvio), fill = "gray", alpha = 0.6) +
  geom_line(aes(x = cycle, y = media), color = "darkred", linewidth = 1) +
  labs(title = "todo período") +
  theme_light()

df <- bandas_extraidas$BF1
periodos <- list(
  list(lab = "18–24 Jan",
       ini = ymd_hms("2021-01-18 02:00:00"),
       fim = ymd_hms("2021-01-24 16:00:00")),
  list(lab = "30 May–03 Jun",
       ini = ymd_hms("2021-05-30 14:00:00"),
       fim = ymd_hms("2021-06-03 23:00:00")),
  list(lab = "24–26 Dec",
       ini = ymd_hms("2021-12-24 04:00:00"),
       fim = ymd_hms("2021-12-26 08:00:00"))
)

# Identifica os ciclos dentro dos períodos
df_dentro <- bind_rows(lapply(periodos, function(per) {
    df %>% filter(cycle >= per$ini & cycle <= per$fim)
  }))

# Dados fora dos períodos
df_fora <- anti_join(df, df_dentro, by = "cycle")

# Gráfico 1: Dentro dos períodos
p1 <- ggplot(df_dentro, aes(x = hour(cycle), y = penilia)) +
  geom_smooth(color = "black",method="loess", span=0.3) +
  theme_light()

# Gráfico 2: Fora dos períodos
p2 <- ggplot(df_fora, aes(x = hour(cycle), y = penilia)) +
  geom_smooth(color = "darkred",method="loess", span=0.3) +
  theme_light() +
  ggtitle("Apenas ciclos FORA dos períodos selecionados")

resumo_sd <- function(data) {
  data %>%
    mutate(hora = hour(cycle)) %>%
    group_by(hora) %>%
    summarise(
      media = mean(penilia, na.rm = TRUE),
      sd    = sd(penilia, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    mutate(
      ymin = media - sd,
      ymax = media + sd
    )
}

df_dentro_sum <- resumo_sd(df_dentro)
df_fora_sum   <- resumo_sd(df_fora)

# 3. Gráfico dentro dos períodos
p3 <- ggplot(df_dentro_sum, aes(x = hora, y = media)) +
  geom_line(color = "blue", size = 1) +
  geom_ribbon(aes(ymin = ymin, ymax = ymax), alpha = 0.2, fill = "blue") +
  theme_light() +
  labs(title = "Ciclos DENTRO dos períodos",
       x = "Hora do dia", y = "Penilia (média ± SD)")

# 4. Gráfico fora dos períodos
p4 <- ggplot(df_fora_sum, aes(x = hora, y = media)) +
  geom_line(color = "darkred", size = 1) +
  geom_ribbon(aes(ymin = ymin, ymax = ymax), alpha = 0.2, fill = "darkred") +
  theme_light() +
  labs(title = "Ciclos FORA dos períodos",
       x = "Hora do dia", y = "Penilia (média ± SD)")
(p1+p3)/(p2+p4)/(p5+p6)

####################################
##########################################################################
#  FUNÇÕES GENÉRICAS
##########################################################################

## 1.  Constrói matriz, roda nMDS e devolve lista com tudo
as_estacao <- function(m) {
  dplyr::case_when(
    m %in%  1:3 ~ "Summer",
    m %in%  4:6 ~ "Autumn",
    m %in%  7:9 ~ "Winter",
    TRUE        ~ "Spring"
  )
}
## ─────────────────────────────────────────────────────────────
## 1.  do_nmds()  (INALTERADA – só muda o nome das estações)
## ─────────────────────────────────────────────────────────────
do_nmds <- function(df, horas_bloc) {
  
  bloc_var <- paste0("group", horas_bloc)   # ex.: group53, group312 …
  n_repl   <- horas_bloc                    # réplicas dentro do bloco
  
  # 1) adiciona colunas bloco, réplica, mês, estação
  df2 <- df |>
    dplyr::arrange(cycle) |>
    dplyr::mutate(
      row_id      = dplyr::row_number(),
      !!bloc_var  := ((row_id - 1) %/% horas_bloc) + 1,
      replica     = factor(((row_id - 1) %% horas_bloc) + 1,
                           levels = 1:n_repl),
      mes         = lubridate::month(cycle),
      estac       = as_estacao(mes)
    )
  
  # 2) matriz  réplicas × blocos
  mat <- df2 |>
    dplyr::select(replica, !!rlang::sym(bloc_var), penilia) |>
    tidyr::pivot_wider(id_cols = replica,
                       names_from  = !!rlang::sym(bloc_var),
                       values_from = penilia,
                       values_fill = 0) |>
    dplyr::select(-replica) |>
    as.matrix()
  
  # 3) shift ≥ 0  p/ Bray–Curtis
  K         <- if (min(mat) < 0) abs(min(mat)) + 1e-9 else 0
  mat_shift <- mat + K
  
  # 4) nMDS
  nmds_obj <- vegan::metaMDS(t(mat_shift),
                             distance = "bray",
                             k        = 2,
                             trymax   = 100)
  
  # 5) mês-ano dominante
  mes_dom <- df2 |>
    dplyr::mutate(ym = format(cycle, "%Y-%m")) |>
    dplyr::count(!!rlang::sym(bloc_var), ym) |>
    dplyr::slice_max(n, by = !!rlang::sym(bloc_var), with_ties = FALSE) |>
    dplyr::rename(grupo = !!rlang::sym(bloc_var),
                  mes_dom = ym)
  
  # 6) estação dominante
  est_dom <- df2 |>
    dplyr::count(!!rlang::sym(bloc_var), estac) |>
    dplyr::slice_max(n, by = !!rlang::sym(bloc_var), with_ties = FALSE) |>
    dplyr::rename(grupo = !!rlang::sym(bloc_var),
                  est_dom = estac)
  
  # 7) scores + junções
  scores <- as.data.frame(vegan::scores(nmds_obj, display = "sites")) |>
    dplyr::mutate(grupo = dplyr::row_number()) |>
    dplyr::left_join(mes_dom, by = "grupo") |>
    dplyr::left_join(est_dom, by = "grupo") |>
    dplyr::mutate(est_dom = factor(est_dom,
                                   levels = c("Summer","Autumn",
                                              "Winter","Spring")))
  
  # 8) retorno
  list(
    df_long  = df2,
    scores   = scores,
    bloc_var = bloc_var,
    stress   = nmds_obj$stress,
    nmds     = nmds_obj
  )
}
## ─────────────────────────────────────────────────────────────
## 2.  plot_nmds()  (cores = estação; rótulo = mês-ano)
## ─────────────────────────────────────────────────────────────
plot_nmds <- function(scores, stress, title_txt) {
  
  scores <- dplyr::arrange(scores, grupo)      # ordem temporal
  
  pal_est <- c(Summer = "red",
               Autumn = "orange",
               Winter = "blue",
               Spring = "pink")
  
  ggplot(scores, aes(NMDS1, NMDS2, colour = est_dom)) +
    geom_point(size = 3, alpha = .9) +
    ggrepel::geom_text_repel(aes(label = mes_dom), color = "black",
                             size = 3, max.overlaps = 30,
                             box.padding = .3, point.padding = .2) +
    scale_colour_manual(values = pal_est, name = "Season") +
    labs(title    = title_txt,
         subtitle = paste0("Stress = ", round(stress, 3)),
         x = "nMDS1", y = "nMDS2") +
    theme_light() +
    theme(axis.text.x = element_text(size = 13, face = "bold", 
                                     family = "Times New Roman", 
                                     color = "black"),
          axis.text.y = element_text(size = 13, face = "bold", 
                                     family = "Times New Roman", 
                                     color = "black"),
          axis.title.x = element_text(size = 17, face = "bold", 
                                      family = "Times New Roman", 
                                      color = "black"),
          axis.title.y = element_text(size = 17, face = "bold", 
                                      family = "Times New Roman", 
                                      color = "black"),
          title = element_text(size = 17, face = "bold", 
                               family = "Times New Roman", 
                               color = "black"), 
          legend.text = element_text(size = 15, face = "bold", 
                                     family = "Times New Roman", 
                                     color = "black"))
}

##########################################################################
# 3.  LOOP SOBRE AS QUATRO ESCALAS
##########################################################################
data_list <- list(list(name = "53 hours",  df = bandas_extraidas$BF1, hrs = 53),
                  list(name = "13 days",  df = bandas_extraidas$BF2, hrs = 13*24),
                  list(name = "15 days",  df = bandas_extraidas$BF3, hrs = 15*24),
                  list(name = "30 days",  df = bandas_extraidas$BF4, hrs = 30*24))

results <- purrr::map(data_list, ~{
  out <- do_nmds(.x$df, .x$hrs)
  
  titulo <- paste0("nMDS – frequency band ", .x$name)
  out$plot <- plot_nmds(out$scores, out$stress, titulo)
  
  out                      # devolve lista enriquecida
})

## Exemplos de acesso
results[[1]]$plot
results[[2]]$plot
results[[3]]$plot
results[[4]]$plot

## 3.  Blocos mais distantes
get_extremes <- function(band, n = 6) {
  if (!"results" %in% ls(.GlobalEnv))
    stop("A lista 'results' não está no ambiente global.")
  
  # 1. converte 'band' → índice (1–4)
  idx <- switch(tolower(as.character(band)),
                "1" = 1, "53h" = 1, "53"  = 1,
                "2" = 2, "13d" = 2, "13"  = 2,
                "3" = 3, "15d" = 3, "15"  = 3,
                "4" = 4, "30d" = 4, "30"  = 4,
                NA)
  if (is.na(idx) || idx > length(results))
    stop("Valor de 'band' inválido. Use 1–4, '53h', '13d', '15d' ou '30d'.")
  
  # 2. extrai objetos da banda escolhida
  obj       <- results[[idx]]
  scores    <- obj$scores         # coordenadas nMDS
  df_long   <- obj$df_long        # série horária
  bloc_var  <- obj$bloc_var       # group53 / group312 …
  
  # 3. distância de cada ponto ao centro (medianas)
  cx <- median(scores$NMDS1)
  cy <- median(scores$NMDS2)
  
  extremes <- scores |>
    dplyr::mutate(dist = sqrt((NMDS1 - cx)^2 + (NMDS2 - cy)^2)) |>
    dplyr::arrange(dplyr::desc(dist)) |>
    dplyr::slice_head(n = n) |>
    dplyr::select(grupo, NMDS1, NMDS2)      # ^ por enquanto só coords
  
  # 4. sumariza intervalo, média e sd de cada grupo extremo
  resumo <- df_long |>
    dplyr::filter((!!rlang::sym(bloc_var)) %in% extremes$grupo) |>
    dplyr::group_by(grupo = !!rlang::sym(bloc_var)) |>
    dplyr::summarise(
      periodo = paste(format(min(cycle), "%Y-%m-%d %H:%M:%S"),
                      "–",
                      format(max(cycle), "%Y-%m-%d %H:%M:%S")),
      media_densidade     = mean(penilia, na.rm = TRUE),
      desvio_densidade    = sd(penilia,   na.rm = TRUE),
      .groups   = "drop"
    )
  
  # 5. junta coordenadas + resumo
  blocks_df <- dplyr::left_join(extremes, resumo, by = "grupo")
  
  # 6. devolve lista
  list(blocks = blocks_df,
       hours  = dplyr::filter(df_long,
                              (!!rlang::sym(bloc_var)) %in% blocks_df$grupo))
}
ext53 <- get_extremes("53h", n = 6)
b53 <- ext53$blocks
h53 <- ext53$hours
# ggplot(ext53$hours |> filter(group53 %in% c(189)) |> droplevels()) +
#   geom_line(aes(x = cycle, y = penilia)) +
#   geom_hline(aes(yintercept = 0), color = "red")
ext13 <- get_extremes("13d", n = 4)
b13 <- ext13$blocks
d13 <- ext13$hours

ext15 <- get_extremes("15d", n = 6)
b15 <- ext15$blocks
d15 <- ext15$hours

ext30 <- get_extremes("30d", n = 6)
b30 <- ext30$blocks
d30 <- ext30$hours

dfs <- list(h53 = h53,
            d13 = d13,
            d15 = d15,
            d30 = d30)

blocks <- bind_rows(list(b53 = b53, b13 = b13, b15 = b15, b30 = b30), .id = "band")
path_to_save <- here("data", "processed", "zooplancton", "6 - metrics by BF")
fwrite(blocks, here(path_to_save, "blocks.csv"))

##########################################################
# ───────────────────────────────────────────────────────────────
# 1.  empilhar séries em formato longo
# ───────────────────────────────────────────────────────────────
dfs <- list(h53 = h53, d13 = d13, d15 = d15, d30 = d30)

long <- bind_rows(
  lapply(names(dfs), \(nm)
         dfs[[nm]] |>
           select(cycle, value = 2) |>
           mutate(frequency_band = nm))
)

# ───────────────────────────────────────────────────────────────
# 2.  criar rótulos de facet (meses consecutivos agrupados)
# ───────────────────────────────────────────────────────────────
long <- long |>
  mutate(mes_ini = floor_date(cycle, "month"),
         ym_num  = year(mes_ini) * 12 + month(mes_ini)) |>
  arrange(mes_ini)

grp_map <- long |>
  distinct(mes_ini, ym_num) |>
  mutate(grp_id = cumsum(ym_num - lag(ym_num, 
                                      default = first(ym_num)) != 1))

label_tbl <- grp_map |>
  group_by(grp_id) |>
  summarise(label = if (n() == 1)
    format(first(mes_ini), "%Y-%m")
    else
      paste(format(first(mes_ini), "%Y-%m"),
            format(last(mes_ini),  "%Y-%m"), sep = "—"),
    .groups = "drop")

long <- long |>
  left_join(grp_map,  by = "mes_ini") |>
  left_join(label_tbl, by = "grp_id")

# ───────────────────────────────────────────────────────────────
# 3.  ordem das bandas (h53 ao fundo)
# ───────────────────────────────────────────────────────────────
ordem    <- c("h53", "d13", "d15", "d30")
long_ord <- long |>
  mutate(frequency_band = factor(frequency_band, levels = ordem)) |>
  arrange(frequency_band, cycle)

# ───────────────────────────────────────────────────────────────
# 4.  limites completos de h53 e faixa ±10 d
# ───────────────────────────────────────────────────────────────
lims_h53 <- long_ord |>
  filter(frequency_band == "h53", !is.na(value)) |>
  summarise(xmin = min(cycle),
            xmax = max(cycle),
            .by  = label) |>
  mutate(win_start = xmin - days(10),
         win_end   = xmax + days(10))

# ───────────────────────────────────────────────────────────────
# 5.  gráfico principal (p_base)  – usa long_ord INTEIRO
# ───────────────────────────────────────────────────────────────
p_base <- ggplot(long_ord) +
  geom_line(aes(cycle, value, colour = frequency_band)) +
  facet_wrap(~ label, scales = "free", nrow = 3) +
  scale_colour_manual(values = c(h53 = "pink",
                                 d13 = "red",
                                 d15 = "green4",
                                 d30 = "black"),
                      breaks = ordem,
                      name   = "frequency\nbands") +
  labs(x = "Date", y = "Density anomaly") +
  scale_x_datetime(labels = scales::label_date("%Y-%m-%d", locale = "en")) +
  theme_light() +
  theme(axis.text.x  = element_text(size = 12, face = "bold",
                                    family = "Times New Roman", 
                                    colour = "black"),
        axis.text.y  = element_text(size = 13, face = "bold",
                                    family = "Times New Roman", 
                                    colour = "black"),
        axis.title.x = element_text(size = 17, face = "bold",
                                    family = "Times New Roman", 
                                    colour = "black"),
        axis.title.y = element_text(size = 17, face = "bold",
                                    family = "Times New Roman",
                                    colour = "black"),
        title = element_text(size = 17, face = "bold",
                             family = "Times New Roman", 
                             colour = "black"),
        legend.text  = element_text(size = 15, face = "bold",
                                    family = "Times New Roman", 
                                    colour = "black"),
        strip.text   = element_text(size = 12, face = "bold",
                                    family = "Times New Roman", 
                                    colour = "black"))

# ───────────────────────────────────────────────────────────────
# 6.  função que devolve o GROB do inset (d30 + vlines ±10 d)
# ───────────────────────────────────────────────────────────────
make_grob <- function(lbl) {
  
  # ── subconjunto da série ---------------------------------------------------
  d30_df <- dplyr::filter(long_ord,
                          label == lbl,
                          frequency_band == "d30")
  
  # ── subconjunto dos limites / janelas -------------------------------------
  lims <- dplyr::filter(lims_h53, label == lbl)
  
  # função que devolve TRUE se um vetor é "bom" p/ usar no xlim
  good_vec <- function(x) length(x) == 2 && !any(is.na(x))
  
  # 1. tenta pegar win_start / win_end
  if (all(c("win_start", "win_end") %in% names(lims))) {
    x_lim_try <- c(lims$win_start[1], lims$win_end[1])
  } else {
    x_lim_try <- NA
  }
  
  # 2. se não der, usa range das próprias datas                ← caso "2020-11"
  x_lim <- if (good_vec(x_lim_try)) x_lim_try else range(d30_df$cycle, na.rm = TRUE)
  
  # idem para as linhas verticais: usa xmin/xmax se existirem ────────────────
  vlines <- if (all(c("xmin", "xmax") %in% names(lims)))
    c(lims$xmin[1], lims$xmax[1]) else NULL
  
  # ── gráfico ----------------------------------------------------------------
  p_in <- ggplot2::ggplot(d30_df) +
    ggplot2::geom_line(ggplot2::aes(cycle, value), colour = "black") +
    { if (!is.null(vlines))
      ggplot2::geom_vline(xintercept = vlines,
                          colour = "pink2", linetype = "dashed") } +
    ggplot2::geom_hline(yintercept = 0,
                        colour = "grey40", linetype = "dotted") +
    ggplot2::scale_x_datetime(date_breaks = "5 days",
                              date_labels = "%Y-%m-%d") +
    ggplot2::coord_cartesian(xlim = x_lim) +
    ggplot2::labs(x = NULL, y = NULL) +
    ggplot2::theme_classic(base_size = 6) +
    ggplot2::theme(
      axis.text.x = ggplot2::element_text(angle = 30, size = 5, colour = "black",
                                          vjust = 1, hjust = 1),
      axis.text.y = ggplot2::element_text(size = 5, colour = "black"),
      plot.margin = ggplot2::margin(2, 2, 2, 2)
    )
  
  ggplot2::ggplotGrob(p_in)
}


grob_list <- setNames(
  lapply(unique(long_ord$label), make_grob),
  unique(long_ord$label)
)

# 8. data-frame com coordenadas onde inserir cada inset
coordenadas_inset <- long_ord %>%
  group_by(label) %>%
  summarise(x_pos = max(cycle,  na.rm = TRUE),
            y_pos = max(value,  na.rm = TRUE),
            .groups = "drop") %>%
  mutate(grob = grob_list[label])

# 9. inserir os grobs
p_final <- p_base +
  ggpp::geom_grob(data = coordenadas_inset,
                  aes(x = x_pos, y = y_pos, label = grob),
                  vp.width  = 0.25, vp.height = 0.75, 
                  hjust = 0.9, vjust = 0.93)

print(p_final)

teste <- long_ord |> 
  filter(frequency_band %in% "h53") |>
  droplevels() |>
  group_by(hora = hour(cycle)) |> 
  summarise(media = mean(value), desvio = sd(value))

h53 |> 
  group_by(estac, hora = hour(cycle)) |> 
  summarise(media = mean(penilia, na.rm = T),
            desvio = sd(penilia, na.rm = T)) |> 
  ungroup() |> 
  ggplot()  + 
  geom_ribbon(aes(ymin = media-desvio, 
                  ymax = media+desvio, 
                  x = hora), 
              alpha = 0.15) + 
  geom_line(aes(x = hora, y = media, color = estac)) + 
  facet_wrap(~estac, scales = "free", nrow = 3) +
  labs(title = "Hourly variability for high frequency", 
       y = "Density anomaly (mean ± sd)",
       x = "Hour") +
  theme_light() +
  theme(axis.text.x  = element_text(size = 12, face = "bold",
                                    family = "Times New Roman", 
                                    colour = "black"),
        axis.text.y  = element_text(size = 13, face = "bold",
                                    family = "Times New Roman", 
                                    colour = "black"),
        axis.title.x = element_text(size = 17, face = "bold",
                                    family = "Times New Roman", 
                                    colour = "black"),
        axis.title.y = element_text(size = 17, face = "bold",
                                    family = "Times New Roman",
                                    colour = "black"),
        title = element_text(size = 11, face = "bold",
                             family = "Times New Roman", 
                             colour = "black"))

# Packages
# Loading packages used run the analysis
library(here)
source(here("scripts", "general", "0-library.R"))
source(here("scripts", "environmental", "3-periodicities_plot.R"))
gc()
theme_set(theme_classic(base_family = "Times New Roman"))

# Caminhos
path_dados <- here("data", "processed", "environ", "3 - data rebuilded")
dir_figs <- here("images", "environ", "4-PCA assumptions plot")
dir_figs_pca <- here("images", "environ", "5 - PCA by BF")
dir_transf <- here("data", "processed", "environ", "5 - series transformed by BF")
dir_metricas <- here("data", "processed", "environ", "6 - metrics by BF")

# Criar diretórios, se não existirem
dir.create(dir_figs, recursive = TRUE, showWarnings = FALSE)
dir.create(dir_figs_pca, recursive = TRUE, showWarnings = FALSE)
dir.create(dir_transf, recursive = TRUE, showWarnings = FALSE)
dir.create(dir_metricas, recursive = TRUE, showWarnings = FALSE)

# Leitura dos dados
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

# Metadata para rotulagem
plot_specs <- tibble(
  nome = c("airtemp.C_inmet", "CDOM.ppb_simcosta", "chl.ugL_simcosta",
           "DO.mlL_simcosta", "eastwestcurrentvelocity.ms_simcosta",
           "eastwestwindvelocity.ms_simcosta", 
           "northsouthcurrentvelocity.ms_simcosta", 
           "northsouthwindvelocity.ms_simcosta", "rain.mm_inmet", 
           "riverdischarge.m3s_votorantim", "sal_laps", "sal_simcosta",
           "solrad.Kjm2_inmet", "subtidalelevation.m_marinha", 
           "tidalelevation.m_marinha", "totsealev.m_marinha", 
           "turb.ntu_simcosta", "wattemp.C_laps", "wattemp.C_simcosta"),
  ylab = c("Air temperature °C - INMET", "CDOM ppp/QSDE - SiMCosta", 
           "Clorofila ug/L - SiMCosta", "DO ml/L - SiMCosta", 
           "East-West cur vel m/s - SiMCosta", 
           "East-West wind vel m/s - SiMCosta",
           "North-South cur vel m/s - SiMCosta", 
           "North-South wind vel m/s - SiMCosta", "Rainfall mm3 - INMET", 
           "River Discharge m3/s - Votorantim", "Salinidade - LAPS", 
           "Salinidade - SiMCosta", "Solar Radiation Kj/M2 - INMET", 
           "Subtidal Elevation m - Marinha", "Tidal Elevation m - Marinha", 
           "Total Sea Elevation m - Marinha", "Turbidity NTU - SiMCosta",
           "Water Temperature °C - LAPS", "Water Temperature °C - SiMCosta"))

label_vec <- setNames(plot_specs$ylab, plot_specs$nome)

# Função para histogramas com linhas adicionais
plot_histogram_detalhado <- function(data, band_name, labeller_vec) {
  
  data |> 
    pivot_longer(-1, names_to = "vars", values_to = "values") |> 
    ggplot(aes(x = values)) +
    geom_histogram(bins = 30, fill = "gray80", color = "black") +
    geom_vline(aes(xintercept = mean(values, na.rm = TRUE)), 
               color = "blue", linetype = "dashed") +
    geom_vline(aes(xintercept = median(values, na.rm = TRUE)), 
               color = "red", linetype = "solid") +
    geom_vline(aes(xintercept = quantile(values, 0.25, na.rm = TRUE)), 
               color = "green", linetype = "dotted") +
    geom_vline(aes(xintercept = quantile(values, 0.75, na.rm = TRUE)), 
               color = "green", linetype = "dotted") +
    geom_vline(aes(xintercept = quantile(values, 0.25, na.rm = TRUE) - 
                     1.5 * IQR(values, na.rm = TRUE)),
               color = "purple", linetype = "twodash") +
    geom_vline(aes(xintercept = quantile(values, 0.75, na.rm = TRUE) + 
                     1.5 * IQR(values, na.rm = TRUE)),
               color = "purple", linetype = "twodash") +
    facet_wrap(~ vars, scales = "free", labeller = as_labeller(labeller_vec)) +
    labs(x = "Values", y = "Counts", title = paste("Histogram -", band_name)) +
    theme_classic(base_family = "Times New Roman") +
    theme(strip.text = element_text(face = "bold", size = 10))
  
}

# Função para boxplots
plot_boxplot <- function(data, band_name, labeller_vec) {
  
  data |> 
    pivot_longer(-1, names_to = "vars", values_to = "values") |> 
    ggplot(aes(y = values)) +
    geom_boxplot() +
    facet_wrap(~ vars, scales = "free", labeller = as_labeller(labeller_vec)) +
    labs(y = "Values", title = paste("Boxplot -", band_name)) +
    theme_classic(base_family = "Times New Roman") +
    theme(strip.text = element_text(face = "bold", size = 10))
  
}

# Estatísticas descritivas e correção
estatisticas_bandas <- lapply(bandas_extraidas, function(df) {
  
  df <- df[, -1]
  tibble(vars = names(df),
         skewness = apply(df, 2, skewness, na.rm = TRUE),
         kurtosis = apply(df, 2, kurtosis, na.rm = TRUE))
  
})

corrigir_anomalias <- function(lista_bandas, lista_stats, 
                               metodo = "sqrt", limiar = 1) {
  
  out_dados <- list()
  out_metricas <- list()
  
  for (nome in names(lista_bandas)) {
    
    df <- lista_bandas[[nome]]
    stats <- lista_stats[[nome]]
    vars_corrigir <- stats$vars[abs(stats$skewness) > limiar]
    
    if (length(vars_corrigir)) {
      
      df[vars_corrigir] <- lapply(df[vars_corrigir],
                                  function(x) sign(x) * sqrt(abs(x)))
      
    }
    
    out_dados[[nome]] <- df
    out_metricas[[nome]] <- tibble(vars = names(df[, -1]),
                                   skewness = apply(df[, -1], 2, skewness),
                                   kurtosis = apply(df[, -1], 2, kurtosis))
    
  }
  
  list(dados = out_dados, metricas = out_metricas)
  
}

resultado_corrigido <- corrigir_anomalias(bandas_extraidas, estatisticas_bandas)

# Gerar e salvar boxplots e histogramas
for (banda in names(bandas_extraidas)) {
  df_orig <- bandas_extraidas[[banda]]
  df_tran <- resultado_corrigido$dados[[banda]]
  
  hist_orig <- plot_histogram_detalhado(df_orig, paste(banda, "Original"), label_vec)
  hist_tran <- plot_histogram_detalhado(df_tran, paste(banda, "Transformed"), label_vec)
  
  box_orig <- plot_boxplot(df_orig, paste(banda, "Original"), label_vec)
  box_tran <- plot_boxplot(df_tran, paste(banda, "Transformed"), label_vec)
  
  ggsave(file.path(dir_figs, paste0("Hist_Detail_", banda, "_original.png")), 
         hist_orig, width = 14, height = 9)
  ggsave(file.path(dir_figs, paste0("Hist_Detail_", banda, "_transformed.png")), 
         hist_tran, width = 14, height = 9)
  ggsave(file.path(dir_figs, paste0("Boxplot_", banda, "_original.png")), 
         box_orig, width = 14, height = 9)
  ggsave(file.path(dir_figs, paste0("Boxplot_", banda, "_transformed.png")), 
         box_tran, width = 14, height = 9)
  
}

# Função para selecionar variáveis com interação do usuário
selecionar_variaveis_interativamente <- function(nomes_variaveis) {
  variaveis_selecionadas <- character()
  for (v in nomes_variaveis) {
    resposta <- readline(paste0("Deseja incluir a variável '", v, "' na PCA? (S/N): "))
    if (toupper(resposta) == "S") {
      variaveis_selecionadas <- c(variaveis_selecionadas, v)
    }
  }
  return(variaveis_selecionadas)
}

# PCA e contribuição
run_pca <- function(lista_dados, sufixo_nome, dir_saida, 
                    labeller_vec, variaveis_usar = NULL) {
  
  for (nome in names(lista_dados)) {
    
    df <- lista_dados[[nome]]
    df <- df[, -1]  # remove primeira coluna (cycle)
    
    # Se não for especificado, perguntar variáveis
    if (is.null(variaveis_usar)) {
      message("Selecionando variáveis")
      variaveis_usar <- selecionar_variaveis_interativamente(colnames(df))
    }
    
    df <- df[, intersect(colnames(df), variaveis_usar), drop = FALSE]
    dados_pad <- decostand(df, "standardize")
    pca_res <- FactoMineR::PCA(dados_pad, scale.unit = FALSE, graph = FALSE)
    cum_var <- pca_res$eig[, 3]
    eixo_final <- which(cum_var >= 50)[1]
    
    g1 <- fviz_pca_var(pca_res, title = paste("PCA -", nome), 
                       repel = TRUE, col.var = "black") +
      theme_classic(base_family = "Times New Roman")
    
    g2 <- fviz_contrib(pca_res, choice = "var", axes = 1:eixo_final,
                       title = paste0("Contrib - ", nome, ", eixo 1:", 
                                      eixo_final, ", Variância acumulada = ", 
                                      round(cum_var[eixo_final], 1), "%")) +
      scale_x_discrete(labels = labeller_vec) +
      theme_classic(base_family = "Times New Roman") +
      theme(axis.text.x = element_text(angle = 90, hjust = 1, 
                                       vjust = 0.3, colour = "black"),
            axis.text.y = element_text(colour = "black"),
            axis.title.x = element_text(colour = "black"),
            axis.title.y = element_text(colour = "black"))
    
    ggsave(file.path(dir_saida, paste0("PCA_", nome, "_", sufixo_nome, ".png")), 
           g1, width = 8, height = 8, dpi = "retina")
    ggsave(file.path(dir_saida, paste0("Contrib_", nome, "_", sufixo_nome, ".png")), 
           g2, width = 6, height = 7, dpi = "retina")
    
  }
}

run_pca(bandas_extraidas, "original", dir_figs_pca, label_vec)
run_pca(resultado_corrigido$dados, "transformed", dir_figs_pca, label_vec)
<<<<<<< HEAD

###############################################################################
###############################################################################
###############################################################################
library(rlang)
# define os limites
start <- ymd_hms("2020-11-04 00:00:00", tz = "UTC")
end   <- ymd_hms("2022-06-11 00:00:00", tz = "UTC")

# aplica o filtro em cada elemento da lista
bandas_extraidas <- purrr::map(
  bandas_extraidas,
  ~ filter(.x, cycle >= start, cycle <= end)
)
# 1) Função que só adiciona as colunas de bloco e réplica,
#    parametrizada por horas_bloc:
as_estacao <- function(m) {
  dplyr::case_when(
    m %in%  1:3 ~ "Summer",
    m %in%  4:6 ~ "Autumn",
    m %in%  7:9 ~ "Winter",
    TRUE        ~ "Spring"
  )
}
add_block_codes <- function(df, horas_bloc) {
  # monta o nome da coluna de bloco (ex.: "group53", "group312"…)
  bloc_var <- paste0("group", horas_bloc)
  
  df %>%
    arrange(cycle) %>%
    mutate(
      row_id  = row_number(),
      # bloco
      !!bloc_var := ((row_id - 1) %/% horas_bloc) + 1,
      # réplica dentro do bloco
      replica  = factor(
        ((row_id - 1) %% horas_bloc) + 1,
        levels = 1:horas_bloc
      ),
      # mês e estação
      mes   = month(cycle),
      estac = as_estacao(mes)
    ) %>%
    select(-row_id)
}

data_list <- list(
  list(name = "53 hours", df = bandas_extraidas$BF1, hrs = 53),
  list(name = "13 days",  df = bandas_extraidas$BF2, hrs = 13*24),
  list(name = "15 days",  df = bandas_extraidas$BF3, hrs = 15*24),
  list(name = "30 days",  df = bandas_extraidas$BF4, hrs = 30*24)
)

processed_list <- purrr::map(data_list, function(el) {
  add_block_codes(el$df, el$hrs)
})

names(processed_list) <- purrr::map_chr(data_list, "name")

######
dir_saida <- here("images", "environ", "5 - teste2")
dir.create(dir_saida, recursive = TRUE, showWarnings = FALSE)

selecionar_variaveis_interativamente <- function(opcoes) {
  sel <- character()
  cat("Variáveis disponíveis para PCA:\n")
  for (v in opcoes) {
    rpt <- readline(sprintf("  Incluir '%s'? (S/N): ", v))
    if (toupper(rpt) == "S") sel <- c(sel, v)
  }
  if (length(sel) == 0) {
    stop("Nenhuma variável selecionada. Abortando PCA.")
  }
  sel
}


# PCA
run_pca_groups <- function(lista_dados,
                           sufixo_nome,
                           dir_saida,
                           rotulos_var,
                           variaveis_usar = NULL) {
  
  for (nome in names(lista_dados)) {
    df <- lista_dados[[nome]]
    
    # 1) coluna de bloco
    grp_col <- grep("^group", names(df), value = TRUE)
    if (length(grp_col) != 1) {
      stop("Em '", nome, "': precisa exatamente 1 coluna 'group...'.")
    }
    
    # 2) checa variáveis de interesse
    todas_vars  <- names(rotulos_var)
    vars_pres   <- intersect(todas_vars, names(df))
    vars_faltam <- setdiff(todas_vars, names(df))
    if (length(vars_faltam) > 0) {
      message("Em '", nome, "': ignorando (não encontradas) → ",
              paste(vars_faltam, collapse = ", "))
    }
    if (length(vars_pres) == 0) {
      stop("Em '", nome, "': nenhuma variável de interesse existe.")
    }
    
    # 3) seleciona variáveis (interativo ou fixo)
    if (is.null(variaveis_usar)) {
      cat("### Selecione variáveis para PCA em '", nome, "' ###\n")
      vars_sel <- character()
      for (v in vars_pres) {
        r <- readline(sprintf("  Incluir '%s'? (S/N): ", v))
        if (toupper(r) == "S") vars_sel <- c(vars_sel, v)
      }
      if (length(vars_sel) == 0) {
        stop("Nenhuma variável selecionada. Abortando PCA.")
      }
    } else {
      vars_sel <- intersect(variaveis_usar, vars_pres)
      if (length(vars_sel) == 0) {
        stop("Em '", nome, "': nenhuma das variaveis_usar existe.")
      }
    }
    
    # 4) calcula médias por grupo
    df_means <- df %>%
      group_by(across(all_of(grp_col))) %>%
      summarise(
        across(all_of(vars_sel), ~ mean(.x, na.rm = TRUE)),
        .groups = "drop"
      ) %>%
      rename(grupo = !!rlang::sym(grp_col))
    
    # 5) estação e mês-ano dominante (MM/YY)
    df_labels <- df %>%
      mutate(ym = format(cycle, "%m/%y")) %>%
      count(!!rlang::sym(grp_col), estac, ym) %>%
      group_by(!!rlang::sym(grp_col)) %>%
      slice_max(n, with_ties = FALSE) %>%
      ungroup() %>%
      rename(
        grupo   = !!rlang::sym(grp_col),
        estac   = estac,
        mes_dom = ym
      )
    
    # 6) padroniza e roda PCA
    dados_pad <- df_means %>%
      select(all_of(vars_sel)) %>%
      decostand("standardize")
    pca_res <- PCA(dados_pad, scale.unit = FALSE, graph = FALSE)
    
    # 7) variação explicada
    eig  <- pca_res$eig
    var1 <- eig[1,2]
    var2 <- eig[2,2]
    
    # 8) scores dos grupos
    scores_ind <- as.data.frame(pca_res$ind$coord[,1:2])
    colnames(scores_ind) <- c("PC1","PC2")
    scores_ind$grupo <- df_means$grupo
    ind <- left_join(scores_ind, df_labels, by = "grupo")
    
    # 9) coordenadas das variáveis (setas)
    var <- as.data.frame(pca_res$var$coord[,1:2])
    colnames(var) <- c("PC1","PC2")
    var$orig_name <- rownames(var)
    # mapeia para os rótulos amigáveis
    var$label <- rotulos_var[var$orig_name]
    # escala das setas
    mult <- min(
      diff(range(ind$PC1)) / diff(range(var$PC1)),
      diff(range(ind$PC2)) / diff(range(var$PC2))
    ) * 0.7
    var <- var %>%
      mutate(
        arrow_x = PC1 * mult,
        arrow_y = PC2 * mult
      )
    
    # 10) monta o biplot
    g_bi <- ggplot() +
      # pontos coloridos por estação
      geom_point(
        data = ind,
        aes(PC1, PC2, color = estac),
        size = 5
      )
    
    # — remove labels dos pontos apenas para "53 hours"
    if (nome != "53 hours") {
      g_bi <- g_bi +
        geom_text_repel(
          data = ind,
          aes(PC1, PC2, label = mes_dom),
          color        = "grey",
          size         = 6,
          max.overlaps = 20
        )
    }
    
    g_bi <- g_bi +
      # setas
      geom_segment(
        data  = var,
        aes(x = 0, y = 0, xend = arrow_x, yend = arrow_y),
        arrow = arrow(length = unit(0.3, "cm")),
        color = "black"
      ) +
      # rótulos das setas, usando var$label
      geom_text_repel(
        data = var,
        aes(arrow_x, arrow_y, label = label),
        color        = "black",
        size         = 8,
        max.overlaps = 40
      ) +
      scale_color_manual(values = c(
        Summer = "red",
        Autumn = "orange",
        Winter = "blue",
        Spring = "pink"
      )) +
      labs(
        title = paste("PCA biplot —", nome),
        x     = sprintf("PC1 (%.1f%%)", var1),
        y     = sprintf("PC2 (%.1f%%)", var2),
        color = "Season"
      ) +
      theme_classic(base_family = "Times New Roman") +
      theme(axis.text.x = element_text(size = 20, face = "bold", colour = "black"),
            axis.text.y = element_text(size = 20, face = "bold", colour = "black"),
            legend.text = element_text(size = 20, face = "bold", colour = "black"),
            axis.title.x = element_text(size = 20, face = "bold", colour = "black"),
            axis.title.y = element_text(size = 20, face = "bold", colour = "black"),
            legend.title = element_text(size = 20, face = "bold", colour = "black"))
    
    # 11) gráfico de contribuições
    cum_var  <- eig[,3]
    eixo_fin <- which(cum_var >= 50)[1]
    g_contrib <- fviz_contrib(
      pca_res,
      choice = "var",
      axes   = 1:eixo_fin,
      title  = paste0(
        "Contribuições — ", nome,
        " (eixos 1:", eixo_fin,
        "; acum. ", round(cum_var[eixo_fin], 1), "%)"
      )
    ) +
      # usa seus rótulos amigáveis no eixo X
      scale_x_discrete(labels = rotulos_var) +
      # mantém a mesma theme e formatação de texto
      theme_classic(base_family = "Times New Roman") +
      theme(
        axis.text.x  = element_text(
          size   = 15,
          angle  = 90,
          hjust  = 1,
          vjust  = 0.3,
          colour = "black"
        ),
        axis.text.y  = element_text(size = 15, colour = "black"),
        axis.title.x = element_text(size = 15, colour = "black"),
        axis.title.y = element_text(size = 15, colour = "black")
      )
    
    # 12) salva os dois gráficos
    ggsave(
      file.path(dir_saida,
                paste0("PCA_Biplot_", nome, "_", sufixo_nome, ".png")),
      g_bi, width = 12, height = 8, dpi = "retina"
    )
    ggsave(
      file.path(dir_saida,
                paste0("Contrib_", nome, "_", sufixo_nome, ".png")),
      g_contrib, width = 6, height = 7, dpi = "retina"
    )
  }
}

rotulos_var <- c(
  airtemp.C_inmet = "Air temperature (°C)",
  CDOM.ppb_simcosta = "CDOM",
  chl.ugL_simcosta   = "Chl",
  DO.mlL_simcosta = "DO",
  eastwestcurrentvelocity.ms_simcosta   = "EWcur",
  eastwestwindvelocity.ms_simcosta = "EWwind",
  northsouthcurrentvelocity.ms_simcosta = "NScur",
  northsouthwindvelocity.ms_simcosta   = "NSwind",
  rain.mm_inmet = "Rain",
  riverdischarge.m3s_votorantim   = "RivDisc",
  sal_simcosta = "Sal",
  solrad.Kjm2_inmet = "Rad",
  subtidalelevation.m_marinha   = "STE",
  tidalelevation.m_marinha = "TE",
  totsealev.m_marinha = "TSE",
  turb.ntu_simcosta = "Turb",
  wattemp.C_simcosta = "Temp"
)
vars <- c("CDOM.ppb_simcosta", "CDOM.ppb_simcosta", "chl.ugL_simcosta", 
          "DO.mlL_simcosta", "eastwestcurrentvelocity.ms_simcosta",
          "eastwestwindvelocity.ms_simcosta", "northsouthcurrentvelocity.ms_simcosta",
          "northsouthwindvelocity.ms_simcosta", "rain.mm_inmet", 
          "riverdischarge.m3s_votorantim", "sal_simcosta", "solrad.Kjm2_inmet", 
          "subtidalelevation.m_marinha", "tidalelevation.m_marinha", 
          "totsealev.m_marinha", "turb.ntu_simcosta", "wattemp.C_simcosta")
# diretório onde quer gravar os gráficos:
dir_figs_pca <- dir_saida
library(ggrepel)
run_pca_groups(
  processed_list,
  sufixo_nome   = "group_means",
  dir_saida     = dir_figs_pca,
  rotulos_var   = rotulos_var,
  variaveis_usar = vars    # faz seleção interativa; ou passe um vetor fixo de nomes
)

=======
>>>>>>> fc9c083e9a513e94ce0c624be017bc851b2432d9

<<<<<<< HEAD
# Script only for environ

=======
>>>>>>> fc9c083e9a513e94ce0c624be017bc851b2432d9
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

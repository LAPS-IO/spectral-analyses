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
extrair_bandas <- function(files_list, bandas = c("BF1", "BF2", "BF3", "BF4")) {
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

bandas_extraidas <- extrair_bandas(files_list)

# Salvar arquivos por banda
dir_bandas <- here("data", "processed", "environ", "4 - bands rebuilded")
dir.create(dir_bandas, recursive = TRUE, showWarnings = FALSE)

for (banda in names(bandas_extraidas)) {
  fwrite(bandas_extraidas[[banda]], file = file.path(dir_bandas, paste0(banda, ".csv")))
}

# Função para histogramas
plot_histogram <- function(data, band_name, labeller_vec) {
  data |>
    pivot_longer(-1, names_to = "vars", values_to = "values") |> 
    ggplot() +
    geom_histogram(aes(x = values), bins = 30) +
    facet_wrap(~ vars, scales = "free", labeller = as_labeller(labeller_vec)) +
    labs(x = "Values", y = "Counts", title = band_name) +
    theme_classic(base_family = "Times New Roman") +
    theme(
      strip.text = element_text(face = "bold", size = 10),
      axis.text = element_text(color = "black"),
      axis.title = element_text(color = "black")
    )
}

# Função para boxplots
plot_boxplot <- function(data, band_name, labeller_vec) {
  data |>
    pivot_longer(-1, names_to = "vars", values_to = "values") |> 
    ggplot() +
    geom_boxplot(aes(y = values)) +
    facet_wrap(~ vars, scales = "free", labeller = as_labeller(labeller_vec)) +
    labs(y = "Values", title = band_name) +
    theme_classic(base_family = "Times New Roman") +
    theme(
      strip.text = element_text(face = "bold", size = 10),
      axis.text = element_text(color = "black"),
      axis.title = element_text(color = "black")
    )
}

# Labels (deve ser carregado de um arquivo ou objeto externo)
label_vec <- setNames(names(files_list), names(files_list))  # placeholder

# Gerar e salvar plots
for (banda in names(bandas_extraidas)) {
  df <- bandas_extraidas[[banda]]
  g1 <- plot_histogram(df, banda, label_vec)
  g2 <- plot_boxplot(df, banda, label_vec)
  ggsave(file.path(dir_figs, paste0("Hist_", banda, ".png")), g1, width = 12, height = 8)
  ggsave(file.path(dir_figs, paste0("Box_", banda, ".png")), g2, width = 12, height = 8)
}

# Estatísticas descritivas
estatisticas_bandas <- lapply(bandas_extraidas, function(df) {
  df <- df[, -1]
  tibble(
    vars = names(df),
    skewness = apply(df, 2, skewness, na.rm = TRUE),
    kurtosis = apply(df, 2, kurtosis, na.rm = TRUE)
  )
})

# Função para correção de skew
corrigir_anomalias <- function(lista_bandas, lista_stats, metodo = "sqrt", limiar = 1) {
  out_dados <- list()
  out_metricas <- list()
  for (nome in names(lista_bandas)) {
    df <- lista_bandas[[nome]]
    stats <- lista_stats[[nome]]
    vars_corrigir <- stats$vars[abs(stats$skewness) > limiar]
    if (length(vars_corrigir)) {
      df[vars_corrigir] <- lapply(df[vars_corrigir], function(x) sign(x) * sqrt(abs(x)))
    }
    out_dados[[nome]] <- df
    out_metricas[[nome]] <- tibble(
      vars = names(df[, -1]),
      skewness = apply(df[, -1], 2, skewness),
      kurtosis = apply(df[, -1], 2, kurtosis)
    )
  }
  list(dados = out_dados, metricas = out_metricas)
}

# Corrigir skew
resultado_corrigido <- corrigir_anomalias(bandas_extraidas, estatisticas_bandas)

# Salvar dados e métricas
for (nome in names(resultado_corrigido$dados)) {
  fwrite(resultado_corrigido$dados[[nome]], file.path(dir_transf, paste0(nome, ".csv")))
  write.csv(resultado_corrigido$metricas[[nome]], file.path(dir_metricas, paste0(nome, "_metrics.csv")), row.names = FALSE)
}

# Função para rodar PCA
run_pca <- function(lista_dados, sufixo_nome, dir_saida) {
  for (nome in names(lista_dados)) {
    df <- lista_dados[[nome]]
    dados_pad <- decostand(df[, -1], "standardize")
    pca_res <- FactoMineR::PCA(dados_pad, scale.unit = FALSE, graph = FALSE)
    cum_var <- pca_res$eig[, 3]
    eixo_final <- which(cum_var >= 50)[1]
    g1 <- fviz_pca_var(pca_res, title = paste("PCA -", nome), repel = TRUE) +
      theme_classic(base_family = "Times New Roman")
    g2 <- fviz_contrib(pca_res, choice = "var", axes = 1:eixo_final,
                       title = paste0("Contrib - ", nome, ", eixo 1:", eixo_final,
                                      ", Variância acumulada = ", round(cum_var[eixo_final], 1), "%")) +
      theme_classic(base_family = "Times New Roman") +
      theme(axis.text.x = element_text(angle = 90, hjust = 1))
    ggsave(file.path(dir_saida, paste0("PCA_", nome, "_", sufixo_nome, ".png")), g1)
    ggsave(file.path(dir_saida, paste0("Contrib_", nome, "_", sufixo_nome, ".png")), g2)
  }
}

# Rodar PCAs
run_pca(bandas_extraidas, "original", dir_figs_pca)
run_pca(resultado_corrigido$dados, "transformed", dir_figs_pca)














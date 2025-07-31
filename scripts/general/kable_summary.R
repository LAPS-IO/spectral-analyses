# Packages
# Loading packages used run the analysis

# Leitura dos dados
path_dados <- here("data", "processed", "environ", "3 - data rebuilded")
files <- list.files(path_dados, pattern = ".csv$", full.names = TRUE)
files_list_env <- lapply(files, fread)
names(files_list_env) <- tools::file_path_sans_ext(basename(files))

rotulos_var <- c(
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

extrair_bandas <- function(files_list, 
                           bandas = c("data")) {
  
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
bandas_extraidas <- extrair_bandas(files_list_env)

#####
resumir_df <- function(
    df,
    metrics       = c("min_max", "var"),  # escolha padrão
    extra_metrics = NULL,                 # p.ex. list(Q1 = \(x) quantile(x, .25))
    digits        = 2                     # casas decimais p/ formatar
) {
  
  # ── 1. prepara dados em formato longo ────────────────────────
  df_long <- df %>%
    dplyr::mutate(
      ym_num = format(cycle, "%Y-%m"),            # p/ ordenar
      ym_lbl = tolower(format(cycle, "%b/%y"))    # p/ header
    ) %>%
    tidyr::pivot_longer(
      -c(cycle, ym_num, ym_lbl),
      names_to       = "var_raw",
      values_to      = "valor",
      values_drop_na = TRUE
    ) %>%
    dplyr::mutate(
      var = dplyr::if_else(var_raw %in% names(rotulos_var),
                           rotulos_var[var_raw],
                           var_raw),
      .keep = "unused"
    )
  
  # ── 2. monta expressões dinâmicas para summarise() ───────────
  # mapa "apelido"  -> nome final no output
  label_map <- c(
    n        = "N",
    mean     = "Mean",
    median   = "Median",
    sd       = "SD",
    var      = "Var",
    min_max  = "Min-Max"
  )
  
  # expressões base
  metric_exprs <- list()
  
  if ("n"      %in% metrics)    metric_exprs$N     <- rlang::expr(dplyr::n())
  if ("mean"   %in% metrics)    metric_exprs$Mean  <- rlang::expr(mean(valor,  na.rm = TRUE))
  if ("median" %in% metrics)    metric_exprs$Median<- rlang::expr(median(valor, na.rm = TRUE))
  if ("sd"     %in% metrics)    metric_exprs$SD    <- rlang::expr(sd(valor,    na.rm = TRUE))
  if ("var"    %in% metrics)    metric_exprs$Var   <- rlang::expr(
    ifelse(dplyr::n() > 1,
           var(valor, na.rm = TRUE), 0))
  if ("min_max" %in% metrics) {
    # guardamos min e max para depois formatarem juntos
    metric_exprs$`_minimo` <- rlang::expr(min(valor, na.rm = TRUE))
    metric_exprs$`_maximo` <- rlang::expr(max(valor, na.rm = TRUE))
  }
  
  # métricas extras (named list de funções)
  if (length(extra_metrics)) {
    for (nm in names(extra_metrics)) {
      fn <- extra_metrics[[nm]]
      stopifnot(is.function(fn))
      metric_exprs[[nm]] <- rlang::expr((!!fn)(valor))
    }
  }
  
  # ── 3. calcula estatísticas ─────────────────────────────────
  stats <- df_long %>%
    dplyr::group_by(ym_num, ym_lbl, var) %>%
    dplyr::summarise(!!!metric_exprs, .groups = "drop")
  
  # ── 4. pós-processa Min-Max e formata números  --------------
  if ("min_max" %in% metrics) {
    stats <- stats %>%
      dplyr::mutate(`Min-Max` = sprintf(
        paste0("%.", digits, "f–%.", digits, "f"),
        `_minimo`, `_maximo`)) %>%
      dplyr::select(-`_minimo`, -`_maximo`)
  }
  
  # formata numéricos (exceto N) --------------------------------
  to_fmt <- setdiff(
    names(stats),
    c("ym_num", "ym_lbl", "var", "N", if ("Min-Max" %in% names(stats)) "Min-Max")
  )
  
  stats <- stats %>%
    dplyr::mutate(dplyr::across(
      dplyr::all_of(to_fmt),
      ~ sprintf(paste0("%.", digits, "f"), .x))
    )
  
  # ── 5. passa para formato longo de novo ----------------------
  stats_long <- stats %>%
    tidyr::pivot_longer(
      -c(ym_num, ym_lbl, var),
      names_to  = "estat",
      values_to = "val"
    )
  
  # ── 6. espalha em largura  (cab = mes__estat) ----------------
  wide <- stats_long %>%
    tidyr::unite("cab", ym_num, estat, sep = "__") %>%
    tidyr::pivot_wider(
      id_cols      = var,
      names_from   = cab,
      values_from  = val,
      names_sort   = TRUE,
      values_fill  = list(val = "0")
    )
  
  # ── 7. ordena linhas conforme ordem original -----------------
  ordem_vars <- setdiff(names(df), "cycle")
  ordem_vars <- ifelse(ordem_vars %in% names(rotulos_var),
                       rotulos_var[ordem_vars],
                       ordem_vars)
  wide <- wide %>% dplyr::arrange(factor(var, levels = ordem_vars))
  
  # ── 8. ordena / garante sequência das colunas ----------------
  meses_ord     <- sort(unique(stats$ym_num))
  metric_labels <- names(metric_exprs)
  # re-mapeia caso Nomes diferentes (min_max virou Min-Max, etc.)
  metric_labels <- dplyr::recode(metric_labels,
                                 `_minimo` = "Min-Max", `_maximo` = "Min-Max",
                                 .default  = metric_labels)
  metric_labels <- unique(metric_labels)   # remove duplicatas
  
  col_seq <- c("var",
               unlist(lapply(
                 meses_ord,
                 function(m) paste0(m, "__", metric_labels)
               ))
  )
  # adiciona eventual métrica extra ausente
  col_seq <- intersect(col_seq, names(wide))
  wide    <- wide[, col_seq, drop = FALSE]
  
  # ── 9. constrói cabeçalhos duplos ----------------------------
  header1 <- c("Environmental Variable",
               rep(tolower(format(as.Date(paste0(meses_ord, "-01")),
                                  "%b/%y")),
                   each = length(metric_labels)))
  header2 <- c("", rep(metric_labels, times = length(meses_ord)))
  
  out <- rbind(
    header1,
    header2,
    cbind(wide$var, as.matrix(wide[,-1]))
  )
  rownames(out) <- NULL
  colnames(out) <- NULL
  as.data.frame(out, stringsAsFactors = FALSE)
}


## ── aplicar à lista e salvar ─────────────────────────────────
resumos <- resumir_df(bandas_extraidas$data,
                      metrics = c("min_max", "mean", "sd"),
                      digits  = 2)

output_dir <- here("data", "processed", "environ", "data_summary")
#############
library(dplyr)
library(tidyr)
library(stringi)      # para normalizar acentos (opcional)
library(knitr)
library(kableExtra)

resumir_para_tabela <- function(df, digits = 2) {
  
  # ── LONG ------------------------------------------------------
  df_long <- df %>%
    mutate(
      ym_num = format(cycle, "%Y-%m"),                # 2020-11
      ym_lbl = tolower(format(cycle, "%b/%y"))        # nov/20
    ) %>%
    pivot_longer(-c(cycle, ym_num, ym_lbl),
                 names_to = "var_raw",
                 values_to = "valor",
                 values_drop_na = TRUE) %>%
    mutate(var = if_else(var_raw %in% names(rotulos_var),
                         rotulos_var[var_raw], var_raw),
           .keep = "unused")
  
  # ── ESTATÍSTICAS ---------------------------------------------
  stats <- df_long %>%
    group_by(ym_num, ym_lbl, var) %>%
    summarise(
      m  = mean(valor,  na.rm = TRUE),
      sd = sd(  valor,  na.rm = TRUE),
      mn = min(valor,   na.rm = TRUE),
      mx = max(valor,   na.rm = TRUE),
      .groups = "drop"
    ) %>%
    mutate(
      `Mean ± SD` = sprintf(paste0("%.", digits, "f ± %.", digits, "f"), m, sd),
      `Min–Max`   = sprintf(paste0("%.", digits, "f–%.", digits, "f"),  mn, mx)
    ) %>%
    select(ym_num, ym_lbl, var, `Mean ± SD`, `Min–Max`)
  
  # ── LINHAS DUPLAS (var + métrica) -----------------------------
  stats_long <- stats %>%
    pivot_longer(c(`Mean ± SD`, `Min–Max`),
                 names_to  = "Métrica",
                 values_to = "valor")
  
  # ── WIDE: colunas = ym_num (ordem natural) --------------------
  wide <- stats_long %>%
    pivot_wider(id_cols = c(var, Métrica),
                names_from  = ym_num,
                values_from = valor,
                values_fill = "-") %>%
    arrange(var, factor(Métrica, levels = c("Mean ± SD", "Min–Max")))
  
  # Renomeia cabeçalhos dos meses para rótulos amigáveis
  mes_cols <- setdiff(names(wide), c("var", "Métrica"))
  mes_lbls <- tolower(format(as.Date(paste0(mes_cols, "-01")), "%b/%y"))
  names(wide)[match(mes_cols, names(wide))] <- mes_lbls
  
  wide
}

tabela <- resumir_para_tabela(bandas_extraidas$data)

salvar_pdf_tabela <- function(df, arquivo_pdf,
                              titulo = "Tabela", fonte = 8) {
  
  # 1ª coluna = Variável ; 2ª = Métrica (Mean ± SD | Min–Max)
  kbl <- kbl(df,
             booktabs  = TRUE,
             longtable = TRUE,
             align     = c("l", "l", rep("c", ncol(df) - 2)),
             col.names = c("Variable", "Metric", names(df)[-(1:2)]),
             caption   = titulo,
             escape    = TRUE) %>%
    
    # agrupa linhas (não repete nome da variável)
    collapse_rows(columns = 1, valign = "top") %>%
    
    # deixa a 1ª linha de cada grupo em negrito
    row_spec(which(df$Métrica == "Mean ± SD"), bold = TRUE) %>%
    
    # ajuste de layout
    kable_styling(latex_options = c("hold_position", "repeat_header"),
                  font_size = fonte)
  
  save_kable(kbl, arquivo_pdf, self_contained = TRUE)   # gera PDF
  message("PDF salvo em: ", arquivo_pdf)
}

salvar_pdf_tabela(tabela, "tabela_resumo.pdf",
                  titulo = "Environmental Data Summary")


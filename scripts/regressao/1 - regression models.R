# Multiple linear regression

# biological
penilia <- df_bands

# environ
path_dados <- here::here("data", "processed", "environ", "3 - data rebuilded")
files <- list.files(path_dados, pattern = ".csv$", full.names = TRUE)
files_list <- lapply(files, fread)
names(files_list) <- tools::file_path_sans_ext(basename(files))

cycles_complete <- tibble(cycle = seq(ymd_hm("2020-11-04 00:00"),
                                      ymd_hm("2022-06-10 23:59"),
                                      by = "1 hour"))
for(i in names(files_list)) {
  
  files_list[[i]] <- right_join(files_list[[i]], cycles_complete)
  
}
list2env(files_list, envir = .GlobalEnv)

df_model <- tibble(
  BF3   = penilia$BF3,
  DO = DO.mlL_simcosta$BF3,
  rain= rain.mm_inmet$BF3,
  # EWcur = eastwestcurrentvelocity.ms_simcosta$BF3,
  chl= chl.ugL_simcosta$BF3,
  # airtemp = airtemp.C_inmet$BF3,
  CDOM  = CDOM.ppb_simcosta$BF3,
  # totsea = totsealev.m_marinha$BF3,
  # subTide  = subtidalelevation.m_marinha$BF3,
  # Sal = sal_simcosta$BF3,
  WatTemp = wattemp.C_simcosta$BF3)
mod1 <- lm(BF3 ~ ., data = df_model) #com sal
mod2 <- lm(BF3 ~ ., data = df_model) #com sal e cdom
mod3 <- lm(BF3 ~ ., data = df_model) #com cdom
AIC(mod1)
AIC(mod2)
AIC(mod3)
# plot(df_model[, 2:11])
car::vif(mod1)
car::vif(mod2)
car::vif(mod3)

summary(mod1)
summary(mod2)
summary(mod3)

acf(resid(mod3))

library(nlme)
df_model$tempo <- 1:nrow(df_model)
mod3 <- gls(BF3 ~ DO + rain + chl + CDOM + WatTemp, 
            correlation = corARMA(p = 0, q = 1, form = ~ tempo), 
            data = df_model)

car::vif(mod3)
acf(df_model$BF3)
pacf(df_model$BF3)
AIC(mod3)
summary(mod3)


library(nlme)
df_model$tempo <- seq_len(nrow(df_model))
auto_gls_arma_safe <- function(formula, data, time_var = "tempo", p_max = 2, q_max = 2) {
  
  arma_grid <- expand.grid(p = 0:p_max, q = 0:q_max)
  arma_grid <- arma_grid[!(arma_grid$p == 0 & arma_grid$q == 0), ]
  
  modelos <- list()
  aics <- rep(NA, nrow(arma_grid))
  
  for (i in seq_len(nrow(arma_grid))) {
    p <- arma_grid$p[i]
    q <- arma_grid$q[i]
    
    modelo <- tryCatch(
      gls(formula,
          data = data,
          correlation = corARMA(p = p, q = q, form = reformulate(time_var)),
          method = "REML"),
      error = function(e) NULL
    )
    
    if (!is.null(modelo)) {
      modelos[[i]] <- modelo
      aics[i] <- AIC(modelo)
    }
  }
  
  # Verifica se ao menos um modelo foi ajustado
  if (all(is.na(aics))) {
    stop("Nenhum modelo ARMA foi ajustado com sucesso.")
  }
  
  best_index <- which.min(aics)
  best_model <- modelos[[best_index]]
  best_pq <- arma_grid[best_index, ]
  
  list(
    model = best_model,
    AIC = aics[best_index],
    structure = paste0("ARMA(", best_pq$p, ",", best_pq$q, ")"),
    pseudo_r2 = 1 - var(resid(best_model)) / var(model.response(model.frame(formula, data)))
  )
}
resultado <- auto_gls_arma_safe(
  formula = BF3 ~ DO + rain + chl + CDOM + WatTemp,
  data = df_model,
  time_var = "tempo",  # variável temporal
  p_max = 2,
  q_max = 2
)

# Ver resultado
resultado$structure       # Melhor estrutura ARMA(p, q)
summary(resultado$model)  # Detalhes do modelo
resultado$pseudo_r2   
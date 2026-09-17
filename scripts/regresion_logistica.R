# ===============================================================
# TP Ciencia de Datos - Predicción Ganador en Tenis (ATP)
# Modelo 3 - Versión final (CV AUC fix + pulido de gráficos)
# ---------------------------------------------------------------
# Sin split temporal. Guarda todo en ~/Desktop/Graficos-TP-RL
# Banderas para saltar cálculos pesados:
#   - RECALC_RF_IMPORTANCE: Random Forest importancia (pesado)
#   - RECALC_CV: Validación cruzada y selección de k (moderado)
# ===============================================================

suppressPackageStartupMessages({
  library(tidyverse)   # ggplot2, dplyr, readr, forcats, tidyr...
  library(caret)
  library(pROC)
  library(randomForest)
  library(car)         # VIF
  library(forcats)
})

set.seed(42)

# -----------------------------
# 0) CONFIG & RUTAS
# -----------------------------
RECALC_RF_IMPORTANCE <- TRUE   # Pone FALSE si ya tenés 01_importancia_rf_top20.png/CSV
RECALC_CV            <- TRUE   # Pone FALSE si ya tenés resumen_cv_auc.csv

ruta_csv <- file.path(path.expand("~"), "Desktop", "TP CIENCIA DE DATOS", "df_transform.csv")
carpeta_graficos <- file.path(path.expand("~"), "Desktop", "Graficos-TP-RL")
if (!dir.exists(carpeta_graficos)) dir.create(carpeta_graficos, recursive = TRUE)

# -----------------------------
# 1) CARGA Y PREPARACIÓN
# -----------------------------
# (Si ya cargaste df_simple/train/test en la sesión, podés saltar desde AQUÍ
#  hasta la sección "2) RANKING DE VARIABLES" o directamente a "3) SELECCIÓN DE TOP-k + CV".)

df <- read_csv(ruta_csv, show_col_types = FALSE)

df <- df %>%
  mutate(
    gano_A         = as.factor(gano_A),
    surface        = as.factor(surface),
    round          = as.factor(round),
    tourney_level  = as.factor(tourney_level),
    player_A_hand  = as.factor(player_A_hand),
    player_B_hand  = as.factor(player_B_hand),
    top10_matchup  = as.factor(top10_matchup)
  )

# Clase positiva = "win"
df$gano_A <- ifelse(as.character(df$gano_A) == "1", "win", "loss")
df$gano_A <- factor(df$gano_A, levels = c("win","loss"))

# Quitar redundancias fuertes de ranking (decisión del TP)
ranking_vars <- c("diff_rank", "player_A_rank_pts", "player_B_rank",
                  "player_A_rank", "player_B_rank_pts")
df_simple <- df %>% select(-all_of(ranking_vars))

# Split 80/20 estratificado
idx   <- createDataPartition(df_simple$gano_A, p = 0.8, list = FALSE)
train <- df_simple[idx, ]
test  <- df_simple[-idx, ]

train$gano_A <- factor(train$gano_A, levels = c("win","loss"))
test$gano_A  <- factor(test$gano_A,  levels = c("win","loss"))

cat("Dimensiones TRAIN/TEST:", nrow(train), nrow(test), "\n")
cat("Prevalencia (train) P(win):", round(mean(train$gano_A == "win"), 4), "\n")

# --------------------------------------------------
# 2) RANKING DE VARIABLES CON RANDOM FOREST (TRAIN)
# --------------------------------------------------
# (Sección pesada. Si ya tenés importancias guardadas en CSV,
#  podés poner RECALC_RF_IMPORTANCE <- FALSE y leerlas.)

imp_csv_path <- file.path(carpeta_graficos, "01_importancia_rf_top20.csv")

if (RECALC_RF_IMPORTANCE || !file.exists(imp_csv_path)) {
  set.seed(42)
  p  <- ncol(train) - 1
  rf <- randomForest(
    gano_A ~ .,
    data = train,
    ntree = 200,
    mtry  = max(1, floor(sqrt(p))),
    importance = TRUE
  )
  
  imp_mat  <- importance(rf)
  imp_df   <- as.data.frame(imp_mat) %>%
    rownames_to_column("variable") %>%
    select(variable, MeanDecreaseAccuracy) %>%
    arrange(desc(MeanDecreaseAccuracy))
  
  invalids <- c("player_A","player_B","tourney_id","tourney_name","tourney_date","match_num","year")
  imp_df_valid <- imp_df %>% filter(!variable %in% invalids)
  
  # Guardar importancias (top-20) para reusar rápido
  imp_df_valid %>% slice_head(n = 20) %>%
    write_csv(imp_csv_path)
  
} else {
  imp_df_valid <- read_csv(imp_csv_path, show_col_types = FALSE)
}

# GRÁFICO 1: Importancia RF (Top-20)
plot_imp <- imp_df_valid %>%
  slice_head(n = 20) %>%
  mutate(variable = fct_reorder(variable, MeanDecreaseAccuracy)) %>%
  ggplot(aes(x = variable, y = MeanDecreaseAccuracy)) +
  geom_col() +
  geom_text(aes(label = round(MeanDecreaseAccuracy, 1)), hjust = -0.1, size = 3) +
  coord_flip() +
  expand_limits(y = max(slice_head(imp_df_valid, n = 20)$MeanDecreaseAccuracy) * 1.10) +
  labs(title = "Importancia de variables (RF - MeanDecreaseAccuracy)",
       x = NULL, y = "MeanDecreaseAccuracy") +
  theme_minimal(base_size = 12)

print(plot_imp)
ggsave(filename = file.path(carpeta_graficos, "01_importancia_rf_top20.png"),
       plot = plot_imp, width = 8, height = 6, dpi = 300)

vars_ordenadas_validas <- (if ("variable" %in% names(imp_df_valid)) imp_df_valid$variable else imp_df_valid[[1]])
cat("Top 10 variables por MDA (RF):\n"); print(head(vars_ordenadas_validas, 10))

# ---------------------------------------------------------------------
# 3) SELECCIÓN DE TOP-k + 4) CV 5-FOLD OPTIMIZANDO AUC (CLASE = 'win')
#     [FIX] Cálculo correcto de AUC (niveles y dirección)
# ---------------------------------------------------------------------
# (Si ya corriste esta parte y tenés resumen_cv_auc.csv, poné RECALC_CV <- FALSE.)

resumen_cv_path <- file.path(carpeta_graficos, "resumen_cv_auc.csv")

if (RECALC_CV || !file.exists(resumen_cv_path)) {
  
  tamanios <- c(5, 8, 10, 12, 15)
  
  ctrl <- trainControl(
    method = "cv",
    number = 5,
    classProbs = TRUE,
    summaryFunction = twoClassSummary,   # calcula ROC, Sens, Spec
    savePredictions = "final",
    verboseIter = FALSE
  )
  
  resumen_cv <- tibble(
    k                = integer(),
    ROC_CV           = double(),
    ROC_CV_SD        = double(),
    Thres_Youden_CV  = double(),
    Sens_CV_at_Thres = double(),
    Spec_CV_at_Thres = double()
  )
  
  umbral_por_k <- list()
  
  for (k in tamanios) {
    top_k <- vars_ordenadas_validas[1:min(k, length(vars_ordenadas_validas))]
    if (length(top_k) == 0) next
    form_k <- as.formula(paste("gano_A ~", paste(top_k, collapse = " + ")))
    
    set.seed(100 + k)
    modelo_cv <- train(
      form = form_k,
      data = train,
      method = "glm",
      family = binomial(),
      trControl = ctrl,
      metric = "ROC"
    )
    
    preds_cv <- modelo_cv$pred
    
    # >>> CHANGED BLOCK 1: AUC y ROC correctamente orientados <<<
    # AUC (usar el que ya calcula caret para el modelo CV)
    auc_cv <- as.numeric(caret::getTrainPerf(modelo_cv)$TrainROC)
    
    # ROC "pooled" sobre TODAS las predicciones de CV, con orientación correcta
    roc_cv <- pROC::roc(
      response  = factor(preds_cv$obs, levels = c("win","loss")),  # caso = "win"
      predictor = preds_cv$win,
      direction = "<"  # prob(win) más alta -> más "caso"
    )
    
    # AUC por fold (para barras de error)
    aucs_fold <- preds_cv %>%
      dplyr::group_by(Resample) %>%
      dplyr::summarize(
        auc = {
          rr <- pROC::roc(
            response  = factor(obs, levels = c("win","loss")),
            predictor = win,
            direction = "<"
          )
          as.numeric(pROC::auc(rr))
        },
        .groups = "drop"
      )
    
    # Umbral de Youden (sobre la ROC "pooled" de CV)
    coords_best <- pROC::coords(
      roc_cv, x = "best", best.method = "youden",
      ret = c("threshold","sensitivity","specificity"),
      transpose = FALSE
    )
    # >>> FIN CHANGED BLOCK 1 <<<
    
    th <- if (is.data.frame(coords_best)) coords_best$threshold[1] else coords_best[1]
    th <- suppressWarnings(as.numeric(th))
    if (!is.finite(th)) th <- 0.5
    
    umbral_por_k[[as.character(k)]] <- th
    
    resumen_cv <- add_row(
      resumen_cv,
      k = k,
      ROC_CV = auc_cv,
      ROC_CV_SD = sd(aucs_fold$auc),
      Thres_Youden_CV = th,
      Sens_CV_at_Thres = if (is.data.frame(coords_best)) coords_best$sensitivity[1] else NA_real_,
      Spec_CV_at_Thres = if (is.data.frame(coords_best)) coords_best$specificity[1] else NA_real_
    )
  }
  
  write_csv(resumen_cv, resumen_cv_path)
  
} else {
  resumen_cv <- read_csv(resumen_cv_path, show_col_types = FALSE)
}

cat("\nResumen CV (ordenado por AUC):\n")
print(resumen_cv %>% arrange(desc(ROC_CV)))

# GRÁFICO 2: AUC vs k (CV) con error bar (SD por fold)
plot_auc_k <- resumen_cv %>%
  ggplot(aes(x = k, y = ROC_CV)) +
  geom_point(size = 3) +
  geom_line() +
  geom_errorbar(aes(ymin = ROC_CV - ROC_CV_SD, ymax = ROC_CV + ROC_CV_SD), width = 0.3) +
  scale_x_continuous(breaks = resumen_cv$k) +
  labs(title = "AUC (CV 5-fold) vs número de variables (k)",
       x = "k (variables seleccionadas)", y = "AUC en CV") +
  theme_minimal(base_size = 12)

print(plot_auc_k)
ggsave(file.path(carpeta_graficos, "02_auc_vs_k_cv.png"),
       plot_auc_k, width = 7, height = 5, dpi = 300)

# GRÁFICO 3: Sensibilidad/Especificidad en Youden (CV)
res_long <- resumen_cv %>%
  mutate(Bal_CV = 0.5*(Sens_CV_at_Thres + Spec_CV_at_Thres)) %>%
  pivot_longer(cols = c(Sens_CV_at_Thres, Spec_CV_at_Thres, Bal_CV),
               names_to = "Metrica", values_to = "Valor")

plot_sens_spec <- res_long %>%
  ggplot(aes(x = k, y = Valor, color = Metrica)) +
  geom_point(size = 3) +
  geom_line() +
  scale_x_continuous(breaks = resumen_cv$k) +
  labs(title = "CV (5-fold): Sensibilidad / Especificidad en umbral de Youden",
       x = "k", y = "Valor") +
  theme_minimal(base_size = 12)

print(plot_sens_spec)
ggsave(file.path(carpeta_graficos, "03_cv_sens_spec_youden.png"),
       plot_sens_spec, width = 7, height = 5, dpi = 300)

# ------------------------------------------------------------
# 5) ELEGIR k POR AUC-CV (desempate por (Sens+Spec)/2 en CV)
# ------------------------------------------------------------
best_row <- resumen_cv %>%
  mutate(Bal_CV = 0.5*(Sens_CV_at_Thres + Spec_CV_at_Thres)) %>%
  arrange(desc(ROC_CV), desc(Bal_CV)) %>%
  slice(1)

mejor_k      <- best_row$k
umbral_final <- best_row$Thres_Youden_CV
vars_finales <- vars_ordenadas_validas[1:mejor_k]

cat("\n>> Selección por CV\n")
cat("   k óptimo (AUC-CV):", mejor_k, "\n")
cat("   Umbral óptimo (Youden en CV):", round(umbral_final, 4), "\n")
cat("   Variables finales (", length(vars_finales), "):\n", paste(vars_finales, collapse = ", "), "\n", sep="")

# -------------------------------------------------------------------
# 6) ENTRENAMIENTO FINAL EN TRAIN Y EVALUACIÓN UNA SOLA VEZ EN TEST
# -------------------------------------------------------------------
form_final <- as.formula(paste("gano_A ~", paste(vars_finales, collapse = " + ")))

set.seed(999)
modelo_final <- train(
  form = form_final,
  data = train,
  method = "glm",
  family = binomial(),
  trControl = trainControl(classProbs = TRUE, summaryFunction = twoClassSummary),
  metric = "ROC"
)

# Probabilidad de 'win' en TEST
prob_test <- predict(modelo_final, newdata = test, type = "prob")[, "win"]

# Clasificación con el UMBRAL elegido en CV
pred_test <- factor(ifelse(prob_test >= umbral_final, "win", "loss"),
                    levels = c("win","loss"))

# Matriz y métricas en TEST
cm_test <- caret::confusionMatrix(pred_test, test$gano_A, positive = "win")
cat("\n=== MÉTRICAS EN TEST ===\n")
print(cm_test)

# ROC/AUC en TEST + GRÁFICO 4 (ROC con punto de corte)
y_true   <- ifelse(test$gano_A == "win", 1, 0)
roc_test <- pROC::roc(response = y_true, predictor = prob_test, levels = c(0,1), direction = "<")
auc_test <- as.numeric(pROC::auc(roc_test))
cat("AUC (TEST):", round(auc_test, 4), "\n")

pt_test <- pROC::coords(roc_test, x = umbral_final, input = "threshold",
                        ret = c("specificity","sensitivity"))

plot_roc <- ggroc(roc_test) +
  # >>> CHANGED BLOCK 2: evitar warning, punto anotado una sola vez <<<
  annotate("point",
           x = 1 - as.numeric(pt_test["specificity"]),
           y = as.numeric(pt_test["sensitivity"]),
           size = 3) +
  # >>> FIN CHANGED BLOCK 2 <<<
  labs(title = sprintf("Curva ROC (TEST) - AUC = %.4f (● corte=%.3f)", auc_test, umbral_final),
       x = "1 - Especificidad", y = "Sensibilidad") +
  theme_minimal(base_size = 12)

print(plot_roc)
ggsave(file.path(carpeta_graficos, "04_roc_test.png"),
       plot_roc, width = 8, height = 6, dpi = 300)

# --------------------------------------------------
# 7) INTERPRETABILIDAD Y DIAGNÓSTICOS
# --------------------------------------------------
glm_final <- modelo_final$finalModel

# OR + IC95% (Wald)
OR  <- exp(coef(glm_final))
SE  <- sqrt(diag(vcov(glm_final)))
ICL <- exp(coef(glm_final) - 1.96*SE)
ICU <- exp(coef(glm_final) + 1.96*SE)

tabla_OR <- tibble(
  Variable = names(OR),
  OR       = as.numeric(OR),
  IC95_L   = as.numeric(ICL),
  IC95_U   = as.numeric(ICU)
)
cat("\nTabla OR + IC95% (Wald):\n"); print(tabla_OR)

# Forestplot (log) resaltando si el IC cruza 1
tabla_OR_plot <- tabla_OR %>%
  filter(Variable != "(Intercept)") %>%
  mutate(
    Variable = fct_reorder(Variable, OR),
    sig = if_else(IC95_L > 1 | IC95_U < 1, "Signif.", "No signif.")
  )

plot_or <- ggplot(tabla_OR_plot, aes(x = Variable, y = OR, ymin = IC95_L, ymax = IC95_U, color = sig)) +
  geom_pointrange() +
  geom_hline(yintercept = 1, linetype = 2) +
  scale_y_log10() +
  coord_flip() +
  scale_color_manual(values = c("Signif." = "black", "No signif." = "grey50")) +
  labs(title = "Efectos (OR) con IC95% - Modelo final (escala log)",
       x = NULL, y = "Odds Ratio (log)") +
  theme_minimal(base_size = 12) +
  theme(legend.position = "bottom")

print(plot_or)
ggsave(file.path(carpeta_graficos, "05_forestplot_or.png"),
       plot_or, width = 8, height = 6, dpi = 300)

# VIF / GVIF
vifs <- tryCatch(car::vif(glm_final), error = function(e) e)
cat("\nVIF / GVIF:\n"); print(vifs)

# Brier score, Brier Skill Score y Calibración por deciles (TEST)
brier <- mean( (prob_test - y_true)^2 )
p0 <- mean(y_true)                      # prevalencia
brier_ref <- mean( (p0 - y_true)^2 )    # baseline sin modelo
bss <- 1 - (brier / brier_ref)          # Brier Skill Score
cat("\nBrier score (TEST):", round(brier, 6), "\n")
cat("Brier Skill Score (TEST):", round(bss, 6), "\n")

cal_df <- tibble(prob = prob_test, y = y_true) %>%
  mutate(bin = ntile(prob, 10)) %>%
  group_by(bin) %>%
  summarise(
    prob_pred = mean(prob),
    prob_obs  = mean(y),
    n = n(),
    .groups = "drop"
  )
cat("\nCalibración por deciles (TEST):\n"); print(cal_df)

plot_cal <- ggplot(cal_df, aes(x = prob_pred, y = prob_obs, label = bin)) +
  geom_abline(slope = 1, intercept = 0, linetype = 2) +
  geom_point(aes(size = n), alpha = 0.8, show.legend = FALSE) +
  geom_text(nudge_y = 0.02, size = 3) +
  labs(title = "Calibración (TEST) por deciles",
       x = "Probabilidad predicha (media del decil)",
       y = "Frecuencia observada (win)") +
  theme_minimal(base_size = 12)

print(plot_cal)
ggsave(file.path(carpeta_graficos, "06_calibracion_test.png"),
       plot_cal, width = 7, height = 5, dpi = 300)

# Calibración diagnóstica: intercepto y slope (no reentrena con test)
logit_p <- qlogis(pmin(pmax(prob_test, 1e-6), 1-1e-6))
cal_lm   <- glm(y_true ~ offset(logit_p), family = binomial())
cal_full <- glm(y_true ~ logit_p,        family = binomial())
cal_intercept <- coef(cal_lm)[["(Intercept)"]]
cal_slope     <- coef(cal_full)[["logit_p"]]
cat("\nCalibración diagnóstica:\n")
cat("  Intercepto (ideal 0):", round(cal_intercept, 4), "\n")
cat("  Slope (ideal 1):    ", round(cal_slope, 4), "\n")

# -----------------
# 8) EXPORTS
# -----------------
write_csv(resumen_cv, file.path(carpeta_graficos, "resumen_cv_auc.csv"))
write_csv(tabla_OR,  file.path(carpeta_graficos, "tabla_or_ic95.csv"))
write_csv(cal_df,    file.path(carpeta_graficos, "calibracion_deciles_test.csv"))

summary_row <- tibble(
  k_optimo = mejor_k,
  umbral_final = umbral_final,
  AUC_TEST = auc_test,
  ACC_TEST = cm_test$overall[["Accuracy"]],
  Sens_TEST = cm_test$byClass[["Sensitivity"]],
  Spec_TEST = cm_test$byClass[["Specificity"]],
  Brier = brier,
  BSS = bss,
  Cal_Intercept = cal_intercept,
  Cal_Slope = cal_slope
)
write_csv(summary_row, file.path(carpeta_graficos, "00_resumen_metricas_test.csv"))

saveRDS(list(modelo_final = modelo_final,
             vars_finales = vars_finales,
             umbral = umbral_final,
             resumen_cv = resumen_cv),
        file.path(carpeta_graficos, "modelo_final_rl.rds"))

sink(file.path(carpeta_graficos, "sessionInfo.txt"))
print(sessionInfo())
sink()

cat("\n>> Listo. Gráficos y tablas en:", carpeta_graficos, "\n")


library(tidyverse)
library(e1071)
library(caret)
library(readxl)

#1. Cargamos el dataset con codificación UTF-8

data <- read.csv("C:/Users/estef/Downloads/df_transform.csv", encoding = "UTF-8", 
                 stringsAsFactors = FALSE,
                 check.names = FALSE) # Evita que R modifique nombres de columnas

# Verificamos las primeras filas y estructura
head(data)
str(data)

# Fijamos una semilla para reproducibilidad
set.seed(123)

#2. Preprocesamiento de datos

# Eliminamos columnas irrelevantes
columnas_no_relevantes <- c("tourney_id", "tourney_name", "match_num", 
                            "player_A", "player_B", "player_A_ioc", "player_B_ioc")
data_modelo <- data %>% select(-all_of(columnas_no_relevantes))

# Convertimos variables categóricas en factores
categorical_cols <- c("surface", "round", "tourney_level", 
                      "player_A_hand", "player_B_hand", 
                      "player_A_entry", "player_B_entry", "top10_matchup")
data_modelo <- data_modelo %>%
  mutate(across(all_of(categorical_cols), as.factor))

# Extraemos el año de tourney_date (si no está en 'year')
if ("tourney_date" %in% colnames(data_modelo)) {
  data_modelo$tourney_date <- as.Date(data_modelo$tourney_date)
  data_modelo$year <- as.numeric(format(data_modelo$tourney_date, "%Y"))
  data_modelo <- data_modelo %>% select(-tourney_date)
}

# Verificamos valores faltantes
colSums(is.na(data_modelo))

# 1. Identificamos columnas numéricas
num_cols <- names(data_modelo)[sapply(data_modelo, is.numeric)]

# 2. Imputamos medianas en columnas numéricas
data_modelo[num_cols] <- lapply(data_modelo[num_cols], function(x) {
  x[is.na(x)] <- median(x, na.rm = TRUE)
  return(x)
})

# 3. Identificamos columnas categóricas (factor o character)
cat_cols <- names(data_modelo)[sapply(data_modelo, function(x) is.factor(x) || is.character(x))]

# 4. Función para imputar moda sin dañar niveles
imputar_moda <- function(x) {
  moda <- names(which.max(table(x)))
  x[is.na(x)] <- moda
  return(as.factor(x))  # Forzamos factor con etiquetas legibles
}

# 5. Imputamos moda
data_modelo[cat_cols] <- lapply(data_modelo[cat_cols], imputar_moda)

levels(data_modelo$surface)

# Convertimos factores a variables dummy para poder aplicar SVM
data_modelo <- model.matrix(~ . - 1, data = data_modelo) %>% as.data.frame()

# 3. Separar variable objetivo y escalado

#separamos las caracteristicas (x) de la variable objetivo (y= gano_A)
#y escalamos las caracteristicas numericas para que el SVM funcione
#correctamente.

# Separamos variable objetivo
X <- data_modelo %>% select(-gano_A)
y <- as.factor(data$gano_A) # Convertimos a factor para clasificación

# Escalamos las características numéricas
preProc <- preProcess(X, method = c("center", "scale"))
X_scaled <- predict(preProc, X)

#4. Division en conjunto de entrenamiento y prueba

#Dividimos los datos en 80% entrenamiento y 20% prueba.

# Dividimos 80% entrenamiento, 20% prueba
trainIndex <- createDataPartition(y, p = 0.8, list = FALSE)
X_train <- X_scaled[trainIndex, ]
X_test <- X_scaled[-trainIndex, ]
y_train <- y[trainIndex]
y_test <- y[-trainIndex]

#6. Entrenamiento del modelo SVM

gamma_value <- 1 / ncol(X_train)
#determina el alcance 
# de la influencia de un solo dato de entrenamiento

# Estratificamos la muestra 
sample_idx <- createDataPartition(y_train, p = 5000 / nrow(X_train), list = FALSE)

# Entrenamos modelo de prueba
svm_model_test <- svm(
  x = X_train[sample_idx, ],
  y = y_train[sample_idx],
  kernel = "radial",
  cost = 1,
  gamma = gamma_value,  
  probability = TRUE
)

svm_model_test

# Muestra estratificada de 50000 para tuning (ya la tenés)
sample_idx <- createDataPartition(y_train, p = 5000 / nrow(X_train), list = FALSE)

# Definimos la grilla de hiperparámetros
param_grid <- list(
  cost = c(0.1, 1, 10),
  gamma = c(0.01, 0.1, 1)
)

# Ajustamos con validación cruzada de 3 folds
svm_tune <- tune(
  METHOD = svm,
  train.x = X_train[sample_idx, ],
  train.y = y_train[sample_idx],
  kernel = "radial",
  ranges = param_grid,
  tunecontrol = tune.control(cross = 3)
)

summary(svm_tune)

# Mostramos la mejor combinación de hiperparámetros
print(svm_tune$best.parameters)

# Guardamos el mejor modelo ajustado
best_svm_model <- svm_tune$best.model

#7. Evaluacion del modelo

# Paso 2 — Evaluación en conjunto de test
library(caret)

# Predicciones en el test set
y_pred <- predict(best_svm_model, X_test)
y_pred

# Matriz de confusión
conf_matrix <- confusionMatrix(y_pred, y_test)
print(conf_matrix)

# Reentrenar el modelo con probability = TRUE para calcular AUC y ROC
best_svm_prob <- svm(
  x = X_train[sample_idx, ],
  y = y_train[sample_idx],
  kernel = "radial",
  cost = svm_tune$best.parameters$cost,
  gamma = svm_tune$best.parameters$gamma,
  probability = TRUE
)
best_svm_prob

# Obtener probabilidades sobre test
pred_prob <- attr(predict(best_svm_prob, X_test, probability = TRUE), "probabilities")[,2]
pred_prob

# ROC y AUC
library(pROC)
roc_obj <- roc(response = y_test, predictor = pred_prob)
auc(roc_obj)
plot(roc_obj)

precision <- posPredValue(y_pred, y_test, positive = "1")
recall <- sensitivity(y_pred, y_test, positive = "1")
f1 <- (2 * precision * recall) / (precision + recall)

precision
recall
f1

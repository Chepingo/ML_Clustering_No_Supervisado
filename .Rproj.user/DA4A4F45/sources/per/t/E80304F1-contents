set.seed(1234)
train_idx_arbol_regre <- sample(seq_len(nrow(data_pokemon_3)), size = 0.8 * nrow(data_pokemon_3))
train <- data_pokemon_3[train_idx_arbol_regre, ]
test  <- data_pokemon_3[-train_idx_arbol_regre, ]

# 3. Ajustar árbol de regresión básico (cp = 0 → sin poda)
arbol_basico <- rpart(
  data_pokemon_3$Base.Stat.Total ~ data_pokemon_3$Weight..hg. + data_pokemon_3$Weight..lbs. + data_pokemon_3$imc_pokemon + data_pokemon_3$Catch.Rate +
    data_pokemon_3$Color.ID + data_pokemon_3$Generation + data_pokemon_3$Speed,
  data = train,
  control = rpart.control(cp = 0,
                          minsplit = 50,     # al menos 50 observaciones para intentar una partición
                          maxdepth = 5)
  
)

# 4. Visualizar el árbol
rpart.plot(
  arbol_basico,
  type          = 2,    # etiquetas en los nodos
  fallen.leaves = TRUE, # hojas al nivel del gráfico
  cex = 0.8,
  main          = "Árbol de regresión (sin poda)"
)




### version 2 K-FOLDS CV | grid search

# 1. Renombra columnas para que sean nombres sintácticos válidos (opcional)
names(data_pokemon_3) <- make.names(names(data_pokemon_3))

# 2. Define en un vector los predictores y crea la fórmula dinámicamente
vars <- c("Weight..hg.","Weight..lbs.","imc_pokemon",
          "Catch.Rate","Defense","Attack","Speed")
fmla <- reformulate(vars, response = "Base.Stat.Total")

# 3. Divide en train/test (80/20)
set.seed(123)
train_idx <- sample(seq_len(nrow(data_pokemon_3)), 
                    size = 0.8 * nrow(data_pokemon_3))
train <- data_pokemon_3[train_idx, ]
test  <- data_pokemon_3[-train_idx, ]

# 4. Configura el control de entrenamiento con k-fold CV
library(caret)
ctrl <- trainControl(
  method      = "cv",    # validación cruzada
  number      = 5,       # 5 pliegues
  verboseIter = TRUE     # muestra progreso
)

# 5. Define la grilla para el hiperparámetro cp
grid <- expand.grid(cp = seq(0.001, 0.05, by = 0.005))

# 6. Entrena con búsqueda en la grilla y k-fold CV
set.seed(123)
modelo_cv <- train(
  fmla,
  data      = train,
  method    = "rpart",
  trControl = ctrl,
  tuneGrid  = grid
)

# 7. Revisa resultados y el mejor cp
print(modelo_cv)                   # métricas para cada cp
best_cp <- modelo_cv$bestTune$cp   # valor óptimo de cp

# 8. Visualiza el árbol podado por k-fold CV
library(rpart.plot)
rpart.plot(
  modelo_cv$finalModel,
  type          = 2,
  cex = 0.6,
  fallen.leaves = TRUE,
  main          = paste("Árbol rpart (cp =", best_cp, ")")
)















#################                   SVM
# predecir si es legendario o no
set.seed(1234)
idx <- createDataPartition(data_pokemon_5$Legendary.Status, p = 0.7, list = FALSE)
train <- data_pokemon_5[idx, ]
test  <- data_pokemon_5[-idx, ]



ctrl <- trainControl(
  method            = "repeatedcv",
  number            = 10,
  repeats           = 3,
  classProbs        = TRUE,
  summaryFunction   = twoClassSummary,
  sampling          = "smote",        # cambio aquí para probar distintos
  savePredictions   = "final",
  verboseIter       = TRUE
)

svmGrid <- expand.grid(
  sigma = c(0.001, 0.005, 0.01, 0.05),
  C     = c(0.25, 0.5, 1, 2, 4)
)
preproc <- c("center", "scale")


set.seed(1234)
svm_model <- train(
  Legendary.Status ~ .,
  data        = train,
  method      = "svmRadial",
  metric      = "ROC",           # optimizar AUC
  preProcess  = preproc,
  tuneGrid    = svmGrid,
  trControl   = ctrl
)


ctrl_boot <- trainControl(
  method          = "boot",
  number          = 100,               # 100 muestras bootstrap
  classProbs      = TRUE,
  summaryFunction = twoClassSummary,
  savePredictions = "final",
  verboseIter     = TRUE
)

set.seed(1234)
svm_boot <- train(
  Legendary.Status ~ .,
  data       = train,
  method     = "svmRadial",
  metric     = "ROC",
  preProcess = c("center","scale"),
  tuneGrid   = svmGrid,
  trControl  = ctrl_boot
)
###     MCCC
preds <- predict(svm_refined, newdata = test)

# 2) Extrae la verdad
truth <- test$Legendary.Status

# 3) Calcula el MCC
mcc_value <- mcc_vec(truth = truth, estimate = preds)

# 4) Muestra el resultado
print(mcc_value)


library(caret)
resamps <- resamples(list(SVM = svm_boot, SVM = svm_model))
summary(resamps)
bwplot(resamps, metric = "ROC")































# -------------------------------
# LIBRERÍAS
# -------------------------------
library(caret)
library(pROC)
library(dplyr)
library(gbm)
library(e1071)
library(xgboost)

# -------------------------------
# PREPARACIÓN DE DATOS
# -------------------------------
num_vars <- data_pokemon_4[, sapply(data_pokemon_4, is.numeric)]
num_vars$Legendary.Status <- data_pokemon_4$Legendary.Status

set.seed(1234)
idx <- createDataPartition(num_vars$Legendary.Status, p = 0.7, list = FALSE)
train <- num_vars[idx, ]
test  <- num_vars[-idx, ]

# -------------------------------
# CONTROL DE ENTRENAMIENTO ROBUSTO
# -------------------------------
ctrl_cv <- trainControl(
  method = "repeatedcv",
  number = 10,
  repeats = 3,
  classProbs = TRUE,
  savePredictions = "final",
  summaryFunction = twoClassSummary,
  verboseIter = TRUE
)

# -------------------------------
# REJILLAS EXTENDIDAS
# -------------------------------

# Random Forest
grid_rf <- expand.grid(mtry = c(2, 4, 6))

# XGBoost
grid_xgb <- expand.grid(
  nrounds = c(50, 100),
  max_depth = c(3, 6),
  eta = c(0.05, 0.1),
  gamma = c(0, 1),
  colsample_bytree = c(0.7, 1),
  min_child_weight = c(1, 5),
  subsample = c(0.8, 1)
)

# SVM radial
grid_svm <- expand.grid(
  sigma = c(0.005, 0.01, 0.05),
  C = c(0.5, 1, 2)
)

# -------------------------------
# ENTRENAR MODELOS BASE
# -------------------------------
set.seed(1234)
model_rf <- train(
  Legendary.Status ~ ., data = train,
  method = "rf",
  trControl = ctrl_cv,
  tuneGrid = grid_rf,
  metric = "ROC"
)

set.seed(1234)
model_xgb <- train(
  Legendary.Status ~ ., data = train,
  method = "xgbTree",
  trControl = ctrl_cv,
  tuneGrid = grid_xgb,
  metric = "ROC",
  verbose = FALSE
)

set.seed(1234)
model_svm <- train(
  Legendary.Status ~ ., data = train,
  method = "svmRadial",
  trControl = ctrl_cv,
  tuneGrid = grid_svm,
  metric = "ROC",
  preProcess = c("center", "scale")
)

# -------------------------------
# PREDICCIONES EN TEST SET
# -------------------------------
pred_rf  <- predict(model_rf,  newdata = test, type = "prob")[, "True"]
pred_xgb <- predict(model_xgb, newdata = test, type = "prob")[, "True"]
pred_svm <- predict(model_svm, newdata = test, type = "prob")[, "True"]

blend_df <- data.frame(
  rf  = pred_rf,
  xgb = pred_xgb,
  svm = pred_svm,
  Legendary.Status = test$Legendary.Status
)

# -------------------------------
# META-MODELO (GLM)
# -------------------------------
blend_ctrl <- trainControl(
  method = "repeatedcv",
  number = 10,
  repeats = 3,
  classProbs = TRUE,
  summaryFunction = twoClassSummary,
  savePredictions = "final"  # NECESARIO para acceder a blend_model$pred
)

blend_model <- train(
  Legendary.Status ~ ., data = blend_df,
  method = "glm",
  trControl = blend_ctrl,
  metric = "ROC"
)
# -------------------------------
# EVALUACIÓN FINAL
# -------------------------------
blend_probs <- predict(blend_model, newdata = blend_df, type = "prob")[, "True"]
blend_preds <- factor(ifelse(blend_probs > 0.5, "True", "False"), levels = c("False", "True"))

conf_matrix <- confusionMatrix(blend_preds, blend_df$Legendary.Status, positive = "True")
print(conf_matrix)

roc_obj <- roc(blend_df$Legendary.Status, blend_probs, levels = c("False", "True"))
plot(roc_obj, main = "Curva ROC - Blending Robusto (RF + XGB + SVM)")
auc(roc_obj)







































# -------------------------------
# LIBRERÍAS NECESARIAS
# -------------------------------
library(caret)
library(caretEnsemble)
library(dplyr)
library(xgboost)
library(e1071)

# -------------------------------
# PREPARACIÓN DE DATOS
# -------------------------------
# Usa solo las variables numéricas más la clase objetivo
num_vars <- data_pokemon_4[, sapply(data_pokemon_4, is.numeric)]
num_vars$Legendary.Status <- data_pokemon_4$Legendary.Status

# Separar en train/test (estratificado)
set.seed(4321)
idx <- createDataPartition(num_vars$Legendary.Status, p = 0.7, list = FALSE)
train <- num_vars[idx, ]
test  <- num_vars[-idx, ]  # test solo para después, no usado ahora

# -------------------------------
# CONTROL DE ENTRENAMIENTO ROBUSTO
# -------------------------------
ctrl <- trainControl(
  method = "repeatedcv",
  number = 10,
  repeats = 5,
  classProbs = TRUE,
  savePredictions = "final",
  summaryFunction = twoClassSummary,
  verboseIter = TRUE,
  allowParallel = TRUE
)

# -------------------------------
# DEFINICIÓN DE MODELOS BASE CON REJILLA
# -------------------------------
model_list <- caretList(
  Legendary.Status ~ ., data = train,
  trControl = ctrl,
  metric = "ROC",
  tuneList = list(
    rf = caretModelSpec(
      method = "rf",
      tuneGrid = expand.grid(mtry = c(2, 4, 6, 8))
    ),
    xgb = caretModelSpec(
      method = "xgbTree",
      tuneGrid = expand.grid(
        nrounds = c(100, 200),
        max_depth = c(3, 6),
        eta = c(0.01, 0.05, 0.1),
        gamma = c(0, 1),
        colsample_bytree = c(0.7, 1),
        min_child_weight = c(1, 5),
        subsample = c(0.8, 1)
      ),
      verbose = FALSE
    ),
    svm = caretModelSpec(
      method = "svmRadial",
      tuneGrid = expand.grid(
        sigma = c(0.005, 0.01, 0.05),
        C = c(0.5, 1, 2, 4)
      ),
      preProcess = c("center", "scale")
    )
  )
)

# -------------------------------
# STACKING FINAL CON glmnet (regresión penalizada)
# -------------------------------
stack_ctrl <- trainControl(
  method = "repeatedcv",
  number = 10,
  repeats = 5,
  classProbs = TRUE,
  summaryFunction = twoClassSummary,
  savePredictions = "final"
)

stack_model <- caretStack(
  model_list,
  method = "glmnet",
  metric = "ROC",
  trControl = stack_ctrl
)

# -------------------------------
# (NO imprimir ni evaluar aún)
# -------------------------------



# -------------------------------
# MATRIZ DE CONFUSIÓN
# -------------------------------
conf_matrix <- confusionMatrix(stack_preds, test$Legendary.Status, positive = "True")
print(conf_matrix)

# -------------------------------
# AUC y Curva ROC
# -------------------------------
library(pROC)
stack_probs <- stack_probs$True  # Asegúrate de usar la columna correcta si es un data frame
roc_obj <- roc(test$Legendary.Status, stack_probs, levels = c("False", "True"))
auc_value <- auc(roc_obj)
plot(roc_obj, main = "Curva ROC - Modelo Stacking")
cat("AUC:", round(auc_value, 4), "\n")

# -------------------------------
# Otras métricas relevantes 
# -------------------------------
# Sensibilidad (Recall)
sens <- conf_matrix2$byClass["Sensitivity"]

# Especificidad
spec <- conf_matrix2$byClass["Specificity"]

# Precisión (PPV)
precision <- conf_matrix2$byClass["Precision"]

# Valor predictivo negativo (NPV)
npv <- conf_matrix2$byClass["Neg Pred Value"]

# Balanced Accuracy
bal_acc <- conf_matrix2$byClass["Balanced Accuracy"]

# F1 Score
F1 <- 2 * (precision * sens) / (precision + sens)

# Kappa
kappa <- conf_matrix2$overall["Kappa"]

# Matthews Correlation Coefficient (MCC)
tp <- conf_matrix2$table[2, 2]
tn <- conf_matrix2$table[1, 1]
fp <- conf_matrix2$table[1, 2]
fn <- conf_matrix2$table[2, 1]
numerador   <- (tp * tn) - (fp * fn)
denominador <- sqrt((tp + fp) * (tp + fn) * (tn + fp) * (tn + fn))
MCC <- ifelse(denominador == 0, NA, numerador / denominador)

# -------------------------------
# Imprimir todas las métricas
# -------------------------------
cat("Accuracy: ", round(conf_matrix2$overall["Accuracy"], 4), "\n")
cat("Kappa: ", round(kappa, 4), "\n")
cat("Sensitivity (Recall): ", round(sens, 4), "\n")
cat("Specificity: ", round(spec, 4), "\n")
cat("Precision (PPV): ", round(precision, 4), "\n")
cat("Negative Predictive Value (NPV): ", round(npv, 4), "\n")
cat("Balanced Accuracy: ", round(bal_acc, 4), "\n")
cat("F1 Score: ", round(F1, 4), "\n")
cat("MCC: ", round(MCC, 4), "\n")
cat("AUC: ", round(auc_value, 4), "\n")
























#####|              CLUSTERING            #####
# ------------------------- 1. Preparación ---------------------------
# 1. Instalación y carga de librerías
install.packages(c("mlr", "clusterSim", "GGally"))
library(mlr)
library(clusterSim)   # para índices internos como Davies-Bouldin
library(GGally)

# 2. Selección y escalado de variables numéricas
#    Asumimos que data_pok_3 ya está en tu entorno.
num_vars <- names(data_pok_3)[sapply(data_pok_3, is.numeric)]
data_num3 <- data_pok_3[, num_vars]
data_num3_scaled <- scale(data_num3)     # centra y divide por SD :contentReference[oaicite:0]{index=0}

# 3. Definición de la tarea de clustering y del learner base
task       <- makeClusterTask(data = as.data.frame(data_num3_scaled))
kMeansLrn  <- makeLearner(
  "cluster.kmeans",
  par.vals = list(iter.max = 100, nstart = 10)
)                                       # iter.max y nstart como en el tutorial :contentReference[oaicite:1]{index=1}

# 4. Espacio de búsqueda de hiperparámetros
kMeansParamSpace <- makeParamSet(
  makeDiscreteParam("centers",   values = 2:10),
  makeDiscreteParam("algorithm", values = c("Hartigan-Wong", "Lloyd", "MacQueen"))
)                                       # ajustar número de clusters y algoritmo :contentReference[oaicite:2]{index=2}

# 5. Configuración de grid search y CV
gridSearch <- makeTuneControlGrid()       # búsqueda exhaustiva tipo “battleship” :contentReference[oaicite:3]{index=3}
cv10       <- makeResampleDesc("CV", iters = 10)  # 10-fold CV :contentReference[oaicite:4]{index=4}


tunedK <- tuneParams(
  learner    = kMeansLrn,
  task       = task,
  resampling = cv10,
  par.set    = kMeansParamSpace,
  control    = gridSearch
)

# Imprime los mejores hiperparámetros
print(tunedK)                                       # optimiza según Davies-Bouldin y pseudo-F :contentReference[oaicite:5]{index=5}

kMeansTuned <- setHyperPars(kMeansLrn, par.vals = tunedK$x)
finalModel  <- train(kMeansTuned, task)

# Para ver el resultado
print(finalModel)



################################
#############################   CLUSTER  V2
##################################
library(cluster)
library(purrr)

# Escalado de las variables numéricas
scaled <- scale(select(data_pok_3, where(is.numeric)))

# Calcular la silueta media para k = 2 a 10
set.seed(123)
sil_scores <- map_dbl(2:10, function(k){
  km <- kmeans(scaled, centers = k, nstart = 25)
  ss <- silhouette(km$cluster, dist(scaled))
  mean(ss[, "sil_width"])
})
plot(2:10, sil_scores, type = "b", xlab = "k", ylab = "Silueta media")   # ← [Imagen: Gráfico de silueta media]


opt_k <- which.max(sil_scores) + 1
set.seed(1234)
km_final <- kmeans(data_num3_scaled, centers = opt_k, nstart = 50)


library(factoextra)
fviz_cluster(km_final,
             data = data_num3_scaled,
             geom = "point",
             ellipse.type = "norm",
             main = "diagrama resultado del modelo kmeans",
             ggtheme = theme_minimal())    # ← [Imagen: Clusters visualizados por PCA]


centers <- km_final$centers
print(round(centers, 2))    # ← [Imagen o tabla: Centros promedio de cada cluster]

data_pok_3$cluster <- km_final$cluster
data_pok_3$cluster

####ootheeeeeeeeeeeee
library(dbscan)
library(cluster)
library(aricode)  # Para NMI
library(mclust)   # Para ARI
library(clusterSim)

# Grid search sobre eps y minPts
eps_values <- seq(0.5, 3, by = 0.1)
minpts_values <- 3:10
results <- expand.grid(eps = eps_values, minPts = minpts_values)
results$ARI <- NA
results$NMI <- NA
results$silhouette <- NA
results$DB <- NA

for(i in 1:nrow(results)){
  db <- dbscan(data_num3_scaled, eps = results$eps[i], minPts = results$minPts[i])
  clusters <- db$cluster
  if(length(unique(clusters)) > 1 && sum(clusters) > 0){ # Hay más de 1 cluster y no todos son ruido
    # Métricas internas
    sil <- silhouette(clusters, dist(data_num3_scaled))
    results$silhouette[i] <- mean(sil[, "sil_width"])
    results$DB[i] <- index.DB(as.matrix(data_num3_scaled), clusters)$DB
    # Métricas externas (si tienes etiqueta real)
    results$ARI[i] <- adjustedRandIndex(clusters, data_pok_3$Legendary.Status)
    results$NMI[i] <- NMI(clusters, data_pok_3$Legendary.Status)
  }
}

# Elige la mejor configuración (por ejemplo, máxima silueta)
best <- which.max(results$silhouette)
cat("Mejor configuración: eps =", results$eps[best], "minPts =", results$minPts[best], "\n")

# Ejecuta DBSCAN final con esos hiperparámetros
db_final <- dbscan(scaled, eps = results$eps[best], minPts = results$minPts[best])
table(db_final$cluster)  # Cuenta de clusters y ruido

# Visualización
library(factoextra)
fviz_cluster(list(data = scaled, cluster = db_final$cluster), geom = "point")



### rendimiento
library(cluster)
library(clusterSim)

clusters <- db_final$cluster  # O km_final$cluster, gmm_model$classification, etc.

# Silueta (si hay más de 1 cluster y no todo es ruido)
if(length(unique(clusters)) > 1 && sum(clusters) > 0){
  sil <- silhouette(clusters, dist(data_num3_scaled))
  sil_media <- mean(sil[, "sil_width"])
} else {
  sil_media <- NA
}

# Davies-Bouldin
db_index <- index.DB(as.matrix(data_num3_scaled), clusters)$DB

# Calinski-Harabasz
ch_index <- index.G1(as.matrix(data_num3_scaled), clusters)$G1

# Dunn
dunn_index <- tryCatch({
  index.Dunn(dist(data_num3_scaled), clusters)
}, error = function(e) NA)






















######        REGLAS DE ASOCIACIÓN       #######
# ---------------------------------------------------------
# 1. Preparar un data-frame transaccional
# ---------------------------------------------------------
library(tidyverse)
library(arules)
library(arulesViz)

df_tx <- data_pokemon_4 %>% 
  transmute(
    Type1   = paste0("Type1_", Type.1),
    Type2   = ifelse(Type.2 == "" | is.na(Type.2),
                     "Type2_None",
                     paste0("Type2_", Type.2)),
    Legend  = paste0("Legend_", Legendary.Status)
  )

# Convertir a objeto transactions
tx <- as(df_tx, "transactions")

# ---------------------------------------------------------
# 2. Generar reglas Apriori
#   – soporte mínimo   = 4 % de los Pokémon
#   – confianza mínima = 50 %
# ---------------------------------------------------------
rules <- apriori(
  tx,
  parameter = list(supp = 0.04, conf = 0.5, minlen = 2),
  appearance = list(default = "both")   # sin restringir lhs/rhs
)

# ---------------------------------------------------------
# 3. Filtrar y ordenar por *lift*
# ---------------------------------------------------------
top_rules <- sort(rules, by = "lift", decreasing = TRUE)[1:15]

# Imprimir resultados clave
inspect(top_rules)






#######




















              
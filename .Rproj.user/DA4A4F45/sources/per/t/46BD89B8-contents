#####         INGENIERIA DE VAR | PCAS
#####
#####

##INICIO
# 1. Selecciona y escala sólo las variables numéricas
num_vars <- data_pokemon_4[, sapply(data_pokemon_4, is.numeric)]
cat_vars <- names(df)[sapply(df, is.factor)]
df <- data_pokemon_4

# -------------------------------
# EXTRAER VARIABLES POR TIPO
# -------------------------------
cat_vars <- names(df)[sapply(df, is.factor)]
num_vars <- names(df)[sapply(df, function(x) is.numeric(x) || is.integer(x))]

# -------------------------------
# MATRIZ CATEGÓRICA vs CATEGÓRICA (Cramér’s V)
# -------------------------------
cramer_matrix <- matrix(NA, nrow = length(cat_vars), ncol = length(cat_vars),
                        dimnames = list(cat_vars, cat_vars))

for (i in cat_vars) {
  for (j in cat_vars) {
    tbl <- table(df[[i]], df[[j]])
    if (min(dim(tbl)) > 1) {
      cramer_matrix[i, j] <- cramersV(tbl)
    }
  }
}

# -------------------------------
# MATRIZ CATEGÓRICA BINARIA vs NUMÉRICA (Punto-biserial)
# -------------------------------
cat_bin <- cat_vars[sapply(df[cat_vars], function(x) nlevels(x) == 2)]

cat_num_matrix <- matrix(NA, nrow = length(cat_bin), ncol = length(num_vars),
                         dimnames = list(cat_bin, num_vars))

for (i in cat_bin) {
  for (j in num_vars) {
    x_bin <- as.numeric(df[[i]]) - 1  # 0/1 para binario
    y_num <- df[[j]]
    if (all(!is.na(x_bin)) && all(!is.na(y_num))) {
      cat_num_matrix[i, j] <- cor(x_bin, y_num, method = "pearson", use = "complete.obs")
    }
  }
}

# -------------------------------
# RESULTADOS
# -------------------------------

# Ver resultados redondeados en consola
print("Matriz Cramér’s V (categóricas vs categóricas):")
print(round(cramer_matrix, 2))

print("Matriz punto-biserial (categóricas binarias vs numéricas):")
print(round(cat_num_matrix, 2))

# Visualización completa (opcional)
View(cramer_matrix)
View(cat_num_matrix)














# 2. Calcula el PCA
pca_res <- prcomp(dat_num, center = FALSE, scale. = FALSE)

# 3. Calcula la varianza de cada componente (autovalores)
varianza <- pca_res$sdev^2

# 4. Dibuja el Scree plot
plot(
  varianza,
  type = "b", 
  pch  = 19,
  xlab = "Componente principal",
  ylab = "Autovalor (varianza)",
  main = "Scree plot: varianza por componente"
)
abline(h = 1, lty = 2)  # línea de Kaiser en 1 (opcional)










## REVISAR TODOS LOS POSIBLES PCAS
pca_res <- prcomp(dat_num)
barplot(pca_res$rotation[,1],
        las   = 2,
        main  = "Cargas de variables en PC1",
        ylab  = "Peso de la variable")








##MOSTRAR MEJORES PCAS
# 1. Calcula la varianza explicada por cada PC
varianza     <- pca_res$sdev^2
prop_var     <- varianza / sum(varianza)       # proporción de varianza por PC
var_acumulada<- cumsum(prop_var)               # varianza acumulada

# 2. Elige un umbral, por ejemplo 80 % de la varianza total
umbral <- 0.80
# 3. Encuentra cuántas PCs hacen falta para superar ese umbral
k <- min(which(var_acumulada >= umbral))

# 4. Mostrar resultados
round(prop_var[1:k], 3)        # varianza individual de las primeras k PCs
round(var_acumulada[1:k], 3)   # varianza acumulada tras k PCs
cat("Se retendrán", k, "componentes (hasta PC", k, ") para abarcar al menos el", umbral*100, "% de la varianza.\n")

# 5. Extraer las puntuaciones de esas PCs en un nuevo data.frame
mejores_pcs <- as.data.frame(pca_res$x[, 1:k])
head(mejores_pcs)              # primeras filas de los índices PC1…PCk

# 6. (Opcional) Ver las cargas de cada variable en esas PCs
cargas_pcs <- pca_res$rotation[, 1:k]
round(cargas_pcs, 2)           # matriz variables × componentes














###### observación de mejores variables
# 1. Calcula tu PCA (sobre dat_num ya escalado)
pca_res <- prcomp(dat_num, center = FALSE, scale. = FALSE)

# 2. Extrae la matriz de cargas (variables × componentes)
cargas <- pca_res$rotation

# 3. Define cuántas variables “top” quieres por componente
topN <- 3

# 4. Para cada una de las k primeras componentes, obtiene las topN variables
top_vars_por_pc <- apply(cargas, 2, function(col) {
  # ordena por carga absoluta y toma los nombres de las primeras topN
  names(sort(abs(col), decreasing = TRUE)[1:topN])
})

# 5. Muestra el resultado
print(top_vars_por_pc)





















####
varianza       <- pca_res$sdev^2
prop_var       <- varianza/sum(varianza)
varianza_acum  <- cumsum(prop_var)
round(varianza_acum[1:6], 2)





















# 1. PCA defensividad (añade centrado si aún no quieres escalar)
pca_def <- prcomp(
  data_pokemon_4[, c("Health","Defense","Special.Defense")],
  center = TRUE,      # resta la media
  scale. = FALSE      # NO divide entre SD
)
data_pokemon_4$PC1_Defensividad <- pca_def$x[,1]

# 2. PCA altura (variables ya log-escaladas)
pca_alt <- prcomp(
  data_pokemon_4[, c("Height..dm.","Height..in.")],
  center = TRUE,      # centra, pero NO escala
  scale. = FALSE
)
data_pokemon_4$PC1_Altura <- pca_alt$x[,1]

# 3. PCA peso (variables ya log-escaladas)
pca_pes <- prcomp(
  data_pokemon_4[, c("Weight..hg.","Weight..lbs.")],
  center = TRUE,      # centra, pero NO escala
  scale. = FALSE
)
data_pokemon_4$PC1_Peso <- pca_pes$x[,1]




pca_pes   <- prcomp(
  data_pokemon_4[, vars_peso],
  center = TRUE,
  scale. = TRUE   # aquí sí escalamos
)
data_pokemon_4$PC1_Peso <- pca_pes$x[,1]













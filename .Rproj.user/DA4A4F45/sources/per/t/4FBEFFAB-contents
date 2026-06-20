numeric_data <- data_pok_1[sapply(data_pok_1, is.numeric)]
numeric_data_for_pca <- names(data_pokemon_3)[ sapply(data_pokemon_3, is.numeric) ]

factor_data <- data_pokemon_3[sapply(data_pokemon_3, is.factor)]

cor_matrix <- cor(numeric_data, use = "complete.obs", method = "pearson")

round(cor_matrix, 3)  # Redondear a 3 decimales


corrplot(cor_matrix, 
         method = "color",
         type = "upper",
         main = "matriz correlacion pearson variables numericas",
         tl.cex = 1.2)

corrgram(cor_matrix, order = FALSE,
         main = "correlograma  de variables numericas de pokemon (Unordered)",
         lower.panel = panel.conf, upper.panel = panel.ellipse,
         diag.panel = panel.minmax, text.panel = panel.txt)

corrplot(cor_matrix,
         method = "color",       # Usa color de fondo
         type = "upper",         # Solo triángulo superior (puedes poner "full")
         addCoef.col = "black",  # Agrega el valor numérico en color negro
         number.cex = 0.8,       # Tamaño de los números
         tl.cex = 1.1,           # Tamaño de etiquetas
         tl.srt = 45,            # Rotación de etiquetas
         main = "Matriz de correlación con valores")


######-----------------||     sacar n más correlacionados     ||-------------------
library(dplyr)
library(tidyr)
library(tibble)   # contiene rownames_to_column

# 1. Matriz de correlación
num_idx   <- sapply(data_pok_3, is.numeric)
datos_num <- data_pok_3[, num_idx]
corr_mat  <- cor(datos_num, use = "pairwise.complete.obs")

# 2. Convertir con rownames_to_column
corr_long <- as.data.frame(corr_mat) %>%
  rownames_to_column("Var1") %>%
  pivot_longer(-Var1, names_to = "Var2", values_to = "corr") %>%
  filter(Var1 < Var2) %>%
  mutate(abs_corr = abs(corr)) %>%
  arrange(desc(abs_corr))

# 3. Mostrar las n mejores
n <- 10
head(corr_long, n)
















sd
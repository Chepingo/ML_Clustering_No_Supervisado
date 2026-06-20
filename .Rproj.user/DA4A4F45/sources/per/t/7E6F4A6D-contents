##numericos
#histograma
hist(data_pok_3$Catch.Rate,
     col = heat.colors(18),
     border = "white",
     breaks = 20,
     main = "histograma de variables 'altura de un pokemon'",
     xlab = "rango de altura por pokemon",
     ylab = "cantidad de pokemon",
     labels = TRUE)
#ploteo regresion
  plot(data_pok_1$Defense,
     ylab = "indice de defensa",
     xlab = "cantidad de pokemon", 
     main = "distribución de variable defensa", 
     col="orange",
     pch=18,
     cex=1.1)
# Ejemplo de boxplot personalizado
boxplot(data_pok_1$Defense,
        ylab = "defensa",
        col = "green",          # Color del boxplot
        border = "black",     # Color del borde
        pch= 19,
        main = "Distribución de defensa por pokemon")

boxplot(data_pokemon_3$Secondary.Typing ~ data_pokemon_3$Primary.Typing,
        data = pokemon_bruto,
        col = "lightblue",
        las = 2,                    # gira etiquetas del eje X
        main = "Peso por tipo principal",
        ylab = "Peso (hg)",
        xlab = "Tipo")

##categoricos
#barplot
barplot(table(data_pok_1$Height..dm.))     # gráfico de barras
########
#   bivariado
plot(numeric_data, factor_data,
      main = "altura / peso")



#####     analisis factor 
plot_frq(data_pokemon_3$Form)














vars_alt <- c("Height..dm.","Height..in.")
pca_alt  <- prcomp(
  data_pokemon_4[, vars_alt],
  center = TRUE,
  scale. = TRUE      # aquí divides por la desviación estándar
)
data_pokemon_4$PC1_Altura <- pca_alt$x[,1]


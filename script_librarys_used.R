#librerias


library(ggplot2) #
library(dplyr) #
library(corrplot) #
library(corrgram) #
library(sjPlot)
library(plotly)
library(lsr)

library(vcd)
library(lsr)
library(dplyr)

library(ggplot2)
library(reshape2)
library(lsr)

####
#       MODELOS
library(rpart)
library(rpart.plot)


### SUPORT VECTOR MACHINE
library(caret)       # framework principal
library(e1071)       # motor SVM
library(DMwR)        # SMOTE (si lo vas a usar)
library(pROC)        # curvas ROC
library(yardstick)


####||    STACKING
library(caretEnsemble)



#####   history of dataset
#   pokemon_bruto 
#   data_pokemon_1 -> creación de variable imc_pokemon | converción de chr a factores y num
#   data_pokemon_2 -> recalculo lbs exacto | escalado log a variable weight.hg | weight.lbs |

#   data_pokemon_3 -> PCAS | 
#   data_pokemon_4 -> eliminación de variables peso y altura e identificadores nombre, colorID, pokedex




###       data_pok_1 -> char a factor | ELIMINACIÓN DEX | ELIMINACIÓN DE NAMES
###       data_pok_2 -> convertir espacios en blanco a none
###       data_pok_3 -> eliminar variable peso y altura
















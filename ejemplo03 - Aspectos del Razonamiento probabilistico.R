# Nombre: Enrique Onieva Caracuel
# Documento: Aspectos del razonamiento probabilístico
#---------------------------------------------------------------------------
## Preparación del entorno de trabajo
rm(list = ls())
cat("\014")
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
getwd()
library(ggplot2)
library(mlbench)
library(arules)
#---------------------------------------------------------------------------

data("PimaIndiansDiabetes")
datos = PimaIndiansDiabetes
head(datos)

# Cálculo de la probabilidad incondicional, hacemos uso del comando table()
tabla = table(datos$diabetes)
tabla

# Tengo que dividir entre el número de datos
tabla = tabla/nrow(datos)
tabla

# Cálculo de la probabilidad condicional para un atributo no numérico
summary(datos$age)
# Como en estos datos sólo hay numéricos, voy a crearme el mío
datos$joven = datos$age<35

# Y puedo usar la función table(), pero sobre 2 columnas
tabla = table(datos$diabetes,datos$joven)
tabla

# Y ahora tengo que dividir entre el número de datos por cada fila
tabla = tabla/rowSums(tabla)
tabla

datos$edadpar = (datos$age %% 2)==0
tabla = table(datos$diabetes,datos$edadpar)
tabla/rowSums(tabla)

datos$edadmultiplode5 = (datos$age %% 5)==0
tabla = table(datos$diabetes,datos$edadmultiplode5)
tabla/rowSums(tabla)

# De la misma manera si tengo más de 2 estados posibles
datos$edad = discretize(datos$age, "fixed", categories = c(20,30,40,50,60,Inf))

tabla = table(datos$diabetes,datos$edad)
tabla
tabla = tabla/rowSums(tabla)
tabla



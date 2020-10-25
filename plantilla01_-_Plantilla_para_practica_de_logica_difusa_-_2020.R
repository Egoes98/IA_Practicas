# Nombre: Egoitz Aranzabal Calvo
# Entrega: 2
# Fecha:
##---------------------------------------------------------------------------
# Tiempo dedicado a la entrega:
# Dificultades encontradas y manera de resolverlas:
##---------------------------------------------------------------------------
# 1. Asegúrese de incluir, junto a esta plantilla, cualquier fichero necesario
#    para su ejecución, incluidos datasets
# 2. Si utiliza una función de un determinado paquete, no olvide incluir la
#    correspondiente llamada a la función "library()"
# 3. No olvide comentar el código, en especial aquellos comandos no-triviales
#    (recuerda que parte de la calificación depende de la limpieza del código)
#---------------------------------------------------------------------------.
rm(list = ls())
cat("\014")
graphics.off()
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
getwd()
library(frbs)
library(ggplot2)
# (incluya aquí cualquier librería adicional)
library(tidyverse)
#---------------------------------------------------------------------------

#---------------------------------------------------------------------------
######################## Parte 1 ###########################################
#--------------------------------------------------------------------------.
# Descarge y lea los datos que puede encontrar en el siguiente link:
# https://www.kaggle.com/jacobbaruch/basketball-players-stats-per-season-49-leagues
# Lea los datos en R
datos = read.csv("datos/players_stats_by_season_full_details.csv")
resultados = datos %>% select(Player,Season,weight_kg,height_cm)

# Añada una columna llamada altocrisp con valores 0 o 1 (numéricos) según si la
# altura del juegador es menor o mayor que 190cm
resultados$altocrisp = ifelse(resultados$height_cm>190,1,0)


# Defina, como un vector de 4 elementos un conjunto difuso llamado alto 
# (como considere debería ser)
# --> alto = c(..., ..., ..., ...)
alto = c(190,210,230,2300)

# Cree una función llamada trapMF que reciba un número y un conjunto difuso y 
# devuelva el valor de pertenencia del número al conjunto definido
# --> trapMF = function(valor,conjunto){....}

trapMF= function(valor, conjunto){
  ifelse(between(valor,conjunto[1], conjunto[2]),((valor-conjunto[1])/(conjunto[2]-conjunto[1])),
         ifelse(between(valor,conjunto[2], conjunto[3]),1,
                ifelse(between(valor,conjunto[3], conjunto[4]),((conjunto[4]-valor)/(conjunto[4]-conjunto[3])),0)))
}

# Añada una columna llamada altofuzzy, resultado de aplicar la función trapMF sobre los elementos de la columna height y el conjunto alto
# --> nombredeldataframe$altofuzzy = apply(data.frame(nombredeldataframe$height), 1, FUN=function(x) trapMF(x,alto))
resultados$altofuzzy = apply(data.frame(resultados$height_cm), 1, FUN=function(x) trapMF(x,alto))


# Dibuje
# Haga un dibujo que muestre la altura del jugador en el eje X y el valor de altocrisp 
# Haga un dibujo que muestre la altura del jugador en el eje X y el valor de altofuzzy
# En ambos casos, utilize el valor de altura para colorear los puntos
# --> Utilize la librería ggplop2
ggplot(resultados,aes(x=height_cm,y=altocrisp))+geom_point()
ggplot(resultados,aes(x=height_cm,y=altofuzzy))+geom_point()

#---------------------------------------------------------------------------
######################## Parte 2 ###########################################
#--------------------------------------------------------------------------.
# De la misma manera que en la primera parte, cree un conjunto llamado "pesado", 
# para el peso de un jugador
# --> pesado = c(..., ..., ..., ...)
pesado = c(95,120,180,180)


# Añada una columna llamada pesadofuzzy, resultado de aplicar la función trapMF sobre los elementos de la columna points_per_minute y el conjunto bueno
# --> nombredeldataframe$pesadofuzzy = apply(..., 1, FUN=function(x) trapMF(x,pesado))
resultados$pesadofuzzy = apply(data.frame(resultados$weight_kg),1,function(x) trapMF(x,pesado))


# Genere una columna llamada altoypesado resultado de la tnorma mínimo sobre las columnas altofuzzy y pesadofuzzy 
# --> nombredeldataframe$altoypesado = ...
resultados$altoypesado = apply(data.frame(resultados$altofuzzy, resultados$pesadofuzzy), 1, max)


# Dibuje
# Haga un dibujo que muestre la altura del jugador en el eje X, su peso en el eje Y, y utilize para dar color el valor de la columna altoypesado, generada
# --> Utilize la librería ggplop2
ggplot(resultados,aes(x=altofuzzy,y=pesadofuzzy),col=altoypesado)+geom_point()
ggplot(resultados,aes(x=height_cm,y=weight_kg),color=altoypesado)+geom_point()



#---------------------------------------------------------------------------
######################## Parte 3 ###########################################
#--------------------------------------------------------------------------.
# Implemente un sistema difuso que dé una valoración entre 0 y 100 de los jugadores de baloncesto 
# a partir de las características del fichero de datos

# Criterio de valoración libre
# Requisitos básicos
# Utilizar al menos 2 de las variables y codificar cada una de ellas con al menos 3 conjuntos difusos
# Incluir al menos 5 reglas difusas
# Añada el resultado de la ejecución del sistema como una nueva columna a los datos y genere un dibujo
# Pista1: En la ayuda de la función "frbs.gen" tenéis un ejemplo de creación de un sistema difuso
# Pista2: Os dejo aquí un desglose de los pasos a dar, con alguna indicación
# Consejo: explore la posibilidad de crear nuevas columnas en función de los datos numéricos que hay en la tabla
#  - % de acierto de 3 puntos
#  - Puntos/partido
#  - Minutos jugados/partido
#  (...)


# Paso 1, definir todos los conjuntos difusos a usar como vectores de 5 elementos
# --> etiqueta1 = c(valor,xxx, xxx, xxx, xxx)
# *nota, valor será 2, 3, 4, dependiendo si es una etiqueta "izquierda"(2), "central"(4), o "derecha"(3)
# *para etiquetas "izquierdas", c(2,b,c,d,NA)
# *para etiquetas "centrales", c(4,a,b,c,d)
# *para etiquetas "derechas", c(3,a,b,c,NA)
# *a,b,c,d representan los puntos de una etiqueta trapezoidal


# Paso 2, pegar todas las etiquetas por columnas y asignar a una variable
# --> varinp.mf = cbind(etiqueta1,etiqueta2,etiqueta3...)


# Paso 3, definir una matriz con el número de etiquetas de cada entrada
# --> num.fvalinput = matrix(c(numeroetiquetasentrada1,numeroetiquetasentrada2), nrow=1)


# Paso 4, dele nombre a cada etiqueta en un vector
# --> names.varinput = c("nombre1", "nombre2", "nombre3", "nombre4", ...)


# Paso 5, defina los rangos de las variables de entrada y de la salida
# --> range.data = matrix(c(minimoentrada1, maximoentrada1, minimoentrada2, maximoentrada2, 0, 100), nrow = 2)


# Paso 6, defina el tipo de defuzificación, tnorma y tconorma, e implicación, así como el tipo de modelo
# --> type.defuz = "COG"
# --> type.tnorm = "MIN"
# --> type.snorm = "MAX"
# --> type.implication.func = "MIN"
# --> type.model <- "MAMDANI"


# Paso 7, cree una variable "newdata" únicamente con las columnas que utilizará su sistema


# Paso 8, Cree un vector con los nombres de las variables
# colnames.var = c("Nombreentrada1", "Nombreentrada2", "Nombresalida")


# Paso 9, defina los nombres de las etiquetas de salida y sus funciones, de la misma manera que en los pasos 1,2,3,4


# Paso 10, Defina una matriz con las reglas, utilizando el siguiente formato (ejemplo para 1 regla)
# --> rule = matrix( c("nombreetiqueta1", "and", "nombreetiqueta2","->", "etiquetadesalida"), nrow = 1, byrow = TRUE)


# Paso 11, utilice la función frbs.gen() para crear el sistema difuso, y la función predict para ponerlo a prueba sobre los datos
# --> sistema = frbs.gen(...)
# --> evaluacion = predict(sistema, newdata)$predicted.val


# Paso 12, añada el resultado como una nueva columna y dibuje las dos variables usadas, dando color con el resultado obtenido
# --> newdata$calidad = evaluacion
# --> ggplot(...col=calidad...)+...

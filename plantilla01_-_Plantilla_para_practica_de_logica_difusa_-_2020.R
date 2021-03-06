# Nombre: Egoitz Aranzabal Calvo
# Entrega: 2
# Fecha:27/10/2020
##---------------------------------------------------------------------------
# Tiempo dedicado a la entrega:8h
# Dificultades encontradas y manera de resolverlas:
#La primera y segunda parte sin problemas. Me ha costado un poco entender la parte 3 pero con el ejemplo lo he solucionado rapido. 
#Me han surgido bastante fallos pero he podido resolverlos sin problemas. Las formulas para la parte 3 calcular las variable son simples ya que 
#he tenido problemas en ese sentido. El pocentaje de tiro verdadero no me daba valores buenos, en algunos casos superaban los 100 cuando deberia ser un porcentaje.
#En principio la formula estaba bien implementada pero en 4 casos de la tabal no funcionaba correctamente. Esto me ha echo tener que cambiar el dato por el FGP.
#Me ha costado bastante definir los valores del conjunto difuso, he intentado ponerlos los mejor posible aunque probablemente sean mejorables.
#No me ha dado mucho tiempo a trastear sobre todo en la parte 3.
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
#He elegido los datos teniendo en cuenta las alturas maximas de los jugadores
alto = c(190,210,250,250)

# Cree una función llamada trapMF que reciba un número y un conjunto difuso y 
# devuelva el valor de pertenencia del número al conjunto definido
trapMF= function(valor, conjunto){
  ifelse(between(valor,conjunto[1], conjunto[2]),((valor-conjunto[1])/(conjunto[2]-conjunto[1])),
         ifelse(between(valor,conjunto[2], conjunto[3]),1,
                ifelse(between(valor,conjunto[3], conjunto[4]),((conjunto[4]-valor)/(conjunto[4]-conjunto[3])),0)))
}

# Añada una columna llamada altofuzzy, resultado de aplicar la función trapMF sobre los elementos de la columna height y el conjunto alto
resultados$altofuzzy = apply(data.frame(resultados$height_cm), 1, FUN=function(x) trapMF(x,alto))


# Dibuje
# Haga un dibujo que muestre la altura del jugador en el eje X y el valor de altocrisp 
# Haga un dibujo que muestre la altura del jugador en el eje X y el valor de altofuzzy
# En ambos casos, utilize el valor de altura para colorear los puntos
ggplot(resultados,aes(x=height_cm,y=altocrisp,col=height_cm))+geom_point()
ggplot(resultados,aes(x=height_cm,y=altofuzzy,col=height_cm))+geom_point()

#---------------------------------------------------------------------------
######################## Parte 2 ###########################################
#--------------------------------------------------------------------------.
# De la misma manera que en la primera parte, cree un conjunto llamado "pesado", 
# para el peso de un jugador
#Como en anterior basandome un poco en los maximos y minimos
pesado = c(95,120,180,180)


# Añada una columna llamada pesadofuzzy, resultado de aplicar la función trapMF sobre los elementos de la columna points_per_minute y el conjunto bueno
resultados$pesadofuzzy = apply(data.frame(resultados$weight_kg),1,function(x) trapMF(x,pesado))


# Genere una columna llamada altoypesado resultado de la tnorma mínimo sobre las columnas altofuzzy y pesadofuzzy 
resultados$altoypesado = apply(data.frame(resultados$altofuzzy, resultados$pesadofuzzy), 1, max)


# Dibuje
# Haga un dibujo que muestre la altura del jugador en el eje X, su peso en el eje Y, y utilize para dar color el valor de la columna altoypesado, generada
ggplot(resultados,aes(x=height_cm,y=weight_kg,color=altoypesado))+geom_point()

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

#Field Goal Percentage: FGP
fgp = function(FGM,FGA){
  return ((FGM/FGA)*100)
}

datos$FGP = apply(data.frame(datos$FGM, datos$FGA), 1,function(x) fgp(x[1],x[2]))


#Minutes per game: MPG
mpg = function(MIN,GP){
  return (MIN/GP)
}

datos$MPG = apply(data.frame(datos$MIN, datos$GP), 1, function(x) mpg(x[1],x[2]))

#Points per game: PPG
ppg = function(PTS,GP){
  return (PTS/GP)
}

datos$PPG = apply(data.frame(datos$PTS, datos$GP), 1, function(x) ppg(x[1],x[2]))

# Paso 1, definir todos los conjuntos difusos a usar como vectores de 5 elementos

#Conjuntos difusos TSP
FGPBajo = c(2,0,30,40,NA)
FGPMedio = c(4,30,40,56,60)
FGPALTO = c(3,56,60,100,NA)

#Conjuntos difusos MPG
MPGBajo = c(2,0,10,20,NA)
MPGMedio = c(4,10,20,30,35)
MPGAlto = c(3,30,35,40,NA)

#Conjuntos difusos PPG
PPGBajo = c(2,0,10,17,NA)
PPGMedio = c(4,10,17,28,32)
PPGAlto = c(3,28,32,45,NA)

# Paso 2, pegar todas las etiquetas por columnas y asignar a una variable
varinp.mf = cbind(FGPBajo,FGPMedio,FGPALTO,MPGBajo,MPGMedio,MPGAlto,PPGBajo,PPGMedio,PPGAlto)

# Paso 3, definir una matriz con el número de etiquetas de cada entrada
num.fvalinput = matrix(c(3, 3, 3), nrow=1)


# Paso 4, dele nombre a cada etiqueta en un vector
varinput.1 = c("Buen_tirador", "Tirador_medio", "Mal_tirador")
varinput.2 = c("Mucho_tiempo_jugado", "Tiempo_jugado_medio", "Poco_tiempo_jugado")
varinput.3 = c("Buen_puntuaje", "Puntuaje_medio", "Mal_puntuaje")
names.varinput = c(varinput.1, varinput.2, varinput.3)


# Paso 5, defina los rangos de las variables de entrada y de la salida
range.data = matrix(c(0,100, 0,48, 0,45, 0,100), nrow=2)


# Paso 6, defina el tipo de defuzificación, tnorma y tconorma, e implicación, así como el tipo de modelo
type.defuz = "WAM"
type.tnorm = "MIN"
type.snorm = "MAX"
type.implication.func = "MIN"
type.model = "MAMDANI"

name = "Sim-0"

# Paso 7, cree una variable "newdata" únicamente con las columnas que utilizará su sistema
newdata = data.frame(datos$FGP, datos$MPG, datos$PPG)
newdata[is.na(newdata)] = 0

# Paso 8, Cree un vector con los nombres de las variables
colnames.var = c("FGP", "MPG", "PPG", "Calidad")


# Paso 9, defina los nombres de las etiquetas de salida y sus funciones, de la misma manera que en los pasos 1,2,3,4
num.fvaloutput = matrix(c(3), nrow = 1)

varoutput.1 = c("Jugador_de_nivel_alto", "Jugador_de_nivel_medio", "Jugador_de_nivel_bajo")
names.varoutput <- c(varoutput.1)

varout.mf = matrix(c(2, 0, 20, 40, NA, 4, 20, 40, 60, 80, 3, 60, 80, 100, NA),
                    nrow = 5, byrow = FALSE)

# Paso 10, Defina una matriz con las reglas, utilizando el siguiente formato (ejemplo para 1 regla)
rule = matrix(c("Buen_tirador", "and", "Mucho_tiempo_jugado","and", "Buen_puntuaje" ,"->", "Jugador_de_nivel_alto",
                "Buen_tirador", "and", "Tiempo_jugado_medio","and","Buen_puntuaje", "->", "Jugador_de_nivel_alto",
                "Tirador_medio", "and", "Tiempo_jugado_medio","and","Puntuaje_medio", "->", "Jugador_de_nivel_medio",
                "Tirador_medio", "and", "Mucho_tiempo_jugado", "and", "Puntuaje_medio","->", "Jugador_de_nivel_medio",
                "Mal_tirador", "and", "Poco_tiempo_jugado", "and", "Mal_puntuaje","->", "Jugador_de_nivel_bajo",
                "Tirador_medio", "and", "Mucho_tiempo_jugado", "and", "Buen_puntuaje","->", "Jugador_de_nivel_alto",
                "Tirador_medio", "and", "Poco_tiempo_jugado", "and", "Mal_puntuaje","->", "Jugador_de_nivel_bajo",
                "Buen_tirador", "and", "Poco_tiempo_jugado", "and", "Mal_puntuaje","->", "Jugador_de_nivel_bajo",
                "Buen_tirador", "and", "Poco_tiempo_jugado", "and", "Puntuaje_medio","->", "Jugador_de_nivel_medio",
                "Mal_tirador", "and", "Mucho_tiempo_jugado", "and", "Mal_puntuaje","->", "Jugador_de_nivel_bajo"), 
               nrow = 10, byrow = TRUE)  


# Paso 11, utilice la función frbs.gen() para crear el sistema difuso, y la función predict para ponerlo a prueba sobre los datos
sistema = frbs.gen(range.data, num.fvalinput, names.varinput, 
                   num.fvaloutput, varout.mf, names.varoutput, rule, 
                   varinp.mf, type.model, type.defuz, type.tnorm, 
                   type.snorm, func.tsk = NULL, colnames.var, type.implication.func, name)
#Graficos del sistema
plotMF(sistema)

#Evalucaion de los jugadores
evaluacion = predict(sistema, newdata)$predicted.val

# Paso 12, añada el resultado como una nueva columna y dibuje las dos variables usadas, dando color con el resultado obtenido
newdata$calidad = evaluacion
ggplot(newdata,aes(x=datos.FGP,y=datos.MPG,color=calidad))+geom_point()
ggplot(newdata,aes(x=datos.FGP,y=datos.PPG,color=calidad))+geom_point()
ggplot(newdata,aes(x=datos.PPG,y=datos.MPG,color=calidad))+geom_point()

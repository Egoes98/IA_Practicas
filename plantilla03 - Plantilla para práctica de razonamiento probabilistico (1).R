# Nombre: Egoitz Aranzabal Calvo
# Entrega: 4
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
#---------------------------------------------------------------------------
rm(list = ls());cat("\014")
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
getwd()
# (incluya aquí cualquier librería a utilizar)
install.packages("caret", dependencies = TRUE)
install.packages("manipulateWidget", dependencies = TRUE)
install.packages("R2HTML")

library(dplyr)
library(lattice)
library(ggplot2)
library(caret)
library(e1071)
library(clusterSim)
library(class)
#---------------------------------------------------------------------------


#---------------------------------------------------------------------------
######################## Parte 1 ###########################################
#---------------------------------------------------------------------------
# Paso 1: Utilice la función read.csv para leer el contenido del fichero "movie_metadata.csv"
#     Además, elimine las siguientes columnas: "director_name", "actor_2_name", "actor_1_name"
#     "movie_title", "plot_keywords", "movie_imdb_link"
# ** Puede automatizar el proceso si intenta eliminar todas las columnas que sean de 
#    tipo factor, y que además, tengan más de 100 niveles diferentes
datos = read.csv("datos/movie_metadata.csv")
datos = dplyr::select(datos, -director_name, -actor_2_name, -actor_1_name, -movie_title, -plot_keywords, -movie_imdb_link)

# Paso 2: Discretice la columna imbd_score para tener rangos en [0, 7.5) [7.5, 9) y [9, 10]
datos$imdb_score = discretize(datos$imdb_score, "fixed", breaks = c(0,7.5,9,10))

# Paso 3: Quédate únicamente con las filas que tienen todos los datos (no tienen NAs)
datos = na.omit(datos)

# Paso 4: utilice el método "createDataPartition", de la librería "caret" para partir los datos
#         en 2 pedazos-> [datostra, con el 80% de los datos] [datostst con el 20% restante]
trainIndex = createDataPartition(datos$duration, times = 1, p = 0.8, list = FALSE)

datostra = datos[trainIndex,]
datostst = datos[-trainIndex,]

# Paso 5: Llame a la función naiveBayes, de la librería "e1071" para entrenar el modelo con los
# datos de entrenamiento (datostra)
# --> modelo1=naiveBayes(........)
modelo1 = naiveBayes(imdb_score ~ ., datostra, laplace = 0)

# Paso 6: Llame a la función naiveBayes para entrenar un modelo2, utilizando el parámetro que aplica
# la corrección de Laplace
# --> modelo2=naiveBayes(...)
modelo2 = naiveBayes(imdb_score ~ ., datostra, laplace = 3)

# Paso 7: Realice predicciones sobre los datos de test (datostst) de los dos modelos con el comando predict
# --> prediccion1=predict(modelo1,...)
# --> prediccion2=predict(modelo2,...)
prediccion1 = predict(modelo1, datostst)
prediccion2 = predict(modelo2, datostst)

table(prediccion1)
table(prediccion2)

# Paso 8: Calcule el porcentaje de aciertos de cada uno de los modelos. ¿Cuál es mejor? Razone por qué
# *Nota: puede utilizar, si le es más cómodo el comando confusionMatrix, de la librería "caret"
aciertosModelo1 = confusionMatrix(prediccion1, datostst$imdb_score)
aciertosModelo2 = confusionMatrix(prediccion2, datostst$imdb_score)

#---------------------------------------------------------------------------
######################## Parte 2 ###########################################
#---------------------------------------------------------------------------
# Paso 1: Utilice la función read.csv para leer el contenido del fichero "movie_metadata.csv" (igual que antes)
# En este caso, elimine toda columna que no sea numérica y toda fila que tenga algún valor perdido.
# Discretice la columna imbd_score igual que antes
# * Puede utilizar los comandos is.numeric()
# * Está permitido eliminar "manualmente" las columnas, pero no es elegante
datos = read.csv("datos/movie_metadata.csv")
datos = na.omit(datos)
datos = datos[,sapply(datos, is.numeric)]
datos$imdb_score = discretize(datos$imdb_score, "fixed", breaks = c(0,7.5,9,10))

# Paso 2: Cree una copia de los datos llamada "datosnor" y normalizela
# * Puede usar la función "data.Normalization()", dentro del paquete "clusterSim"
# * ojo, lea la documentación para ver qué valor tiene que darle al parámetro "type"
# * Cuidado, te dará error si intentas normalizarlo todo, ya que imbd_score no es numérica
index = match("imdb_score", names(datos))
datosnor = datos[,-index]

datosnor = data.Normalization(datosnor,"n8","column")
datosnor$imdb_score = datos$imdb_score;

# Paso 3: (igual que antes) utilice el método "createDataPartition", de la librería "caret" para partir los datos
#   en 2 pedazos-> [datostra, con el 80% de los datos] [datostst con el 20% restante] 
# * Ojo, para ejecutar KNN la clase tiene que estar por un lado, y la columna a predecir separada, por otro
# * por ello, deberá partir datostra en [datostra, con todas las columnas, salvo imbd_score] [labeltra, con únicamente la columna imbd_score]
# * (idem para datostst), y tenga en cuenta que debe realizar el mismo proceso con datosnor
trainIndex = createDataPartition(datos$duration, times = 1, p = 0.8, list = FALSE)

datostra = datos[trainIndex,]
labeltra = matrix(datostra$imdb_score)
index = match("imdb_score", names(datostra))
datostra = datostra[,-index]

datostst = datos[-trainIndex,]
labeltst = matrix(datostst$imdb_score)
index = match("imdb_score", names(datostst))
datostst = datostst[,-index]

trainIndex = createDataPartition(datosnor$duration, times = 1, p = 0.8, list = FALSE)

datostranor = datosnor[trainIndex,]
labeltranor = matrix(datostranor$imdb_score)
index = match("imdb_score", names(datostranor))
datostranor = datostranor[,-index]

datoststnor = datosnor[-trainIndex,]
labeltstnor = matrix(datoststnor$imdb_score)
index = match("imdb_score", names(datoststnor))
datoststnor = datoststnor[,-index]

# Paso 4: Aplique KNN con 3 valores de K diferentes (a su elección), para los datos normalizados y sin normalizar
# --> prediccion1 = knn(datostra, datostst, labeltra, k = XX)
# --> prediccion2 = knn(datostranor, datoststnor, labeltranor, k = XX)
# (hasta 6)
prediccion1 = knn(datostra, datostst, labeltra, k=10)
prediccion2 = knn(datostra, datostst, labeltra, k=1)
prediccion3 = knn(datostra, datostst, labeltra, k=5)

table(prediccion1)
table(prediccion2)
table(prediccion3)

prediccion4 = knn(datostranor, datoststnor, labeltranor, k=10)
prediccion5 = knn(datostranor, datoststnor, labeltranor, k=1)
prediccion6 = knn(datostranor, datoststnor, labeltranor, k=5)

table(prediccion4)
table(prediccion5)
table(prediccion6)

# Paso 5: Calcule el porcentaje de aciertos de cada uno de los 6 modelos. ¿Cuál es mejor? Razone por qué
# *Nota: puede utilizar, si le es más cómodo el comando confusionMatrix, de la librería "caret"




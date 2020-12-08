# Nombre: 
# Entrega:
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
#---------------------------------------------------------------------------


#---------------------------------------------------------------------------
######################## Parte 1 ###########################################
#---------------------------------------------------------------------------
# Paso 1: Utilice la función read.csv para leer el contenido del fichero "movie_metadata.csv"
#     Además, elimine las siguientes columnas: "director_name", "actor_2_name", "actor_1_name"
#     "movie_title", "plot_keywords", "movie_imdb_link"
# ** Puede automatizar el proceso si intenta eliminar todas las columnas que sean de 
#    tipo factor, y que además, tengan más de 100 niveles diferentes



# Paso 2: Discretice la columna imbd_score para tener rangos en [0, 7.5) [7.5, 9) y [9, 10]


# Paso 3: Quédate únicamente con las filas que tienen todos los datos (no tienen NAs)


# Paso 4: utilice el método "createDataPartition", de la librería "caret" para partir los datos
#         en 2 pedazos-> [datostra, con el 80% de los datos] [datostst con el 20% restante] 


# Paso 5: Llame a la función naiveBayes, de la librería "e1071" para entrenar el modelo con los
# datos de entrenamiento (datostra)
# --> modelo1=naiveBayes(........)


# Paso 6: Llame a la función naiveBayes para entrenar un modelo2, utilizando el parámetro que aplica
# la corrección de Laplace
# --> modelo2=naiveBayes(...)


# Paso 7: Realice predicciones sobre los datos de test (datostst) de los dos modelos con el comando predict
# --> prediccion1=predict(modelo1,...)
# --> prediccion2=predict(modelo2,...)


# Paso 8: Calcule el porcentaje de aciertos de cada uno de los modelos. ¿Cuál es mejor? Razone por qué
# *Nota: puede utilizar, si le es más cómodo el comando confusionMatrix, de la librería "caret"





#---------------------------------------------------------------------------
######################## Parte 2 ###########################################
#---------------------------------------------------------------------------
# Paso 1: Utilice la función read.csv para leer el contenido del fichero "movie_metadata.csv" (igual que antes)
# En este caso, elimine toda columna que no sea numérica y toda fila que tenga algún valor perdido.
# Discretice la columna imbd_score igual que antes
# * Puede utilizar los comandos is.numeric()
# * Está permitido eliminar "manualmente" las columnas, pero no es elegante



# Paso 2: Cree una copia de los datos llamada "datosnor" y normalizela
# * Puede usar la función "data.Normalization()", dentro del paquete "clusterSim"
# * ojo, lea la documentación para ver qué valor tiene que darle al parámetro "type"
# * Cuidado, te dará error si intentas normalizarlo todo, ya que imbd_score no es numérica



# Paso 3: (igual que antes) utilice el método "createDataPartition", de la librería "caret" para partir los datos
#   en 2 pedazos-> [datostra, con el 80% de los datos] [datostst con el 20% restante] 
# * Ojo, para ejecutar KNN la clase tiene que estar por un lado, y la columna a predecir separada, por otro
# * por ello, deberá partir datostra en [datostra, con todas las columnas, salvo imbd_score] [labeltra, con únicamente la columna imbd_score]
# * (idem para datostst), y tenga en cuenta que debe realizar el mismo proceso con datosnor



# Paso 4: Aplique KNN con 3 valores de K diferentes (a su elección), para los datos normalizados y sin normalizar
# --> prediccion1 = knn(datostra, datostst, labeltra, k = XX)
# --> prediccion2 = knn(datostranor, datoststnor, labeltranor, k = XX)
# (hasta 6)



# Paso 5: Calcule el porcentaje de aciertos de cada uno de los 6 modelos. ¿Cuál es mejor? Razone por qué
# *Nota: puede utilizar, si le es más cómodo el comando confusionMatrix, de la librería "caret"




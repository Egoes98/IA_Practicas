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
install.packages("tictoc")

library(neuralnet)
library(clusterSim)
library(caret)
library(ModelMetrics)
library(tictoc)
#---------------------------------------------------------------------------

# Paso 1: Utilice la función read.csv para leer el contenido del fichero "hormigon.csv"
datos = read.csv("datos/hormigon.csv")

# Paso 2: Normalice todas las columnas del dataset (como en la práctica anterior)
datos = data.Normalization(datos,"n4","column")

# Paso 3: Utilice la función createDataPartition (paquete: caret) para dividir los datos en
# training (80%) y test (20%)
trainIndex = createDataPartition(datos$cement, p = 0.8, list = FALSE)

datostra = datos[trainIndex,]
datostst = datos[-trainIndex,]

# Paso 4: Cree un modelo de perceptrón simple (1 capa con 1 neurona), haciendo uso de la función neuralnet
# para predecir la fuerza (columna strength) en función del resto de atributos.
# Mida el tiempo que tarda en entrenar y guárdelo en una variable
# --> modelo = neuralnet(...)
# * Asegurese de que la red converge, por lo que posiblemente deba cambiar algún parámetro, como 
# * el umbral, número de pasos máximos o repeticiones
tic()
modelo = neuralnet(strength ~ ., datostra, hidden = 1,linear.output = FALSE)
toc()

# Paso 5: Dibuje la "red" resultante haciendo uso del comando "plot"
plot(modelo,"best")

# Paso 6: Realice predicciones para los datos de test, utilizando la función "compute"
evaluation = compute(modelo,datostst, 1)

# Paso 7: Calcule el error absoluto medio (mean absolute error, MAE) entre las predicciones y la 
# columna strength de conjunto de datos de test
# * Puede calcularlo o utilizar una (de las varias) implementaciones que hay disponibles en R
values = unlist(evaluation[2])
eam = mae(datostst$strength, values)

# Paso 8: Repita los pasos del 4 al 7 (guarde cada valor de MAE en una variable diferente) para 
# las siguientes configuraciones:
# - Una red de una capa, con 5 neuronas
tic()
modelo1 = neuralnet(strength ~ ., datostra, hidden = 5,linear.output = FALSE)
toc()
plot(modelo1,"best")

evaluation = compute(modelo1, datostst, 1)

values = unlist(evaluation[2])
eam1 = mae(datostst$strength, values)

# - Una red de una capa, con 100 neuronas
tic()
modelo2 = neuralnet(strength ~ ., datostra, hidden = 100,linear.output = FALSE)
toc()
plot(modelo2,"best")

evaluation = compute(modelo2, datostst, 1)

values = unlist(evaluation[2])
eam2 = mae(datostst$strength, values)

# - Una red de 2 capas con 5 y 20 neuronas, respectivamente
tic()
modelo3 = neuralnet(strength ~ ., datostra, hidden = c(5,20),linear.output = FALSE)
toc()
plot(modelo3,"best")

evaluation = compute(modelo3, datostst, 1)

values = unlist(evaluation[2])
eam3 = mae(datostst$strength, values)

# - Una red de 2 capas con 20 y 5 neuronas, respectivamente
tic()
modelo4 = neuralnet(strength ~ ., datostra, hidden = c(20,5),linear.output = FALSE)
toc()
plot(modelo4,"best")

evaluation = compute(modelo4, datostst, 1)

values = unlist(evaluation[2])
eam4 = mae(datostst$strength, values)

# - Una red de 2 capas con 20 y 20 neuronas, respectivamente
tic()
modelo5 = neuralnet(strength ~ ., datostra, hidden = c(20,20),linear.output = FALSE)
toc()
plot(modelo5,"best")

evaluation = compute(modelo5, datostst, 1)

values = unlist(evaluation[2])
eam5 = mae(datostst$strength, values)

# Para todas ellas, guarde el MAE y tiempo en variables separadas

# Paso 9: Analice los resultados en función del tiempo de entrenamiento y MAE, ¿cuál funciona mejor?
# ¿Son los resultados coherentes con lo visto en clase?

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
rm(list = ls());cat("\014");graphics.off()
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
getwd()
library(GGally)
library(ggplot2)
library(gridExtra)
# (incluya aquí cualquier librería adicional)
#---------------------------------------------------------------------------

problema = 'datos/bayg29.txt';
puntos = read.table(problema);
long = nrow(puntos);
ggplot(puntos, aes(x = V1, y = V2)) + geom_point() + 
  geom_label(label = as.character(1:long))


# Paso 1: Crear una matriz llamada distancias, de longxlong elementos. En cada 
# posición debe almacenar la distancia entre 2 ciudades
# distancias[i,j] <- distancia euclídea entre puntos[i,] y puntos[j,]



# Paso 2: Defina la función fitness, que reciba un individuo y la matriz de distancias
# y devuelva la longitud total de los dos caminos recorridos
# --> fitness = function(ind,distancias){...}



# Paso 3: Defina la función de inicialización, que reciba el número de individuos a crear
# y el número de puntos (variable long). Ésta función debe devolver una matriz con tantas
# filas como individuos y "long" columnas.
# En cada fila, habrá una permutación (aleatoria) de los valores entre 1 y long
# --> initial = function(number,long){...}



# Paso 4: Puede utilizar la función de torneo binario vista en clase
tournamentselection = function(evaluation,number){
  indexes = matrix(0,1,length(evaluation))
  for (i in 1:length(evaluation)){
    a = sample(length(evaluation),size=number)
    indexes[i]=a[which.min(evaluation[a])]
  } 
  return(indexes)
}



# Paso 5: Operador de cruce de orden
# Implemente una función que recibe los índices de padres, la población y la probabilidad de cruce
# y realiza el operador de cruce de orden, como ha visto en clase
# crossover = function(indexparents,population,pcross){
# -->   offspring = population # "Reservar memoria"
# -->     for (i in seq(1,length(indexparents),2)){
# -->       if (runif(1)<pcross){
# -->         ...Realizar el cruce aquí...
# -->       }else{
# -->         ...Pasar a la población "offspring" ambos padres
# -->       }
# -->     }
# -->     return(offspring)
# -->   }




# Paso 6: Operador de Mutación
# Para cada hijo, con probabilidad pmut, intercambiar dos posiciones elegidas aleatoriamente
# -->   mutation = function(population,pmut){...}



# Paso 7: Realize hasta 5 pruebas con diferentes configuraciones de los siguientes parámetros
# analice y comente los resultados a modo de comentarios:
#---------------------------------------------------------------------------
# Prueba1: 
# Parámetros--> ...
# Resultados--> ...
# Justificación/Razonamiento--> ...
#---------------------------------------------------------------------------
generations = 500
tournamentsize = 2
probcrossover = 0.9
probmutation  = 0.05
popsize=100



# Paso 8: (para nota) Implemente una función que reciba como argumento el mejor individuo obtenido
# por el algoritmo y las posiciones de los puntos y dibuje las 2 rutas seguidas por los camiones
# Llame a la función al final de la ejecución del algoritmo
#---------------------------------------------------------------------------


#---------------------------------------------------------------------------
# Si todo está bien, el código debería funcionar a partir de aquí
# haga las comprobaciones pertienentes para cada operador
# (Si hubiera alguna errata o fallo en el código, que lo puede haber, 
# comunícamelo lo antes posible con el fin de solventarlo)
#---------------------------------------------------------------------------
best = c()
bestfitness = Inf
population = initial(popsize,long)
evaluation = apply(population,1,fitness,distancias)
progreso = data.frame(g=numeric(),mejor=numeric(),promedio=numeric(),peor=numeric(),distancia=numeric())
for (g in 1:generations){
  indexparents = tournamentselection(evaluation,tournamentsize)
  offspring1 = crossover(indexparents,population,probcrossover)
  offspring2 = mutation(offspring1,probmutation)
  population = offspring2
  evaluation = apply(population,1,fitness,distancias)
  
  # Actualizamos el mejor individuo
  if (min(evaluation)<bestfitness){
    bestfitness=min(evaluation)
    best = population[which.min(evaluation),]
  }
  # Hacemos elitismo
  if (bestfitness!=min(evaluation)){
    population[1,]=best
  }
  print(paste("Generación ",g," Fitness Mejor individuo - ",bestfitness))
  print(best)
  progreso = rbind(progreso,
                   data.frame(g=g,mejor=bestfitness,
                              promedio=mean(evaluation),
                              peor=max(evaluation),
                              distancia=mean(as.matrix(dist(population, method = "euclidean")))))
}

# Dibujamos los resultados
plot1 = ggplot(progreso)+
  geom_line(aes(x=g,y=mejor),col="green")+
  geom_line(aes(x=g,y=promedio),col="blue")+
  geom_line(aes(x=g,y=peor),col="red")+
  scale_y_log10()+
  labs(title = "Evolución de los fitness Mejor, Promedio y Peor",
       subtitle = paste("Mejor Individuo Final: ",bestfitness),
       caption = "Universidad de deusto")

plot2 = ggplot(progreso,aes(x=g,y=distancia))+geom_line()+
  labs(title = "Evolución de la distancia promedio entre individuos",
       subtitle = "Ojo, en este caso la distancia euclídea no es significativa",
       caption = "Universidad de deusto")
grid.arrange(plot1,plot2,ncol=1)
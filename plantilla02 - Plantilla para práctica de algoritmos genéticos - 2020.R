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
distancias = matrix(nrow = 29,ncol = 29)
for(i in 1:long){
  for(j in 1:long){
    distancias[i,j] = sqrt(((puntos[j,1]-puntos[i,1])^2)+((puntos[j,2]-puntos[i,2])^2))
  }
}

# Paso 2: Defina la función fitness, que reciba un individuo y la matriz de distancias
# y devuelva la longitud total de los dos caminos recorridos
# --> fitness = function(ind,distancias){...}
fitness = function(ind,distancias){
  ruta1 = 0
  ruta2 = 0
  rutaTotal = 0

  zeroPos = match(0,ind)
  
  #En caso de que una ruta tenga solo un punto, o no tenga ninguno devuelve Inf ya qye he decidido que quiero evitar que uno de los caminos no tenga una ruta.
  #Al devolver infinito quiero que se pase por alto estos casos concretos
  if(length(ind) == 1 || zeroPos == 2 || zeroPos == (length(ind)-1) || zeroPos == 1 || zeroPos == length(ind)){
    return(Inf)
  }
  
  puntosRuta1 = ind[1:zeroPos-1]
  
  sizePR2 = length(ind)-length(puntosRuta1)-1
  puntosRuta2 = ind[zeroPos+1:sizePR2]
  
  for(i in 2:length(puntosRuta1)){
    ruta1 = ruta1 + distancias[puntosRuta1[i],puntosRuta1[i-1]]
  }
  ruta1 = ruta1 + distancias[puntosRuta1[1],puntosRuta1[length(puntosRuta1)]]
  
  for(i in 2:length(puntosRuta2)){
    ruta2 = ruta2 + distancias[puntosRuta2[i],puntosRuta2[i-1]]
  }
  ruta2 = ruta2 + distancias[puntosRuta2[1],puntosRuta2[length(puntosRuta2)]]
  
  rutaTotal = ruta1[1] + ruta2[1]
  
  #He decido que voy a premiar que las rutas tengas, mas o menos los mismos puntos
  puntoCentral = as.integer(length(ind)/2)
  if(zeroPos == puntoCentral || zeroPos == (puntoCentral+1)){
    rutaTotal = rutaTotal - 2000
  }

  return(rutaTotal)
}

# Paso 3: Defina la función de inicialización, que reciba el número de individuos a crear
# y el número de puntos (variable long). Ésta función debe devolver una matriz con tantas
# filas como individuos y "long" columnas.
# En cada fila, habrá una permutación (aleatoria) de los valores entre 1 y long
# --> initial = function(number,long){...}
initial = function(number, long){
  individuos = matrix(nrow = number, ncol = long+1)
  for(i in 1:number){
    individuos [i,] = append(sample(1:29,29,replace = FALSE),0,after = sample(1:29,1))
  }
  individuos
}

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

crossover = function(indexparents, population,pcross){
  offspring = population
  for(i in seq(1, length(indexparents),2)){
    if(runif(1)<pcross){
      cutPoint = sample(1:ncol(population),2,replace = FALSE)
      cutPoint = sort(cutPoint,decreasing = FALSE)
      
      cutValues1 = population[i,cutPoint[1]:cutPoint[2]]
      cutValues2 = population[i+1,cutPoint[1]:cutPoint[2]]
      
      newOffspring1 = setdiff(population[i+1,],cutValues1)
      newOffspring2 = setdiff(population[i,],cutValues2)
      
      newOffspring1 = append(newOffspring1, cutValues1, after = cutPoint[1])
      newOffspring2 = append(newOffspring2, cutValues2, after = cutPoint[1])
      
      offspring[i,] = newOffspring1
      offspring[i+1,] = newOffspring2 
    }
  }
  return(offspring)
}

#cada 2 se ejecuta coges el index parent i y i+1 dices que dos cortar con sample, de cada uno coges y guardas los que vas a quitar. Quitarle a cada uno lo del otro


# Paso 6: Operador de Mutación
# Para cada hijo, con probabilidad pmut, intercambiar dos posiciones elegidas aleatoriamente
# -->   mutation = function(population,pmut){...}
mutation = function(population,pmut){
    for(i in 1:nrow(population)){
      if(runif(1)<pmut){
        pos = sample(1:ncol(population),2,replace = FALSE)
        value = population[i,pos[1]]
        population[i,pos[1]] = population[i,pos[2]]
        population[i,pos[2]] = value
      }
    }
  population
}

# Paso 7: Realize hasta 5 pruebas con diferentes configuraciones de los siguientes parámetros
# analice y comente los resultados a modo de comentarios:
#---------------------------------------------------------------------------
# Prueba1: 
# Parámetros--> ...
# Resultados--> ...
# Justificación/Razonamiento--> ...
#---------------------------------------------------------------------------
generations = 1000
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


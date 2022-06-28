current_working_dir <- dirname(rstudioapi::getActiveDocumentContext()$path)
setwd(current_working_dir)
library(doParallel)
registerDoParallel(detectCores())

#Crea una instancia con los valores recibidos por parametro
createInstance <- function(ruta, nLotes, nBloqueLote, nAplicaciones,
                           nMaquinas, Q, lambdaPrincipal){
  #Creacion de conjuntos
  Lote <- paste("Lote",1:nLotes)
  Bloque <- paste("Bloque",1:(nLotes*nBloqueLote))
  Aplicacion <- paste("Aplicacion",1:nAplicaciones)
  Maquina <- paste("Maquina",1:nMaquinas)
  
  #Creacion base de lotes
  x <- runif(nLotes, -1, 1)
  y <- runif(nLotes, -1, 1)
  infoLotes <- data.frame(Lote = Lote, x = x, y = y)
  rownames(infoLotes) <- infoLotes$Lote
  
  write.csv(infoLotes, paste(ruta, "/infoLotes.csv", sep = ""), row.names = F)
  
  #Creacion base de aplicaciones
  tProm <- round(runif(nAplicaciones, 5, 30))/10
  infoAplicaciones <- data.frame(Aplicacion = Aplicacion, tProm = tProm)
  rownames(infoAplicaciones) <- infoAplicaciones$Aplicacion
  
  write.csv(infoAplicaciones, paste(ruta, "/infoAplicaciones.csv", sep = ""), row.names = F)
  
  #Creacion archivo de trabajos
  tProcesamiento <- expand.grid(Bloque, Aplicacion)
  names(tProcesamiento) <- c("Bloque", "Aplicacion")
  tProcesamiento <- as.data.frame(rbind(c("Bloque 0", "-"), as.matrix(tProcesamiento)))
  
  tProcesamiento$tProcesamiento <- 0
  tProcesamiento$dInicio <- 0
  tProcesamiento$dFin <- 0
  
  ##Tiempos de procesamiento y feechas de inicio y fin de la primera aplicacion
  for(h in Aplicacion){
    val <- rexp(nBloqueLote*nLotes, 1/infoAplicaciones[h,"tProm"])
    while(sum(val>Q/2) > 0){
      incumplen <- which(val>Q/2)
      val[incumplen] <- rexp(length(incumplen), 1/infoAplicaciones[h,"tProm"])
    }
    j <- which(tProcesamiento$Aplicacion == h)
    tProcesamiento[j,]$tProcesamiento <- val
    if(h == Aplicacion[1]){
      tamVentana <- sample(seq(2, 10, by = 2), nBloqueLote*nLotes, replace = T)
      
      tProcesamiento[j,]$dInicio <- sample(0:5, nBloqueLote*nLotes, replace = T)
      tProcesamiento[j,]$dFin <- 
        tProcesamiento[j,]$dInicio + tamVentana
    }
  }
  
  ##Fechas de inicio y fin otras aplicaciones
  for(b in Bloque){
    for(h in Aplicacion[-1]){
      n <- as.numeric(strsplit(h, " ")[[1]][2])
      
      difAplicaciones <- sample(10:15, 1, replace = T)
      
      j <- which(tProcesamiento$Aplicacion == h & tProcesamiento$Bloque == b)
      jAnt <- which(tProcesamiento$Aplicacion == paste("Aplicacion", n-1) & 
                      tProcesamiento$Bloque == b)
      
      tamVentana <- sample(seq(2, 10, by = 2), 1, replace = T)
      
      tProcesamiento[j,]$dInicio <- tProcesamiento[jAnt,]$dInicio + difAplicaciones
      tProcesamiento[j,]$dFin <- tProcesamiento[j,]$dInicio + tamVentana
    }
  }
  
  write.csv(tProcesamiento, paste(ruta, "/tProcesamiento.csv", sep = ""), row.names = F)
  
  #Creacion archivo de maquinas
  fVelocidad <- data.frame(Maquina = Maquina, fVelocidad = NA)
  vel <- sample(1:3, nMaquinas, replace = T)
  while(sum(vel == 1) == 0){
    vel <- sample(1:3, nMaquinas, replace = T)
  }
  fVelocidad$fVelocidad <- vel
  
  write.csv(fVelocidad, paste(ruta, "/fVelocidad.csv", sep = ""), row.names = F)
  
  #Creacion archivo bloques inalcanzables
  probInalcanzable <- 0.25
  inalcanzable <- expand.grid(Maquina, Bloque)
  names(inalcanzable) <- c("Maquina", "Bloque")
  inalcanzable$u <- NA
  
  inalcanzable <- inalcanzable[which(inalcanzable$Maquina%in%fVelocidad[which(fVelocidad$fVelocidad == 1),"Maquina"]),]
  inalcanzable$u <- rbinom(nrow(inalcanzable), 1, probInalcanzable)
  inalcanzable <- inalcanzable[which(inalcanzable$u == 1),]
  
  write.csv(inalcanzable, paste(ruta, "/inalcanzable.csv", sep = ""), row.names = F)
  
  #Creacion archivo coordenadas
  Bloque <- paste("Bloque", 0:(nLotes*nBloqueLote))
  coord <- data.frame(Bloque = Bloque, x = NA, y = NA, row.names = Bloque)
  
  desvCoord <- 0.15
  for(b in Bloque){
    numLote <- ceiling(as.numeric(strsplit(b, " ")[[1]][2])/nBloqueLote)
    if(numLote > 0){
      coord[b,"x"] <- rnorm(1,infoLotes[paste("Lote", numLote),"x"], desvCoord)
      coord[b,"y"] <- rnorm(1,infoLotes[paste("Lote", numLote),"y"], desvCoord)
    }else{
      coord[b,"x"] <- 0
      coord[b,"y"] <- 0
    }
  }
  
  write.csv(coord, paste(ruta, "/coordenadas.csv", sep = ""), row.names = F)
  
  #Creacion archivo distancias - Calculo con distancias rectilineas
  tDistancias <- expand.grid(Bloque,Bloque)
  names(tDistancias) <- c("Bloque1", "Bloque2")
  tDistancias$Distancia <- NA
  for(d in rownames(tDistancias)){
    tDistancias[d,"Distancia"] <- abs(coord[tDistancias[d,"Bloque1"],"x"] - coord[tDistancias[d,"Bloque2"],"x"]) +
      abs(coord[tDistancias[d,"Bloque1"],"y"] - coord[tDistancias[d,"Bloque2"],"y"])
  }
  
  write.csv(tDistancias, paste(ruta, "/tDistancias.csv", sep = ""), row.names = F)
  
  #Creacion archivo parametros
  #Parametros
  numDias <- ceiling(max(tProcesamiento$dFin)*1.1)
  lambda1 <- lambdaPrincipal
  lambda2 <- 1 - lambda1
  valor <- c(numDias, Q, lambda1, lambda2)
  parametro <- c("numDias", "Q", "lambda1", "lambda2")
  parametros <- data.frame(parametro = parametro, valor = valor)
  
  write.csv(parametros, paste(ruta, "/parametros.csv", sep = ""), row.names = F)
}

#Crea un conjunto de instancias
createInstances <- function(nInst, tam){
  
  if(!dir.exists(tam)){
    dir.create(tam)
  }
  
  if(nInst > 0){
    foreach(i = 1:nInst) %dopar% {
      ruta <- paste(tam, "/Instancia ", i, sep = "")
      if(!dir.exists(ruta)){
        dir.create(ruta)
        Q <- 8
        lambdaPrincipal <- 0.7
        nMaquinas = sample(3:4, 1)
        nAplicaciones <- sample(4:5, 1)
        
        if(tam == "Pequena"){
          nLotes <- sample(2:3, 1)
          nBloqueLote <- 2
        }else if(tam == "Mediana"){
          nLotes <- 3
          nBloqueLote <- sample(4:5, 1)
        }else if(tam == "Grande"){
          nLotes <- sample(5:6, 1)
          nBloqueLote <- 7
        }else if(tam == "Real"){
          nLotes <- sample(15:20, 1)
          nBloqueLote <- 15
        }
        createInstance(ruta, nLotes, nBloqueLote, nAplicaciones, nMaquinas, Q, lambdaPrincipal)
      }
    }
  }
}

createInstances(10, "Pequena")
createInstances(10, "Mediana")
createInstances(5, "Grande")

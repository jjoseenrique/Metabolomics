#' @title Selecciona las mejores lecturas para un set analizado por TagFinder
#' @description Función que genera una lista final de los metabolitos seleccionados (Plantilla Tiempos Especificos)
#' @param set .txt generado por tag finder
#' @param tiempos Plantilla de Tiempos Especificos. Por defecto hay 3 (Hoja, Fruto y Split)(Todo de fresa)
#' @param normalizar Decidir si se desean normalizar los datos. Se sirve del nombre de "tiempos" para decidir si usar el ribitol (splitless) o no (split)
#' @return Tras correr este código se presentan los metabolitos que faltan por encontrar. Incluir los tiempos especificos en TagFinder para un segundo filtro
#'

primarios <- function(set, tiempos = Tiempos_Especificos_Fruto, normalizar=TRUE) {
  if (typeof(set)=="character") {
    set <- read.delim(set, header = FALSE)
  }
  
  # Seleccionamos solo aquellos valores doble true
  trues <- set %>%
    dplyr::filter(V1 == "true" & V2 == "true")
  
  data <- rbind(set[1:6, ], trues)
  
  # Seleccionamos aquellos que coinciden con la tabla de tiempos aportada
  Matriz <- data %>%
    dplyr::filter(V8 %in% tiempos[, 2] & V16 %in% tiempos[, 3])
  
  Matriz[, 8] <- as.numeric(Matriz[, 8])
  Matriz[, 15] <- as.numeric(Matriz[, 15])
  
  # Seleccionamos aquellas filas que son correctas
  MatrizFinal <- data.frame()
  
  for (i in 1:nrow(tiempos)) {
    Matriz2 <- Matriz %>%
      dplyr::filter(V16 == tiempos[i, 3] & V8 == tiempos[i, 2])
    
    if (nrow(Matriz2) > 0) {
      v <- abs(Matriz2$V8 - Matriz2$V15)
      MatrizFinal <- rbind(MatrizFinal, Matriz2[which.min(v), ])
    }
  }
  
  MatrizFinal <- rbind(data[1:6, ], MatrizFinal)
  
  orden = as.numeric(MatrizFinal[2, 44:ncol(MatrizFinal)])
  idx = order(orden)
  MatrizFinal[, 44:ncol(MatrizFinal)] = MatrizFinal[, idx + 43]
  
  # Seleccionamos aquellos que faltan comparándolo con la matriz tiempos aportada
  Faltan_Tiempos <- setdiff(tiempos[, 2], MatrizFinal[, 8])
  
  if (length(Faltan_Tiempos) > 0) {
    Faltan <- tiempos %>%
      dplyr::filter(tiempos[, 2] %in% Faltan_Tiempos) %>%
      dplyr::arrange(Tag_Mass)
    
    resultados <- list(MatrizFinal, Faltan)
    
    mostrar <- function(x) {
      cat("\n", "FALTAN LOS SIGUIENTES", "\n")
      print(x[[2]])
      invisible(x)
    }
    
    mostrar(resultados)
    
    assign("resultados", resultados, envir = .GlobalEnv)
    
    #return(resultados)
    
  } else {
    
    ############ AÑADIMOS LO SIGUIENTE PARA LAS NORMALIZACIONES ############
    
    if (normalizar==TRUE){
    
    if (deparse(substitute(tiempos))=="Tiempos_Especificos_Fruto" | deparse(substitute(tiempos))=="Tiempos_Especificos_Hojas"){
      
      # Separar los nombres de las muestras y los datos numéricos
      nombres_muestras <- MatrizFinal[1:6,]
      datos_numericos <- as.data.frame(MatrizFinal[-(1:6),])
      
      
      datos_numericos[,44:ncol(datos_numericos)]=suppressWarnings(as.data.frame(lapply(datos_numericos[,44:ncol(datos_numericos)],numerico)))
      
      normalizado_ribitol <- datos_numericos
      
      rib <- which(normalizado_ribitol[,3]=="T_9067")
      
      if (length(rib == 0)){
        
        for (j in 44:ncol(normalizado_ribitol)){
          for (k in 1:(nrow(normalizado_ribitol))){
            if (!is.na(datos_numericos[k,j]) & !is.na(datos_numericos[rib,j])) {
              normalizado_ribitol[k,j] <- datos_numericos[k,j]/datos_numericos[rib,j]
            } else {
              normalizado_ribitol[k,j]=NA
            }
          }
        }
        
        normalizado_control=normalizado_ribitol
        
        medias=which(nombres_muestras[2,]==3)
        
        for (l in 44:ncol(normalizado_control)) {
          for (m in 1:nrow(normalizado_control)) {
            normalizado_control[m,l] <- suppressWarnings(as.numeric(normalizado_ribitol[m,l])/mean(as.numeric(normalizado_ribitol[m,medias]),na.rm=TRUE))
          }
        }
        
      } else {
        normalizado_control <- datos_numericos
        
        medias=which(nombres_muestras[2,]==3)
        
        for (l in 44:ncol(normalizado_control)) {
          for (m in 1:nrow(normalizado_control)) {
            normalizado_control[m,l] <- suppressWarnings(as.numeric(normalizado_ribitol[m,l])/mean(as.numeric(normalizado_ribitol[m,medias]),na.rm=TRUE))
          }
        }
      }
    }

      normalizado_ribitol=rbind(nombres_muestras,normalizado_ribitol)
      normalizado_control=rbind(nombres_muestras,normalizado_control) 
      
      output_path <- ifelse(any(dir("../") == "Resultados"), "../Resultados", "Resultados")
      
      xlsx::write.xlsx(MatrizFinal, file.path(output_path, "Results_Final.xlsx"), col.names = FALSE, row.names = FALSE, showNA = TRUE, sheetName = "Resultados")
      xlsx::write.xlsx(normalizado_ribitol, file.path(output_path, "Results_Final.xlsx"), col.names = FALSE, row.names = FALSE, showNA = TRUE, sheetName = "Ribitol", append = T)
      xlsx::write.xlsx(normalizado_control, file.path(output_path, "Results_Final.xlsx"), col.names = FALSE, row.names = FALSE, showNA = TRUE, sheetName = "Controles", append = T)
      
      assign("resultados", MatrizFinal, envir = .GlobalEnv)
      print("Set completo, no falta ninguno")
      
    } else {
    
    output_path <- ifelse(any(dir("../") == "Resultados"), "../Resultados", "Resultados")
    
    xlsx::write.xlsx(MatrizFinal, file.path(output_path, "Results_Final.xlsx"), col.names = FALSE, row.names = FALSE, showNA = TRUE, sheetName = "Resultados")
    
    assign("resultados", MatrizFinal, envir = .GlobalEnv)
    print("Set completo, no falta ninguno")

    }
  }
}

#' @title Selecciona las mejores lecturas para un set analizado por TagFinder
#' @description Función que genera una lista final de los metabolitos seleccionados (Plantilla Tiempos Especificos)
#' @param set .txt generado por tag finder
#' @param tiempos Plantilla de Tiempos Especificos. Por defecto hay 3 (Hoja, Fruto y Split)(Todo de fresa)
#' @return Tras correr este código se presentan los metabolitos que faltan por encontrar. Incluir los tiempos especificos en TagFinder para un segundo filtro
#'

primarios_p <- function(set, tiempos = Tiempos_Especificos_Fruto) {
  if (!(set %in% ls())) {
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
    
    return(resultados)
    
  } else {
    if (any(dir("../") == "Resultados")) {
      xlsx::write.xlsx(MatrizFinal, "Results_Final.xlsx", col.names = FALSE, row.names = FALSE, showNA = TRUE)
    } else if (any(dir() == "Resultados")) {
      xlsx::write.xlsx(MatrizFinal, "Resultados/Results_Final.xlsx", col.names = FALSE, row.names = FALSE, showNA = TRUE)
    } else {
      dir.create("Resultados")
      xlsx::write.xlsx(MatrizFinal, "Resultados/Results_Final.xlsx", col.names = FALSE, row.names = FALSE, showNA = TRUE)
    }
    
    assign("resultados", MatrizFinal, envir = .GlobalEnv)
    
    return(MatrizFinal)
  }
}

#' @title Selecciona las mejores lecturas para un set analizado por TagFinder
#' @description Función que genera una lista final de los metabolitos seleccionados (Plantilla Tiempos Especificos)
#' @param set2 .txt generado por tag finder aplicando los filtros especificos de la funcion "primarios
#' @param set_primarios lista generada por la funcion "primarios". Se usa como input en esta funcion
#' @param tiempos Plantilla de Tiempos Especificos. Por defecto hay 3 (Hoja, Fruto y Split)(Todo de fresa)
#' @return Tras correr este código se presentan los metabolitos que faltan por encontrar.
#'

primarios_faltan <- function(set2, set_primarios=resultados, tiempos = Tiempos_Especificos_Fruto) {
  
  if (typeof(set2)=="character") {
    set2 <- read.delim(set, header = FALSE)
  }
  
  MatrizFinal <- set_primarios[[1]]
  Faltan <- set_primarios[[2]]
  
  trues2 <- set2 %>%
    dplyr::filter(V1 == "true" & V2 == "true")
  
  faltan <- rbind(set2[1:6, ], trues2)
  
  Matriz_faltan <- NULL
  
  for (i in 1:nrow(Faltan)) {
    for (n in 7:(nrow(faltan))) {
      if ((faltan[n, 8] == Faltan[i, 2]) & (faltan[n, 16] == Faltan[i, 3])) {
        Matriz_faltan <- rbind(Matriz_faltan, faltan[n, ])
      }
    }
  }
  
  if (!is.null(Matriz_faltan)) {
    Matriz_faltan <- as.data.frame(Matriz_faltan)
    Matriz_faltan[, 8] <- as.numeric(Matriz_faltan[, 8])
    Matriz_faltan[, 15] <- as.numeric(Matriz_faltan[, 15])
  }
  
  nombres <- MatrizFinal[1:6, ]
  Matriz_faltan <- rbind(MatrizFinal[-(1:6), ], Matriz_faltan)
  Matriz_faltan[, 8] <- as.numeric(Matriz_faltan[, 8])
  Matriz_faltan[, 15] <- as.numeric(Matriz_faltan[, 15])
  
  Matriz2_faltan <- NULL
  Matriz_faltan <- dplyr::arrange(Matriz_faltan, V8)
  MatrizFinal_faltan <- NULL
  
  for (i in 1:nrow(tiempos)) {
    for (a in 1:nrow(Matriz_faltan)) {
      if (Matriz_faltan[a, 16] == tiempos[i, 3] & Matriz_faltan[a, 8] == tiempos[i, 2]) {
        Matriz2_faltan <- rbind(Matriz2_faltan, Matriz_faltan[a, ])
      } else {
        p <- abs(Matriz2_faltan$V8 - Matriz2_faltan$V15)
        MatrizFinal_faltan <- rbind(MatrizFinal_faltan, Matriz2_faltan[which.min(p), ])
        Matriz2_faltan <- NULL
      }
    }
    if (!is.null(Matriz2_faltan)) {
      v <- abs(Matriz2_faltan$V8 - Matriz2_faltan$V15)
      MatrizFinal_faltan <- rbind(MatrizFinal_faltan, Matriz2_faltan[which.min(v), ])
      Matriz2_faltan <- NULL
    }
  }
  
  MatrizFinal <- as.data.frame(MatrizFinal)
  
  frases <- c("DE PUTA MADRE", "ESTAN TODOS BRO", "INCREIBLE, PEDAZO DE SET", "TREMENDA MARAVILLA", "DE LOCOS HERMANO")
  
  Faltan_Tiempos <- setdiff(tiempos[, 2], MatrizFinal_faltan[, 8])
  Faltan <- NULL
  
  if (!is.na(Faltan_Tiempos[1])) {
    for (i in 1:length(Faltan_Tiempos)) {
      for (n in 1:(nrow(tiempos))) {
        if ((tiempos[n, 2] == Faltan_Tiempos[i])) {
          Faltan <- rbind(Faltan, tiempos[n, ])
        }
      }
    }
  }
  
  if (!dir.exists("Resultados") && !dir.exists("../Resultados")) {
    dir.create("Resultados")
  }
  
  if (!is.null(Faltan)) {
    Faltan_relleno <- matrix(NA, nrow(Faltan), ncol(MatrizFinal_faltan) - 14)
    Faltan_relleno <- cbind(Faltan, Faltan_relleno)
    Faltan_relleno <- cbind(matrix(data = NA, nrow(Faltan_relleno), 3), Faltan_relleno)
    
    for (j in 1:nrow(Faltan_relleno)) {
      MatrizFinal_faltan <- berryFunctions::insertRows(MatrizFinal_faltan, (as.numeric(rownames(Faltan_relleno[j, ])) + (1 - j)), Faltan_relleno[j, ])
    }
  }
  
  MatrizFinal_faltan <- rbind(nombres, MatrizFinal_faltan)
  
  output_path <- ifelse(any(dir("../") == "Resultados"), "../Resultados", "Resultados")
  
  xlsx::write.xlsx(MatrizFinal_faltan, file.path(output_path, "Results_Final.xlsx"), col.names = FALSE, row.names = FALSE, showNA = TRUE)
  
  if (!is.null(Faltan)) {
    Faltan <- Faltan[order(Faltan$Tag_Mass), ]
    xlsx::write.xlsx(Faltan, file.path(output_path, "Faltan.xlsx"), col.names = FALSE, row.names = FALSE, showNA = FALSE)
    return(Faltan)
  } else {
    cat("\n", sample(frases, 1), "\n", homer)
  }
}

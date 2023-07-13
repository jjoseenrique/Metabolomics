#' @title Selecciona las mejores lecturas para un set analizado por TagFinder
#' @description Función que genera una lista final de los metabolitos seleccionados (Plantilla Tiempos Especificos)
#' @param set .txt generado por tag finder
#' @param tiempos Plantilla de Tiempos Especificos. Por defecto hay 3 (Hoja, Fruto y Split)(Todo de fresa)
#' @return Tras correr este código se presentan los metabolitos que faltan por encontrar. Incluir los tiempos especificos en TagFinder para un segundo filtro
#'

primarios=function(set,tiempos=Tiempos_Especificos_Fruto){

# DEFINIMOS LAS VARIABLES QUE VAN A SER UTILIZADAS

  Matriz=NULL
  MatrizFinal= NULL
  Matriz2=NULL

# HACEMOS UN FILTRO SELECCIONANDO SOLO AQUELLOS VALORES DOBLE TRUE

  trues= set %>%
    dplyr::filter(set[,1]=="true" & set[,2]=="true")

  data=rbind(set[1:6,],trues)

# SELECCIONAMOS AQUELLOS QUE COINCIDEN CON LA TABLA DE TIEMPOS APORTADA

  for (i in 1:nrow(tiempos)){
    for (n in 7:(nrow(data))){
      if ((data[n,8]==tiempos[i,2]) & (data[n,16]==tiempos[i,3])){
        Matriz=rbind(Matriz,data[n,])}
    }}

  Matriz=as.data.frame(Matriz)
  Matriz[,8]=as.numeric(Matriz[,8])
  Matriz[,15]=as.numeric(Matriz[,15])

# PASAMOS A SELECCIONAR AQUELLAS FILAS QUE SON CORRECTAS

  for (i in 1:nrow(tiempos))
  {
    for (a in 1:nrow(Matriz))
    {
      if (Matriz[a,16] == tiempos[i,3] & Matriz[a,8]==tiempos[i,2])
      {
        Matriz2= rbind(Matriz2, Matriz[a,]) #
      }
      else
      {
        v=c()
        v=abs(Matriz2$V8 - Matriz2$V15)
        MatrizFinal= rbind(MatrizFinal, Matriz2[which.min(v),])
        Matriz2=NULL
      }
    }
    if (!is.null(Matriz2))
    {
      v=c()
      v=abs(Matriz2$V8 - Matriz2$V15)
      MatrizFinal= rbind(MatrizFinal, Matriz2[which.min(v),])
      Matriz2=NULL
    }
  }

  MatrizFinal=rbind(data[1:6,],MatrizFinal)
  MatrizFinal=as.data.frame(MatrizFinal)

# SELECCIONAMOS AQUELLOS QUE FALTAN COMPARÁNDOLO CON LA MATRIZ TIEMPOS APORTADA

  Faltan_Tiempos=c(setdiff(tiempos[,2],MatrizFinal[,8]))
  Faltan=NULL

  frases =c("DE PUTA MADRE", "ESTAN TODOS BRO", "INCREIBLE, PEDAZO DE SET", "TREMENDA MARAVILLA", "DE LOCOS HERMANO")

# DEFINIMOS TODAS LAS POSIBLES OPCIONES PARA QUE EL CODIGO SEPA QUE HACER EN CADA CASO

  if (any(dir()=="Resultados")==TRUE | any (dir("../")=="Resultados")){
  } else {
    dir.create("Resultados")
  }

if (!is.na(Faltan_Tiempos[1])){

  for (i in 1:length(Faltan_Tiempos)){
    for (n in 1:(nrow(tiempos))){
      if ((tiempos[n,2]==Faltan_Tiempos[i])){
        Faltan=rbind(Faltan,tiempos[n,])}
    }
  }
    Faltan=Faltan[order(Faltan$Tag_Mass),]
    resultados=(list(MatrizFinal,Faltan))

    mostrar=function(x){
      cat("\n","FALTAN LOS SIGUIENTES", "\n")
      print(x[[2]])
      invisible(x)
    }
      mostrar(resultados)
  } else {
    if (any(dir("../")=="Resultados")){
      xlsx::write.xlsx(MatrizFinal,"Results_Final.xlsx",col.names = FALSE, row.names = FALSE, showNA = TRUE)
    } else if (any(dir)=="Resultados"){
      xlsx::write.xlsx(MatrizFinal,"Resultados/Results_Final.xlsx",col.names = FALSE, row.names = FALSE, showNA = TRUE)
    }
  }

}

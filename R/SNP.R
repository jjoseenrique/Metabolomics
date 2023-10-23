#' @title SNP Finder
#' @description Selecciona aquellos SNP que tengan un p-value aceptable
#' @param path Ruta de la carpeta con los documentos
#' @param min.P.value El minimo valor de P.value usado como limite para seleccionar los SNPs. Por defecto, 1e-07
#' @return Tras correr este codigo se genera un .xlsx en la carpeta Resultados, en el que se muestran los SNPs relevantes de cada trait
#'

SNP=function(path=getwd(), min.P.value=1e-07){

  documentos=list.files(path,pattern="Results")

  Final=data.frame()
  nombres=c()

  #HACEMOS UN BUCLE PARA QUE SE VAYAN ABRIENDO UNO A UNO LOS DOCUMENTOS

  for (a in 1:length(documentos)){

    file_trait=read.csv(paste0(path,"/",documentos[a]))
    input=as.data.frame(file_trait)

    if (min(input$P.value)<=min.P.value){  
      Final=rbind(Final,input[which(input$P.value<=min.P.value),])
      nombres = c(nombres, gsub(".*CMLM.", "", gsub(".*MLMM.", "", gsub(".*BLINK.", "", gsub(".csv", "", documentos[a])))))
  }
}

  if (nrow(Final)!=0){
  
        if (any(dir(path)=="Resultados")){
} else {
  dir.create(paste0(path,"/Resultados"))
}
  
  Final=cbind(nombres,Final)
  Final = as.data.frame(Final)

xlsx::write.xlsx(Final, paste0(path, "/Resultados/SNP.xlsx"))
} else {
    print("No hay ningún SNP candidato")
    }
        }

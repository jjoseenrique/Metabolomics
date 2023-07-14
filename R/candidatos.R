#' @title Busca los candidatos dentro de nuestro archivo de anotaciones de F. vesca
#' @description Selecciona aquellos candidatos en funcion del numero de SNPs relevantes y la distancia hacia ambos extremos
#' @param path Ruta de la carpeta con los documentos
#' @param distance_SNPs Numero de pb a desplazarse hacia ambos extremos comenzando en la localizacion del SNP
#' @param min.P.value El minimo valor de P.value usado como limite para seleccionar los SNPs. Por defecto, 1e-07
#' @param rm.duplicados Parametro que determina si se eliminan los genes duplicados o no
#' @return Tras correr este codigo se genera un .xlsx en la carpeta Resultados, en el que se muestran los genes candidatos con los criterios seleccionados
#'

candidatos=function(path=getwd(), distance_SNPs=350, min.P.value=1e-07, rm.duplicados=TRUE){

  #MIRAMOS QUE DOCUMENTOS HAY EN EL PATH

  documentos=list.files(path, pattern = "Results")

  #HACEMOS UN BUCLE PARA QUE SE VAYAN ABRIENDO UNO A UNO LOS DOCUMENTOS

  for (a in 1:length(documentos)){

    file_trait=read.csv(paste0(path,"/",documentos[a]))

    ##PREPARAMOS LAS VARIABLES

    Final=NULL
    input=as.data.frame(file_trait)
    GWAS=input[order(input$P.value),]

    #PASAMOS A LA SEGUNDA PARTE DEL CODIGO

    interesting=which(GWAS$P.value<=min.P.value)

    if(!is.na(interesting[1])){
      for (n in 1:length(interesting)){

        myChr = Annotation %>% dplyr::filter(Annotation[,2]==GWAS$Chr[n])

        #SEGUNDA PARTE DEL BUCLE

        for (i in 1:nrow(myChr)){

          if ((GWAS$Pos[n] >= myChr$Start[i] & GWAS$Pos[n] <= myChr$Stop[i]) | (GWAS$Pos[n]-myChr$Stop[i]<=distance_SNPs*1000 & GWAS$Pos[n]-myChr$Stop[i]>= 0) | (myChr$Start[i]-GWAS$Pos[n]<=distance_SNPs*1000 & myChr$Start[i]-GWAS$Pos[n]>= 0)) {

            Final=rbind(Final,myChr[i,])
            rownames(Final)[nrow(Final)]=paste(nrow(Final),"_",GWAS$Chr[n],GWAS$Pos[n])
          }
        }

      }
    }

    ##EN EL CASO DE QUE HAYA ALGUNA COINCIDENCIA

    if (!is.null(Final)){

      ##CREAMOS LA CARPETA RESULTADOS SI NO ESTÁ CREADA

      if (any(dir(path)=="Resultados")){
      } else {
        dir.create(paste0(path,"/Resultados"))
      }

      nombre=gsub(".*_Results","",gsub(".csv","",documentos[a]))

      ## GUARDAMOS NUESTRA TABLA EN LA CARPETA

      Final=as.data.frame(Final)
      Final=Final[!duplicated(Final$`Gene ID (v4.0.a1)`),]
      Final=as.matrix(Final)

      xlsx::write.xlsx(x=Final,file=paste0(path,"/Resultados/Candidates_",distance_SNPs,"kb_",nombre,".xlsx"))

      ##SI QUEREMOS LOS DUPLICADOS, CREAMOS OTRA CARPETA DIFERENTE

      if (rm.duplicados==FALSE){
        if (any(dir(path)=="Resultados_dup")){
      } else {
        dir.create(paste0(path,"/Resultados_dup"))
      }

      nombre=gsub(".*_Results_dup","",gsub(".csv","",documentos[a]))

      ## GUARDAMOS NUESTRA TABLA EN LA CARPETA

      Final=as.data.frame(Final)
      Final=as.matrix(Final)

      xlsx::write.xlsx(x=Final,file=paste0(path,"/Resultados_dup/Candidates_dup",distance_SNPs,"kb_",nombre,".xlsx"))
        }


      ##POR EL CONTRARIO, SI NO HAY NINGUN CANDIDATO

    } else {
      print("No se ha encontrado ningún gen candidato")
    }

  }

}

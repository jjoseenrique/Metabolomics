#' @title Busca los candidatos dentro de nuestro archivo de anotaciones de F. vesca
#' @description Selecciona aquellos candidatos en función del número de SNPs relevantes y la distancia hacia ambos extremos
#' @param input Nuestra tabla obtenida para un trait después de correr GAPIT
#' @param number_SNPs Número de SNPs con menor Pvalue a tener en cuenta. Por lo general, se selecciona mirando el Manhattan plot
#' @param distance_SNPs Número de pb a desplazarse hacia ambos extremos comenzando en la localización del SNP
#' @return Tras correr este codigo se genera un .xlsx en la carpeta Resultados, en el que se muestran los genes candidatos con los criterios seleccionados
#'

candidatos_old_version=function(input,number_SNPs,distance_SNPs=350000){

  #TOMAMOS EL NOMBRE PARA GUARDAR EL XLSX CON EL NOMBRE DEL TRAIT

  originalName=as.character(substitute(input))

  #PREPARAMOS NUESTRAS VARIABLES

  Final=NULL
  input=as.data.frame(input)
  GWAS=input[order(input$P.value),]

  ##PASAMOS A LA SEGUNDA PARTE DEL CODIGO

  for (n in 1:number_SNPs) {

    myChr = Annotation %>% dplyr::filter(Annotation[,2]==GWAS$Chr[n])

    ##SEGUNDA PARTE DEL BUCLE FOR

      for (i in 1:nrow(myChr)){

        if ((GWAS$Pos[n] >= myChr$Start[i] & GWAS$Pos[n] <= myChr$Stop[i]) | (GWAS$Pos[n]-myChr$Stop[i]<=distance_SNPs & GWAS$Pos[n]-myChr$Stop[i]>= 0) | (myChr$Start[i]-GWAS$Pos[n]<=distance_SNPs & myChr$Start[i]-GWAS$Pos[n]>= 0)) {

           Final=rbind(Final,myChr[i,])
           rownames(Final)[nrow(Final)]=paste(nrow(Final),"_",GWAS$SNP[n])
      }
    }

  }

  ##EN EL CASO DE QUE HAYA ALGUNA COINCIDENCIA

  if (!is.null(Final)){

    ##CREAMOS LA CARPETA RESULTADOS SI NO ESTÁ CREADA

    if (any(dir()=="Resultados")==TRUE | any (dir("../")=="Resultados")){
    } else {
      dir.create("Resultados")
    }

    nombre=gsub(".*CMLM.","",originalName)

    ## GUARDAMOS NUESTRA TABLA EN LA CARPETA

    if (any(dir("../")=="Resultados")){
      xlsx::write.xlsx(x=Final,file=paste0("Candidates.",nombre,".xlsx"))}
    else if (any(dir()=="Resultados")){
      xlsx::write.xlsx(x=Final,file=paste0("Resultados/Candidates_",distance_SNPs/1000,"kb_",nombre,".xlsx"))}


  ##POR EL CONTRARIO, SI NO HAY NINGÚN CANDIDATO

  } else {
    print("No se ha encontrado ningún gen candidato")
}

}

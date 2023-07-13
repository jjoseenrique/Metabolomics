#' @title Clasifica las lecturas en funcion del Patron de Genotipos (Coleccion Malaga/Finlandia)
#' @description Añade el numero de correspondencia de cada genotipo en la 2 columna. Genera un .txt que puede ser empleado como samplelist en TagFinder
#' @return Tras correr este codigo se genera un .txt que es directamente guardado en la carpeta en la que nos encontremos
#'

clasificar=function(samplelist){

originalName=deparse(substitute(samplelist))

assign(originalName,samplelist)

samplelist$SAMPNUMB=gsub(".*_","",
                          gsub("-.*","",samplelist$SAMPNUMB)
)

for (i in 1:nrow(samplelist)){
    for (n in 1:nrow(Patron_genotipos)){
      if (samplelist[i,4]==Patron_genotipos[n,1]){
        samplelist[i,2]=Patron_genotipos[n,2]
      }
    }
  }
samplelist=as.data.frame(samplelist)

if (any(dir()=="Resultados")==TRUE | any (dir("../")=="Resultados")){
} else {
  dir.create("Resultados")
}

#write.table(x=samplelist,file="Resultados/Samplelist_SetX.txt", row.names = F, sep="\t", quote = FALSE)
if (any(dir("../")=="Resultados")){
write.table(x=samplelist,file= paste0(originalName,".txt") , row.names = F, sep="\t", quote = FALSE)}
else if (any(dir()=="Resultados")){
  write.table(x=samplelist,file= paste0("Resultados/",originalName,".txt") , row.names = F, sep="\t", quote = FALSE)}
}

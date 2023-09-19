#' @title Genera un heatmap normalizando los datos por la mediana
#' @description Función que genera un heatmap de nuestros datos. Para que funcione, hay que importar los datos de manera que la primera fila sea nuestra variable independiente (genotipos), y la primera columna sea el nombre de nuestros metabolitos
#' @param datos La lista con las lecturas correspondiente a cada genotipo
#' @param barra_secundaria Permite activar o desactivar la barra secundaria a mostrar en el heatmap
#' @param barra_secundaria_nombre Lo que significa esta barra secundaria (Genotipo, localizacion...)
#' @param reorganizar_col Permita activar o desactivar el clustering de columnas
#' @param modelo_orden Función a usar para clusterizar. Por defecto es "dist", pero se puede seleccionar entre "pearson", "spearman" y "kendall"
#' @return Tras correr este código se muestra el Heatmap en la ventana "Viewer".
#'

heatmaps=function(datos, barra_secundaria=TRUE, barra_secundaria_nombre="Location", reorganizar_Col=TRUE, modelo_orden="dist"){

  if (any(dir()=="Resultados")==TRUE) {
    setwd("Resultados")
  } else if (any (dir("../")=="Resultados")) {
  } else {
    dir.create("Resultados")
    setwd("Resultados")
  }

  names=as.matrix(datos[,1])
  data=as.matrix(datos[,-1])

  if (any(grepl("[a-zA-Z]", data))==TRUE){
    stop (cat("
._____________________________________________________________________________________________.
| Error en la matriz de datos. Puedes ver un set de prueba escribiendo:       Ejemplo         |
|                                                                                             |
| O bien guardándola como una variable:                                       Ejemplo=Ejemplo |
|                                                                                             |
|                                                                                             |
| (asegúrate de que la primera columna sean los metabolitos, y de que la primera fila no      |
| contenga los nombres de los genotipos)                                                      |
|_____________________________________________________________________________________________|
              "))
  }

  #CONVERTIMOS LOS DATOS A NUMERIC

  class(data)="numeric"

  #DEFINIMOS LA FUNCION MEDIANA PARA QUE OBVIE LOS NAs

  mediana=function (x){
    median(x,T)
  }

  #APLICAMOS LA FUNCIÓN A NUESTROS DATOS CALCULANDO LA MEDIANA POR FILA (POR METABOLITO)

  data_median=apply(data,1,mediana)

  #SUSTITUIMOS LOS NAs Y LOS 0 DE NUESTRA FUNCIÓN PARA QUE LOS SUSTITUYA POR LA MEDIANA

  for (i in 1:ncol(data)){
    for (n in 1:nrow(data))
      if (is.na(data[n,i])==TRUE || data[n,i]==0){
        data[n,i]=data_median[n]
      };rm(n,i)}

  #NORMALIZAMOS CON RESPECTO A LA MEDIANA

  data_norm=sweep(data,1,data_median,"/")

  #APLICAMOS EL LOGARITMO

  data_log=log(data_norm,2)

  #PONEMOS LOS NOMBRES A NUESTRA MATRIZ

  row.names(data_log)=names

  #DEFINIMOS NUESTRO CÓDIGO DE COLOR

  colores=colorRampPalette(c("navy", "ivory","firebrick3"))(n=200)

  #ADICIONALMENTE, POR DEFECTO SE PUEDE ACTIVAR O DESACTIVAR LA BARRA DE COLOR SECUNDARIA
  #ESTO SIRVE PARA AGRUPARLO EN FUNCIÓN DE LA LOCALIZACIÓN

  maximo=max(data_log,na.rm=TRUE)
  minimo=min(data_log,na.rm=TRUE)

  rango=ceiling(max(abs(maximo),abs(minimo)))

  #############################################################################

  nombres_sin_numero = as.data.frame(tm::removeNumbers(colnames(data_log)))
  colnames(nombres_sin_numero) = barra_secundaria_nombre
  
  if (barra_secundaria){
    col_side_colors_values = nombres_sin_numero
  } else {
    col_side_colors_values = NULL
  }
  
  if (reorganizar_Col && barra_secundaria){
    subplot_heights_values=c(0.15,0.04,0.8)
  } else if (reorganizar_Col && !barra_secundaria){
    subplot_heights_values = c(0.15,0.84)
  } else if (!reorganizar_Col && barra_secundaria){
    subplot_heights_values = c(0.04,0.95)
  } else {
    subplot_heights_values= 1
  }
  
  heatmaply::heatmaply(data_log, colors = colores, Colv = reorganizar_Col,
                       distfun = modelo_orden,
                       showticklabels = c(T, T),
                       column_text_angle = 90,
                       col_side_colors = col_side_colors_values,
                       subplot_heights = subplot_heights_values,
                       subplot_widths = c(0.85, 0.14),
                       limits = c(-rango, rango),
                       file = NULL)



}

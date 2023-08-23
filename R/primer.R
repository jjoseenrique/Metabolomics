#' @title Comprueba los primers con el genoma de F. vesca
#' @description Permite conocer las posiciones a las que se unen los primers diseñados, para asi comprobar si existen SNPs en esas posiciones
#' @param seq_primer Secuencia que deseas comparar con el genoma de F. vesca
#' @pararm chromosome Numero del cromosoma en el que deseas buscar
#' @return Tras correr este codigo se muestra en pantalla el resultado
#'

## Primero creamos la funcion que nos servira para hacer el reverso complementario:

RevComp=function(seq){
  comp_dna<-chartr("ACGT","TGCA",seq)
  comp_dna_int<-utf8ToInt(comp_dna)
  comp_dna_int_rev<-rev(comp_dna_int)
  rc_dna<-intToUtf8(comp_dna_int_rev)
}

## Ahora ya procedemos con nuestra funcion:

primer=function(seq_primer,chromosome){
  results<-vmatchPattern(seq_primer,Fvesca_genome[[chromosome]])
  if (!is.null(results@ends[[1]])){
    if (length(results@ends[[1]])>1){
      stop("\n\033[1mPrimer no válido: Hay varias coincidencias\033[0m\n")
    }
    cat("\033[1mPrimer Fw\033[0m\n\n")
    results[[1]]
  } else {
    seq_primer<-RevComp(seq_primer)
    results<-vmatchPattern(seq_primer,Fvesca_genome[[chromosome]])
    if (is.null(results@ends[[1]])){
      stop("Primer no valido: No se une a la secuencia")
    }
    if (length(results@ends[[1]])>1){
      stop("\n\033[1mPrimer no válido: Hay varias coincidencias\033[0m\n")
    }
    cat("\033[1mPrimer Rv\033[0m\n\n")
    results[[1]]
  }
}

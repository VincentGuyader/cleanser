nb_modalite_possible <- function(vec){
  length(unique(as.character(vec)))
}
# on utilise cela pour de maniere intelligente, au debut choisir entre quali factor et character
considere_comme_factor <- function(vec,seuil=15){
  
  if (is.numeric(vec) && length(vec) == 1){return(FALSE)}
  
  nb_modalite_possible(vec)<seuil
}
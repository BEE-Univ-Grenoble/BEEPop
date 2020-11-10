#' Obtenir la taille de la population à l'année suivante dans une communauté
#'
#' @param community
#' @param K
#'
#' @return
#' @export
#'
#' @examples
modele <- function(community, K){
  interaction<-attributes(community)$interaction
  popt0<-as.integer(current_size(community))
  popevol=c()
  for (i in 1:ncol(community)){
    somme=0
    for (j in 1:ncol(community)){
      somme=somme+interaction[i,j]*popt0[j]
    }
    R=attributes(community[[i]])$rate
    popevol=c(popevol, popt0[i]*R*(1-(somme)/K))
  }
  community<-append_size(community, as.integer(popevol))
  community
}

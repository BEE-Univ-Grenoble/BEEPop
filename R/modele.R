#' Obtenir la taille de la population à l'année suivante dans une communauté
#'
#' @param community
#' @param K
#'
#' @return
#' @export
#'
#' @examples
modele <- function(community){
  interaction<-attributes(community)$interaction
  popt0<-current_size(community)
  popevol=numeric(ncol(community))
  for (i in 1:ncol(community)){
    somme=sum(interaction[i,]*popt0)

    R=attributes(community[[i]])$rate
    K=attributes(community[[i]])$max_capacity
    popevol[i] <- popt0[i]*R*(1-(somme)/K)
  }
  community<-append_size(community, popevol)
  community
}

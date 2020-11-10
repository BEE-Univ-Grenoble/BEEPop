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

  R <- growth_rate(community)
  K <- maximum_capacity(community)
  somme <- interaction %*% popt0

  popevol <- (1 - somme/K) * R * popt0

  community<-append_size(community, popt0 + popevol)
  community
}

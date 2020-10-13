#' Ajouter une nouvelle taille de population dans l'historique
#' des tailles
#'
#' @param data population ou communauté à modifier
#' @param new_size nouvelle taille de la population
#'
#' @return une version modifiée de la population ou communauté
#' @export
#'
append_size <- function(data,new_size) {
  UseMethod("append_size",data)
}

#' Methode append_size applicable à une population
#'
#' @export
#'
#' @rdname append_size
#' @examples
#'   pop <- new_population(10,"species1",rate=1.3)
#'   pop2 <- append_size(pop,20)
#'   pop2
#'
append_size.beepop_population <- function(data,new_size) {
  data[length(data)+1] <- new_size
  data
}

#' Retourne la derniere taille d'une population ou d'une communauté
#'
#' @param data population ou communauté
#'
#' @return la dernierre taille de la population ou de la communauté
#' @export
#'
#' @examples
current_size <- function(data) {
  UseMethod("current_size",data)
}

#' Methode current_size applicable à une population
#'
#' @export
#'
#' @rdname current_size
#' @examples
#' pop <- new_population(c(10,20),"species1",rate=1.3)
#'   popfinale<- current_size(pop)
#'   popfinale
#'
current_size.beepop_population <- function(data) {
  data[length(data)]
}

#' Methode append_size applicable à une community
#'
#' @rdname append_size
#' @export
#'
#' @examples
#' pikachu <- new_population(c(10,20),"pikachu", rate=1.3)
#' salameche <- new_population(c(10,20),"salameche", rate=1.5)
#' pokecom <- new_community(pikachu=pikachu, salameche=salameche)
#' newpokecom<- append_size(pokecom, c(30,40))
#' newpokecom
append_size.beepop_community <- function(data,new_size) {

  new_com <- do.call(new_community,mapply(append_size,
                                          data,
                                          new_size,
                                          SIMPLIFY = FALSE)
  )

  attributes(new_com)$interaction <- attributes(data)$interaction
  attributes(new_com)$label <- attributes(data)$label

  new_com
}

#' Methode current_size appliquée à une community
#'
#' @export
#'
#' @rdname current_size
#' @examples
#' pikachu <- new_population(c(10,20),"pikachu", rate=1.3)
#' salameche <- new_population(c(10,20),"salameche", rate=1.5)
#' pokecom <- new_community(pikachu=pikachu, salameche=salameche)
#' pokecomfinale<- current_size(pokecom)
#' pokecomfinale
current_size.beepop_community <- function(data) {
  as.integer(data[nrow(data),]) -> sizes
  names(sizes) <- colnames(data)
  sizes
}


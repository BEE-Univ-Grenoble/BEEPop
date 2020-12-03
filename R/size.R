#' Adding a new population size in the size history
#'
#' @param data population or community to modify
#' @param new_size new population size
#'
#' @return a modified version of the population or community
#' @export
#'
append_size <- function(data,new_size) {
  UseMethod("append_size",data)
}

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

#' Returns the last size of a population or community
#'
#' @param data population or community 
#'
#' @return the last size of the population or community
#' @export
#'
#' @examples
current_size <- function(data) {
  UseMethod("current_size",data)
}

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


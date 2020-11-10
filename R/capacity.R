#' Returns the maximum capacity for population
#'
#' @param data the instance to consider
#'
#' @return the maximum capacity of the instance
#' @export
#'
maximum_capacity <- function(data) {
  UseMethod("maximum_capacity",data)
}


#' @export
#' @rdname maximum_capacity
#' @examples
#'   pop1 <- new_population(c(10,20,30),"toto",1.2, 100)
#'   maximum_capacity(pop1)
#'
maximum_capacity.beepop_population <- function(data) {
  attributes(data)$max_capacity
}

#' @export
#' @rdname maximum_capacity
#' @examples
#'   pop1 <- new_population(c(10),"Ursus Arctos", 1.2, 100)
#'   pop2 <- new_population(c(100),"Bufo bufo", 1.4, 1000)
#'   com  <- new_community(bear = pop1,
#'                         todd = pop2)
#'   maximum_capacity(com)
#'
maximum_capacity.beepop_community <- function(data) {
    sapply(data,maximum_capacity)
}

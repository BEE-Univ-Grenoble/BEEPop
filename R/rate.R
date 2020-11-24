#' Creation of a global growth rate method
#'
#' @param data the instance to consider
#'
#' @return the growth rate of the instance
#' @export
#'
growth_rate <- function(data) {
  UseMethod("growth_rate",data)
}

#' @export
#' @rdname growth_rate
#' @examples
#'   pop1 <- new_population(c(10,20,30),"Ursus Arctos", 1.2, 100)
#'   growth_rate(pop1)
#'
growth_rate.beepop_population <- function(data) {
  attributes(data)$rate
}

#' @export
#' @rdname growth_rate
#' @examples
#'   pop1 <- new_population(c(10),"Ursus Arctos", 1.2, 100)
#'   pop2 <- new_population(c(100),"Bufo bufo", 1.4, 1000)
#'   com  <- new_community(bear = pop1,
#'                         todd = pop2)
#'   growth_rate(com)
#'
growth_rate.beepop_community <- function(data) {
  sapply(data,growth_rate)
}

#' @param data the instance to consider
#' @param value the new growth rate
#'
#' @return the growth rate of the instance
#' @rdname growth_rate
#' @export
#'
`growth_rate<-` <- function(data,value) {
  UseMethod("growth_rate<-",data)
}

#' @export
#' @rdname growth_rate
#' @examples
#'   pop1 <- new_population(c(10),"Ursus Arctos", 1.2, 100)
#'   growth_rate(pop1)
#'   growth_rate(pop1) <- 1.4
#'   growth_rate(pop1)
#'

`growth_rate<-.beepop_population` <- function(data,value) {
  attributes(data)$rate <- value
  data
}


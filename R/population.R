
#' Create a new instance of population
#'
#' A population is represented by object
#' of class `beepop_population`. Such instance
#' can be created using that function.
#'
#' @param pop_size a numeric vector with the size of the population
#'                 over time
#' @param label    a character indicating the species name
#' @param rate     the growth rate of the population from one generation
#'                 to the following.
#'
#' @return an instance of `beepop_population`
#' @export
#'
#' @examples
#'   new_population(c(10,20,30),"Ursus Arctos", 1.2)
#'
#' @md
new_population <- function(pop_size, label, rate) {
  structure(pop_size,
            label= label,
            rate = rate,
            class = "beepop_population")
}


#' Print method for class `beepop_population`
#'
#' @param x the instance to print
#' @param ... other parameter, not used in that version;
#'
#' @export
#'
#' @examples
#'   z <- new_population(10,"toto",1.3)
#'   print(z)
#'
#' @md
print.beepop_population <- function (x, ...) {
  cat("Species : ", species(x),"\n")
  cat("Growth rate : ", growth_rate(x),"\n")
  cat("Population size : \n")
  print(as.integer(x))
}

#' Title
#'
#' @param data
#'
#' @return
#' @export
#'
#' @examples
growth_rate <- function(data) {
  UseMethod("growth_rate",data)
}

#' Title
#'
#' @param data
#'
#' @return
#' @export
#'
#' @examples
growth_rate.beepop_population <- function(data) {
  attributes(data)$rate
}

#' Title
#'
#' @param data
#'
#' @return
#' @export
#'
#' @examples
species <- function(data) {
  UseMethod("species",data)
}

#' Title
#'
#' @param data
#'
#' @return
#' @export
#'
#' @examples
species.beepop_population <- function(data) {
  attributes(data)$label
}

#' Title
#'
#' @param data
#' @param label
#'
#' @return
#' @export
#'
#' @examples
`species<-` <- function(data,value) {
  UseMethod("species<-",data)
}

#' Title
#'
#' @param data
#' @param label
#'
#' @return
#' @export
#'
#' @examples
`species<-.beepop_population` <- function(data,value) {
  attributes(data)$label <- value
  data
}


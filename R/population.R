
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
#' @param max_capacity maximum size a population can take in a community
#'
#' @return an instance of `beepop_population`
#' @export
#'
#' @examples
#'   new_population(c(10,20,30),"Ursus Arctos", 1.2, 100)
#'
#' @md
new_population <- function(pop_size, label, rate, max_capacity) {
  structure(pop_size,
            label= label,
            rate = rate,
            max_capacity = max_capacity,
            class = "beepop_population")
}


#' Print method for class `beepop_population`
#'
#' @param x the instance to print
#' @param ... other parameter, not used in that version;
#'
#' @return the instance regarded
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
  cat("Maximum capacity :", maximum_capacity(x), "\n")
  cat("Population size : \n")
  print(as.integer(x))
}

#' Creation of a global growth rate method
#'
#' @param data the instance to consider
#'
#' @return the growth rate of the instance
#' @export
#'
#' @examples
#' z <- new_population(10,"toto",1.3)
#' growth_rate (z)
#'
growth_rate <- function(data) {
  UseMethod("growth_rate",data)
}

#' Growth rate method for class `beepop_population`
#'
#' @param data the instance to consider belonging to the class `beepop_population`
#'
#' @return the growth rate of the instance
#' @export
#'
#' @examples
#' z <- new_population(10,"toto",1.3)
#'   growth_rate.beepop_population(z)
#'
growth_rate.beepop_population <- function(data) {
  attributes(data)$rate
}

#' Creation of a global species method
#'
#' @param data the instance to consider
#'
#' @return the species of the instance
#' @export
#'
#' @examples
#' z <- new_population(10,"toto",1.3)
#'   species(z)
#'
species <- function(data) {
  UseMethod("species",data)
}

#' Species method for class `beepop_population`
#'
#' @param data the instance to consider belonging to the class `beepop_population`
#'
#' @return the species of the instance
#' @export
#'
#' @examples
#'  z <- new_population(10,"toto",1.3)
#'   species.beepop_population(z)
#'
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


#' Title
#'
#' @param data
#'
#' @return
#' @export
#'
#' @examples
maximum_capacity <- function(data) {
  UseMethod("maximum_capacity",data)
}


#' Title
#'
#' @param data
#'
#' @return
#' @export
#'
#' @examples
maximum_capacity.beepop_population <- function(data) {
  attributes(data)$max_capacity
}

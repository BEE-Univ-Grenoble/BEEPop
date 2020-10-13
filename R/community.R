#' @import tibble
NULL

#' Title
#'
#' @param ...
#' @param label
#' @param interaction
#'
#' @return
#' @export
#'
#' @examples
new_community <- function(..., label, interaction = NULL) {

  pops <- list(...)

  pops <- as_tibble(pops)

  if (is.null(interaction)) {
    interaction <- diag(ncol(pops))
  }

  if (missing(label)) {
    label = "community"
  }

  structure(pops,
            label= label,
            interaction = interaction,
            class = c("beepop_community",class(pops)))
}

#' Title
#'
#' @param data
#' @param new_size
#'
#' @return
#' @export
#'
#' @examples
append_size.beepop_community <- function(data,new_size) {

  new_com <- do.call(new_community,mapply(append_size,
                                          com,
                                          new_size,
                                          SIMPLIFY = FALSE)
                     )

  attributes(new_com)$interaction <- attributes(data)$interaction
  attributes(new_com)$label <- attributes(data)$label

  new_com
}

#' Title
#'
#' @export
#'
#' @rdname current_size
#' @examples
current_size.beepop_community <- function(data) {
  as.integer(data[nrow(data),]) -> sizes
  names(sizes) <- colnames(data)
  sizes
}


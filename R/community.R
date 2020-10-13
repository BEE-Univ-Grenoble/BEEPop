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


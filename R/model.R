#' @import tibble
#' @import RColorBrewer
#' @import ggplot2
#' @import reshape2
NULL

#' Obtain next year species size for each species of the community
#'
#' @param community from new_community() function
#'
#' @return a tibble with the diverse species size for the following year
#'
#' @examples
#' wolf <- new_population(14, "wolf", 0.3, 100)
#' sheep <- new_population(26, "sheep", 0.7, 100)
#' goat <-new_population(23, "goat", 0.5, 100)
#' shepherd <- new_community("wolf" = wolf, "sheep" = sheep, "goat" = goat)
#'
#' beepop_model(shepherd)
#'
#' @export

beepop_model <- function(community){
  interaction <- attributes(community)$interaction
  popt0<-current_size(community)

  R <- growth_rate(community)
  K <- maximum_capacity(community)
  somme <- interaction %*% popt0

  popevol <- (1 - somme/K) * R * popt0

  community <- append_size(community, popt0 + popevol)
  community
}



#' Obtain next year species size for each species of the community
#'
#' @param community from new_community() function
#'
#' @param n number of years
#'
#' @return a tibble with the diverse species evolution through years
#'
#' @examples
#' wolf <- new_population(14, "wolf", 0.3, 100)
#' sheep <- new_population(26, "sheep", 0.7, 100)
#' goat <- new_population(23, "goat", 0.5, 100)
#' shepherd <- new_community("wolf" = wolf, "sheep" = sheep, "goat" = goat)
#'
#' model_multi_years(shepherd,10)
#'
#' @export

model_multi_years <- function(community, n){
  for (i in 1:n){
    community <- beepop_model(community)
  }
  community
}



#' Plot species' community evolution
#'
#' @param community from new_community() function
#'
#' @param n number of years
#'
#' @param gg.plot boolean, whether to use 'classic' plots with gg.plot=TRUE or ggplots with gg.plot=FALSE
#'
#' @return a plot or ggplot and a tibble with the diverse species evolution through years
#'
#' @examples
#' wolf <- new_population(14, "wolf", 0.3, 100)
#' sheep <- new_population(26, "sheep", 0.7, 100)
#' goat <- new_population(23, "goat", 0.5, 100)
#' shepherd <- new_community("wolf" = wolf, "sheep" = sheep, "goat" = goat)
#'
#' model_multi_years_plot(shepherd, 10, gg.plot=TRUE)
#' model_multi_years_plot(shepherd, 10)
#'
#' @export

model_multi_years_plot <- function(community, n, gg.plot=FALSE){
  evol_model <- model_multi_years(community, n)
  if(gg.plot==FALSE){
    ncolor <- RColorBrewer::brewer.pal(n = ncol(evol_model), name = "Set1")
    plot(evol_model[,1], lty=1, type="l", col="red", ylim=c(0,max(evol_model)),
         main="Species number evolution",
         xlab="Years",
         ylab="Species number")
    for(i in 2:ncol(evol_model)){
      lines(evol_model[,i], col=ncolor[i])
    }
    legend("bottomright",legend=c(colnames(evol_model)), lty=1, cex=0.8,
           col=ncolor[c(1:ncol(evol_model))])
  }
  else{
    evol_model$annee <- 1:nrow(evol_model)
    evol_model <- data.frame(reshape2::melt(evol_model[,1:(ncol(evol_model))-1]),"annee"=c(rep(evol_model$annee,ncol(evol_model)-1)))
    ggplot2::ggplot(data=evol_model, aes(y=value, x=annee, color=variable))+
      geom_line()+
      labs(color = "Species")+xlab("Years")+ylab("Species numbers")+
      ggtitle("Species number evolution")
  }
}




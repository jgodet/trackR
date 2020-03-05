# cleanNearest.r
# written by JuG
# March 05 2020


#' Do something
#' @author JuG
#' @description 
#' @param 
#' @details 
#' @examples 
#'
#'
#' @return 
#' @export


cleanNearest<- function(data, k=10){
  require(spatstat)
  AZppp <- ppp(x = data$x,y = data$y,window=owin(range(data$x), range(data$y)))
  X <- nnclean(AZppp, k=k,convergence=.0005, edge.correct = TRUE,verbose = FALSE)
  return(data[X$marks[,1]=='feature',])
}

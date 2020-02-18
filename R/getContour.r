# getContour.r
# written by JuG
# January 10 2018


#' Return the convexhull contour of corrdinates
#' @author JuG
#' @description Return the convexhull contour of corrdinates
#' @param data coordinate data
#' @param drawContour if TRUE draw a convexhull contour line
#' @param nbPix box size for the owin in ppp object
#' @details
#' @examples
#'
#'
#' @return
#' @export


getContour<- function(data,drawContour=TRUE,nbPix=10,...){
    xmed <- median(data[,1])
    ymed  <-median(data[,2])
    options( warn = -1 )
    pPp <- ppp(data[,1],data[,2],c(xmed+c(-1,1)*nbPix),c(ymed+c(-1,1)*nbPix))
    hpts <- convexhull(pPp)
    options( warn = 1 )
    hpts$bdry[[1]]$x <- c(hpts$bdry[[1]]$x,hpts$bdry[[1]]$x[1])
    hpts$bdry[[1]]$y <- c(hpts$bdry[[1]]$y,hpts$bdry[[1]]$y[1])
    #plot(data[,c("x","y")])
    if(drawContour){
      lines(hpts$bdry[[1]])
    }
  return(hpts$bdry[[1]])
}

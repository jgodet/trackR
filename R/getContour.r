# getContour.r
# written by JuG
# January 10 2018


#' Return the convexhull contour of coordinates
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


getContour<- function(data,drawContour=FALSE,...){
  require(spatstat)
    hpts <- convexhull.xy(x = data$x, y = data$y)
    options( warn = 1 )
    hpts$bdry[[1]]$x <- c(hpts$bdry[[1]]$x,hpts$bdry[[1]]$x[1])
    hpts$bdry[[1]]$y <- c(hpts$bdry[[1]]$y,hpts$bdry[[1]]$y[1])
    #plot(data[,c("x","y")])
    if(drawContour){
      lines(hpts$bdry[[1]])
    }
  return(hpts$bdry[[1]])
}

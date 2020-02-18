# calcDist.r
# written by JuG
# January 10 2018


#' Compute euclidian distance between 2 points
#' @author JuG
#' @description
#' @param data a dataframe with 2 lines and 2 or 3 columns (x,y,(z))
#' @param spaceRes
#' @details
#' @examples
#' data2D <- data.frame(x =c(0,1), y=c(0,1))
#' calcDist(data2D)
#' data3D <- data.frame(x =c(0,1), y=c(0,1), z=c(0,1))
#' calcDist(data3D)
#' @return
#' @export


calcDist <- function(data,spaceRes=1){
  distc <- NA
  if(dim(data)[2]==2){
    distc <- sqrt(((data[2,2]-data[1,2])*spaceRes)^2+
                  ((data[2,1]-data[1,1])*spaceRes)^2)
  }
  if(dim(data)[2]==3){
    distc <- sqrt(((data[2,3]-data[1,3])*spaceRes)^2+
                  ((data[2,2]-data[1,2])*spaceRes)^2 +
                  ((data[2,1]-data[1,1])*spaceRes)^2  )
  }
  return(distc)
}

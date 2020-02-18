# createTestEllipse.r
# written by JuG
# January 10 2018


#' Create a test ellipse
#' @author JuG
#' @description Create a test ellipse
#' @param Rx X-radius
#' @param Ry Y-radius
#' @param Cx X-center
#' @param Cy Y-center
#' @param Rotation Radians
#' @param N Nb of points
#' @param NoiseLevel Gaussian Noise level
#' @details
#' @examples
#'
#'
#' @return
#' @export

createTestEllipse <- function(Rx=300,         # X-radius
                              Ry=200,         # Y-radius
                              Cx=250,         # X-center
                              Cy=150,         # Y-center
                              Rotation=0.4,   # Radians
                              N = 200,         # Nb of points
                              NoiseLevel=10) # Gaussian Noise level
{
  set.seed(42)
  t <- runif(N,0, 2*pi)
  x <- Rx * cos(t)
  y <- Ry * sin(t)
  nx <- x*cos(Rotation)-y*sin(Rotation) + Cx
  nx <- nx + rnorm(length(t))*NoiseLevel
  ny <- x*sin(Rotation)+y*cos(Rotation) + Cy
  ny  <- ny + rnorm(length(t))*NoiseLevel
  return(cbind(x=nx, y=ny))
}

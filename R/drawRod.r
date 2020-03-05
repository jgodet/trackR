# drawRod.r
# written by JuG
# January 10 2018


#' Draw rod contour
#' @author JuG
#' @description
#' @param data coordinates of the contour
#' @examples
#' coord <- createSpheroCylinderCoord( l =1.8,r=.3,rot=120)
#' plot(coord[,1:2],asp=1,pch='.')
#' gcont <- getContour(coord[,1:2],drawContour = TRUE)
#' drawRod(gcont,lty=3)
#' @return
#' @export


drawRod<- function(data,lwd, ...){
  if(missing(lwd)){lwd <- 3}
  fit <- fitEllipse(data)
  A = pi * fit$major * fit$minor
  L = pi * 3 * (fit$major/2 + fit$minor/2) - sqrt(( 3 * fit$major/2 + fit$minor/2)*( fit$major/2 + 3 * fit$minor/2 ) )
  r =( L - sqrt( L**2 - 4 * pi * A)) * 1.05/(2*pi)  #1.05 (experimental)

  xctr <- unname(fit$center[1])
  yctr <- unname(fit$center[2])
  theta <- unname(fit$angle)
  theta <- ifelse(fit$sA[2] > fit$sA[1] , theta, theta +pi/2 )
  theta <- ifelse(fit$coef[1] >= fit$coef[3], theta, theta +pi/2 )

  rac = fit$major * .95  #0.95 (experimental)
  x <- c((xctr - rac + r) + 1:11*2*(rac-r)/11,
         xctr+rac - r + r*cos(seq(-pi/2, pi/2,length = 8)),
         (xctr + rac - r) - 1:11*2*(rac-r)/11,
         xctr-rac + r - r*cos(seq(pi/2, -pi/2,length = 8))
  )
  y <- c(rep(yctr - r, 11),
         yctr + r*sin(seq(-pi/2, pi/2,length = 8)),
         rep(yctr + r, 11),
         yctr + r*sin(seq(pi/2, -pi/2,length = 8))
  )

  xx = x - xctr
  yy = y - yctr

  xpoints = xctr + xx * cos(theta) - yy * sin(theta)
  ypoints = yctr + yy * cos(theta) + xx * sin(theta)
  xpoints = c(xpoints,xpoints[1])
  ypoints = c(ypoints,ypoints[1])
  lines(xpoints,ypoints,lwd=3,...)
  #text(paste(i),x = xctr +.5, y = yctr +.5)
}

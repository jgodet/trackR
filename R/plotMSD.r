# plotMSD.r
# written by JuG
# March 08 2020


#' Plot MSD
#' @author JuG
#' @description 
#' @param msdData MSD data (list) (output of calcMSD function)
#' @param deltaT time elapsed between two consecutive frame
#' @param fitMSD boolean 
#' @param printMSDfit boolean
#' @param npoint4fit number of points to use for MSD fitting
#' @details 
#' @examples 
#'
#'
#' @return 
#' @export


plotMSD <- function(msdData, deltaT, fitMSD =TRUE,printMSDfit=TRUE,npoint4fit =4,...){
  if(missing(msdData)){
    return(cat('MSD data are missing'))
  }
  if(missing(deltaT)){
    deltaT <- 1
    xlab <- "Time (timesteps)"
  }else{
    xlab <- "Time (ms)"
  }
  tmax <- dim(msdData)[1]
  tt <- (1:tmax) * deltaT
  xlim <- c(0,tmax)
  ylim <- c(0, max(msdData$mean + msdData$sd, na.rm=T)*1.05)
  plot(tt,msdData$mean, xlim = xlim, ylim=ylim, xlab=xlab, ylab = "MSD", las=1,pch=21, bg='lightgrey', ...)
  points(0,0)
  for(i in tt){
    segments(x0 = i, y0 = msdData$mean[i] - msdData$sd[i], y1 =  msdData$mean[i] + msdData$sd[i],... )
  }
  if(fitMSD){
    mod <- lm(mean~ tt[1:npoint4fit] - 1, weights = c(n), data=msdData[1:npoint4fit,])
    abline(mod, col="red")
    if(printMSDfit){
      print(summary(mod))
      cat("Diffusion", round(coefficients(mod)/4, 5)," [", round(confint(mod)[[1]]/4, 4)," - ",round(confint(mod)[[2]]/4, 4),  "] \n")
    }
    
  }
}

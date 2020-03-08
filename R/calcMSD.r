# calcMSD.r
# written by JuG
# March 08 2020


#' Calculate Mean Square Distance 
#' @author JuG
#' @description 
#' @param data data frame (including trace, x, y and t)
#' @param N for MSD, dt should be up to 1/N of number of data points (4 recommended)
#' @details 
#' @examples 
#' xmlPath <- "//Users/jgodet/Seafile/MaBibliotheque/Hanna/1_3_MMStack_Pos0.ome.xml"
#' xmlPath <- "/Users/jgodet/Seafile/MaBibliotheque/Code/TrackMate/nmeth.2808-sv1.xml"
#' data <- readTrackMateXML(XMLpath = xmlPath)
#' data$jump<-jump(data, spaceRes=1)
#' msdMat <- calcMSD(data, N=3)
#' plotMSD(msdMat, col='blue')
#' @return 
#' @export


calcMSD <- function(data, N){
  require(tidyverse)
  traceList <- unique(data$trace)
  tList <- list()
  for (i in traceList){
    a <- data %>% filter(trace == i) %>% select(Frame, x)
    Frame0 <- a$Frame[1]
    a <- a[-1,]
    tList[[i]] <-  a$Frame -Frame0
  }
  tListmax <- max(unlist(lapply(tList, max)))
  displacementXMat <- matrix(data = NA, nrow = tListmax, ncol=length(traceList))
  colnames(displacementXMat) <- traceList
  rownames(displacementXMat) <- 1:tListmax
  displacementYMat <- displacementXMat
  
  for (i in traceList){
    a <- data %>% filter(trace == i) %>% select(x,y, Frame)
    Frame0 <- a$Frame[1]
    a <- a[-1,]
    a$Frame <- a$Frame -Frame0
    displacementXMat[a$Frame,i] <- a$x 
    displacementYMat[a$Frame,i] <- a$y 
  }
  
  tListmax2 <- quantile(unlist(lapply(tList, max)), probs=.9)
  numberOfdeltaT = floor(tListmax2/N) # for MSD, dt should be up to 1/4 of number of data points
  msd <- matrix(data = NA, nrow =numberOfdeltaT, ncol=3 ) # [mean, std, n]
  colnames(msd) <- c("mean", "sd", "n")
  
  for( deltaT in 1:numberOfdeltaT){
    deltaXCoords = displacementXMat[(1+deltaT):tListmax,] - displacementXMat[1: (tListmax-deltaT),] 
    deltaYCoords = displacementYMat[(1+deltaT):tListmax,] - displacementYMat[1: (tListmax-deltaT),] 
    
    squaredDisplacement = deltaXCoords**2 + deltaYCoords**2
    msd[deltaT,1] = mean(squaredDisplacement, na.rm=T) # average
    msd[deltaT,2] = sd(squaredDisplacement, na.rm=T) # std
    msd[deltaT,3] = sum(!is.na(squaredDisplacement)) # n
  }
  return( as.data.frame(msd))
}

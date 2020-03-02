# -*- utf8 -*-

# Compute jump distance between consecutive localisations
# Date : 2017-11-30
# Original function : Julien Godet
# Refactored by : Julien Godet

#' Compute jump distance 
#' @description  Compute jump distance within a frame between two consecutive frames. Note that if a gap exist between consecutive frames (gap-closing frame gap) the jump is calculated as the average displacement (distance / delta frame)  
#' @author Julien Godet
#'
#' @param data Dataframe with track (trace identification) and Frame variable
#' @param spaceRes Pixel size
#' @examples a <- 2
#' b <- 2 * a
#'
#' @export

jump<-function(data,spaceRes){
  jump<-numeric() #declarer les jumps
  deltaFrame <- numeric() #declarer les deltas temps
  jump[1] <- NA
  deltaFrame[1] <- NA
  data <- data[order(data$trace,data$t),] #ordonne dataframe en track puis temps
  for (i in 2:dim(data)[1]){
    deltaFrame[i] <-data[i,'Frame']-data[i-1,'Frame']
    if(data$trace[i]==data$trace[i-1]){    #si trace identique calcul
      #calcul jump entre 2 frames
      jump[i]<-sqrt(((data[i,'y']-data[i-1,'y'])*spaceRes)^2+
                      ((data[i,'x']-data[i-1,'x'])*spaceRes)^2)
    }else{
      jump[i]<-NA        #sinon NA
    }
  }
  return(jump/deltaFrame)
}

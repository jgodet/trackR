# -*- utf8 -*-

# Compute jump distance between consecutive localisations
# Date : 2017-11-30
# Original function : Julien Godet
# Refactored by : Julien Godet

#' Compute jump distance between consecutive localisations
#' @description  Extra things
#' @author Julien Godet
#'
#' @param data Dataframe with track (track identification) and t (frame or time) variable
#' @param spaceRes Pixel size
#' @examples a <- 2
#' b <- 2 * a
#'
#' @export

jump<-function(data,spaceRes){
  jump<-numeric() #declarer les jumps
  deltaT <- numeric() #declarer les deltas temps
  jump[1] <- NA
  deltaT[1] <- NA
  data <- data[order(data$track,data$t),] #ordonne dataframe en track puis temps
  for (i in 2:dim(data)[1]){
    deltaT[i] <-data[i,'t']-data[i-1,'t']
    if(data$track[i]==data$track[i-1]){    #si trace identique calcul
      #calcul jump entre 2 frames
      jump[i]<-sqrt(((data[i,'y']-data[i-1,'y'])*spaceRes)^2+
                      ((data[i,'x']-data[i-1,'x'])*spaceRes)^2)
    }else{
      jump[i]<-NA        #sinon NA
    }
  }
  return(jump/deltaT)
}

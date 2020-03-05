# clusterTraces.r
# written by JuG
# March 05 2020


#' cluster traces in space (and/or time) using DBscan
#' @author JuG
#' @description 
#' @param dtf data frame with coordinates and traces ID
#' @param coord coordinates dataframe (x,y) or (x,y,t)
#' @param eps Reachability distance, see Ester et al. (1996).
#' @param minPts Reachability minimum no. of points, see Ester et al. (1996).
#' @param method "dist" treats data as distance matrix (relatively fast but memory expensive), "raw" treats data as raw data and avoids calculating a distance matrix (saves memory but may be slow), "hybrid" expects also raw data, but calculates partial distance matrices (very fast with moderate memory requirements).
#' @details 
#' @examples 
#' xmlPath <- "//Users/jgodet/Seafile/MaBibliotheque/Hanna/1_3_MMStack_Pos0.ome.xml"
#' data <- readTrackMateXML(XMLpath = xmlPath)
#' data$jump<-jump(data, spaceRes=1)
#' bacteria <- clusterTraces(dtf = data, eps = .2,minPts = 100)
#' table(bacteria)
#' bact1 <- data[which(bacteria==7),]
#' summary(bact1)
#' bact1 %>% select(x,y) %>% plot(., asp=1)
#' drawRod(data = getContour(bact1),col='green')
#' drawRod(data = getContour(cleanNearest(data = bact1,k = 3)),col='blue')

#' @return 
#' @export


clusterTraces<- function(dtf, coord = c("x", "y"), eps, minPts, method, ...){
  if(!require("fpc")){install.packages('fpc')}
  library('fpc')
  require(tidyverse)
  if(missing(method)){method <- "hybrid"}
  #aggregate tracs by their xmedian and ymedian
  if(length(coord)==2){
    datTrace <- dtf %>% group_by(trace) %>% summarise(xmed = median(x, na.rm=T),
                                                        ymed = median(y, na.rm=T)
    )
  } 
  if(length(coord)==3){
    datTrace <- dtf %>% group_by(trace) %>% summarise(xmed = median(x, na.rm=T),
                                                        ymed = median(y, na.rm=T),
                                                        tmed = median(t, na.rm=T)
    )
  }
  if(length(coord)<2 | length(coord)>3){
      return("coord length is not correct")
  }
  clustLoc <- dbscan(data = datTrace[,-1],eps=eps,MinPts=minPts, method=method, ...)
  DBclust <- clustLoc$cluster
  DBclust[ DBclust==0] <- NA
  
  clustList <- unique(unique(DBclust))
  bacteria <- NA
  for (i in seq(along=clustList)){
    bacteria[which(dtf$trace %in% datTrace$trace[which(DBclust==clustList[i])])] <- clustList[i]
  }
  return(bacteria)
}

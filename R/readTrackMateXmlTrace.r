# readTrackMateXmlTrace.r
# written by JuG
# March 11 2020


#' read TrackMate XML traces output files
#' @author JuG
#' @description Function to read TrackMate XML traces output files: much faster than reading the complete TRack Mate XML file
#' @param XMLpath XML file path
#' @param timeRes time exposure per frame
#' @details 
#' @examples 
#' xmlPath <- "/Users/jgodet/Seafile/MaBibliotheque/Code/TrackMate/nmeth.2808-sv1Trace.xml"
#' data <- readTrackMateXmlTrace(XMLpath = xmlPath)
#'
#' data$jump<-jump(data, spaceRes=1)
#' hist(data$jump, breaks=150)
#' fitJumpDistHist(data=data, breaks = 150)
#' msdMat <- calcMSD(data, N=2)
#' plotMSD(msdMat, col='blue')
#'
#' @return 
#' @export


readTrackMateXmlTrace<- function(XMLpath, timeRes = 1){
  if(!require("xml2")){devtools::install_github("r-lib/xml2")}
  require("xml2")
  if(!require("XML")){install.packages('XML')}
  require("XML")
  #if(!require("foreach")){install.packages("foreach")}
  #library("foreach")
  #if(!require("doParallel")){install.packages("doParallel")}
  #library("doParallel")
  
  #numCores <- detectCores()
  
  e = xmlParse(XMLpath)
  #e <- xmlTreeParse(XMLpath)
  
  part = getNodeSet(e, "//particle")
  trackL <- sapply(part, xmlGetAttr, "nSpots")
  track = getNodeSet(e, "//detection")
  tt <- as.numeric(as.character(sapply(track, xmlGetAttr, "t")))
  frame <- round(tt / timeRes)
  x <- as.numeric(as.character(sapply(track, xmlGetAttr, "x")))
  y <- as.numeric(as.character(sapply(track, xmlGetAttr, "y")))
  
  traceID <- rep(x = 1:length(trackL), as.numeric(as.character(trackL)))
  IDtrace <- paste("ID",traceID,sep='')
  
  dtf <- data.frame(ID = IDtrace,
                    trace = traceID,
                    Frame = frame,
                    t = tt,
                    x = round(x,3),
                    y = round(y,3))
  return(dtf)
}

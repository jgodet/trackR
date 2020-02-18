# readTrackMateXML.r
# written by JuG
# February 18 2020


#' read TrackMate XML output files
#' @author JuG
#' @description read TrackMate XML output files
#' @param XMLpath path to the xml file
#' @details 
#' @examples 
#' xmlPath <- "/Users/jgodet/Desktop/sv1.xml"
#' readTrackMateXML(XMLpath = xmlPath)
#'
#' @return 
#' @export


readTrackMateXML<- function(XMLpath){
  if(!require("xml2")){devtools::install_github("r-lib/xml2")}
  require("xml2")
  e = xmlParse(XMLpath)
  #e <- xmlTreeParse(XMLpath)
  
  track = getNodeSet(e, "//Track")
  subdoc <- getNodeSet(e,"//AllSpots//SpotsInFrame//Spot")
  
  dtf <- as.data.frame(cbind(sapply(subdoc, xmlGetAttr, "name"),
                             sapply(subdoc, xmlGetAttr, "POSITION_T"),
                             sapply(subdoc, xmlGetAttr, "MEDIAN_INTENSITY"),
                             sapply(subdoc, xmlGetAttr, "MEAN_INTENSITY"),
                             sapply(subdoc, xmlGetAttr, "TOTAL_INTENSITY"),
                             sapply(subdoc, xmlGetAttr, "SNR"),
                             sapply(subdoc, xmlGetAttr, "POSITION_X"),
                             sapply(subdoc, xmlGetAttr, "POSITION_Y"),
                             sapply(subdoc, xmlGetAttr, "STANDARD_DEVIATION")))
  
  for (i in 2:9){
    dtf[,i] <- round(as.numeric(as.character(dtf[,i])),3)
  }
  names(dtf) <- c("ID","Frame","MEDIAN_INTENSITY","MEAN_INTENSITY", "TOTAL_INTENSITY", "SNR", "POSITION_X", "POSITION_Y", "STANDARD_DEVIATION")
  
  
  IDtrace <- data.frame(ID=NA, trace=NA)  
  
  for (i in seq(along=track)){ 
    subDoc = xmlDoc(track[[i]])
    IDvec <- unique(c(unlist(xpathApply(subDoc, "//Edge", xmlGetAttr, "SPOT_SOURCE_ID")),
                      unlist(xpathApply(subDoc, "//Edge", xmlGetAttr, "SPOT_TARGET_ID"))))
    traceVec <- rep(sapply(track[i], function(el){xmlGetAttr(el, "TRACK_ID")}), length(IDvec))
    IDtrace <- rbind(IDtrace, data.frame(ID=paste("ID",IDvec,sep=''), trace=traceVec))
  }
  
  daten <- merge(IDtrace,dtf, by="ID")
  daten <- daten[order(daten$trace, daten$Frame),]
  
  return(daten)
}

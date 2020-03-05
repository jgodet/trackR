# readTrackMateXML.r
# written by JuG
# February 18 2020


#' read TrackMate XML output files
#' @author JuG
#' @description read TrackMate XML output files
#' @param XMLpath path to the xml file
#' @param multiCore use multi-cores (boolean)
#' @details 
#' @examples 
#' xmlPath <- "/Users/jgodet/Seafile/MaBibliotheque/Code/TrackMate/nmeth.2808-sv1.xml"


#' data <- readTrackMateXML(XMLpath = xmlPath)

#' data <- readTrackMateXML(XMLpath = xmlPath, multiCore = FALSE)
#' data$jump<-jump(data, spaceRes=1)
#' hist(data$jump, breaks=150)
#' @return data frame
#' @export


readTrackMateXML<- function(XMLpath, multiCore = TRUE){
  if(!require("xml2")){devtools::install_github("r-lib/xml2")}
  require("xml2")
  if(!require("XML")){install.packages('XML')}
  require("XML")
  if(!require("foreach")){install.packages("foreach")}
    library("foreach")
  if(!require("doParallel")){install.packages("doParallel")}
  library("doParallel")
  
  numCores <- detectCores()
  
  e = xmlParse(XMLpath)
  #e <- xmlTreeParse(XMLpath)
  
  track = getNodeSet(e, "//Track")
  subdoc <- getNodeSet(e,"//AllSpots//SpotsInFrame//Spot")

  if(multiCore & numCores > 1){
    registerDoParallel(numCores)
    cat("Collecting data...\n")
    attrName <- c("name","FRAME","POSITION_T","MEDIAN_INTENSITY","MEAN_INTENSITY", "TOTAL_INTENSITY", "SNR", "POSITION_X", "POSITION_Y", "STANDARD_DEVIATION")
    
    dtf <- as.data.frame(foreach(i=1:10, .combine = cbind) %dopar% {
      sapply(subdoc, xmlGetAttr, attrName[i])
    })
    for (i in 2:10){
      dtf[,i] <- round(as.numeric(as.character(dtf[,i])),3)
    }
    names(dtf) <- c("ID","Frame","t","MEDIAN_INTENSITY","MEAN_INTENSITY", "TOTAL_INTENSITY", "SNR", "x", "y", "STANDARD_DEVIATION")
   
    cat("\nConcatenating data...\n") 
    # concIt <- function(i){
    #   subDoc = xmlDoc(track[[i]])
    #   IDvec <- unique(c(unlist(xpathApply(subDoc, "//Edge", xmlGetAttr, "SPOT_SOURCE_ID")),
    #                     unlist(xpathApply(subDoc, "//Edge", xmlGetAttr, "SPOT_TARGET_ID"))))
    #   traceVec <- rep(sapply(track[i], function(el){xmlGetAttr(el, "TRACK_ID")}), length(IDvec))
    #   return(data.frame(ID=paste("ID",IDvec,sep=''), trace=traceVec))
    # }
    # 
    # IDtrace <- as.data.frame(foreach(i=1:length(track), .combine = rbind) %dopar% {
    #   concIt(i)})
    # IDtrace[,1] <- as.character(IDtrace[,1])
    # IDtrace[,2] <- as.character(IDtrace[,2])
    IDtrace <- data.frame(ID=NA, trace=NA)  
    
    for (i in seq(along=track)){ 
      subDoc = xmlDoc(track[[i]])
      IDvec <- unique(c(unlist(xpathApply(subDoc, "//Edge", xmlGetAttr, "SPOT_SOURCE_ID")),
                        unlist(xpathApply(subDoc, "//Edge", xmlGetAttr, "SPOT_TARGET_ID"))))
      traceVec <- rep(sapply(track[i], function(el){xmlGetAttr(el, "TRACK_ID")}), length(IDvec))
      IDtrace <- rbind(IDtrace, data.frame(ID=paste("ID",IDvec,sep=''), trace=traceVec))
    }
  }
  
    
  if(!multiCore | numCores < 2){
  iName <- sapply(subdoc, xmlGetAttr, "name")
  cat("ID exctracted...\n")
  iFRAME <- sapply(subdoc, xmlGetAttr, "FRAME")
  cat("FRAME exctracted...\n")
  iPOSITION_T <- sapply(subdoc, xmlGetAttr, "POSITION_T")
  cat("POSITION_T exctracted...\n")
  iMEDIAN_INTENSITY <- sapply(subdoc, xmlGetAttr, "MEDIAN_INTENSITY")
  cat("MEDIAN_INTENSITY exctracted...\n")
  iMEAN_INTENSITY <- sapply(subdoc, xmlGetAttr, "MEAN_INTENSITY")
  cat("MEAN_INTENSITY exctracted...\n")
  iTOTAL_INTENSITY <- sapply(subdoc, xmlGetAttr, "TOTAL_INTENSITY")
  cat("TOTAL_INTENSITY exctracted...\n")
  iSNR <- sapply(subdoc, xmlGetAttr, "SNR")
  cat("SNR exctracted...\n")
  iPOSITION_X <- sapply(subdoc, xmlGetAttr, "POSITION_X")
  cat("POSITION_X exctracted...\n")
  iPOSITION_Y <- sapply(subdoc, xmlGetAttr, "POSITION_Y")  
  cat("POSITION_Y exctracted...\n")
  iSTANDARD_DEVIATION <- sapply(subdoc, xmlGetAttr, "STANDARD_DEVIATION")
  cat("STANDARD_DEVIATION exctracted...\n")
  
  dtf <- data.frame(ID = iName,
                    Frame = round(as.numeric(as.character(iFRAME)),3),
                    t = round(as.numeric(as.character(iPOSITION_T)),3),
                    MEDIAN_INTENSITY = round(as.numeric(as.character(iMEDIAN_INTENSITY)),3),
                    MEAN_INTENSITY = round(as.numeric(as.character(iMEAN_INTENSITY)),3),
                    TOTAL_INTENSITY = round(as.numeric(as.character(iTOTAL_INTENSITY)),3), 
                    SNR = round(as.numeric(as.character(iSNR)),3), 
                    x = round(as.numeric(as.character(iPOSITION_X)),3),
                    y = round(as.numeric(as.character(iPOSITION_Y)),3),
                    STANDARD_DEVIATION = round(as.numeric(as.character(iSTANDARD_DEVIATION)),3))

  cat("\nConcatenating data...\n") 
  IDtrace <- data.frame(ID=NA, trace=NA)  

  for (i in seq(along=track)){ 
    subDoc = xmlDoc(track[[i]])
    IDvec <- unique(c(unlist(xpathApply(subDoc, "//Edge", xmlGetAttr, "SPOT_SOURCE_ID")),
                      unlist(xpathApply(subDoc, "//Edge", xmlGetAttr, "SPOT_TARGET_ID"))))
    traceVec <- rep(sapply(track[i], function(el){xmlGetAttr(el, "TRACK_ID")}), length(IDvec))
    IDtrace <- rbind(IDtrace, data.frame(ID=paste("ID",IDvec,sep=''), trace=traceVec))
  }
  }
  #dtf[,1] <- as.character(paste("ID",dtf[,1],sep=''))
  daten <- merge(IDtrace,dtf, by="ID")

  daten <- daten[order(daten$trace, daten$t),]
  
  return(daten)
}

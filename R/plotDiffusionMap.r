# plotDiffusionMap.r
# written by JuG
# February 27 2020


#' Plot Diffusion Map
#' @author JuG
#' @description 
#' @param data  data frame (muste contain trace, x, y, and jump)
#' @param traceNb (optional) number id of the traces to plot
#' @param nbPix number of pixel (or calibrated distance) to add on each x-and y-range tail
#' 
#' @details 
#' Plot Diffusion Map or more exactly jump distance map as in Fig 2c of Gasser et al. 2020 doi:10.1017/S0033583519000155
#' @examples 
#' xmlPath <- "/Users/jgodet/Seafile/MaBibliotheque/Code/TrackMate/nmeth.2808-sv1.xml"
#' data <- readTrackMateXML(XMLpath = xmlPath)
#' data$jump<-jump(data, spaceRes=1)
#' plotDiffusionMap(data = data,traceNb = c(12),nbPix = 5 )
#' plotDiffusionMap(data = data,nbPix = 5 )
#' @return plot
#' @export


plotDiffusionMap <- function(data, traceNb,nbPix=20, jumpMax, nbCol = 10){
  if(missing(traceNb)){
    traceNb <- as.numeric(unique(data$trace))
  }
  if(missing(jumpMax)){
    jumpMax <- max(data$jump, na.rm = T)
  }
  if(!require(colorRamps)){install.packages('colorRamps')}
  require(colorRamps)
  xlimits = range(data[data$trace %in% traceNb,"x"])+c(-1,1)*nbPix
  ylimits = range(data[data$trace %in% traceNb,"y"])+c(-1,1)*nbPix
  xmed <- data[data$trace %in% traceNb,"x"]
  ymed  <- data[data$trace %in% traceNb,"y"]
    
  plot(NA,xlim=xlimits,ylim=ylimits,asp=1)
  jumpCut <- as.numeric(cut(data$jump,breaks = seq(0,jumpMax, length.out = nbCol)))
  coul <- matlab.like(nbCol)
  data$colo <- coul[jumpCut]
  
  for (j in seq(along=traceNb)){
    dataPlot= data[data$trace==traceNb[j],]
    for (i in 1:dim(dataPlot)[1]){
      if(!is.na(dataPlot[i,"jump"])){
        segments(dataPlot[i-1,"x"],dataPlot[i-1,"y"],dataPlot[i,"x"],dataPlot[i,"y"], col=dataPlot$colo[i])
      }
    }
  }
  return("Plot done")
}

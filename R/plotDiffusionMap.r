# plotDiffusionMap.r
# written by JuG
# January 10 2018


#' Do something
#' @author JuG
#' @description
#' @param azc2 dataframe with x, y, track, jd and cluster variable
#' @param nbPix boxsize (expressed in x or y units)
#' @param jdRange jump distance limit (ignore all jumps larger than jdlim)
#' @param colRange rage of jump distance
#' @details
#' @examples
#'
#'dataTest <- expData[expData$cluster %in% "PvdA9_8",]
#'plotDiffusionMap(dataTest, nbPix=.5 )
#'gcont <- getContour(dataTest[, c("x","y")],drawContour = FALSE)
#'drawRod(gcont,lty=3)
#' @return
#' @export


plotDiffusionMap <- function(azc2,nbPix=2,jdRange = c(0,12), colRange = NULL, ...){

  xlimits = range(azc2[,"x"]) + c(-nbPix,nbPix)
  ylimits = range(azc2[,"y"]) + c(-nbPix,nbPix)
  xmed <- azc2[,"x"]
  ymed  <-azc2[,"y"]

  plot(NA,xlim=xlimits,ylim=ylimits,asp=1,...)
  if(is.null(colRange)){
    colRange <- c(min(azc2$jd,na.rm=T), max(azc2$jd,na.rm=T))
  }
  jumpCut <- as.numeric(cut(azc2$jd,seq(colRange[1],colRange[2],length=10)))
  coul <- matlab.like(10)
  azc2$colo <- coul[jumpCut]

  data= azc2[,]
    for (i in 1:dim(data)[1]){
      if(!is.na(data[i,"jd"]) & data[i,"jd"] < jdRange[2] & data[i,"jd"] >= jdRange[1] ){
        if(data[i-1,"track"] == data[i,"track"]){
          segments(data[i-1,"x"],data[i-1,"y"],data[i,"x"],data[i,"y"], col=data$colo[i])
        }
      }
    }
}

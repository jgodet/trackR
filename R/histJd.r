# histJd.r
# written by JuG
# February 19 2020


#' Plot JD histogram
#' @author JuG
#' @description Plot JD histogram (inherit from hist)
#' @param jd jump distribution vector
#' @param breaks histogram breaks
#' @param xlim xlim
#' @param pixSize pixel size in µm (if picels are not calibrated in trackMate)
#' @param xlab xlab
#' @param panel.first panel first options (use for exmaple grid())
#' @details 
#' @examples 
#' histJd(jd, main="", panel.first=grid())
#' box()
#' @return graph
#' @export


histJd<- function(jd, breaks, xlim,freq, pixSize, xlab,panel.first, ...){
  if(missing(breaks)){breaks = 150}
  if(missing(freq)){freq = TRUE}
  if(missing(xlim)){xlim=c(0,0.5)}
  if(missing(xlab)){xlab="Jump Distance, µm"}
  if(missing(pixSize)){pixSize=0.110}
  if(missing(panel.first)){panel.first = NULL}
  return(hist(jd*pixSize,breaks=breaks,xlim=xlim, freq =freq,
              xlab = xlab, 
              panel.first = panel.first, ...))
  
}

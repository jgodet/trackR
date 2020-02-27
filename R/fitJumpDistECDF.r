# fitJumpDistECDF.r
# written by JuG
# February 27 2020


#' Do something
#' @author JuG
#' @description 
#' @param data dataframe with jump 
#' @param nbPop number of populations of diffusing species (1 or 2)
#' @param initHist initialisation parameters for the nls fit for exmaple list(D2 = 200,  D1=0.1) or list(D2 = .01,  D1=.1, D3=10, D4=100)
#' @param timeRes time resolution per unit of jump
#' @details 
#' @examples 
#' xmlPath <- "/Users/jgodet/Seafile/MaBibliotheque/Code/TrackMate/nmeth.2808-sv1.xml"
#' xmlPath <- "/Users/jgodet/Seafile/MaBibliotheque/Code/TrackMate/nmeth.2808-sv3.xml"
#' data <- readTrackMateXML(XMLpath = xmlPath)
#' data$jump<-jump(data, spaceRes=1)
#' #' fitJumpDistECDF(data=data, nbPop = 1)
#' fitJumpDistECDF(data=data, nbPop = 1, cex=.85, colLine = 'green')
#' fitJumpDistECDF(data=data, nbPop = 2,  initECDF = list(D1=0.001, D2=.4, D3=.1), cex=.85, colLine = 'green')

#' @return 
#' @export


fitJumpDistECDF<- function(data, nbPop=1, initECDF = list(D1=0.05),timeRes = 1,xlab, ylab, main,cex,colLine = 'red',...){
  
  if(nbPop>2){
    cat('Un peu de patience...\n')
    return()}
  #data pour fit
  hd<-hist(data$jump,breaks=breaks,  plot =FALSE)
  hdata<-data.frame(counts=hd$counts,mid=hd$mids,
                    countsCum = cumsum(hd$counts)/sum(hd$counts))

  if(missing(xlab)){xlab  <- 'Displacement, µm'}
  if(missing(ylab)){ylab  <- 'Frequency'}
  if(missing(main)){main  <- ''}
  if(missing(cex)){cex  <- .75}
  
  plot(hd$mid, hdata$countsCum,ylim=c(0,1.1),
       xlab=xlab,ylab=ylab,cex=cex,...)
    
  if( nbPop == 1){
    #1 population
    #----------------
    fitc <- nls(hdata$countsCum ~ 1 -  exp(-hdata$mid^2/(4*D1*timeRes)),
                data = hdata,
                start = initECDF)
    print(fitc);print(confint(fitc))
    #nrc <- nlsResiduals(fitc)
    #plot(nrc, which = 0)
    
    #graph  pour cumulative

    x<-seq(0,max(hdata$mid),length=10*length(hdata$mid))
    y <-1 -  exp(-x^2/(4*coef(fitc)[1]*timeRes))
    lines(x,y,col=colLine,lwd=2)
  }
  #2 population
  #----------------
  if(nbPop == 2){
    fitc2<- nls(hdata$countsCum ~ (1 - D2 * exp(-hdata$mid^2/(4*D1*timeRes))
                                   - (1-D2) * exp(-hdata$mid^2/(4*D3*timeRes))),
                data = hdata,
                start = list(D1=0.001, D2=.4, D3=.1))
    print(fitc2)#;print(confint(fitc2))
    #nrc2 <- nlsResiduals(fitc2)
    #plot(nrc2, which = 0)
    #graph  pour cumulative
    plot(hd$mid, hdata$countsCum,ylim=c(0,1.1),
         xlab='Displacement, µm',ylab='Frequency')
    x<-seq(0,max(hdata$mid),length=10*length(hdata$mid))
    y <-(1 - coef(fitc2)[2] * exp(-x^2/(4*coef(fitc2)[1]*timeRes))
         - (1-coef(fitc2)[2]) * exp(-x^2/(4*coef(fitc2)[3]*timeRes)))
    lines(x,y,col='red',lwd=2)
    y1 <- coef(fitc2)[2]-(coef(fitc2)[2] * exp(-x^2/(4*coef(fitc2)[1]*timeRes)))
    lines(x,y1,col='blue',lwd=2)
    y2 <-(1-coef(fitc2)[2])-(1-coef(fitc2)[2])*exp(-x^2/(4*coef(fitc2)[3]*timeRes))
    lines(x,y2,col='green',lwd=2)
  }
  
  
  return()
}

# fitJumpDistHist.r
# written by JuG
# February 27 2020


#' Fit jump distribution histogram
#' @author JuG
#' @description 
#' @param data dataframe with jump 
#' @param nbPop number of populations of diffusing species (1 or 2)
#' @param initHist initialisation parameters for the nls fit for exmaple list(D2 = 200,  D1=0.1) or list(D2 = .01,  D1=.1, D3=10, D4=100)
#' @param timeRes time resolution per unit of jump
#' @param breaks binning of the histogram (number of breaks)
#' @details 
#' @examples 
#' xmlPath <- "/Users/jgodet/Seafile/MaBibliotheque/Code/TrackMate/nmeth.2808-sv1.xml"
#' data <- readTrackMateXML(XMLpath = xmlPath)
#' data$jump<-jump(data, spaceRes=1)
#' fitJumpDistHist(data=data, nbPop = 1, initHist = list(D2 = 200,  D1=0.1))
#' fitJumpDistHist(data=data, xlim=c(0,10), col=rgb(.2,.2,.2,.2), main="test")
#' # Results are affected by the histogram binning
#' fitJumpDistHist(data=data, breaks = 30)
#' fitJumpDistHist(data=data, breaks = 200)
#' fitJumpDistHist(data=data, nbPop=3)
#' fitJumpDistHist(data=data, nbPop=2, initHist = list(D2 = .01,  D1=.1, D3=10, D4=100))
#' @return 
#' @export


fitJumpDistHist<- function(data, nbPop = 1, initHist = list(D2 = 200,  D1=0.1), timeRes=1, breaks=100, xlim, ylim,xlab,main, ...){
  # if(!require("nlstools")){install.packages("nlstools")}
  # require(nlstools)
  if(nbPop>2){
    cat('Un peu de patience...\n')
    return()}
  #data pour fit
  hd<-hist(data$jump,breaks=breaks,  plot =FALSE)
  hdata<-data.frame(counts=hd$counts,mid=hd$mids,
                    countsCum = cumsum(hd$counts)/sum(hd$counts))
  
  if(missing(ylim)){xlim <- c(0,1.5*max(hdata$counts))}
  if(missing(xlim)){ylim <- c(0,1.5*max(hdata$mid))}
  if(missing(xlab)){xlab  <- 'Displacement, µm'}
  if(missing(main)){main  <- ''}
  
  hist(data$jump,breaks=breaks, xlim=xlim, ylim=ylim,
       xlab = xlab,main=main,panel.first=grid(),...)
  
  #par(mfrow=c(1,1))
  if(nbPop == 1){
    #fit histogram
    fit <- nls(hdata$counts ~ D2*hdata$mid/(2*D1*timeRes)*
                 exp(-hdata$mid^2/(4*D1*timeRes)),
               data = hdata,
               start = list(D2 = 200,  D1=0.1))
    print(fit);print(suppressMessages(confint(fit)))
    #nr <- nlsResiduals(fit)
    #plot(nr, which = 0)
    
    #graph
    x<-seq(0,max(hdata$mid),length=10*length(hdata$mid))
    y <-(coef(fit )[[1]])*x/(2*(coef(fit )[[2]])*timeRes)*
      exp(-x^2/(4*(coef(fit)[[2]])*timeRes))
    lines(x,y,col='red',lwd=2)
    box()
  }
  
  #2 population
  #----------------
  if(nbPop == 2){
    fit2<- nls(hdata$counts ~ D3*hdata$mid/(2*D1*timeRes)*
                 exp(-hdata$mid^2/(4*D1*timeRes))
               +  D4*hdata$mid/(2*D2*timeRes)*exp(-hdata$mid^2/(4*D2*timeRes)),
               data = hdata,
               start = initHist)
    print(fit2)
    print(suppressMessages(confint(fit2)))#print(confint(fit2)[2])
    #nr2 <- nlsResiduals(fit2)
    #plot(nr2, which = 0)
    #graph
    x<-seq(0,max(hdata$mid),length=10*length(hdata$mid))
    y <-(coef(fit2 )[[4]])*x/(2*(coef(fit2 )[[1]])*timeRes)*
      exp(-x^2/(4*(coef(fit2)[[1]])*timeRes)) +
      (coef(fit2 )[[3]])*x/(2*(coef(fit2 )[[2]])*timeRes)*
      exp(-x^2/(4*(coef(fit2)[[2]])*timeRes))
    lines(x,y,col='red',lwd=2)
    y1 <-(coef(fit2 )[[4]])*x/(2*(coef(fit2 )[[1]])*timeRes)*
      exp(-x^2/(4*(coef(fit2)[[1]])*timeRes))
    lines(x,y1,col='blue',lwd=2)
    y2 <-(coef(fit2 )[[3]])*x/(2*(coef(fit2 )[[2]])*timeRes)*
      exp(-x^2/(4*(coef(fit2)[[2]])*timeRes))
    lines(x,y2,col='green',lwd=2)
    # text(x=.3, y = max(hdata$counts),adj = 0, paste("D1 = ",
    #                                                 round(coef(fit2 )[[2]],4)," (",
    #                                                 round(100*coef(fit2 )[[3]]/(coef(fit2 )[[3]]+ coef(fit2 )[[4]]),1) ,
    #                                                 "%)\nD2 = ",round(coef(fit2 )[[1]],4)),
    #      cex=.9)
    box()
  }
  # if( nbPop == 1){
  #   nr <- nlsResiduals(fitc)
  #   x11();plot(nr, which = 0)
  # }
  # if( nbPop == 2){
  #   nr <- nlsResiduals(fitc2)
  #   x11();plot(nr, which = 0)
  # }
}

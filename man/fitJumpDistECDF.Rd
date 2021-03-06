% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/fitJumpDistECDF.r
\name{fitJumpDistECDF}
\alias{fitJumpDistECDF}
\title{Do something}
\usage{
fitJumpDistECDF(
  data,
  nbPop = 1,
  initECDF = list(D1 = 0.05),
  timeRes = 1,
  xlab,
  ylab,
  main,
  cex,
  colLine = "red",
  breaks = 150,
  ...
)
}
\arguments{
\item{data}{dataframe with jump}

\item{nbPop}{number of populations of diffusing species (1 or 2)}

\item{timeRes}{time resolution per unit of jump}

\item{initHist}{initialisation parameters for the nls fit for exmaple list(D2 = 200,  D1=0.1) or list(D2 = .01,  D1=.1, D3=10, D4=100)}
}
\value{

}
\description{
Do something
}
\examples{
xmlPath <- "/Users/jgodet/Seafile/MaBibliotheque/Code/TrackMate/nmeth.2808-sv1.xml"
xmlPath <- "/Users/jgodet/Seafile/MaBibliotheque/Code/TrackMate/nmeth.2808-sv3.xml"
data <- readTrackMateXML(XMLpath = xmlPath)
data$jump<-jump(data, spaceRes=1)
fitJumpDistECDF(data=data, nbPop = 1)
fitJumpDistECDF(data=data, nbPop = 1, cex=.85, colLine = 'green')
fitJumpDistECDF(data=data, nbPop = 2,  initECDF = list(D1=0.001, D2=.4, D3=.1), cex=.85, colLine = 'green')
}
\author{
JuG
}

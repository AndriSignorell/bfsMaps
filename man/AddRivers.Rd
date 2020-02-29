\name{AddRivers}
\alias{AddRivers}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Add Rivers to Switzerland Map
}
\description{
Add rivers to an already existing Switzerland map. The rivers are defined in 5 categories 1:5, wheras category 1 contains the largest rivers, category 5 the smallest ones.
}
\usage{
AddRivers(categ = 1:5, col = "lightskyblue3", ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{categ}{category of the rivers, out of \code{1:5}. Category \code{1} contains the large rivers, \code{5} the small ones.
}
  \item{col}{color of the rivers, defaults to \code{"lightskyblue1"}
}
  \item{\dots}{the dots are passed to the \code{\link{plot}} command

}
}
\details{ Rivers are defined in the files:
\itemize{
\item 00_TOPO/K4_flusyyyymmdd/c_shp/k4flusyyyymmdd11_ch2007.shp
\item 00_TOPO/K4_flusyyyymmdd/c_shp/k4flusyyyymmdd22_ch2007.shp
\item 00_TOPO/K4_flusyyyymmdd/c_shp/k4flusyyyymmdd33_ch2007.shp
\item 00_TOPO/K4_flusyyyymmdd/c_shp/k4flusyyyymmdd44_ch2007.shp
\item 00_TOPO/K4_flusyyyymmdd/c_shp/k4flusyyyymmdd55_ch2007.shp
}}

\author{ Andri Signorell <andri.signorell@helsana.ch>
}

\seealso{
\code{\link{AddLakes}}
}
\examples{
if(interactive()){
  PlotCH()
  AddLakes(categ=1)      # adds the lakes of category 1 to the map
  AddRivers(categ=1:2)   # adds the rivers of category 1 and 2 to the map
}}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{aplot }

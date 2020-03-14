\name{AddLakes}
\alias{AddLakes}
\title{ Add Lakes to Switzerland Map
}
\description{Add lakes to an already existing Switzerland map.
The lakes are defined in 2 categories 1 and 2, whereas category 1 contains the bigger ones, category 2 the smaller ones.
}
\usage{
AddLakes(categ = 1:2, col = "lightskyblue1", border = "lightskyblue3",
         lwd = 1, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{categ}{
category of the lakes, out of \code{c(1, 2)}. \code{1} are the bigger lakes, \code{2} the smaller ones.
}
  \item{col}{color of the lakes, defaults to \code{"lightskyblue1"}
}
  \item{border}{bordercolor of the lakes, defaults to \code{"lightskyblue3"}
}
  \item{lwd}{linewidth of border
}
  \item{\dots}{the dots are passed to the plot command
}
}
\details{
Lakes are defined in the original files:
\itemize{
\item 00_TOPO/K4_seenyyyymmdd/c_shp/k4seenyyyymmdd11_ch2007Poly.shp
\item 00_TOPO/K4_seenyyyymmdd/c_shp/k4seenyyyymmdd22_ch2007Poly.shp
}
}
\author{Andri Signorell <andri@signorell.net}

\seealso{\code{\link{AddRivers}}
}

\examples{
if(interactive()){
  PlotKant()
  AddLakes(categ=1)   # adds the lakes of category 1 to the map
}}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{aplot}

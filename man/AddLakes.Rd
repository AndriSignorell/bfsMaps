\name{AddLakes}
\alias{AddLakes}
\alias{AddWaters}
\alias{AddRivers}
\title{ Add Waters to Switzerland Map
}
\description{Add lakes and rivers to an already existing Switzerland map.
The lakes are defined in 2 categories 1 and 2, whereas category 1 contains the bigger ones, category 2 the smaller ones.\cr
The rivers are defined in 5 categories 1:5, wheras category 1 contains the largest rivers, category 5 the smallest ones.

}
\usage{
AddLakes(categ = 1:2, col = "lightskyblue1", border = "lightskyblue3",
         lwd = 1, ...)

AddRivers(categ = 1:5, col = "lightskyblue3", ...)

AddWaters(lakes = 1, rivers = 1:5, col = NULL,
          border = "lightskyblue3", lwd = 1, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{categ}{
category of the lakes (\code{1, 2}) and rivers (\code{1:5}) . \code{1} are the bigest waters, \code{2}, resp. \code{5} the smallest ones.
}
\item{lakes}{the category for the lakes}
\item{rivers}{the category for the rivers}
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
\code{AddWaters()} is a wrapper with sensible defaults. If the color is not provided it will be set to a less intense tint of the border.

Lakes are defined in the original files:
\itemize{
\item 00_TOPO/K4_seenyyyymmdd/c_shp/k4seenyyyymmdd11_ch2007Poly.shp
\item 00_TOPO/K4_seenyyyymmdd/c_shp/k4seenyyyymmdd22_ch2007Poly.shp
}

Rivers are defined in the files:
\itemize{
\item 00_TOPO/K4_flusyyyymmdd/c_shp/k4flusyyyymmdd11_ch2007.shp
\item 00_TOPO/K4_flusyyyymmdd/c_shp/k4flusyyyymmdd22_ch2007.shp
\item 00_TOPO/K4_flusyyyymmdd/c_shp/k4flusyyyymmdd33_ch2007.shp
\item 00_TOPO/K4_flusyyyymmdd/c_shp/k4flusyyyymmdd44_ch2007.shp
\item 00_TOPO/K4_flusyyyymmdd/c_shp/k4flusyyyymmdd55_ch2007.shp
}
}
\value{None}

\author{Andri Signorell <andri@signorell.net}

\examples{\donttest{
PlotKant()
AddLakes(categ=1)      # adds the lakes of category 1 to the map
AddRivers(categ=1:2)   # adds the rivers of category 1 and 2 to the map

}}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{aplot}


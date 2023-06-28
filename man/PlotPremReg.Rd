\name{PlotPremReg}
\alias{PlotPremReg}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Plot Premium Regions CH
%%  ~~function to do ... ~~
}
\description{Plot premium regions in Switzerland.
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
PlotPremReg(id = NULL, col = NA, pbg = "white", main = "",
            border = "grey", lwd = 1,
            labels = NULL, tmtxt = TRUE, add = FALSE, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{id}{vector of region ids. The premium regions can be addressed via their their abbreviation:
  \code{"AG0"}, \code{"AI0"}, \code{"AR0"}, \code{"BE1"}, ...
}
  \item{col}{vector of colors, defining the colors of the region area.
}
  \item{pbg}{color for the plot background.
%%     ~~Describe \code{add.lakes} here~~
}
  \item{main}{main title in the plot.
%%     ~~Describe \code{main} here~~
}
  \item{border}{vector of colors for region borders. Default is \code{"grey30"}.
%%     ~~Describe \code{\dots} here~~
}
  \item{lwd}{linewidth for region borders.
%%     ~~Describe \code{\dots} here~~
}
  \item{labels}{optional labels to be placed in the map, by default the centroids of the map is used for that.}

  \item{tmtxt}{logical, should the copyright text be displayed. Default is \code{TRUE}.
%%     ~~Describe \code{\dots} here~~
}
\item{add}{default \code{FALSE}; if \code{TRUE}, add to existing plot.}

  \item{\dots}{the dots are passed to the plot command.
%%     ~~Describe \code{\dots} here~~
}
}

\value{A list containing x and y components which are the centroids of the plotted spatial units.}

\author{
Andri Signorell <andri@signorell.net>
%%  ~~who you are~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{\code{\link{PlotCH}}, \code{\link{d.bfsrg}}
}
\examples{
# Note:
#   The examples can not be run without having the map data installed before!
try( {

preg_x <- sort(unique(d.bfsrg$preg_x))

PlotPremReg(id=preg_x, border="grey60",
            col=c("white","olivedrab4", "olivedrab3", "olivedrab2")[
                StrVal(preg_x,as.numeric=T)+1],
            main="Prämienregionen CH")

legend(x="topleft", fill=c("white","olivedrab4","olivedrab3","olivedrab2"),
       cex=0.8,
       legend=c("Region 0","Region 1","Region 2","Region 3") )

AddLakes()

# plot all premium regions
# find all regions
d.bfsrg$preg_x <- paste0(d.bfsrg$kt_x, d.bfsrg$preg_c)
preg <- unique(d.bfsrg$preg_x)

cols <- c("white","darkolivegreen3", "darkolivegreen2", "darkolivegreen1")

PlotPremReg(preg, cols[ZeroIfNA(StrVal(preg, as.numeric = T))+1], labels =NA)
PlotKant(add=TRUE, border="grey55")
AddLakes()

# plot some selected premium regions
PlotPremReg(c("ZH1", "GR2"), c("blue", "yellow"), labels=TRUE)
PlotKant(add=TRUE, border="grey55")
AddLakes()
})

}

\name{PlotMapDot}
\encoding{UTF-8}

\alias{PlotMapDot}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Plot a Map and a Dotplot
%%  ~~function to do ... ~~
}
\description{Prepares the layout to plot a map and a dotplot side by side.
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
PlotMapDot(mar = c(5.1,4.1,0,1), oma = c(0,0,5,0), widths = c(2,0.8))
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{mar}{defines the plot margins.
%%     ~~Describe \code{main} here~~
}
  \item{oma}{defines the outer margins. We use that for displaying a title.
%%     ~~Describe \code{main} here~~
}
  \item{widths}{
  a vector of values for the widths of two columns, the first for the map, the second for dotplot. Relative widths are specified with numeric values. Absolute widths (in centimetres) are specified with the lcm() function. Default is \code{c(2, 0.8)}.


  }

}
\author{ Andri Signorell <andri@signorell.net>
}

\seealso{\code{\link{layout}}}

\examples{
if(interactive()){

require(DescTools)

# get some data
yes_p <- c(ZH=0.465, BE=0.417, LU=0.376, UR=0.308, SZ=0.276,
           OW=0.273, NW=0.277, GL=0.324, ZG=0.344, FR=0.469, SO=0.352,
           BS=0.602, BL=0.414, SH=0.457, AR=0.325, AI=0.24, SG=0.365,
           GR=0.325, AG=0.347, TG=0.321, TI=0.446, VD=0.532, VS=0.329,
           NE=0.562, GE=0.601, JU=0.532)


PlotMapDot()
cols <- colorRampPalette( colors=c("red","white","green"), space ="rgb")(10)
PlotKant(id=names(yes_p),
         col=FindColor(yes_p, cols=cols, min.x=0, max.x=1 ), main="",
         labels=TRUE)

ColorLegend(x="left", width=10000,
            labels=paste(seq(0, 100, 10), "\%", sep=""),
            cols=cols, cex=0.8, adj=c(1,0.5), frame="grey")

x <- Sort(yes_p, decreasing=TRUE)

opt <- DescToolsOptions(stamp=NULL)
PlotDot(x, labels=gettextf("\%s (\%s)", names(x), Format(x, fmt="\%", digits=1)),
        cex=0.9, xlim=c(0,1))
abline(v=0.5, col="grey")

title(main="Volksinitiative 'Mehr bezahlbare Wohnungen'
            Abstimmung vom 09.02.2020", outer=TRUE)
DescToolsOptions(opt)
}
}
\keyword{ hplot }

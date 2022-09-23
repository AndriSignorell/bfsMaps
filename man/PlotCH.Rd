\name{PlotCH}
\alias{PlotCH}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Plot a Map of Switzerland
%%  ~~function to do ... ~~
}
\description{Simple map plot of Switzerland following the borders valid since 1848.
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
PlotCH(col = "grey90", pbg = "white", main = "", col.vf = NA,
       border = "grey", border.vf = "grey", lwd = 1,
       tmtxt = TRUE, add = FALSE, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{

  \item{col}{vector of colors, defining the colors of the cantons.\cr
    Note: \code{NA}s are recoded as white.
}
  \item{pbg}{color of the plot background.
%%     ~~Describe \code{add.lakes} here~~
}
  \item{main}{main title in the plot.
%%     ~~Describe \code{main} here~~
}
  \item{col.vf}{defines a color for the vegetational area ("Vegetationsflaeche").  \cr
    If \code{NA} only the total area is used.
%%     ~~Describe \code{cols} here~~
}
  \item{border}{color of map border. Default is \code{"grey"}.
%%     ~~Describe \code{\dots} here~~
}
  \item{border.vf}{color of borders for the vegetational area. Default is \code{"grey"}.
%%     ~~Describe \code{\dots} here~~
}
  \item{lwd}{linewidth for the border. Default is \code{par("lwd")}.
%%     ~~Describe \code{\dots} here~~
}
  \item{tmtxt}{logical, should the copyright text be displayed. Default is \code{TRUE}.
%%     ~~Describe \code{\dots} here~~
}
\item{add}{default \code{FALSE}; if \code{TRUE}, add to existing plot.}

  \item{\dots}{the dots are passed to the plot command.
%%     ~~Describe \code{\dots} here~~
}
}
\details{
The list of all cantons and their ids is given by d.bfsrg:\cr
  \kbd{  cantons <- unique(d.bfsrg[,c("kt_c", "kt_x", "kt_bez_x")])}

}
\value{A list containing x and y component of the centroid of the plotted spatial unit.}

\author{
Andri Signorell <andri@signorell.net>
}


\seealso{\code{\link{PlotGreg}}, \code{\link{PlotKant}}, \code{\link{PlotBezk}}, \code{\link{PlotPolg}}, \code{\link{d.bfsrg}}
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{\donttest{\dontrun{
PlotCH(col="lightgrey")
AddLakes()

# use the result to add a semitransparent label
xy <- PlotCH(col.vf = "grey90", col="grey75", border="grey50", border.vf = NA)
AddLakes()
AddRivers()
PlotCH(add=TRUE, col=NA)
BoxedText(x=xy$x, y=xy$y, labels = "Visit\n Switzerland", cex=3, txt.col = "grey40",
          col=SetAlpha("white", 0.6), border=NA, ypad=0.5)


# wawing flag ...
PlotCH(col="red", main="Switzerland")
sw <- 15000;
xc <- 2671975;
yc <- 1200600;

ccol <- rgb(1,1,1,0.85)
rect(xleft=xc-sw, ytop=yc-sw, xright=xc+sw, ybottom=yc+sw, col=ccol, border=NA)
rect(xleft=(xc-2*sw)-sw, ytop=yc-sw, xright=(xc-2*sw)+sw, ybottom=yc+sw, col=ccol, border=NA)
rect(xleft=(xc+2*sw)-sw, ytop=yc-sw, xright=(xc+2*sw)+sw, ybottom=yc+sw, col=ccol, border=NA)
rect(xleft=xc-sw, ytop=(yc-2*sw)-sw, xright=xc+sw, ybottom=(yc-2*sw)+sw, col=ccol, border=NA)
rect(xleft=xc-sw, ytop=(yc+2*sw)-sw, xright=xc+sw, ybottom=(yc+2*sw)+sw, col=ccol, border=NA)

# using panel.first ensures that the borders are not hidden by waters
PlotCH(col=NA, lwd=2, panel.first=AddLakes())
}}}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ hplot }

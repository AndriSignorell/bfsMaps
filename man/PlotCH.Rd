\name{PlotCH}
\alias{PlotCH}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Plot a map of Switzerland
%%  ~~function to do ... ~~
}
\description{Simple map plot of Switzerland with cantons.
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
PlotCH(col = "grey90", pbg = "white", main = "", col.vf = NA,
       waters = TRUE, border = "grey70", lwd = 1,
       tmtxt = TRUE, add = FALSE, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{

  \item{col}{vector of colors, defining the colors of the cantons.\cr
    Note: NAs are recoded as white.
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
\item{waters}{logical. Defining if lakes and rivers should be displayed (default is \code{TRUE}).}
  \item{border}{color of map borders. Default is "grey70".
%%     ~~Describe \code{\dots} here~~
}
  \item{lwd}{linewidth of cantons.
%%     ~~Describe \code{\dots} here~~
}
  \item{tmtxt}{logical, should the copyright text be displayed. Default is TRUE.
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
\author{
Andri Signorell <andri.signorell@helsana.ch>
}


\seealso{\code{\link{PlotGreg}}, \code{\link{PlotKant}}, \code{\link{PlotBezk}}, \code{\link{PlotPolg}}, \code{\link{d.bfsrg}}
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
if(interactive()){
  PlotCH(col="lightgrey")
  AddLakes()

  PlotCH(col="red")
  sw <- 15000;
  xc <- 2671975;
  yc <- 1200600;

  # overlay a cross
  ccol <- rgb(1,1,1,0.85)
  rect(xleft=xc-sw, ytop=yc-sw, xright=xc+sw, ybottom=yc+sw, col=ccol, border=NA)
  rect(xleft=(xc-2*sw)-sw, ytop=yc-sw, xright=(xc-2*sw)+sw, ybottom=yc+sw, col=ccol, border=NA)
  rect(xleft=(xc+2*sw)-sw, ytop=yc-sw, xright=(xc+2*sw)+sw, ybottom=yc+sw, col=ccol, border=NA)
  rect(xleft=xc-sw, ytop=(yc-2*sw)-sw, xright=xc+sw, ybottom=(yc-2*sw)+sw, col=ccol, border=NA)
  rect(xleft=xc-sw, ytop=(yc+2*sw)-sw, xright=xc+sw, ybottom=(yc+2*sw)+sw, col=ccol, border=NA)

  title(main="Switzerland")
}
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ hplot }

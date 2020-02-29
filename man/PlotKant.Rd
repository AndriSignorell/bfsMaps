\name{PlotKant}
\alias{PlotKant}
\alias{PlotGreg}
\alias{PlotPolg}
\alias{PlotBezk}
\alias{PlotMSRe}

\encoding{latin1}

%- Also NEED an '\alias' for EACH other topic documented here.
\title{ Plot CH-Map with Cantons }
\description{
The function plots a CH-map with cantons. It uses the \code{col} vector to colorize the cantons.\cr
With \code{vf} set to \code{TRUE} the vegetational area will be plotted.
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
PlotKant(id = NULL, col = NA, pbg = "white", main = "",
         border = "grey", lwd = 1, col.vf = NA, labels = NULL,
         tmtxt = TRUE, add = FALSE, map_x = "kant.map", ...)

PlotMSRe(id = NULL, col = NA, pbg = "white", main = "",
         border = "grey", lwd = 1, col.vf = NA, labels = NULL,
         tmtxt = TRUE, add = FALSE, map_x = "msre.map", ...)


PlotBezk(id = NULL, col = NA, pbg = "white", main = "",
         border = "grey", lwd = 1, col.vf = NA, labels = NULL,
         tmtxt = TRUE, add = FALSE, map_x = "bezk.map", ...)


PlotPolg(id = NULL, col = NA, pbg = "white", main = "",
         border = "grey", lwd = 1, col.vf = NA, labels = NULL,
         tmtxt = TRUE, add = FALSE, map_x = "polg.map", ...)

}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{id}{cantons to plot, can either be defined via a 2-character-abbreviation:\cr
    \kbd{"AG","AI","AR","BE","BL","BS","FR","GE","GL","GR","JU","LU","NE",}\cr
    \kbd{"NW","OW","SG","SH","SO","SZ","TG","TI","UR","VD","VS","ZG","ZH"}\cr
  or as index from 1 .. \code{ZH} to 26 .. \code{JU}
}
  \item{col}{vector of colors, defining the colors of the cantons.\cr
    Note: \code{NAs} are recoded as \code{"white"}.
}
  \item{pbg}{color of the plot background.
%%     ~~Describe \code{add.lakes} here~~
}
  \item{main}{main title in the plot.
%%     ~~Describe \code{main} here~~
}
  \item{border}{color of cantons borders. Default is \code{grey30}.
%%     ~~Describe \code{\dots} here~~
}
  \item{lwd}{linewidth of cantons.
%%     ~~Describe \code{\dots} here~~
}
  \item{col.vf}{defines if only the vegetational area ("Vegetationsflaeche") is to be plotted. \cr
    By default \code{vf} is FALSE and so will the total area be used.
%%     ~~Describe \code{cols} here~~
}
  \item{labels}{optional labels to be placed in the map, by default the centroids of the map is used for that.}

  \item{tmtxt}{logical, should the copyright text be displayed. Default is \code{TRUE}.
%%     ~~Describe \code{\dots} here~~
}
\item{add}{default \code{FALSE}; if \code{TRUE}, add to existing plot.}

\item{map_x}{the name ot the path of a map to be used. This is convenient, if we want to plot a newer map with the logic of this function.}

  \item{\dots}{the dots are passed to the plot command.
%%     ~~Describe \code{\dots} here~~
}
}
\details{
The list of all cantons and their ids is given by a defined variable \code{kt}. \cr
More details can be extracted form \code{d.bfsrg}:\cr
  \kbd{  cantons <- unique(d.bfsrg[,c("kt_c", "kt_x", "kt_bez_x")])}
%%  ~~ If necessary, more details than the description above ~~

\code{PlotGreg} uses a map for Swiss regions (Grossregionen), as defined in greg.map@data
\itemize{
\item Region lémanique\cr
\item Expace Mittelland\cr
\item Nordwestschweiz\cr
\item Zürich\cr
\item Ostschweiz\cr
\item Zentralschweiz\cr
\item Ticino\cr
}

  \tabular{ll}{
			1\tab  Region lémanique\cr
			2\tab  Espace Mittelland\cr
			3\tab  Nordwestschweiz\cr
			4\tab  Zürich\cr
			5\tab  Ostschweiz\cr
			6\tab  Zentralschweiz\cr
			7\tab  Ticino
			}

Plot CH-Map with Districts (Bezirke)

Note that district id bezk consists of the canton nr first (1 or 2 digit) and a 2-digits 'Bezirk-nr'.
So is Hinwil the 5th district in Zurich (canton 1).


}

\note{The coordinates of the regions' centroids are available via \kbd{data(greg.pnt)}.
%%  ~~further notes~~
}

\note{
The list of ms-regions and their ids is given by \code{d.bfsrg}:\cr
  \kbd{  msregions <- unique(d.bfsrg[, c("ms_reg_c","ms_reg_x","ms_typ_c","ms_typ_x")])}

%%  ~~further notes~~
}

\note{
The list of all political communities is given by \code{d.bfsrg}:\cr
  \kbd{polg <- d.bfsrg[,c("bfs_nr", "gemeinde_name_x", "kt_x")]}
}

\note{
The list of all districts is given by \code{d.bfsrg}: \cr
  \kbd{  districts <- unique(d.bfsrg[,c("bezirk_c", "kt_c", "bezirk_x", "kt_x")])}
}


\author{
Andri Signorell <andri.signorell@helsana.ch>
%%  ~~who you are~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{\code{\link{PlotGreg}}, \code{\link{PlotBezk}}, \code{\link{PlotPolg}}, \code{\link{d.bfsrg}}
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{

if(interactive()){

PlotKant(id=c("GR","ZH","VS"), col=c("lightgrey","lightblue","lightsalmon"))

require(DescTools)

# get some data
eco_p <- c(AG=0.48,AI=0.47,AR=0.4,BE=0.48,BL=0.44,BS=0.4,FR=0.48,GE=0.28,GL=0.51,
           GR=0.4,JU=0.61,LU=0.49,NE=0.54,NW=0.43,OW=0.58,SG=0.45,SH=0.36,SO=0.45,
           SZ=0.39,TG=0.47,TI=0.46,UR=0.4,VD=0.46,VS=0.45,ZG=0.41,ZH=0.41)

cols <- colorRampPalette(c("white",hred))(100)

PlotKant(id=names(eco_p), col=FindColor(eco_p, cols=cols), main="ECO in CH")
ColorLegend(x="left", inset=-0.01, cols=cols,
            labels=formatC((seq(0, 1, .2)), digits=2, format="f"),
            width=15000, frame="grey", cex=0.8 )

# Greater regions **************

# Simple plot with text remark
PlotGreg(col=colorRampPalette(c("blue", "white", "red"), space = "rgb")(7),
         main="Grossregionen CH")

# Plot just 3 regions
PlotGreg(id = c(2,4,7), col = c("bisque","darkolivegreen1","khaki"),
         main="Mittelland, Zuerich und Tessin")
AddLakes(col="grey90", border="darkgrey")

# Get midpoints of regions and put text there
text(tkart$greg.pnt@coords[c(2,4,7),1], tkart$greg.pnt@coords[c(2,4,7),2],
     tkart$greg.pnt@data[c(2,4,7),"ID1"], col="black")

# Data structure of swiss regions' map
tkart$greg.map@data[,1:2]


## bez
RequireMap("bezk.map")
# get the list of the districts
head(tkart$bezk.map@data)

# plot only districts in the canton of Lucerne
PlotBezk(id=311:316, col=colorRampPalette(c("red", "white", "blue"), space = "rgb")(5))


# plot all
PlotBezk(bezk=tkart$bezk.map@data$ID0, col=rainbow(147), main="Bezirke in CH")


# msreg
cols <- c(y=rgb(255,247,174,max=255), o=rgb(251,208,124,max=255),
          v=rgb(228,201,224,max=255), b=rgb(211,230,246,max=255),
          g=rgb(215,233,205,max=255), r=rgb(244,182,156,max=255),
          p=rgb(255,248,236,max=255))

RequireMap(c("msre.map", "msre.pnt"))
PlotKant(col=cols[c("g","g","o","r","v","b","y","g","y","o",
                          "v","o","y","v","y","v","o","y","r","b",
                          "v","y","b","r","v","b")],
         border="grey20", lwd=1, pbg=cols["p"],
         main="106 MS-Regionen")


plot(tkart$msre.map, add=TRUE, border="grey60")
plot(tkart$kant.map, add=TRUE, border="grey30")
AddLakes(1:2, col=rgb(235, 247, 253, max=255), border=rgb(0,166,235, max=255))
AddRivers(1:5, col=rgb(0, 166, 235, max=255))

text(x=tkart$msre.pnt@coords[,1], y=tkart$msre.pnt@coords[,2],
     tkart$msre.pnt@data$ID0, cex=0.6)


# pol gemeinde
# get the list of the political communities
RequireMap("polg.map")
tkart$polg.map@data
nrow(tkart$polg.map@data)

# plot only the comunities 1:10
PlotPolg(id=1:10,
         col=colorRampPalette(c("red", "white", "blue"), space = "rgb")(10))


# plot all
RequireMap("polg.map")
PlotPolg(id=tkart$polg.map@data$ID0, col=rainbow(nrow(tkart$polg.map@data)),
         main="Political communities in CH")


}
}
\keyword{ hplot }
\keyword{ spatial }

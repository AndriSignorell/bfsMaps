\name{Plot Swiss Regions}
\alias{PlotKant}
\alias{PlotGreg}
\alias{PlotPolg}
\alias{PlotBezk}
\alias{PlotMSRe}

% \encoding{latin1}
\encoding{UTF-8}

%- Also NEED an '\alias' for EACH other topic documented here.
\title{ Plot Swiss Regions}
\description{
The function plots a map of Switzerland overlayed with different types of regions. Included are greater regions ('Grossregionen'), MS regions ('mobilité spatiale'), cantons, districts and political communities. The single regions can be given a defined color, whereas the color need not be defined for all.\cr
The vegetational area is the spatial area where people live, excluding mountains and further uninhabitable area. The vegetational area can be drawn over an already existing map.
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
PlotGreg(id = NULL, col = NA, pbg = "white", main = "",
         border = "grey", lwd = 1, col.vf = NA, border.vf = NA, labels = NULL,
         tmtxt = TRUE, add = FALSE, map_x = "greg.map", ...)

PlotKant(id = NULL, col = NA, pbg = "white", main = "",
         border = "grey", lwd = 1, col.vf = NA, border.vf = NA, labels = NULL,
         tmtxt = TRUE, add = FALSE, map_x = "kant.map", ...)

PlotMSRe(id = NULL, col = NA, pbg = "white", main = "",
         border = "grey", lwd = 1, col.vf = NA, border.vf = NA, labels = NULL,
         tmtxt = TRUE, add = FALSE, map_x = "msre.map", ...)

PlotBezk(id = NULL, col = NA, pbg = "white", main = "",
         border = "grey", lwd = 1, col.vf = NA, border.vf = NA, labels = NULL,
         tmtxt = TRUE, add = FALSE, map_x = "bezk.map", ...)

PlotPolg(id = NULL, col = NA, pbg = "white", main = "",
         border = "grey", lwd = 1, col.vf = NA, border.vf = NA, labels = NULL,
         tmtxt = TRUE, add = FALSE, map_x = "polg.map", ...)
}
%- maybe also 'usage' for other objects documented here.
% Note: \code{NAs} are recoded as \code{"white"}.
\arguments{
  \item{id}{vector of region ids. All types of regions can be addressed via their numeric ID, cantons can additionally be identified with their abbreviation:
  \code{"AG"}, \code{"AI"}, \code{"AR"}, \code{"BE"}, \code{"BL"}, \code{"BS"}, \code{"FR"}, \code{"GE"}, \code{"GL"}, \code{"GR"}, \code{"JU"}, \code{"LU"}, \code{"NE"},
  \code{"NW"}, \code{"OW"}, \code{"SG"}, \code{"SH"}, \code{"SO"}, \code{"SZ"}, \code{"TG"}, \code{"TI"}, \code{"UR"}, \code{"VD"}, \code{"VS"}, \code{"ZG"}, \code{"ZH"}
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
  \item{col.vf}{vector of colors for the vegetational. If set to \code{NA} (default)
the vegetational area will not be drawn.
%%     ~~Describe \code{cols} here~~
}
  \item{border.vf}{color of borders for the vegetational area. If set to \code{NA} (default)
the borders of the vegetational area will not be drawn.
%%     ~~Describe \code{\dots} here~~
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
\details{The different functions all use the same core code, but use different default maps. The default maps are named: \code{"greg.map"}, \code{"msre.map"}, \code{"kant.map"}, \code{"bezk.map"} and \code{"polg.map"}.

\code{PlotGreg} uses a map for Swiss regions (Grossregionen), as defined in \code{greg.map@data}. The regions are:
  \tabular{ll}{
			1\tab  Region lémanique\cr
			2\tab  Espace Mittelland\cr
			3\tab  Nordwestschweiz\cr
			4\tab  Zürich\cr
			5\tab  Ostschweiz\cr
			6\tab  Zentralschweiz\cr
			7\tab  Ticino
			}

The list of MS regions with names and ids can be found in \code{d.bfsrg}:\cr
  \code{unique(d.bfsrg[, c("ms_reg_c", "ms_reg_x", "ms_typ_c", "ms_typ_x")])}

The abbreviations of the cantons are compiled in the variable \code{kt}. More details can be extracted from\cr \code{unique(d.bfsrg[,c("kt_c", "kt_x", "kt_bez_x")])}
%%  ~~ If necessary, more details than the description above ~~

Districts (german: 'Bezirke') are associations of communities. The district id internally consists of the canton nr (1 or 2 digits) and a 2-digits 'Bezirk-nr'. So is 'Hinwil' with the district nr '51' the 5th district in Zurich (canton '1').\cr
The list of all districts is given in \code{d.bfsrg}: \cr
  \code{unique(d.bfsrg[, c("bezirk_c", "kt_c", "bezirk_x", "kt_x")])}


The list of all political communities is given in \code{d.bfsrg}:\cr
  \code{d.bfsrg[,c("bfs_nr", "gemeinde_name_x", "kt_x")]}

All the regions can also be accessd and plotted by manually loading the maps and use the generic \code{plot} function.
\preformatted{
cant <- GetMap("kant.map")
plot(cant)
}


There are also dedicated maps for all regions, which contain only the coordinates of the regions' centroids. They can be accessed using according mapname with the extension \code{.pnt}, e.g. for the cantons \code{GetMap("kant.pnt")}.

To simplify the description, the function returns the center coordinates. These can then be used with the function \code{text()}.
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

# define the ids for the cantons and the according colors
PlotKant(id=c("GR","ZH","VS"), col=c("lightgrey","lightblue","lightsalmon"))

require(DescTools)
# get some percentage values...
some_p <- c(AG=0.48,AI=0.47,AR=0.4,BE=0.48,BL=0.44,BS=0.4,FR=0.48,GE=0.28,GL=0.51,
           GR=0.4,JU=0.61,LU=0.49,NE=0.54,NW=0.43,OW=0.58,SG=0.45,SH=0.36,SO=0.45,
           SZ=0.39,TG=0.47,TI=0.46,UR=0.4,VD=0.46,VS=0.45,ZG=0.41,ZH=0.41)

# and a color ramp from white to hred
cols <- colorRampPalette(c("white", hred))(100)

PlotKant(id=names(some_p), col=FindColor(some_p, cols=cols), main="ECO in CH")
ColorLegend(x="left", inset=-0.01, cols=cols,
            labels=formatC((seq(0, 1, .2)), digits=2, format="f"),
            width=12000, frame="grey", cex=0.8 )


# greater regions
PlotGreg(col=colorRampPalette(c("blue", "white", "red"), space = "rgb")(7),
         main="Greater Regions CH")

PlotGreg(id = c(2,4,7), col = c("bisque","darkolivegreen1","khaki"),
         main="Espace Mittelland, Zurich und Ticino")
AddLakes(col="grey90", border="darkgrey")

xy <- sf::st_coordinates((greg.pnt <- GetMap("greg.pnt"))$geometry[c(2,4,7)])
text(xy[,1], xy[,2], greg.pnt$name[c(2,4,7)], col="black")


# plot the districts
bezk.map <- GetMap("bezk.map")
head(bezk.map)

PlotBezk(id=311:316, col=colorRampPalette(c("red", "white", "blue"), space = "rgb")(5))

PlotBezk(id=bezk.map[[1]], col=rainbow(147), main="Districts in CH")

cols <- c(y=rgb(255,247,174,max=255), o=rgb(251,208,124,max=255),
          v=rgb(228,201,224,max=255), b=rgb(211,230,246,max=255),
          g=rgb(215,233,205,max=255), r=rgb(244,182,156,max=255),
          p=rgb(255,248,236,max=255))

# display MS regions
# start with a cantons map

# start with a cantons map
PlotKant(1:26,col=cols[c("g","g","o","r","v","b","y","g","y","o",
                    "v","o","y","v","y","v","o","y","r","b",
                    "v","y","b","r","v","b")],
         border="grey20", lwd=1, pbg=cols["p"],
         main="106 MS-Regions")

# add the MS regions borders
xy <- PlotMSRe(add=TRUE, border="grey60")

# reoutline the cantons, as they have been overplotted in the step before
PlotKant(1:26, add=TRUE, border="grey30", lwd=1)

# add the waters
AddLakes(1:2, col=rgb(235, 247, 253, max=255), border=rgb(0,166,235, max=255))
AddRivers(1:5, col=rgb(0, 166, 235, max=255))

# ... and finally add labels
text(x=xy$x, y=xy$y, GetMap("msre.map")$id, cex=0.6)


# plot political communities

# plot only the first 10 elements
PlotPolg(id=1:10,
         col=colorRampPalette(c("red", "white", "blue"), space = "rgb")(10))

# plot all communities
PlotPolg(id=(map <- GetMap("polg.map"))$id, col=rainbow(nrow(map)),
         main="Political communities in CH")

})}
\keyword{ hplot }
\keyword{ spatial }

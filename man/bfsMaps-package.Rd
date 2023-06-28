\name{bfsMaps-package}
\alias{bfsMaps-package}
\alias{bfsMaps}
\docType{package}
\encoding{UTF-8}

\title{Plotting Switzerland Maps from the Swiss Federal Statistical Office (SFSO)
}
\description{
This package contains convenience functions for plotting Switzerland maps distributed free of charge by the Swiss Federal Office of Statistics (SFSO). It uses the package \code{'rgdal'} for reading and plotting ESRI (Environmental Systems Research Institute) shapefiles.}
\details{
The generation of spatial images with maps normally requires several steps, which makes the handling for occasional users complex and confusing. Functions on a higher level of abstraction simplify the daily work. The purpose is to allow the user to get to the desired map as quickly and easily as possible.\cr
The idea behind the functions is to load the specific map, assign the desired color to the regions and create the plot. The arguments are kept  straightforward, what is needed is a vector with the ids of the regions and an equally sized vector for the colors.
The maps can be loaded and plotted independently of each other as single maps. \cr There are specific functions for the most important spatial divisions in Switzerland.
Cantons can be plotted with \code{\link{PlotKant}()}, political municipalities with  \code{\link{PlotPolg}()}, large regions with  \code{\link{PlotGreg}()} and districts with  \code{\link{PlotBezk}()}.
Lakes and rivers in multiple categories can be added to existing images with  \code{\link{AddLakes}()} or  \code{\link{AddRivers}()}.

Before the maps can be drawn, a few preparations must be made:
\itemize{
\item download the maps following the link 'Swiss Federal Office of Statistics - Base maps' (below) and unzip them into a folder

\item{declare the location as \code{options(bfsMaps.base = "//path_to_my_maps/")} }
\item{names and shortnames of the maps are stored in a file named \code{'maps.csv'}, which can be stored either in the \code{'bfsMaps.base'} folder or alternatively in the packages installation folder. \cr An example file for the last map edition ('ThemaKart map boundaries - Set 2022') is included in the package and can be found in the packages \code{.../extdata} folder. If you are using a different edition, you have to adjust the file accordingly. }

}}

\note{Loading the maps for the first time will display a warning "1: In OGRSpatialRef(" concerning a "Discarded datum CH1903 ...". This does NOT impact the use of the maps. The warnings can however only be suppressed by setting an option tag in R's \code{.Rprofile} file:

\code{options(rgdal_show_exportToProj4_warnings="none")}

(See also https://cran.r-project.org/web/packages/rgdal/vignettes/PROJ6_GDAL3.html)
}

\author{Andri Signorell <andri@signorell.net>
}
\references{
Swiss Federal Office of Statistics - Base maps: \url{https://www.bfs.admin.ch/bfs/de/home/statistiken/regionalstatistik/kartengrundlagen.html}

Swiss Federal Office of Statistics - Spatial divisions: \url{https://www.agvchapp.bfs.admin.ch/de/typologies/query}

Official directory of towns and cities (PLZ): \url{https://www.cadastre.ch/de/services/service/registry/plz.html}

Swiss Premium Regions: \url{https://www.priminfo.admin.ch/}
}

\keyword{ package }

\examples{
# Note:
#   The examples can not be run without having the map data installed before!
try( {

# PlotKant simply tasks for the id and the color of the spatial region
# labels can be directly placed
PlotKant(id=c("ZH", "FR"), col=c("yellow","limegreen"), label=TRUE)
PlotKant(id="GR", col="orange", label=TRUE, add=TRUE)
AddLakes()
title("Switzerland with some cantons")
# mark the national border
PlotCH(col=NA, add=TRUE, lwd=2)


# The maps have all a general area and a vegetational area
PlotKant(c("VS", "BE"), SetAlpha(c("yellow","limegreen"),.50),
         col.vf=c("yellow","limegreen"), label=TRUE)


# The function returns the centroid points of the objects, which can be used
# to label the plot afterwards
xy <- PlotGreg(c(3,6), SetAlpha(c("plum1", "lightslateblue"),.50),
               col.vf=c("plum1", "lightslateblue"), labels=NA)
AddLakes()
BoxedText(xy$x, xy$y, labels = c("here", "there"), border=NA,
          col = SetAlpha("white", 0.8))


# Plot political communities
PlotPolg(border="grey85" )
PlotBezk(border="grey55", add=TRUE  )
PlotKant(border="black", lwd=1, add=TRUE)
AddLakes()
AddRivers()

# Cantonal capitals
points(sf::st_coordinates(GetMap("stkt.pnt")$geometry),
       pch=21, col="grey", bg="red")


# Display vegetational area
PlotCH(col="wheat3", col.vf="wheat", border="wheat3", main="CH Vegetation Area")
AddRivers()
AddLakes()
PlotKant(col=NA, border="wheat4", add=TRUE, lwd=1)


# Use extended spatial divisions (language regions)
cols <- c("peachpuff2","gainsboro","honeydew3","lightgoldenrodyellow")
PlotPolg(d.bfsrg$gem_id, col=cols[d.bfsrg$sprgeb_c], border="grey70",
         main="Language CH" )
PlotBezk(d.bfsrg$bezk_c, col=NA, border="grey40", add=TRUE)
AddLakes(col="lightsteelblue1", border="lightskyblue" )
legend(x="topleft", legend=c("german", "french","italian","romanche"), bg="white",
       cex=0.8, fill= cols )


# Swiss premiumregions demonstrating combinations of polygons
PlotCH(col="white", main="Premiumregions CH")

plot(CombinePolg(id=d.bfsrg$gem_id, g=d.bfsrg$preg_c),
     col=c("white","olivedrab4","olivedrab3","olivedrab2"), add=TRUE)

legend(x="topleft", fill=c("white","olivedrab4","olivedrab3","olivedrab2"), cex=0.8,
       legend=c("Region 0","Region 1","Region 2","Region 3") )

PlotKant(col=NA, border="grey40", add=TRUE)
AddLakes()

# Cities
cols <- as.vector(sapply(c(hred, hblue, hyellow),
                         SetAlpha, alpha=c(1, 0.7, 0.5)))
old <- Mar(right=20)
PlotPolg(id=d.bfsrg$gem_id, col=cols[as.numeric(d.bfsrg$gem_typ9_x)],
         border="grey70")
AddLakes(col="grey90", border="grey50")
PlotKant(add=TRUE, col=NA, border="grey30")
legend(x=2854724, y=1292274, fill=cols, border=NA, box.col=NA,
       y.intersp=c(1,1,1, 1.1,1.05,1.05, 1.1,1.07,1.07),
       legend=StrTrunc(levels(d.bfsrg$gem_typ9_x), 50),
       xjust=0, yjust=1, cex=0.8, xpd=NA)
par(mar=old)


# Degree of urbanisation
PlotPolg(col=SetAlpha(c(hred, hblue, hyellow), 0.8)[as.numeric(d.bfsrg$degurba_x)],
         main="Degree of Urbanisation 2022")
PlotKant(add=TRUE, border="grey30")
AddLakes(col = "grey90", border = "grey50")


# get cantons' area
area <- sf::st_area(GetMap("kant.map")) / 1E6

# plot cantons
xy <- PlotKant(col=colorRampPalette(c("white", "steelblue"),
                                    space = "rgb")(720)[trunc(area)/10],
               main=expression(paste( "Cantons' area in ", km^2)) )
AddLakes(col="grey90", border="grey60")
text(xy, labels=round(area,1), cex=0.7)


kant.gr <- GetMap("kant.map") |> (\(.) .[.$name=="Graubünden", "geometry"])()
# prepare plot
plot(kant.gr, asp=1, axes=FALSE, xlab="", ylab="",
     main="Beautiful Grisons", col="steelblue", lwd=2)

loctext <- function(x, y, text){
  points(x, y, pch=15, col="lightgrey" )
  text(x, y, text, adj=c(0,0.5), col="white", font=2)
}
# the new swiss coordinates LV95 are:   x_new = x_old + 2e6, y_new = y_old + 1e6
loctext(2782783, 1185993,"  Davos")
loctext(2761412, 1176112,"  Valbella")
loctext(2784192, 1152424,"  St. Moritz")
loctext(2714275, 1175027,"  Rabius")


# Swiss metropolitan areas
cols <- c("royalblue1","red","bisque3","yellow","orange","beige")
# we have to prepare the background here, for some reasons...
PlotCH(col="darkolivegreen1", border="grey", lwd=2, main="Swiss metropolitan areas")
# require other map
metr.map <- GetMap("metr.map")
plot(metr.map$geometry, add=TRUE, border="grey60", col=cols)
AddLakes(col="grey90", border="grey70")
legend( x="topleft", legend=c("Ländliche Gemeinde", metr.map$name),
        fill=c("darkolivegreen1", cols),
        bg="white", cex=0.8, xpd=TRUE )


# We can find the neighbor cantons, here for the canton Glarus (id=8)
nbs <- Neighbours(map=GetMap("kant.map"), id=8)
PlotKant(id = c(8, nbs), col=c("steelblue", rep("grey80", length(nbs))),
         main="Find Neighbours")



})}



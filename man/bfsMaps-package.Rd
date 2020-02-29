\name{bfsMaps-package}
\alias{bfsMaps-package}
\alias{bfsMaps}
\docType{package}
\encoding{latin1}
\title{ Plotting Switzerland Maps from Swiss Statistics BFS
}
\description{
Convenience functions for plotting Switzerland maps distributed by Swiss Statistics BFS ThemaKart.
Uses the package \code{maptools} for reading and plotting ESRI shapefiles from 'Cartographic bases for small-scale thematic mapping', obtainable from Swiss Statistics www.bfs.admin.ch.
}
\details{
\tabular{ll}{
Package: \tab bfsMaps\cr
Type: \tab Package\cr
Version: \tab 1.0\cr
Date: \tab 2020-03-09\cr
License: \tab code GPL (>= 2), maps: (c) Federal Statistical Office, Neuchatel 2020\cr
LazyLoad: \tab yes\cr
}
The package contains a number of functions for quickly displaying spatial distributions in CH maps. The idea of the functions is, to load the specific map, assign the desired color to the regions and display the results.
The maps can be loaded and plotted independently of each other as single maps. There are also special functions for the most important spatial segmentations in Switzerland that accept a vector with the region ID and the corresponding color vector.
Cantons can be plotted with \code{\link{PlotKant}()}, political municipalities with  \code{\link{PlotPolg}()}, large regions with  \code{\link{PlotGreg}()} and districts with  \code{\link{PlotBezk}()}.
Lakes and rivers in multiple categories can be added to existing images with  \code{\link{AddLakes}()} or  \code{\link{AddRivers}()}.

}
\author{
Andri Signorell <andri.signorell@helsana.ch>

Maintainer: Andri Signorell <andri.signorell@helsana.ch>

}
\references{
Further Information:\cr
\itemize{
BfS Kartengrundlagen: \url{https://www.bfs.admin.ch/bfs/de/home/statistiken/regionalstatistik/kartengrundlagen.html}\cr
BfS Raumgliederungen: \url{https://www.bfs.admin.ch/bfs/de/home/grundlagen/raumgliederungen.assetdetail.2546562.html}\cr
BfS PLZ <-> Gemeinde-Nr: \url{https://www.bfs.admin.ch/bfs/de/home/grundlagen/agvch/ortschaftenverzeichnis.assetdetail.275785.html}\cr
BAG Praemienregionen: \url{https://www.priminfo.admin.ch/de/regionen}
}}

\keyword{ package }

\examples{
if(interactive()){


# PlotKant simply tasks for the id and the color of the spatial region
# labels can be directly placed
PlotKant(id=c("ZH", "FR"), col=c("yellow","limegreen"), label=TRUE)
PlotKant(id="GR", col="orange", label=TRUE, add=TRUE)
AddLakes()
title("Switzerland with some cantons")
# Mark the national border
PlotCH(col=NA, add=TRUE, lwd=2, waters=FALSE)


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

# Kantons-Hauptorte
points(RequireMap("stkt.pnt")@coords, pch=21, col="grey", bg="red")



# MS-Regions
cols <- c(y=rgb(255,247,174,max=255), o=rgb(251,208,124,max=255),
          v=rgb(228,201,224,max=255), b=rgb(211,230,246,max=255),
          g=rgb(215,233,205,max=255), r=rgb(244,182,156,max=255),
          p=rgb(255,248,236,max=255))

PlotKant(col=cols[c("g","g","o","r","v","b","y","g","y","o","v","o","y",
                    "v","y","v","o","y","r","b","v","y","b","r","v","b")],
         border="grey20", lwd=1, pbg=cols["p"], main="106 MS-Regionen")

xy <- PlotMSRe(add=TRUE, border="grey60",  col=NA)
PlotKant(add=TRUE, border="grey30",  col=NA)

AddLakes(1:2, col=rgb(235,247,253, max=255), border=rgb(0,166,235,max=255))
AddRivers(1:5, col=rgb(0,166,235,max=255))

text(x=xy, labels = tkart$msre.map$ID0, cex=0.6)


# Display vegetational area
PlotCH(col="wheat3", col.vf="wheat", border="wheat3", main="CH Vegetationsfläche")
AddRivers()
AddLakes()
PlotKant(col=NA, border="wheat4", add=TRUE, lwd=1)


# Use Raumregions data
cols <- c("peachpuff2","gainsboro","honeydew3","lightgoldenrodyellow")
PlotPolg(d.bfsrg$gem_id, col=cols[d.bfsrg$sprgeb_c], border="grey70",
         main="Sprachgebiete CH" )
PlotBezk(d.bfsrg$bezk_c, col=NA, border="grey40", add=TRUE)
AddLakes(col="lightsteelblue1", border="lightskyblue" )
legend(x="topleft", legend=levels(d.bfsrg$sprgeb_x), bg="white",
       cex=0.8, fill= cols )


PlotPolg( d.bfsrg$gem_id, col=c("white","olivedrab4","olivedrab3","olivedrab2")[d.bfsrg$preg_c + 1],
          border="grey65")
legend(x="topleft", fill=c("white","olivedrab4","olivedrab3","olivedrab2"), cex=0.8
       , legend=c("Praemienregion 0","Praemienregion 1","Praemienregion 2","Praemienregion 3") )
AddLakes()



# Praemienregionen BAG
PlotCH(col="white", main="Praemienregionen CH")

plot(CombinePolg(id=d.bfsrg$gem_id, g=d.bfsrg$preg_c),
     col=c("white","olivedrab4","olivedrab3","olivedrab2"), add=TRUE)

legend(x="topleft", fill=c("white","olivedrab4","olivedrab3","olivedrab2"), cex=0.8,
       legend=c("Praemienregion 0","Praemienregion 1","Praemienregion 2","Praemienregion 3") )

PlotKant(col=NA, border="grey40", add=TRUE)
AddLakes()




# Use data out of shape files slots (here: cantons' area)
acant <- sapply( slot(tkart$kant.map, "polygons"), function(x) slot(x, "area"))

# plot cantons
xy <- PlotKant(col=colorRampPalette(c("white", "steelblue"),
                                    space = "rgb")(720)[trunc(acant /10000000)],
               main=expression(paste( "Cantons' area in ", km^2)) )
AddLakes(col="grey90", border="grey60")
text(xy, labels=round(acant/1E6,1), cex=0.7)


# Plot single cantons
kant.gr <- slot(tkart$kant.map,"polygons")[[18]]@Polygons[[1]]@coords
# prepare plot
plot(kant.gr, asp=1, axes=FALSE, frame.plot=FALSE, xlab="", ylab="",
     main="Beautiful Graubünden", type="n",
     xlim=c(2671494, 2862205), ylim=c(1108314, 1219629))
polygon(kant.gr, col="steelblue", lwd=2 )

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
cols <- c("darkolivegreen1","royalblue1","red","bisque3","yellow","orange","beige")
RequireMap("metr.map")   # require other map
plot(tkart$metr.map, usePolypath=TRUE, col=cols[as.numeric(tkart$metr.map@data$ID0)] )
legend(x="topleft", legend=tkart$metr.map@data$ID1, fill=cols, bg="white", cex=0.8 )
title(main="Swiss metropolitan areas")


# We can find the neighbor cantons, here for the canton Glarus (id=8)
nbs <- Neighbours(map=tkart$kant.map, id=8)[[1]]
PlotKant(id = c(8, nbs), col=c("steelblue", rep("grey80", length(nbs))), main="Neighbours")
}
}

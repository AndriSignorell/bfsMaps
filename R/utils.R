

# Utility functions


BfSStamp <- function(xy = NULL, year_n = 2020, txt=NULL, cex=0.6, adj=c(1,0), ...){

  if(is.null(xy)){
    xy = list(x=2835000, y = 1075000)
  } else if(identical(xy, "bottomright")) {
    plt <- par('plt')
    usr <- par('usr')

    ## when a logarithmic scale is in use (i.e. par('xlog') is true),
    ## then the x-limits would be 10^par('usr')[1:2].  Similarly for
    ## the y axis
    xcoord <- usr[2] + (usr[2] - usr[1])/(plt[2] - plt[1]) *
      (1-plt[2]) - cex*strwidth('m')
    ycoord <- usr[3] - diff(usr[3:4])/diff(plt[3:4])*(plt[3]) +
      cex*strheight('m')

  }
  # else nothing, just use xy as coordinates

  # use main package version here
  if(is.null(txt) )
    txt <- gettextf("Kartengrundlage:\n\U00A9 BFS, ThemaKart, %s",
           Coalesce(year_n, packageVersion("bfsMaps")[1,1]))

  # implement a language switch?
  # Map basis

  # text(x = 835000, y = 75000,
  text(x = xy, labels = txt, cex=0.6, adj=c(1,0), ...)


}




Neighbours <- function(map, id = NULL){
  # require(spdep)
  # defined as import
  # Findet alle Nachbarn
  nbs <- poly2nb(as(map, "SpatialPolygons"))
  nbslist <- nb2listw(nbs, style="W", zero.policy=TRUE)$neighbours
  attributes(nbslist) <- NULL
  if (!is.null(id)) {
    # nbslist <- nbslist[[which(map@data[, 1] %in% id)]]
    nbslist <- nbslist[match(id, map@data[, 1])]
    nbslist <- lapply(nbslist, function(i) as.numeric(as.character(map@data[i, 1])))
    }
  return(nbslist)
}



SwissLocator <- function(){
  xy.sp <- SpatialPoints(data.frame(locator()))
  polg.map <- RequireMap("polg.map")

  # we must apply some kind of proj4string since 2017 ...
  # project string is not completely set with that statement! (no to +towgs84=674...)
  # proj4string(xy.sp) <- proj4string(polg.map)
  xy.sp@proj4string <- polg.map@proj4string

  xy.bfsnr <- over(xy.sp, polg.map)[, 1]

  note <- gettextf("\033[36m\nNote: ------\n  Found communities: %s.\n\n\033[39m", paste(xy.bfsnr, collapse = ", "))
  cat(note)

  return(data.frame(xy.sp, SetNames(d.bfsrg[ match(xy.bfsnr, d.bfsrg$gem_id),
                                     c("gem_id", "gemeinde_x", "bezk_x", "msre_x", "kt_x")],
                                    rownames=NULL)))
}



CombinePolygons <- function(map, g){

  # map containing the regions to be combined
  # grp the vector of the same length containing the groups

  unionSpatialPolygons(SpatialPolygons(unlist(slot(map, "polygons"))), g)

}


CombinePolg <- function(id, g, map=RequireMap("polg.map")){

  # idx <- merge(polg.map@data, d.hsagem, by.x="Primary_ID", by.y="bfs_nr", all.x=TRUE, all.y=FALSE)

  d.grp <- merge(map@data, data.frame(id=id, g=g), by.x=1, by.y="id", all.x=TRUE)
  grp <- d.grp[order(as.numeric(as.character(d.grp[, 1]))), "g"]

  CombinePolygons(map, grp)

}


CombineKant <- function(id, g, map=RequireMap("kant.map")){

  # Gruppen zuweisen und danach sortieren f?r die Polygone
  d.grp <- merge(map@data, data.frame(id=id, g=g), by.x="ID0", by.y="id", all.x=TRUE)
  grp <- d.grp[order(as.numeric(as.character(d.grp$ID0))), "g"]

  # die Sortierung muss gleich sein, wie in der Karte
  CombinePolygons(map, grp)

}


RemoveHoles <- function (x) {

  if (!any(which(utils::installed.packages()[, 1] %in% "maptools")))
    stop("please install maptools package before running this function")

  xp <- slot(x, "polygons")

  holes <- lapply(xp, function(x) sapply(methods::slot(x, "Polygons"),
                                         methods::slot, "hole"))
  res <- lapply(1:length(xp),
                function(i) methods::slot(xp[[i]], "Polygons")[!holes[[i]]])

  IDs <- row.names(x)

  x.fill <- sp::SpatialPolygons(lapply(1:length(res),
                                       function(i) sp::Polygons(res[[i]],
                                                  ID = IDs[i])), proj4string = sp::CRS(sp::proj4string(x)))
  methods::slot(x.fill, "polygons") <- lapply(methods::slot(x.fill, "polygons"),
                                              maptools::checkPolygonsHoles)
  methods::slot(x.fill, "polygons") <- lapply(methods::slot(x.fill, "polygons"),
                                              "comment<-", NULL)
  pids <- sapply(methods::slot(x.fill, "polygons"),
                 function(x) methods::slot(x,"ID"))

  x.fill <- sp::SpatialPolygonsDataFrame(x.fill,
                                         data.frame(row.names = pids, ID = 1:length(pids)))

  return(x.fill)

}




DownloadBfSMaps <- function(url="https://www.bfs.admin.ch/bfsstatic/dam/assets/11927607/master",
                            path=paste0(path.expand("~"), "/MapData")) {

  cat("\nAttempt to download mapdata from Swiss Federal Office of Statistics (SFSO):\n\n")

  cat("start downloading...")
  temp <- tempfile()
  download.file(url = url, temp, mode="wb", quiet = TRUE)
  cat("OK\n")

  cat("unzipping file...")
  unzip(temp, exdir=path)
  cat("OK\n")

  cat("renaming folders...")
  flaeche <- grep("fl.che(?!.*/)", list.dirs(path), value=TRUE, perl = TRUE)
  for(x in flaeche){
    file.rename(x, gsub("fl.che", "fl\xE4che", x))
  }
  cat("OK\n\n")

  opt <- gettextf('options(bfsMaps.base="%s")',
                  paste(path, list.files(path),
                        grep(pattern = "GEOM",
                             x = list.files(paste(path, list.files(path), sep="/")),
                             value = TRUE),
                        sep="/"))

  cat(gettextf("Enter the following entry in your .RProfile file:\n  %s ", opt), "\n\n")

  invisible(opt)

}




# with(d.bfsrg,
#      plot(
#        CombinePolg(bfs_nr, preg_x),
#        col=sample(rainbow(length(preg_anz)))))
#
#
# tkt <- table(d.bfsrg$kt_c, d.bfsrg$sprgeb_x)
# grp <- levels(d.bfsrg$sprgeb_x)[apply(tkt, 1, which.max)]
# plot(CombineKant(rownames(tkt), grp), col=c("red","blue","green"))


# Beispiel mit HSAs machen!! ******************



# # configure HSAs
# d.hsa <- XLGetRange(file = "O:\\E\\EGW\\Intern\\Transfer_Intern_SW-EGW\\Projekte\\EOL 2\\HSA_2013_161214.xlsx",
#                     sheet = "Tabelle1",
#                     range = c("A1:B706"),
#                     as.data.frame = TRUE, header = TRUE, stringsAsFactors = FALSE)
#
# d.medstat<- XLGetRange(file = "O:\\E\\EGW\\Intern\\Transfer_Intern_SW-EGW\\Projekte\\EOL 2\\MedStat GeoCod.xls",
#                        sheet = "REGION=CH",
#                        range = c("A1:G3674"),
#                        as.data.frame = TRUE, header = TRUE, stringsAsFactors = FALSE)
#
# d.hsaplz <- merge(x=d.hsa, y=d.medstat, by.x="medstat", by.y="MedStat", all.x=TRUE, all.y=FALSE)
# head(d.hsaplz)
#
# d.plz
#
# d.hsaxt <- merge(x=d.hsaplz, y=d.plz, by.x="NPA/PLZ", by.y="plz", all.x=TRUE, all.y=FALSE)
#
# d.hsagem <- unique(d.hsaxt[, c("hsa","bfs_nr")])
#
# PlotPolg(d.hsagem$bfs_nr, col=Pal("RedToBlack", 61)[as.numeric(factor(d.hsagem$hsa))])
#
# d.hsagem
# levels(factor(d.hsagem$hsa))
# polg.map
#
#
#
# bfsnr <- c(1218,1367,1631)
# idx <- match(bfsnr, polg.map@data[, 1])
#
# PlotPolg(1, "black")
# gem1 <- slot(polg.map, "polygons")[[633]]@Polygons[[1]]@coords
# gem2 <- slot(polg.map, "polygons")[[657]]@Polygons[[1]]@coords
# gem3 <- slot(polg.map, "polygons")[[685]]@Polygons[[1]]@coords
# polygon(gem1, col="steelblue", lwd=2)
# polygon(gem2, col="steelblue", lwd=2)
# polygon(gem3, col="steelblue", lwd=2)
#
# unionSpatialPolygons(
#   SpatialPolygons(list(slot(polg.map, "polygons")[[633]])),
#   SpatialPolygons(list(slot(polg.map, "polygons")[[657]])),
#   SpatialPolygons(list(slot(polg.map, "polygons")[[685]])),
#   "dd")
#
# pol <- slot(polg.map, "polygons")[[633]]
# class(pol)
#
# SpatialPolygons(list(pol))
#
#
# nc1 <- readShapePoly(system.file("shapes/sids.shp", package="maptools")[1],
#                      proj4string=CRS("+proj=longlat +datum=NAD27"))
# class(nc1)
#
# nc1 <- slot(polg.map, "polygons")[c(633,657,685)]
# class(nc1)
#
# SpatialPolygonsDataFrame(SpatialPolygons(nc1))
# class(SpatialPolygons(nc1))
#
#
# PlotCH()
# PlotPolg(1:1000, col=NA,border.polg = "grey80" ,add=TRUE)
# plot(unionSpatialPolygons(SpatialPolygons(nc1), rep(1,3)), col="steelblue", add=TRUE)
#
#
#
#
#
# lps <- coordinates(nc1)
# ID <- cut(lps[,1], quantile(lps[,1]), include.lowest=TRUE)
# reg4 <- unionSpatialPolygons(nc1, ID)
# row.names(reg4)
#
#
# summary(r.pois)
# ch <- coef(r.pois)
# ch <- ch[grep("hsa", names(ch))]
#
# PlotPolg()
#
# PlotPolg(d.hsagem$bfs_nr, col=Pal("RedToBlack", 61)[as.numeric(factor(d.hsagem$hsa))])
#
#
# d.kk <- merge(d.hsagem, data.frame(hsa=StrRight(names(ch), -3), coef=ch), by.x="hsa", by.y="hsa")
#
# PlotPolg(d.kk$bfs_nr, col=FindColor(x=d.kk$coef, cols=colorRampPalette(c("white", hred))(100))
#          , border.polg = Pal("Helsana")[5])
# AddLakes(col = "grey80")
# PlotPolg(d.kk$bfs_nr, col=FindColor(x=d.kk$coef, cols=colorRampPalette(c("white", hred))(100))
#          , border.polg = NA)
#
#
# col=c("palevioletred1")d.kk$coef
#
# sort(ch)
#
#
# col
# plot(Pal("Helsana"))
#
#
# coordinates(slot(polg.map, "polygons")[1:3])
#
#
# PlotCH()
# PlotPolg(100000, col=NA, border.polg = "grey80" ,add=TRUE)
# gemsp <- SpatialPolygons(slot(polg.map, "polygons"))
# plot(unionSpatialPolygons(gemsp, idx$hsa), col=Pal("RedToBlack", 62), add=TRUE)
# plot(unionSpatialPolygons(gemsp, idx$hsa), col=Pal("Helsana", alpha = 0.3), add=TRUE, lwd=1)
#
# AddLakes(col = "white", border = "grey50")
#
# length(gemsp)
# length(idx$hsa)
#
# zz <- unique(idx[, c(1,6)])
# idx[AllDuplicated(zz$Primary_ID),]
#
# SwissLocator()
# id <- c(rep(1, 1500), rep(2,852))
#
# d.plz[d.plz$bfs_nr==1322,]
# d.hsaplz[d.hsaplz$"NPA/PLZ" %in% d.plz[d.plz$bfs_nr==1322,"plz"],]
#
# d.hsagem[d.hsagem$bfs_nr %in% c(1507,1401,1322) & !is.na(d.hsagem$bfs_nr),]
#
# idx <- merge(polg.map@data, d.hsagem, by.x="Primary_ID", by.y="bfs_nr", all.x=TRUE, all.y=FALSE)
# mm <- PartitionBy(idx$hsa, idx$Primary_ID, order)
# idx <- idx[mm==1,]
# idx <- idx[order(as.numeric(as.character(idx$Primary_ID))),]
#
#
#


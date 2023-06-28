

# Utility functions


BfSStamp <- function(xy = NULL, year_n = getOption("bfsMaps.year", Year(Today())),
                     txt=NULL, cex=0.6, adj=c(1,0), ...){

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



SwissLocator <- function(){

  xy.sf <- sf::st_as_sf(data.frame(locator()), coords=c("x","y"))
  polg.map <- GetMap("polg.map")

  # we must apply some kind of proj4string since 2017 ...
  sf::st_crs(xy.sf) <- sf::st_crs(polg.map)
  xy.bfsnr <- sf::st_intersects(xy.sf, polg.map)

  bfs_id <- polg.map$id[unlist(xy.bfsnr)]

  note <- gettextf("\033[36m\nNote: ------\n  Found communities: %s.\n\n\033[39m", paste(xy.bfsnr, collapse = ", "))
  cat(note)

  res <- data.frame(xy.sf, SetNames(d.bfsrg[ match(bfs_id, d.bfsrg$gem_id),
                                     c("gem_id", "gemeinde_x", "bezk_x", "kt_x")],
                                    rownames=NULL))

  assign("found", res, envir = tkart)

  return(res)

}




Neighbours <- function(map, id = NULL){

  # nbs <- poly2nb(as(map, "SpatialPolygons"))
  # nbslist <- nb2listw(nbs, style="W", zero.policy=TRUE)$neighbours
  # attributes(nbslist) <- NULL
  # if (!is.null(id)) {
  #   # nbslist <- nbslist[[which(map@data[, 1] %in% id)]]
  #   nbslist <- nbslist[match(id, map@data[, 1])]
  #   nbslist <- lapply(nbslist, function(i) as.numeric(as.character(map@data[i, 1])))
  # }
  # return(nbslist)
  nbslist <- list()
  for(x in as.character(id)) {
    found <- sf::st_is_within_distance(sf::st_geometry(map)[map$id == x], map, dist=0)[[1]]
    nbslist[[x]] <- map[[1]][found]
    nbslist[[x]] <- nbslist[[x]][nbslist[[x]] %nin% id]
  }

  if(length(nbslist)==1)
    return(nbslist[[1]])
  else
    return(nbslist)

}




CombinePolygons <- function(map, g){

  # map containing the regions to be combined
  # grp the vector of the same length containing the groups

  # map <- as(unionSpatialPolygons(SpatialPolygons(unlist(slot(map, "polygons"))), g),
  #    "SpatialPolygonsDataFrame")
  #
  # # update metadata
  # map@data <- data.frame(id=1:length(ID <- sapply(map@polygons, slot, "ID")),
  #                             name=ID, ID2=ID)
  # return(map)

  sf::st_union(map[map$id %in% g, ]$geometry)

}




CombinePolg <- function(id, g, map=GetMap("polg.map")){

  d.grp <- merge(map$id, data.frame(id=id, g=g), by.x=1, by.y="id", all.x=TRUE)

  sf::st_sfc(sapply(sort(unique(g)),
                    function(g) CombinePolygons(map, d.grp[d.grp$g==g,1])))

}


CombineKant <- function(id, g, map=GetMap("kant.map")){

  # define groups and then sort for the polynoms
  d.grp <- merge(map$id, data.frame(id=id, g=g), by.x=1, by.y="id", all.x=TRUE)

  # the order must be the same as in the map
  sf::st_sfc(sapply(sort(unique(g)),
                    function(g) CombinePolygons(map, d.grp[d.grp$g==g,1])))

}


# RemoveHoles <- function (x) {
#
#   if (!any(which(utils::installed.packages()[, 1] %in% "maptools")))
#     stop("please install maptools package before running this function")
#
#   xp <- slot(x, "polygons")
#
#   holes <- lapply(xp, function(x) sapply(methods::slot(x, "Polygons"),
#                                          methods::slot, "hole"))
#   res <- lapply(1:length(xp),
#                 function(i) methods::slot(xp[[i]], "Polygons")[!holes[[i]]])
#
#   IDs <- row.names(x)
#
#   x.fill <- sp::SpatialPolygons(lapply(1:length(res),
#                                        function(i) sp::Polygons(res[[i]],
#                                                   ID = IDs[i])), proj4string = sp::CRS(sp::proj4string(x)))
#   methods::slot(x.fill, "polygons") <- lapply(methods::slot(x.fill, "polygons"),
#                                               maptools::checkPolygonsHoles)
#   methods::slot(x.fill, "polygons") <- lapply(methods::slot(x.fill, "polygons"),
#                                               "comment<-", NULL)
#   pids <- sapply(methods::slot(x.fill, "polygons"),
#                  function(x) methods::slot(x,"ID"))
#
#   x.fill <- sp::SpatialPolygonsDataFrame(x.fill,
#                                          data.frame(row.names = pids, ID = 1:length(pids)))
#
#   return(x.fill)
#
# }




DownloadBfSMaps <- function(url="https://dam-api.bfs.admin.ch/hub/api/dam/assets/21245514/master",
                            path=paste0(path.expand("~"), "/MapData")) {

  # 2020: https://www.bfs.admin.ch/bfsstatic/dam/assets/11927607/master
  # 2022: https://dam-api.bfs.admin.ch/hub/api/dam/assets/21245514/master

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



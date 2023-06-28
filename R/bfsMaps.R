
# ****************************************************************************
#
# Projekt:	  bfsMaps.r
#
# Zweck:	    Convenience-wrapper for plotting CH maps
#
# Autor:	    Andri Signorell
# Version:	  2.0.0
#
# Datum:      2023-04-30  rewritten for package sf
#             2020-12-20  undergone some garbage collection
#             2011-01-18  goes back to a first version in 2011
#
#
# ****************************************************************************


# ToDo:
# * implement Stamp() as option, default text as defined


# globalVariables
utils::globalVariables(c("d.bfsrg","tkart","kt"))


AddLakes <- function(categ=1:2, col="lightskyblue1", border="lightskyblue3", lwd=1, ...) {

  # Add lakes to an already plotted map
  # categ defines the categories, of which there are 2 for the lakes.
  # (the lakes can be found in two separate shape files *.shp)

  # Example:		plot(ch.map)
  #			        AddLakes(categ=1)	  # add lakes of category 1 (the biggest ones)

  ch.lakes <- GetMap(gettextf("see%s.map", 1:2))

  for( i in categ ) plot(ch.lakes[[i]]$geometry, col = col, border=border,
                         lwd = lwd, add = TRUE, ...)
}


AddRivers <- function(categ=1:5, col="lightskyblue3", ...) {

  # Add rivers to an already plotted map
  # categ defines the categories, of which there are 3 for the rivers
  # (the lakes can be found in two separate shape files *.shp)

  # Example:		plot(ch.map)
  #			        AddRivers(categ=1:3)	# add lakes of category 1 (the biggest ones)

  ch.rivers <- GetMap(gettextf("fluss%s.map", 1:5))

  for( i in categ ) plot(ch.rivers[[i]]$geometry, col = col, add = TRUE, ... )

}


AddWaters <- function(lakes=1, rivers=1:5, col=NULL,
                      border="lightskyblue3", lwd=1, ...) {

  if(is.null(col))
    col <- ColToOpaque(SetAlpha(col=border, alpha=0.6))

  AddRivers(categ=rivers, col=border, lwd=lwd, ...)
  AddLakes(categ=lakes, col=col, border=border, lwd=lwd, ...)
}



GetMap <- function(name_x,
                   basedir = getOption("bfsMaps.base",
                                       default = file.path(find.package("bfsMaps"), "extdata"))) {

  LoadMap <- function(name_x, basedir) {

    if(getOption("debug", default = FALSE))
      cat(gettextf("Used basedir: %s\n", basedir))

    # load map named name_x
    fn <- gettextf("%s/maps.csv", basedir)

    # check if file exists in basedir,
    # if it does not check the packages extdata directory
    if(!file.exists(fn))
      fn <- gettextf("%s/maps.csv", file.path(find.package("bfsMaps"), "extdata"))

    # stop if file was not found neither in basedir nor in extdata
    if(!file.exists(fn))
      stop(gettextf("Maps information file could not be found as %s. \nCheck location!", fn))

    maps <- read.csv(file = fn, header = TRUE, sep=";", stringsAsFactors = FALSE)

    if(is.numeric(name_x)){
      fn <- maps$path[name_x]
      name_x <- paste0("m_", name_x)

    } else {

      fn <- character(length(name_x))
      for(i in 1:length(fn)){
        #        jj <- grepl(name_x[i], maps$name_x)
        # we need exactly matching file names here, not similar ones
        jj <- match(name_x[i], maps$name_x)

        if(all(is.na(jj)))
          stop(gettextf("No entry in maps.csv for shortname:  %s \n", name_x[i]))

        # at least on file exists, use the last if there are several
        fn[i] <- maps$path[tail(jj, 1)]
      }
    }

    fn <- gettextf("%s/%s", basedir, fn)
    if(!file.exists(fn))
      stop(gettextf("Map file could not be found as:  %s \n", fn))

    opt <- options(stringsAsFactors = FALSE,
                   rgdal_show_exportToProj4_warnings="none")
    # do not display these warnings
    # https://cran.r-project.org/web/packages/rgdal/vignettes/PROJ6_GDAL3.html
    on.exit(options(opt))

    map <- sf::st_read(fn, quiet = TRUE)


    if(name_x %in% c("kant.map", "kantvf.map") ){
      map$idx <- c("ZH","BE","LU","UR","SZ","OW","NW","GL","ZG","FR","SO","BS","BL","SH",
                   "AR","AI","SG","GR","AG","TG","TI","VD","VS","NE","GE","JU")
    }

    return(map)

  }


  if(identical(name_x, NA)){
    basedir  <- getOption("bfsMaps.base",
                          default = file.path(find.package("bfsMaps"), "extdata"))
    fn <- gettextf("%s/maps.csv", basedir)
    View(read.csv(file = fn, header = TRUE, sep=";", stringsAsFactors = FALSE))

    note <- gettextf("\033[36m\nNote: ------\n  Maps information file is located in %s.\n\n\033[39m", fn)
    message(note)

    res <- NULL

  } else {

    res <- lapply(name_x, LoadMap, basedir)

    if(length(res)==1)
      res <- res[[1]]

  }

  return(res)
}




PlotMap <- function(map_x, id=NULL, col=NA, pbg="white", main="",
                    vf=FALSE, border="grey", lwd=1, labels = NULL, tmtxt=TRUE,
                    add=FALSE, ...) {

  # check for loaded maps
  if(is.character(map_x)){
    if (vf)
      map_x <- paste0(strsplit(map_x, "\\.")[[1]], c("vf", ""), collapse = ".")
    map <- GetMap(map_x)

  } else {
    map <- map_x
  }

  if(is.null(id))
    id <- map$id

  # # recycle color
  col <- rep(col, length.out=length(id))
  border <- rep(border, length.out=length(id))

  # suggested window: windows(width=4/3*7)

  acol <- rep(ifelse(add, NA, pbg), length(map$id))
  bcol <- rep(ifelse(add, NA, "grey"), length(map$id))



  # area can be defined by nr (special BfS order) or by 2char-abbreviation
  if (is.numeric(id)) {
    # old (<2013): idx <- match(kt, map@data$ID0)
    idx <- match(id, map[[1]])
  } else {

    idx <- match(id, map[[1]])
  }

  if(anyNA(idx))
    warning( gettextf( "The following entities do not exist:\n    %s", paste( id[is.na(idx)],collapse=", ")) )

  # # ktcol <- rep(ifelse(add, NA, col), nrow(map@data))
  acol[idx[!is.na(idx)]] <- col[!is.na(idx)]
  bcol[idx[!is.na(idx)]] <- border[!is.na(idx)]

  # set margin default only if it has not been changed by the user either
  # by par(mar or as argument par=...)

  # if(all(InDots(..., arg = "mar", default = par("mar")) == .pardefault$mar )){
  #
  #   Mar(bottom = 2.5, left = 1, top = 2, right = 1)
  # }

  plot(map$geometry, col=acol, border=bcol, lwd=lwd, add=add, ... )

  if(is.character(map_x)){
    # get the point map with the same name
    map_pnt <- GetMap(gsub(".map", ".pnt", map_x, fixed = TRUE))

    xy <- sf::st_coordinates(map_pnt$geometry[idx])

  } else {
    # user defined map, extract midpoints from sp object
    # xy <- SetNames(t(sapply(map@polygons, slot, "labpt")),
    #                colnames=c("coords.x1","coords.x2"))[idx, , drop = FALSE]
    xy <- sf::st_centroid(map)
  }

  if(!is.null(labels) && !identical(labels, FALSE)){
    if(identical(labels, TRUE))
      labels <- id
    text(x = xy, labels = labels)
  }


  if(tmtxt & !add)  BfSStamp()

  if(!add) title( main = main )

  return(xy.coords(x = xy[,1], y = xy[,2], xlab = "x", ylab = "y"))

}




PlotCH <- function(col=NA, main="", col.vf=NA,
                   border="grey", border.vf=NA,
                   lwd=1, tmtxt=TRUE, add=FALSE, ...) {

  # # plot CH-border
  # ch <- CombineKant(1:26, g = 1,  map = GetMap("kant.map"))
  # # delete holes (lakes)
  # h <- sapply(ch@polygons[[1]]@Polygons, slot, "hole")
  # ch@polygons[[1]]@Polygons[h] <- NULL

  plot(GetMap("ch.map")$geometry, col=col, lwd=lwd, border=border, add=add, ...)

  if(!is.na(col.vf)){
    plot(GetMap("chvf.map")$geometry, col=col.vf, border=border.vf, add=TRUE, ...)
  }

  if(tmtxt & !add)  BfSStamp()

  if(!add) title( main = main )

  # return CH centroid
  invisible(xy.coords(x = 2660623, y = 1183997, xlab = "x", ylab = "y"))

}




PlotBfsMap <- function(map_x, id=NULL, col="white", pbg="white", main="", border="grey", lwd=1,
                     col.vf=NA, border.vf=NA,
                     labels=NULL,
                     tmtxt=TRUE, add=FALSE, ...) {

  vf <- ( !(identical(col.vf, NA) & identical(border.vf, NA)) )

  xy <- PlotMap(map_x=map_x, id = id, col = col, pbg=pbg, main=main, vf=FALSE, border=border,
                lwd=lwd, tmtxt=tmtxt, add=add, labels=if(!vf) labels else NULL, ...)

  if(vf)
    xy <- PlotMap(map_x=map_x, id = id, col = col.vf, pbg=pbg, main=main, vf=TRUE, border=border.vf,
            lwd=lwd, tmtxt=FALSE, add=TRUE, labels=labels, ...)

  invisible(xy)

}



PlotKant <- function(id=NULL, col=NA, pbg="white", main="", border="grey", lwd=1,
                       col.vf=NA, border.vf=NA,
                       labels=NULL,
                       tmtxt=TRUE, add=FALSE, map_x="kant.map", ...) {

  if(is.null(id)) id <- 1:26

  if(identical(labels, TRUE))  labels <- id

  if(all(grepl("[A-Z]{2}", id)))  id <- match(id, bfsMaps::kt)

  PlotBfsMap(map_x=map_x, id=id, col=col, pbg=pbg, main=main, border=border, lwd=lwd,
                         col.vf=col.vf, border.vf=border.vf,
                         labels=labels,
                         tmtxt=tmtxt, add=add, ...)
}


PlotGreg <- function(id=NULL, col=NA, pbg="white", main="", border="grey", lwd=1,
                     col.vf=NA,  border.vf=NA,
                     labels=NULL,
                     tmtxt=TRUE, add=FALSE, map_x="greg.map", ...) {

  PlotBfsMap(map_x=map_x, id=id, col=col, pbg=pbg, main=main, border=border, lwd=lwd,
             col.vf=col.vf, border.vf=border.vf,
             labels=labels,
             tmtxt=tmtxt, add=add, ...)
}


PlotBezk <- function(id=NULL, col=NA, pbg="white", main="", border="grey", lwd=1,
                     col.vf=NA, border.vf=NA,
                     labels=NULL,
                     tmtxt=TRUE, add=FALSE, map_x="bezk.map", ...) {

  PlotBfsMap(map_x=map_x, id=id, col=col, pbg=pbg, main=main, border=border, lwd=lwd,
             col.vf=col.vf, border.vf=border.vf,
             labels=labels,
             tmtxt=tmtxt, add=add, ...)
}

PlotPolg <- function(id=NULL, col=NA, pbg="white", main="", border="grey", lwd=1,
                     col.vf=NA, border.vf=NA,
                     labels=NULL,
                     tmtxt=TRUE, add=FALSE, map_x="polg.map", ...) {

  PlotBfsMap(map_x=map_x, id=id, col=col, pbg=pbg, main=main, border=border, lwd=lwd,
             col.vf=col.vf, border.vf=border.vf,
             labels=labels,
             tmtxt=tmtxt, add=add, ...)
}



PlotMSRe <- function(id=NULL, col=NA, pbg="white", main="", border="grey", lwd=1,
                     col.vf=NA, border.vf=NA,
                     labels=NULL,
                     tmtxt=TRUE, add=FALSE, map_x="msre.map", ...) {

  PlotBfsMap(map_x=map_x, id=id, col=col, pbg=pbg, main=main, border=border, lwd=lwd,
             col.vf=col.vf, border.vf=border.vf,
             labels=labels,
             tmtxt=tmtxt, add=add, ...)
}




PlotPremReg <- function(id=NULL, col=NA, pbg="white", main="", border="grey", lwd=1,
                        labels=NULL,
                        tmtxt=TRUE, add=FALSE, ...) {


  # define premium regions based on d.bfsrg
  polg.map <- GetMap("polg.map")
  polg.map <- merge(polg.map, d.bfsrg[, c("gem_id", "preg_x")],
                    by.x="id", by.y="gem_id")

  preg.map <- aggregate(polg.map, by=list(polg.map$preg_x), FUN=length)

  if(is.null(id)) id <- preg.map[[1]]

  if(identical(labels, TRUE))  labels <- preg.map[[1]]

  if(!add) PlotCH(col=NA, main=main)

  plot(preg.map$geometry[match(id, preg.map[[1]])], col=col,
       border=border, lwd=lwd,
       labels=labels,
       add=TRUE, ...)

}




PlotMapDot <- function(mar=c(5.1,4.1,0,1), oma=c(0,0,5,0), widths = c(2, 0.8)) {

  oldpar <- par(mar = mar, oma = oma)
  # on.exit(par(oldpar))

  layout(matrix(c(1, 2), nrow = 1, byrow = TRUE), widths=widths, TRUE)

}


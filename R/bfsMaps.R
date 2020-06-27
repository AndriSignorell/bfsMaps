


# ****************************************************************************
#
# Projekt:	      bfsMaps.r
#
# Zweck:	        Convenience-wrapper fuer die Erzeugung von CH-Landkarten
#			            Naehere Infos zur Nomenklatur:
#			            http://www.bfs.admin.ch/bfs/portal/de/index/infothek/nomenklaturen/blank/blank/raum_glied/01.html
#
# Autor:	        Andri Signorell
# Version:	      17.1
#
# Datum:
#     01.12.2017  Neugestaltung Kartenablage
#                 neue Funktionen CombinePolygons, CombinePolg, CombineKant, BfSStamp
#     01.05.2015  neue Funktion Neighbours
#     01.03.2014  Aktualisierung auf Kartenmaterial und Daten 2014
#                 Neu: Dependent: DescTools, damit FindColor und ColorLegend weg
#                 Check nach CRAN Norm
#			19.03.2013  Aktualisierung auf Kartenmaterial und Daten 2013
#			            Ersatz ColorStrip durch ColorLegend und GetColor durch FindColor
#     25.09.2012  PlotCH hinzugefuegt
#     13.06.2012  Beschriftungsposition im ColorStrip am Rand des Strips und nicht in der Mitte
#     08.05.2012  Korrektur Bug in GetColor (NA anstatt 0 bei findInterval)
#     17.12.2011  Ergaengzung plot.mapdot
#     09.08.2011  Korrekturen
#			25.03.2011	fuer die Erzeugung des Package
#     18.01.2011  erzeugt
#
#
# ****************************************************************************


# ToDo:
# * Karten in data ablegen
# * Bug in Neighbours beheben --> Beat
# * HelpFile CombinePolygons mit Beispiel HSA
# * Dokumentation nachfuehren "CombinePolygons"
# * neue BfS-Zahlen Gemeinden
# * Stamp als option implementieren, default Text wie unten



# globalVariables(c("kt","ch.lakes","ch.rivers", "ch.map", "bezk.map", "bezkvf.map", "d.bfsrg", "greg.map",
#                   "gregvf.map", "kant.map", "kantvf.map", "msreg.map", "msregvf.map", "polg.map", "polgvf.map"))

# kt <-  as.character(unique(d.bfsrg$kt_x))

# kt <- c("ZH", "BE", "LU", "UR", "SZ", "OW", "NW", "GL", "ZG", "FR",
#   "SO", "BS", "BL", "SH", "AR", "AI", "SG", "GR", "AG", "TG", "TI",
#   "VD", "VS", "NE", "GE", "JU")




utils::globalVariables(c("d.bfsrg","tkart","kt"))


AddLakes <- function(categ=1:2, col="lightskyblue1", border="lightskyblue3", lwd=1, ...) {



  # Fuegt Seen einer CH-Karte hinzu
  # categ definiert die Kategorien, wovon es 2 (von den grossen zu den kleinen) gibt
  # (die Seen sind nach Kategorie in unterschiedlichen shp-Files abgelegt)

  # Bsp:		plot(ch.map)
  #			AddLakes( categ=1 )	# Seen der Kategorie 1 zuf?gen

  ch.lakes <- RequireMap(gettextf("see%s.map", 1:2))

  for( i in categ ) plot(ch.lakes[[i]], col = col, border=border,
                         lwd = lwd, add = TRUE, ...)
}


AddRivers <- function(categ=1:5, col="lightskyblue3", ...) {

  # Fuegt Fluesse einer CH-Karte hinzu
  # categ definiert die Kategorien, wovon es 5 (von den grossen zu den kleinen) gibt
  # (die Fluesse sind nach Kategorie in unterschiedlichen shp-Files abgelegt)

  # Bsp:		plot(ch.map)
  #			AddRivers( categ=c(1:3) )	# Nur Fluesse der categ 1:3 zufuegen

  ch.rivers <- RequireMap(gettextf("fluss%s.map", 1:5))

  for( i in categ ) plot(ch.rivers[[i]], col = col, add = TRUE, ... )

}


AddWaters <- function(lakes=1, rivers=1:5, col=NULL,
                      border="lightskyblue3", lwd=1, ...) {

  if(is.null(col))
    col <- ColToOpaque(SetAlpha(col=border, alpha=0.6))

  AddRivers(categ=rivers, col=border, lwd=lwd, ...)
  AddLakes(categ=lakes, col=col, border=border, lwd=lwd, ...)
}



RequireMap <- function(name_x, verbose=FALSE){

  GetMap <- function(name_x) {

    if(identical(name_x, NA)){
      basedir  <- getOption("bfsMaps.base",
                            default = file.path(find.package("bfsMaps"), "extdata"))
      fn <- gettextf("%s/maps.csv", basedir)
      View(read.csv(file = fn, header = TRUE, sep=";", stringsAsFactors = FALSE))

      note <- gettextf("\033[36m\nNote: ------\n  Maps information file is located in %s.\n\n\033[39m", fn)
      message(note)

      res <- NULL

    } else {

      if(is.numeric(name_x))
        res <- LoadMap(name_x)

      else {
        if(!exists(name_x, envir=tkart))
          LoadMap(name_x)

        res <- get(name_x, tkart)
      }
    }

    return(res)
  }

  if(missing(name_x))
    name_x <- NA

  res <- sapply(name_x, GetMap)

  if(length(res)==1)
    res <- res[[1]]

  if(verbose)
    return(res)
  else
    invisible(res)

}



LoadMap <- function(name_x,
                    basedir=getOption("bfsMaps.base",
                                      default = file.path(find.package("bfsMaps"), "extdata"))) {

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
        jj <- grepl(name_x[i], maps$name_x)
        if(all(jj==FALSE))
          stop(gettextf("No entry in maps.csv for shortname:  %s \n", name_x[i]))

        # at least on file exists, use the last if there are several
        fn[i] <- maps$path[tail(which(jj), 1)]
      }
    }

  fn <- gettextf("%s/%s", basedir, fn)
  if(!file.exists(fn))
    stop(gettextf("Map file could not be found as:  %s \n", fn))

  opt <- options(stringsAsFactors = FALSE)
  on.exit(options(opt))

  map <- rgdal::readOGR(fn, verbose = FALSE)

  # unclear if still needed:
      # Encoding(levels(map[[i]]@data[, 1])) <- "latin1"
      # levels(map[[i]]@data[,1]) <- iconv(levels(map[[i]]@data[, 1]), "latin1", "UTF-8")
      # Encoding(levels(map[[i]]@data[, 2])) <- "latin1"
      # levels(map[[i]]@data[,2]) <- iconv(levels(map[[i]]@data[, 2]), "latin1", "UTF-8")

  if(name_x %in% c("kant.map", "kantvf.map") )
  map@data$ID2 <- c("ZH","BE","LU","UR","SZ","OW","NW","GL","ZG","FR","SO","BS","BL","SH",
                         "AR","AI","SG","GR","AG","TG","TI","VD","VS","NE","GE","JU")

  assign(name_x, map, envir = tkart)

  return(map)

}



PlotMap <- function(map_x, id=NULL, col=NA, pbg="white", main="", vf=FALSE, border="grey", lwd=1,
                    labels = NULL,
                    tmtxt=TRUE, add=FALSE, ...) {

  # check for loaded maps
  if(vf)
    map_x <- paste0(strsplit(map_x, "\\.")[[1]], c("vf", ""), collapse=".")

  map <- RequireMap(map_x)

  if(is.null(id))
    # will be a factor else... we can't use stringsAsFactors when reading (!)
    # id <- as.numeric(as.character(map@data$ID0))
    # Bezirke und Gemeinden have no ID0... :-(
    id <- as.numeric(as.character(map@data[, 1]))

  # recycle color
  col <- rep(col, length.out=length(id))
  border <- rep(border, length.out=length(id))

  # suggested window: windows(width=4/3*7)

  acol <- rep(ifelse(add, NA, pbg), nrow(map@data))
  bcol <- rep(ifelse(add, NA, "grey"), nrow(map@data))
  # area can be defined by nr (special BfS order) or by 2char-abbreviation
  if (is.numeric(id)) {
    # old (<2013): idx <- match(kt, map@data$ID0)
    idx <- match(id, map@data[,1])
  } else {
    idx <- match(id, map@data$ID2)
  }

  if(anyNA(idx))
    warning( gettextf( "The following entities do not exist:\n    %s", paste( id[is.na(idx)],collapse=", ")) )

  # ktcol <- rep(ifelse(add, NA, col), nrow(map@data))
  acol[idx[!is.na(idx)]] <- col[!is.na(idx)]
  bcol[idx[!is.na(idx)]] <- border[!is.na(idx)]

  # set margin default only if it has not been changed by the user either
  # by par(mar or as argument par=...)
  if(all(InDots(..., arg = "mar", default = par("mar")) == .pardefault$mar )){
    Mar(bottom = 2.5, left = 1, top = 2, right = 1)
  }

  plot(map, col=acol, pbg=pbg, border=bcol, lwd=lwd, add=add, ... )

  # get the point map with the same name
  map_pnt <- RequireMap(gsub(".map", ".pnt", map_x, fixed = TRUE))

  xy <- map_pnt@coords[idx,, drop=FALSE]

  if(!is.null(labels)){
    if(identical(labels, TRUE))
      labels <- id
    text(x = xy[,1], y = xy[,2], labels = labels)
  }


  if(tmtxt & !add)  BfSStamp()

  if(!add) title( main = main )

  return(xy.coords(x = xy[,1], y = xy[,2], xlab = "x", ylab = "y"))

}




PlotCH <- function(col="grey90", pbg="white", main="", col.vf=NA,
                   border="grey", border.vf="grey",
                   lwd=1, tmtxt=TRUE, add=FALSE, ...) {

  # plot CH-border
  ch <- CombineKant(1:26, g = 1,  map = RequireMap("kant.map"))
  # delete holes (lakes)
  h <- sapply(ch@polygons[[1]]@Polygons, slot, "hole")
  ch@polygons[[1]]@Polygons[h] <- NULL

  plot(ch, col=col, pbg=pbg, lwd=lwd, border=border, add=add, ...)

  if(!is.na(col.vf)){
    plot(RequireMap("kantvf.map"), col=col.vf, border=border.vf, add=TRUE, ...)
  }

  if(tmtxt & !add)  BfSStamp()

  if(!add) title( main = main )

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



PlotMapDot <- function(mar=c(5.1,4.1,0,1), oma=c(0,0,5,0), widths = c(2, 0.8)) {

  oldpar <- par(mar = mar, oma = oma)
  # on.exit(par(oldpar))

  layout(matrix(c(1, 2), nrow = 1, byrow = TRUE), widths=widths, TRUE)

}

.InternDummyFun <- function() {
  # this is only to use a function from rgeos to be allowed to add the package to
  # the depends list
  gLength(readWKT("POINT(1 1)"))
}

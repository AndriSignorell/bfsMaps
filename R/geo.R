
# geocoding


.Coords <- function(adr){

  # https://api3.geo.admin.ch/services/sdiservices.html#find

  # sr ... The spatial reference. Supported values: 21781 (LV03), 2056 (LV95), 4326 (WGS84) and 3857 (Web Pseudo-Mercator). Defaults to “21781”.

  # Here is a list of possible origins sorted in ascending ranking order:
  #   zipcode (ch.swisstopo-vd.ortschaftenverzeichnis_plz)
  #   gg25 (ch.swisstopo.swissboundaries3d-gemeinde-flaeche.fill)
  #   district (ch.swisstopo.swissboundaries3d-bezirk-flaeche.fill)
  #   kantone (ch.swisstopo.swissboundaries3d-kanton-flaeche.fill)
  #   gazetteer (ch.swisstopo.swissnames3d, ch.bav.haltestellen-oev)
  #   address (ch.bfs.gebaeude_wohnungs_register with EGID or use prefix ‘addresse’, ‘adresse’, ‘indirizzo’, ‘address’ without EGID)
  #   parcel (use prefix “parcel”, “parzelle”, “parcelle” or “parcella” in your requests to filter out other origins)


  # https://api3.geo.admin.ch/rest/services/api/SearchServer?searchText=Waldwis 4 Madetswil&origins=address&type=locations&sr=2056
  url <- gettextf("https://api3.geo.admin.ch/rest/services/api/SearchServer?searchText=%s&origins=address&type=locations&sr=2056",
                  gsub(" ", "%20", adr))

  out <- httr::content(httr::GET(url))

  if(length(out$results) == 0)
    res <- data.frame(detail=adr, lat=NA_real_, lon=NA_real_, x=NA_real_, y=NA_real_)
  else
    res <- as.data.frame(out$results[[1]]$attrs[ c("detail","lat","lon","x","y") ] )


  attr(res, "full") <- out
  return(res)

}


GeoCode <- function(x){
  do.call(rbind, lapply(x, .Coords))
}


# example:
#
# adr <- c("Quaderaweg 3 Trimmis","chemin des Fraisiers 19 Grand-Lancy",
#          "Im Spitzacker 21 Basel","Rue du Moleson 8 Broc")
#
# geocode(adr)
# attr(coords(adr="Quaderaweg 3 Trimmis"), "full")




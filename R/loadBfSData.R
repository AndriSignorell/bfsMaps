#
# # x_ <- grep("_x", colnames(d.bfsrg), v=T)
# # sapply(d.bfsrg[, x_], unique)
# #
# # for(x in grep("_x", colnames(d.bfsrg), v=T)){
# #   d.bfsrg[,x] <- iconv(d.bfsrg[,x], to="UTF-8")
# # }
# # Load Raumregionen
# # https://www.agvchapp.bfs.admin.ch/de/typologies/query
#
# library(DescTools)
#
# fn <- "C:/Users/andri/Documents/MapData/2023_GEOM_TK/Raumgliederungen.xlsx"
# d.bfsrg <- as.data.frame(readxl::read_xlsx(path = fn, sheet = "Daten", skip = 2))
# colnames(d.bfsrg) <- c("gem_id","gemeinde_x","kt_c","kt_x","bezk_c","bezk_x",
#                        "greg_c","aggl_grp_c", "aggl_c",  "stadt_char_c",
#                        "stadtland_c","gem_typ9_c","gem_typ25_c",
#                        "sprgeb_c","degurba_c")  # rest gibt's nicht mehr ab 1.1.2022: ,"msre_c","msre_typ_c")
#
# for(x in grep("_c$", colnames(d.bfsrg), v=T))
#   d.bfsrg[,x] <- as.integer(d.bfsrg[,x])
#
#
# sheets <- c("CH1+CL_REGCH+2011.2","CH1+CL_AGGLGK+2000.0","CH1+CL_AGGL+2012.0","CH1+CL_RSTCKT+2014.2",
#             "CH1+CL_STALAN+2012.1","CH1+CL_GDET9+2012.1","CH1+CL_GDET25+2012.1",
#             "CH1+CL_DEGURB+2017.1","CH1+CL_SPRGEB+2011.2") # ,"CH1+CL_MSREG+2011.2","CH1+CL_TYPMSR+2011.2")
# vars <- c("greg_c","aggl_grp_c", "aggl_c", "stadt_char_c","stadtland_c","gem_typ9_c","gem_typ25_c",
#           "degurba_c", "sprgeb_c") # ,"msre_c","msre_typ_c")
#
# lst <- list()
# for(sheet in sheets){
#   lst[[sheet]] <- SetNames(as.data.frame(readxl::read_xlsx(path = fn, sheet = sheet, skip = 1)),
#                            colnames=readxl::read_xlsx(path = fn, sheet = sheets[1])[1,])
# }
#
#
# for(i in 1:length(vars)){
#   # the codes
#   codes <- lst[[i]]
#   var_c <- vars[i]
#   var_x <- gsub("_c$", "_x", var_c)
#   d.bfsrg[, var_x] <- factor(d.bfsrg[, var_c])
#   levels(d.bfsrg[, var_x]) <- codes[, 2][match(levels(d.bfsrg[, var_x]), codes[, 1])]
# }
#
# attr(d.bfsrg, "call") <- NULL
#
# d.bfsrg$gemeinde_x <- factor(d.bfsrg$gemeinde_x,
#                              levels = Sort(unique(d.bfsrg[, c("gem_id","gemeinde_x")]))[,2])
#
# d.bfsrg$kt_bez_x <- factor(d.bfsrg$kt_x, levels=Sort(unique(d.bfsrg[,c("kt_c","kt_x")]))[,2])
# levels(d.bfsrg$kt_bez_x) <- c("Zürich","Bern","Luzern","Uri","Schwyz","Obwalden","Nidwalden","Glarus",
#                               "Zug","Fribourg","Solothurn","Basel-Stadt","Basel-Landschaft","Schaffhausen",
#                               "Appenzell Ausserrhoden","Appenzell Innerrhoden","St. Gallen","Graubünden",
#                               "Aargau","Thurgau","Ticino","Vaud","Wallis","Neuchâtel","Genève","Jura")
#
# d.bfsrg$kt_x <- factor(d.bfsrg$kt_x,
#                        levels=Sort(unique(d.bfsrg[,c("kt_c","kt_x")]))[,2])
#
# d.bfsrg$bezk_x <- factor(d.bfsrg$bezk_x,
#                        levels=Sort(unique(d.bfsrg[,c("bezk_c","bezk_x")]))[,2])
#
# d.bfsrg <- d.bfsrg[,c("gem_id","gemeinde_x","kt_c","kt_x","kt_bez_x","bezk_c","bezk_x",
#                    "greg_c","greg_x","aggl_c","aggl_x","aggl_grp_c","aggl_grp_x","stadt_char_c","stadt_char_x",
#                    "stadtland_c", "stadtland_x",
#                    "gem_typ9_c","gem_typ9_x","gem_typ25_c","gem_typ25_x",
#                    "degurba_c","degurba_x","sprgeb_c","sprgeb_x"
#                    )]
#
# # ,"msre_c","msre_x","msre_typ_c","msre_typ_x"
#
# # d.bfsrg$greg_x
# # table(d.bfsrg$greg_c, d.bfsrg$greg_x)
#
#
# # d.bfsrg$msre_c
# # RequireMap("msre.map")
# # tkart$msre.map@data
#
#
# # https://www.priminfo.admin.ch/de/regionen
# # neu: https://www.priminfo.admin.ch/de/downloads/aktuell
#
# fn <- "C:/Users/andri/Documents/MapData/2023_GEOM_TK/praemienregionen.xlsx"
#
# d.preg <- unique(as.data.frame(readxl::read_xlsx(path = fn, sheet = "A_COM", skip = 5))[, c(1,4)])
# colnames(d.preg) <- c("gem_id", "preg_c")
#
#
# # expectec: 0 rows
# d.preg[AllDuplicated(d.preg$gem_id), ]
#
# d.bfsrg <- merge(d.bfsrg, d.preg, by="gem_id", all.x=TRUE)
#
# d.bfsrg[d.bfsrg$gem_id == 291, "preg_c"] <- 3
# d.bfsrg[d.bfsrg$gem_id == 4186, "preg_c"] <- 0
# d.bfsrg[d.bfsrg$gem_id == 6811, "preg_c"] <- 0
#
# d.bfsrg$preg_x <- paste0(d.bfsrg$kt_x, d.bfsrg$preg_c)
# d.bfsrg$preg_x <- factor(paste0(d.bfsrg$kt_x, d.bfsrg$preg_c),
#                          levels = Sort(unique(d.bfsrg[, c("kt_c","preg_c","preg_x")]))$preg_x)
#
#
# Abstract(d.bfsrg)
#
# save(d.bfsrg, file="./data/d.bfsrg.rda")
#
# ## **** Check-Code
#
#
# library(bfsMaps)
# PlotPremReg("ZH3", "blue")
#
# PlotPol
#
# preg <- unique(d.bfsrg$preg_x)
#
# cols <- c("white","darkolivegreen3", "darkolivegreen2", "darkolivegreen1")
#
# PlotPremReg(preg, cols[ZeroIfNA(StrVal(preg, as.numeric = T))+1], labels =NA)
# PlotKant(add=TRUE, border="grey55")
# AddLakes()
#
# PlotPolg(d.bfsrg[is.na(d.bfsrg$preg_c), "gem_id"], "blue", add=TRUE)
#
# tkart$polg.map@data
#
# d.bfsrg[d.bfsrg$id %nin% tkart$polg.map@data$id, ]
# tkart$polg.map@data[as.numeric(tkart$polg.map@data$id) %nin% d.bfsrg$gem_id, ]
#
# d.bfsrg[d.bfsrg$gem_id %nin% d.preg$gem_id, ]
#
# fertig lustig...!
#
#
#
#
#
#

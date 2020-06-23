

# Load Raumregionen
# https://www.agvchapp.bfs.admin.ch/de/typologies/query


# library(DescTools)
#
# d.bfsrg <- XLGetRange(file = "C:\\Users\\andri\\Desktop\\2019_THK_PRO\\Raumgliederungen20.xlsx", sheet = "Daten",
#      range = c("A4:Q2205"),
#      as.data.frame = TRUE, header = FALSE, stringsAsFactors = FALSE)
#
#
# colnames(d.bfsrg) <- c("gem_id","gemeinde_x","kt_c","kt_x","bezk_c","bezk_x","greg_c",
#                        "aggl_c", "aggl_grp_c","stadt_char_c","stadtland_c","gem_typ9_c","gem_typ25_c",
#                        "degurba_c","sprgeb_c","msre_c","msre_typ_c")
#
# for(x in grep("_c$", colnames(d.bfsrg), v=T))
#   d.bfsrg[,x] <- as.integer(d.bfsrg[,x])
#
#
# sheets <- c("CH1+CL_REGCH+2011.2","CH1+CL_AGGL+2012.0","CH1+CL_AGGLGK+2000.0","CH1+CL_RSTCKT+2014.2",
#             "CH1+CL_STALAN+2012.1","CH1+CL_GDET9+2012.1","CH1+CL_GDET25+2012.1","CH1+CL_DEGURB+2017.1",
#             "CH1+CL_SPRGEB+2011.2","CH1+CL_MSREG+2011.2","CH1+CL_TYPMSR+2011.2")
# vars <- c("greg_c","aggl_c", "aggl_grp_c","stadt_char_c","stadtland_c","gem_typ9_c","gem_typ25_c",
#           "degurba_c","sprgeb_c","msre_c","msre_typ_c")
#
# lst <- list()
# for(sheet in sheets){
#   lst[[sheet]] <- XLGetRange(file = "C:\\Users\\andri\\Desktop\\2019_THK_PRO\\Raumgliederungen20.xlsx",
#                     sheet = sheet,
#                     range = XLCurrReg("A1"),
#                     as.data.frame = TRUE, header = FALSE, stringsAsFactors = FALSE)
# }
#
#
#
# for(i in 1:length(vars)){
#   # the codes
#   codes <- lst[[i]][-c(1:2), ]
#   var_c <- vars[i]
#   var_x <- gsub("_c$", "_x", var_c)
#   d.bfsrg[, var_x] <- factor(d.bfsrg[, var_c])
#   levels(d.bfsrg[, var_x]) <- codes[, 2][match(levels(d.bfsrg[, var_x]), codes[, 1])]
# }
#
# attr(d.bfsrg, "call") <- NULL
#
# d.bfsrg$kt_bez_x <- factor(d.bfsrg$kt_x, levels=Sort(unique(d.bfsrg[,c("kt_c","kt_x")]))[,2])
# levels(d.bfsrg$kt_bez_x) <- c("Zürich","Bern","Luzern","Uri","Schwyz","Obwalden","Nidwalden","Glarus",
#                               "Zug","Fribourg","Solothurn","Basel-Stadt","Basel-Landschaft","Schaffhausen",
#                               "Appenzell Ausserrhoden","Appenzell Innerrhoden","St. Gallen","Graubünden",
#                               "Aargau","Thurgau","Ticino","Vaud","Wallis","Neuchâtel","Genève","Jura")
#
#
#
# d.bfsrg <- d.bfsrg[,c("gem_id","gemeinde_x","kt_c","kt_x","kt_bez_x","bezk_c","bezk_x",
#                    "greg_c","greg_x","aggl_c","aggl_x","aggl_grp_c","aggl_grp_x","stadt_char_c","stadt_char_x",
#                    "stadtland_c", "stadtland_x",
#                    "gem_typ9_c","gem_typ9_x","gem_typ25_c","gem_typ25_x",
#                    "degurba_c","degurba_x","sprgeb_c","sprgeb_x","msre_c","msre_x","msre_typ_c","msre_typ_x"
#                    )]
#
#
# d.bfsrg$greg_x
#
# table(d.bfsrg$greg_c, d.bfsrg$greg_x)
#
#
# # https://www.priminfo.admin.ch/de/regionen
#
#
# d.preg <- XLGetRange(file = "C:\\Users\\andri\\Desktop\\2019_THK_PRO\\praemienregionen_2020-version-2019-09-24.xlsx", sheet = "A_COM",
#      range = c("A6:D6162"),
#      as.data.frame = TRUE, header = FALSE, stringsAsFactors = FALSE)
#
# d.preg <- unique(d.preg[, c(1,4)])
# colnames(d.preg) <- c("gem_id", "preg_c")
#
# d.preg[AllDuplicated(d.preg$gem_id), ]
#
# d.bfsrg[d.bfsrg$gem_id == 889, "preg_c"] <- 2
# d.bfsrg[d.bfsrg$gem_id == 2117, "preg_c"] <- 2
# d.bfsrg[d.bfsrg$gem_id == 2237, "preg_c"] <- 1
#
# d.bfsrg[is.na(d.bfsrg$preg_c), ]
#
# d.bfsrg <- merge(d.bfsrg, d.preg, by="gem_id", all.x=TRUE)



# fertig lustig...!

# d.bfsrg$preg_c
#
#
#









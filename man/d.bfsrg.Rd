\name{d.bfsrg}
\alias{d.bfsrg}
\docType{data}
\encoding{UTF-8}

\title{ BfS Raumregionsdaten
%%   ~~ data name/kind ... ~~
}
\description{Raumregionsdaten des BfS
%%  ~~ A concise (1-5 lines) description of the dataset. ~~
}
\usage{data("d.bfsrg")}
\format{
  A data frame with 2202 observations on the following 29 variables.
  \describe{
    \item{\code{gem_id}}{a numeric vector}
    \item{\code{gemeinde_x}}{a character vector}

    \item{\code{kt_c}}{a numeric vector}
    \item{\code{kt_x}}{a factor with levels \code{ZH} \code{BE} \code{LU} \code{UR} \code{SZ} \code{OW} \code{NW} \code{GL} \code{ZG} \code{FR} \code{SO} \code{BS} \code{BL} \code{SH} \code{AR} \code{AI} \code{SG} \code{GR} \code{AG} \code{TG} \code{TI} \code{VD} \code{VS} \code{NE} \code{GE} \code{JU}}

    \item{\code{kt_bez_x}}{a factor with levels \code{Zuerich} \code{Bern} \code{Luzern} \code{Uri} \code{Schwyz} \code{Obwalden} \code{Nidwalden} \code{Glarus} \code{Zug} \code{Fribourg} \code{Solothurn} \code{Basel-Stadt} \code{Basel-Landschaft} \code{Schaffhausen} \code{Appenzell Ausserrhoden} \code{Appenzell Innerrhoden} \code{St. Gallen} \code{Graubuenden} \code{Aargau} \code{Thurgau} \code{Ticino} \code{Vaud} \code{Wallis} \code{Neuchatel} \code{Geneve} \code{Jura}}

    \item{\code{bezk_c}}{a numeric vector}
    \item{\code{bezk_x}}{a factor with levels \code{Bezirk Affoltern} \code{Bezirk Andelfingen} \code{Bezirk Buelach} \code{Bezirk Dielsdorf} \code{Bezirk Hinwil} ...}
    \item{\code{greg_c}}{a numeric vector}
    \item{\code{greg_x}}{a factor with levels \code{Region lemanique} \code{Espace Mittelland} \code{Nordwestschweiz} \code{Zuerich} \code{Ostschweiz} \code{Zentralschweiz} \code{Ticino}}
    \item{\code{aggl_c}}{a numeric vector}
    \item{\code{aggl_x}}{a factor with levels \code{keine Agglomerationsgemeinde und keine Kerngemeinde ausserhalb von Agglomerationen} \code{Winterthur} \code{Zuerich} \code{Bern} }

    \item{\code{aggl_grp_c}}{a numeric vector}
    \item{\code{aggl_grp_x}}{a factor with levels \code{keine Agglomerationszugehoerigkeit} \code{>= 500000 Einwohner/innen} \code{250000 - 499999 Einwohner/innen} \code{100000 - 249999 Einwohner/innen} }

    \item{\code{stadt_char_c}}{a numeric vector}
    \item{\code{stadt_char_x}}{a factor with levels \code{Laendliche Gemeinde ohne staedtischen Charakter} \code{Agglomerationskerngemeinde (Kernstadt)} \code{Agglomerationskerngemeinde (Hauptkern)} \code{Agglomerationskerngemeinde (Nebenkern)} \code{Agglomerationsguertelgemeinde} \code{Mehrfach orientierte Gemeinde} \code{Kerngemeinde ausserhalb Agglomerationen}}

    \item{\code{stadtland_c}}{a numeric vector}
    \item{\code{stadtland_x}}{a factor with levels \code{stadt} \code{agglo} \code{land}}

   \item{\code{gem_typ9_c}}{a numeric vector}
    \item{\code{gem_typ9_x}}{a factor with levels \code{Staedtische Gemeinde einer grossen Agglomeration} \code{Staedtische Gemeinde einer mittelgrossen Agglomeration} \code{Staedtische Gemeinde einer kleinen oder ausserhalb einer Agglomeration} \code{Periurbane Gemeinde hoher Dichte} \code{Periurbane Gemeinde mittlerer Dichte} \code{Periurbane Gemeinde geringer Dichte} \code{Laendliche Zentrumsgemeinde} \code{Laendliche zentral gelegene Gemeinde} \code{Laendliche periphere Gemeinde}}

    \item{\code{gem_typ25_c}}{a numeric vector}
    \item{\code{gem_typ25_x}}{a factor with levels \code{Kernstadt einer grossen Agglomeration} \code{Staedtische Arbeitsplatzgemeinde einer grossen Agglomeration} \code{Staedtische Wohngemeinde einer grossen Agglomeration} \code{Kernstadt einer mittelgrossen Agglomeration} \code{Staedtische Arbeitsplatzgemeinde einer mittelgrossen Agglomeration} \code{Staedtische Wohngemeinde einer mittelgrossen Agglomeration} \code{Staedtische Tourismusgemeinde einer kleinen oder ausserhalb einer Agglomeration} \code{Staedtische Industriegemeinde einer kleinen oder ausserhalb einer Agglomeration} \code{Staedtische Dienstleistungsgemeinde einer kleinen oder ausserhalb einer Agglomeration} \code{Periurbane Industriegemeinde hoher Dichte} \code{Periurbane Dienstleistungsgemeinde hoher Dichte} \code{Periurbane Industriegemeinde mittlerer Dichte} \code{Periurbane Dienstleistungsgemeinde mittlerer Dichte} \code{Periurbane Agrargemeinde geringer Dichte} \code{Periurbane Industriegemeinde geringer Dichte} \code{Periurbane Dienstleistungsgemeinde geringer Dichte} \code{Tourismusgemeinde eines laendlichen Zentrums} \code{Industriegemeinde eines laendlichen Zentrums} \code{Dienstleistungsgemeinde eines laendlichen Zentrums} \code{Laendliche zentral gelegene Agrargemeinde} \code{Laendliche zentral gelegene Industriegemeinde} \code{Laendliche zentral gelegene Dienstleistungsgemeinde} \code{Laendliche periphere Tourismusgemeinde } \code{Laendliche periphere Agrargemeinde} \code{Laendliche periphere Mischgemeinde}}

    \item{\code{degurba_c}}{a numeric vector}
    \item{\code{degurba_x}}{a factor with levels \code{dense} \code{intermediate} \code{thin}}

    \item{\code{sprgeb_c}}{a numeric vector}
    \item{\code{sprgeb_x}}{a factor with levels \code{d} \code{f} \code{i} \code{r}}

    \item{\code{msre_c}}{a numeric vector}
    \item{\code{msre_x}}{a factor with levels \code{Zuerich  } \code{Glattal-Furttal } \code{Limmattal} \code{Knonaueramt } \code{Zimmerberg  } }

    \item{\code{msre_typ_c}}{a numeric vector}
    \item{\code{msre_typ_x}}{a factor with levels \code{Zuerich  } \code{Glattal-Furttal } \code{Limmattal} \code{Knonaueramt } \code{Zimmerberg  } }

    \item{\code{preg_c}}{a numeric vector}
  }
}
\examples{
# (kt <- unique(d.bfsrg[,c("kt_c","kt_x","kt_bez_x")][order(d.bfsrg$kt_c),]))
# (bezk <- unique(d.bfsrg[,c("bezirk_c","bezirk_x","kt_x")][order(d.bfsrg$bezirk_c),]))
# (msreg <- unique(d.bfsrg[,c("ms_reg_c","ms_reg_x","ms_typ_x")][order(d.bfsrg$ms_reg_c),]))
}
\keyword{datasets}

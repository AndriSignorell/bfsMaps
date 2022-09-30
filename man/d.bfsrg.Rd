\name{d.bfsrg}
\alias{d.bfsrg}
\docType{data}
\encoding{UTF-8}

\title{Swiss Federal Statistical Office (SFSO) Spatial Divisions
%%   ~~ data name/kind ... ~~
}
\description{The Swiss Federal Statistical Office (SFSO) produces, publishes and maintains various spatial divisions for Switzerland. A dataset for the year 2020 is part of the package.
%%  ~~ A concise (1-5 lines) description of the dataset. ~~
}
\usage{data("d.bfsrg")}
\format{
  A data frame with 2202 observations on the following 29 variables.
  \describe{
    \item{\code{gem_id}}{community id, a numeric vector}
    \item{\code{gemeinde_x}}{community name, character vector}

    \item{\code{kt_c}}{canton id, numeric vector}
    \item{\code{kt_x}}{canton abbreviation, a factor with levels \code{ZH} \code{BE} \code{LU} \code{UR} \code{SZ} \code{OW} \code{NW} \code{GL} \code{ZG} \code{FR} \code{SO} \code{BS} \code{BL} \code{SH} \code{AR} \code{AI} \code{SG} \code{GR} \code{AG} \code{TG} \code{TI} \code{VD} \code{VS} \code{NE} \code{GE} \code{JU}}

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
    \item{\code{gem_typ9_x}}{a factor with 9 levels}

    \item{\code{gem_typ25_c}}{a numeric vector}
    \item{\code{gem_typ25_x}}{a factor with 25 levels, definig types of communities}

    \item{\code{degurba_c}}{a numeric vector}
    \item{\code{degurba_x}}{a factor with levels \code{dense} \code{intermediate} \code{thin}}

    \item{\code{sprgeb_c}}{a numeric vector}
    \item{\code{sprgeb_x}}{a factor with levels \code{d} \code{f} \code{i} \code{r}}

    \item{\code{preg_c}}{a numeric vector}
  }
}
\examples{
head(kt <- unique(d.bfsrg[,c("kt_c","kt_x","kt_bez_x")][order(d.bfsrg$kt_c),]))
head(bezk <- unique(d.bfsrg[,c("bezk_c","bezk_x","kt_x")][order(d.bfsrg$bezk_c),]))
}
\keyword{datasets}

\name{SwissLocator}
\alias{SwissLocator}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Get the Community, District and Canton of a Located Mappoint
%%  ~~function to do ... ~~
}
\description{Locate a point in a Switzerland map and get the according community, district and canton.
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
SwissLocator()
}
%- maybe also 'usage' for other objects documented here.
\value{For each clicked and identified point the coordinates, the political community, the district and the canton will be returned.\cr
\preformatted{
            x        y bfs_nr   gemeinde_name_x                 bezirk_x kt_x
1014 536281.5 167176.3   2703            Riehen       Kanton Basel-Stadt   BS
1781 616565.2 268959.6   5136         Onsernone     Distretto di Locarno   TI
1962 690861.6 119006.1   5524 Goumoens-la-Ville District du Gros-de-Vaud   VD
}

}
\author{Andri Signorell <andri.signorell@helsana.ch>
%%  ~~who you are~~
}
\keyword{ iplot }
\keyword{ spatial }

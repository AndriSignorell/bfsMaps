\name{GeoCode}
\alias{GeoCode}
\title{
Get the Coordinates For a Given Address Provided by GeoAdmin API
}
\description{Get the coordinates for a given address in Switzerland. The function just
prepares the url needed to feed the GeoAdmin API.
}
\usage{
GeoCode(x)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{a character vector containing addresses in the form of
            "street number place"
}
}
\details{See further details in \url{https://api3.geo.admin.ch/services/sdiservices.html#find}.
}
\value{
a data.frame with 5 columns\cr
\code{detail}, \code{lat},
\code{lon}, \code{x}, \code{y}
}
\author{Andri Signorell <andri@signorell.net>
}

\examples{
\dontrun{
  adr <- c("Quaderaweg 3 Trimmis","chemin des Fraisiers 19 Grand-Lancy",
          "Im Spitzacker 21 Basel","Rue du Moleson 8 Broc")
  GeoCode(adr)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory (show via RShowDoc("KEYWORDS")):
% \keyword{ ~kwd1 }
% \keyword{ ~kwd2 }
% Use only one keyword per line.
% For non-standard keywords, use \concept instead of \keyword:
% \concept{ ~cpt1 }
% \concept{ ~cpt2 }
% Use only one concept per line.

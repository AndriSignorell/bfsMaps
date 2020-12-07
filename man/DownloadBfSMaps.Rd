\name{DownloadBfSMaps}
\alias{DownloadBfSMaps}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Helps to Get the Map Data
%%  ~~function to do ... ~~
}
\description{Helperfunction to download the mapdata and unzip them into a user defined location.
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
DownloadBfSMaps(url = "https://www.bfs.admin.ch/bfsstatic/dam/assets/11927607/master",
                path = paste0(path.expand("~"), "/MapData"))
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{url}{the url for the data (might outdate and must then be redefined).
%%     ~~Describe \code{url} here~~
}
  \item{path}{the path where the data are to be installed.
%%     ~~Describe \code{path} here~~
}
}
\details{It is convenient to not be forced to download the data by oneself. This function can be helpful.
%%  ~~ If necessary, more details than the description above ~~
}
\value{the option entry is returned invisibly
}
\author{Andri Signorell <andri@signorell.net>
}
\examples{
\dontrun{

DownloadBfSMaps()
# enter the returned option
options(bfsMaps.base="*** your map folder ***")

library(bfsMaps)
PlotKant("ZH", "blue")
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

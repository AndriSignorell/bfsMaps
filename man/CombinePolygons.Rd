\name{CombinePolygons}
\alias{CombinePolygons}
\alias{CombineKant}
\alias{CombinePolg}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Combine Multiple Polygons to One Spatial Polygon
%%  ~~function to do ... ~~
}
\description{The function combines polygons to one single spatial polygon object, according to the ID vector that specifies which input polygons belong to which output polygon. %%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
CombinePolygons(map, g)

CombineKant(id, g, map = RequireMap("kant.map"))
CombinePolg(id, g, map = RequireMap("polg.map"))
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{map}{the map containing the regions to be combined.
%%     ~~Describe \code{map} here~~
}
\item{id}{the id of the cantons or communities to be aggregated.}
  \item{g}{a vector defining the assignment of the elements to the output polygons to be created. It may contain \code{NA} values for input objects not included in the union.
%%     ~~Describe \code{grp} here~~
}

}
\value{Returns an aggregated spatial polygons object named with the aggregated IDs values in their sorting order; see the ID values of the output object to view the order.}
\author{Juerg Guggenbuehl, Andri Signorell <andri@signorell.net>
%%  ~~who you are~~
}
\seealso{\code{\link{unionSpatialPolygons}}
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
require(DescTools)

try( {

# Representation of the language areas in CH combined via cantons
# by majority per canton
tkt <- table(d.bfsrg$kt_c, d.bfsrg$sprgeb_x)
grp <- unique(d.bfsrg$sprgeb_x)[apply(tkt, 1, which.max)]

# combine and plot cantons
plot(CombineKant(rownames(tkt), grp), col=SetAlpha(c(horange, hyellow, hecru), 0.8),
     border="grey40", main="Languages in CH")

# copyright is mandatory for these SFSO maps
BfSStamp()
# waters make the maps more realistic ...
AddLakes(col = "grey80", border = "grey40")

})}

% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ spatial }% use one of  RShowDoc("KEYWORDS")

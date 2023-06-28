\name{GetMap}
\alias{GetMap}

%- Also NEED an '\alias' for EACH other topic documented here.
\title{Load a Map
%%  ~~function to do ... ~~
}
\description{ \code{GetMap} directly looks up the path of a map based on a shortcut name or number, loads the map from this location and returns the object.
}
\usage{
GetMap(name_x, basedir = getOption("bfsMaps.base",
       default = file.path(find.package("bfsMaps"), "extdata")))
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{name_x}{the name of a map, currently supported are \code{"kant.map"}, \code{"bezk.map"}, \code{"polg.map"}, \code{"greg.map"}, \code{"ch.map"} or any number referring to a row in the 'maps.csv' file.}

  \item{basedir}{the root directory for the maps to reside. bfsMaps by default looks for the mapfiles in its install location in the extdata directory. The \code{basedir} can be set as an option too.}
}
\details{Loading the cards no longer causes performance problems and can thus be performed directly.
%%  ~~ If necessary, more details than the description above ~~
}

\value{the map object
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
}
\author{Andri Signorell <andri@signorell.net>
%%  ~~who you are~~
}
\examples{try( {

# use map containing Swiss metropolitan regions
mymap <- GetMap("metr.map")$geometry
PlotCH()
plot(mymap, col=Pal("Helsana"), add=TRUE, border=NA)

})}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory (show via RShowDoc("KEYWORDS")):
% \keyword{ ~kwd1 }
% \keyword{ ~kwd2 }
% Use only one keyword per line.
% For non-standard keywords, use \concept instead of \keyword:
% \concept{ ~cpt1 }
% \concept{ ~cpt2 }
% Use only one concept per line.

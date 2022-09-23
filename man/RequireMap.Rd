\name{RequireMap}
\alias{RequireMap}
\alias{LoadMap}

%- Also NEED an '\alias' for EACH other topic documented here.
\title{Ensure Availability of a Map
%%  ~~function to do ... ~~
}
\description{ \code{LoadMap} directly looks up the path of a map based on a shortcut name or number, loads the map from this location and return the object. \cr\cr
\code{RequireMap} does the same, but if the map has been loaded before, it returns the temporarily cached copy. The reason for this is that loading the map objects appears to be computationally intensive. It takes relatively long and should not have to be done every time we need a map.
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
RequireMap(name_x, verbose = FALSE)

LoadMap(name_x, basedir = getOption("bfsMaps.base",
        default = file.path(find.package("bfsMaps"), "extdata")))
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{name_x}{the name of a map, currently supported are \code{"kant.map"}, \code{"bezk.map"}, \code{"polg.map"}, \code{"greg.map"}, \code{"ch.map"} or any number referring to a row in the 'maps.csv' file.}
  \item{verbose}{the function will return the result invisibly, unless verbose is set to \code{TRUE}.}
  \item{basedir}{the root directory for the maps to reside. bfsMaps by default looks for the mapfiles in it's install location in the extdata directory. The \code{basedir} can be set as an option too.}
}
\details{The maps are loaded to the package's environment as soon as they are used the first time in the session. Later access is so made considerably faster.
%%  ~~ If necessary, more details than the description above ~~
}

\note{Loading the maps for the first time will display a warning "1: In OGRSpatialRef(" concerning a "Discarded datum CH1903 ...". This does not impact the use of the maps. The warnings can however only be suppressed by setting an option tag in R's \code{.Rprofile} file:

\code{options(rgdal_show_exportToProj4_warnings="none")}

(See also https://cran.r-project.org/web/packages/rgdal/vignettes/PROJ6_GDAL3.html)
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
\examples{\donttest{\dontrun{
# use map containing Swiss metropolitan regions
mymap <- RequireMap(78)
plot(mymap, col=Pal("Helsana"))}
}}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory (show via RShowDoc("KEYWORDS")):
% \keyword{ ~kwd1 }
% \keyword{ ~kwd2 }
% Use only one keyword per line.
% For non-standard keywords, use \concept instead of \keyword:
% \concept{ ~cpt1 }
% \concept{ ~cpt2 }
% Use only one concept per line.

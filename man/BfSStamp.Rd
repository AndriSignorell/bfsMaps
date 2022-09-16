\name{BfSStamp}
\alias{BfSStamp}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Stamp the Current Plot
%%  ~~function to do ... ~~
}
\description{Stamp the current plot in the lower right corner with the copyright of the BfS-maps:\cr\cr \bold{"Kartengrundlage: (c) BFS, ThemaKart, 20xx"\cr\cr}
This copyright is mandatory for all maps in public publications. The default coodinates are chosen by default in the bottomright corner of a Swiss map, but can be redefined by user.
}
\usage{
BfSStamp(xy = NULL,
         year_n = getOption("bfsMaps.year", Year(Today())),
         txt = NULL, cex = 0.6, adj = c(1,0), ...)
}
\arguments{
  \item{xy}{the coordinates for the text to be placed.}
  \item{year_n}{the year for the compulsory BfS copyright message.}
  \item{txt}{the text to be used.}
  \item{cex}{the character extension for the text (default is 0.6)}
  \item{adj}{one or two values in [0, 1] which specify the x (and optionally y) adjustment ('justification') of the labels, with 0 for left/bottom, 1 for right/top, and 0.5 for centered. On most devices values outside [0, 1] will also work. See below.}
  \item{\dots}{the dots are passed to the function \code{\link{text}()}}

}
\details{The default value for the year can be entered as option in the .Rprofile file.
bfsMaps.year=2022 would set the default to 2022.}
\value{None}

\author{Andri Signorell <andri@signorell.net>
%%  ~~who you are~~
}
\seealso{\code{\link[DescTools]{Stamp}()}
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\keyword{aplot }

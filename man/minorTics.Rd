\name{minorTics}

\alias{minorTics}

\title{Place Minor Tick Marks on Plot Axis}

\description{
Adds minor axis tic marks to the active plot.
}

\usage{
minorTics(side, loc.tics = NULL)
}

\arguments{
\item{side}{integer; a vector of values specifying the plot sides for the axis to be drawn.}
\item{loc.tics}{numeric; a vector giving the locations of major tic marks}
}

\details{
The plot sides are designated as: 1 = below, 2 = left, 3 = above, and 4 = right.
}

\author{Fisher, J. C. and J. Lewis}

\seealso{
\code{\link{axis}}
}

\examples{
plot(1:10)
minorTics(1)
minorTics(2, loc.tics = 4:8)
minorTics(3:4)
}

\keyword{aplot}

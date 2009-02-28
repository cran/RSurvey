\name{plotSurface2d}

\alias{plotSurface2d}

\title{Plot 2D Interpolated Surface}

\description{
A filled contour plot showing the distributed spatial data is drawn. 
A key showing how the colors map to state variable values is shown to the right of the plot.
}

\usage{
plotSurface2d(const.poly = FALSE)
}

\arguments{
\item{const.poly}{a logical; if \code{TRUE} the user is required to interactively construct the spatial domain within the active plot.}
}

\author{Fisher, J. C.}

\seealso{
\code{mba.surf}, \code{\link{filled.contour}}
}

\examples{
data(confluence)
plotSurface2d()
}

\keyword{hplot}

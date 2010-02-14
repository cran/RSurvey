\name{point.on.surface}

\alias{point.on.surface}

\title{Determine Point on Interpolated Surface}

\description{
This function returns \code{z} values corresponding to point locations within the interpolated surface.
}

\usage{
point.on.surface(x, y = NULL, z = NULL, px, py = NULL)
}

\arguments{
\item{x, y}{numeric; locations of grid lines at which the values in z are measured. These must be in ascending order. If \code{x} is a list, its components \code{x$x} and \code{x$y} are used for \code{x} and \code{y}, respectively. If the list has component \code{x$z} this is used for \code{z}.}
\item{z}{matrix; the values to be plotted. Note that \code{x} can be used instead of \code{z} for convenience.}
\item{px}{numeric; a vector of \code{x} coordinates for the observation points. If \code{px} is a list, its components \code{px$px} and \code{px$py} are used for \code{px} and \code{py}, respectively.}
\item{py}{numeric; a vector of \code{y} coordinates for the observation points. }
}

\value{
Returns a data frame containing the following components:
  \item{px, py}{numeric; see Arguments section}
  \item{pz}{numeric; a vector of \code{z} values corresponding to point locations \code{(x,y)} within the interpolated surface}
}

\author{Fisher, J. C.}

\seealso{
\code{point.in.polygon}
}

\examples{
data(project)
d <- srvy.dat("data.grd")
px <- c(-5.0, 0.0, 9.0)
py <- c(-5.0, 0.0, 0.0)
point.on.surface(x = d, px = px, py = py)
}

\keyword{file}

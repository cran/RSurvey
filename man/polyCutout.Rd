\name{polyCutout}

\alias{polyCutout}

\title{Determine Grid Points within Polygon}

\description{
This function excludes data points outside a given polygon.
}

\usage{
polyCutout(dat, ply = NULL)
}

\arguments{
\item{dat}{list; with components \code{dat$x}, \code{dat$y}, and \code{dat$z}, see Value section}
\item{ply}{gpc.poly; a polygon}
}

\details{
Values of \code{z} corresponding to nodes (\code{x}, \code{y}) located outside the polygon will be set to \code{NA}.
}

\value{
Returns a list containing the following components:
  \item{x}{numeric; a vector of grid point locations in the x-direction}
  \item{y}{numeric; a vector of grid point locations in the y-direction}
  \item{z}{matrix; state variable values corresponding to nodes in the grid}
}

\author{Fisher, J. C.}

\seealso{
\code{point.in.polygon}
}

\examples{
x11()

ply <- as(cbind(c(2, 8, 9, 6, 3), c(3, 1, 4, 8, 6)), "gpc.poly")
x <- seq(0, 10, 0.1)
y <- seq(0, 10, 0.1)
z <- matrix(runif(length(x) * length(y)), 
     nrow = length(y), ncol = length(x))

d <- list(x = x, y = y, z = z)
filled.contour(d, plot.axes = {axis(1); axis(2); plot(ply, add = TRUE)})

d <- polyCutout(d, ply)
filled.contour(d, color.palette = terrain.colors)
}

\keyword{manip}

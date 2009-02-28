\name{polyCutout}

\alias{polyCutout}

\title{Cutout Grid Points within Polygon}

\description{
This function excludes data points outside a given polygon.
}

\usage{
polyCutout(dat, ply)
}

\arguments{
\item{dat}{a list containing \code{x}, \code{y}, and \code{z} components, see Value section.}
\item{ply}{a polygon of class \code{gpc.poly-class}.}
}

\details{
Returns \code{NA} values for \code{z} when points (\code{x}, \code{y}) are located outside the polygon.
}

\value{
A list containing the following components:
  \item{x}{grid point locations in the x-direction.}
  \item{y}{grid point locations in the y-direction.}
  \item{z}{a matrix of state variable values corresponding to the grid.}
}

\author{Fisher, J. C.}

\seealso{
\code{point.in.polygon}, \code{filled.contour}
}

\examples{
x11()

ply <- as(cbind(c(2, 8, 9, 6, 3), c(3, 1, 4, 8, 6)), "gpc.poly")
x <- seq(0, 10, 0.1)
y <- seq(0, 10, 0.1)
z <- matrix(runif(length(x) * length(y)), 
     nrow = length(y), ncol = length(x))

dat.old <- list(x = x, y = y, z = z)
filled.contour(dat.old, 
    plot.axes = {axis(1); axis(2); plot(ply, add = TRUE)})

dat.new <- polyCutout(dat.old, ply)
filled.contour(dat.new, color.palette = terrain.colors)
}

\keyword{manip}

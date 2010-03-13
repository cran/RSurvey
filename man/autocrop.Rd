\name{autocrop}

\alias{autocrop}

\title{Autocrop Spatial Domain}

\description{
This function removes mesh cells with arc lengths greater than a user specified maximum.
}

\usage{
autocrop(mesh, maxLength, maxItr)
}

\arguments{
\item{mesh}{tri; a Delaunay triangulation}
\item{maxLength}{numeric; maximum arc length for a cell within the mesh}
\item{maxItr}{integer; maximum number of iterations}
}

\value{
Returns a polygon object of class \code{gpc.poly}.
}

\seealso{
\code{\link{polyAutocrop}}, \code{tri.mesh}
}

\author{Fisher, J. C.}

\examples{
data(tritest)
mesh <- tri.mesh(tritest$x, tritest$y)
plot(mesh)
ply <- autocrop(mesh, maxLength = 0.5, maxItr = 100)
plot(ply, add = TRUE, poly.args = list(col = 2))
}

\keyword{misc}

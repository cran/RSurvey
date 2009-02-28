\name{autocrop}

\alias{autocrop}

\title{Auto Crop Spatial Domain}

\description{
Eliminate mesh cells with arc lengths greater than a user defined maximum.
}

\usage{
autocrop(mesh, maxLength, maxItr)
}

\arguments{
\item{mesh}{an object of class \code{tri}.}
\item{maxLength}{the maximum arc length for a cell within the mesh.}
\item{maxItr}{the maximum number of iterations.}
}

\value{
A polygon of class \code{gpc.poly}.
}

\seealso{
\code{\link{polyAutocrop}}
}

\author{Fisher, J. C.}

\examples{
data(tritest)
mesh <- tri.mesh(tritest$x, tritest$y)
plot(mesh); axis(1); axis(2); box()
ply <- autocrop(mesh, maxLength = 0.5, maxItr = 100)
plot(ply, add = TRUE, poly.args = list(col = 2))
}

\keyword{misc}

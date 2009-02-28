\name{polyAutocrop}

\alias{polyAutocrop}

\title{Auto Crop Parameters}

\description{
Control parameters associated with the autocrop algorithm.
}

\usage{
polyAutocrop(tt1 = NULL)
}

\arguments{
\item{tt1}{a Tk toplevel widget.}
}

\details{
Requires access to the following components within \code{\link{srvy.dat}}: \code{data.raw}, \code{win.width}, \code{csi}, and \code{vars[2:3]}. 
The default maximum arc length for a cell is determined from the maximum length within the Delaunay triangulation mesh 
and an interation limit for the algorithm.  Entering arc lengths less than the default value 
will reduce the polygons size.
}

\value{
A polygon of class \code{gpc.poly}.
}

\author{Fisher, J. C.}

\seealso{
\code{\link{autocrop}}, \code{tri.mesh}, \code{get.pts}
}

\examples{
\donttest{
data(confluence)
polyAutocrop()
}
}

\keyword{misc}

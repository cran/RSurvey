\name{polyAutocrop}

\alias{polyAutocrop}

\title{Set Autocrop Input Parameters}

\description{
A \acronym{GUI} for specify input parameters and running the \code{autocrop} function.
}

\usage{
polyAutocrop(dat, xlab = NULL, ylab = NULL, zlab = NULL, parent)
}

\arguments{
\item{dat}{list; its components \code{x$x} and \code{x$y} are used to specify \code{x} and \code{y} coordinates of the mesh vertices, respectively.}
\item{xlab}{character; the label for the \code{x} axis}
\item{ylab}{character; the label for the \code{y} axis}
\item{zlab}{character; the label for the \code{z} legend}
\item{parent}{tkwin; the parent window (optional)}
}

\details{
The maximum outer arc length for a cell and the maximum number of iterations are stopping criteria for the 
\code{autocrop} algorithm. The default maximum arc length is 
determined by the maximum outer length within the Delaunay triangulation mesh. 
Entering arc lengths less than the default will reduce the area of the polygon. 
A point plot shows the resulting polygon based on the user defined input parameters. 
}

\value{
Returns a polygon object of class \code{gpc.poly}.
}

\author{Fisher, J. C.}

\seealso{
\code{tri.mesh}, \code{\link{autocrop}}, \code{\link{plotSurvey2d}}
}

\examples{
\donttest{
data(project)
d <- srvy.dat("data.grd")
polyAutocrop(d)
}
}

\keyword{misc}

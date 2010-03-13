\name{polyManage}

\alias{polyManage}

\title{Manage Polygons}

\description{
A \acronym{GUI} for managing and manipulating polygons that is based on the \pkg{gpclib} package.
}

\usage{
polyManage(parent, ply = NULL, ply.ran = NULL, ply.lim = NULL, 
encoding = getOption("encoding"))
}

\arguments{
\item{parent}{tkwin; the parent window (optional)}
\item{ply}{list; its components are objects of class \code{gpc.poly-class}}
\item{ply.ran}{character; the name of the \code{ply} component that specifies the range polygon (optional)}
\item{ply.lim}{character; the name of the \code{ply} component that specifies the limit polygon (optional)}
\item{encoding}{character; encoding to be assumed for input strings. If the value is \code{"latin1"} or \code{"UTF-8"} it is used to mark character strings as known to be in Latin-1 or UTF-8: it is not used to re-encode the input.}
}

\details{
The text file representation of a polygon is of the following format:\cr\cr
<number of contours>\cr
<number of points in first contour>\cr
<hole flag>\cr
x1 y1\cr
x2 y2\cr
...\cr
<number of points in second contour>\cr
<hole flag>\cr
x1 y1\cr
x2 y2\cr
...\cr\cr
The hole flag is either 1 to indicate a hole, or 0 for a regular contour. 
See \code{read.polyfile} within the \pkg{gpclib} package for details.
}

\value{
Returns a list with components \code{ply}, \code{ply.ran}, and \code{ply.lim}, see Arguments section.
}

\author{Fisher, J. C.}

\seealso{
\code{polyfile}, \code{union}, \code{setdiff}, \code{intersect}
}

\examples{
\donttest{polyManage()}
}

\keyword{misc}

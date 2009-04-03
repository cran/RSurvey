\name{srvy}

\alias{srvy}

\title{Main Tcl/Tk Graphical User Interface}

\description{
This function activates the main graphical user interface (\acronym{GUI}) for \pkg{RSurvey}.
}

\usage{
srvy()
}

\details{
Most functions within \pkg{RSurvey} are accessible within this \acronym{GUI}. 
The text file format of survey data is described in \code{\link{readFile}}. 
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
The following components within \code{\link{srvy.dat}} are set: 
\code{sd}, \code{ed}, \code{sh}, \code{sm}, \code{em}, \code{ss}, \code{min.x}, \code{max.x}, 
\code{min.y}, \code{max.y}, \code{min.z}, \code{max.z}, \code{off.z}, \code{off.t}.
}

\author{Fisher, J. C.}

\examples{
\donttest{srvy()}
}

\keyword{misc}

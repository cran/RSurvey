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

\name{srvy.pref}

\alias{srvy.pref}

\title{Error Correction, Interpolation, and Water Surface Parameters}

\description{
A \acronym{GUI} defining error correction, interpolation, and water surface parameters and flags.
}

\usage{srvy.pref(tt1 = NULL)}

\arguments{
\item{tt1}{a Tk toplevel widget.}
}

\value{
The following components within \code{\link{srvy.dat}} are set: 
\code{grad.tol}, \code{time.gap}, \code{grid.res}, \code{wtr.elev}, \code{depth}.
}

\author{Fisher, J. C.}

\examples{
\donttest{srvy.pref()}
}

\keyword{misc}

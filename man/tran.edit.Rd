\name{tran.edit}

\alias{tran.edit}

\title{Interpolation and Error Correction Parameters}

\description{
A \acronym{GUI} for defining parameters associated with a transect.
}

\usage{tran.edit(id = NULL, tt2 = NULL)}

\arguments{
\item{id}{the name of the transect.}
\item{tt2}{a Tk toplevel widget.}
}

\value{
The following components within \code{\link{tran.dat}} are set: 
\code{x1}, \code{x2}, \code{y1}, \code{y2}, \code{fix.zero}, 
\code{v.origin}, \code{v.offset}, \code{h.offset}, 
\code{grid.dx}, \code{grid.dy}, \code{zero.int}, \code{asp.ration}.
}

\author{Fisher, J. C.}

\examples{
\donttest{tran.edit()}
}

\keyword{misc}

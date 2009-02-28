\name{srvy.vars}

\alias{srvy.vars}

\title{Data Variables}

\description{
A \acronym{GUI} for identifying temporal and spatial variables associated with survey data.
}

\usage{
srvy.vars(cols, vars, tt1 = NULL)
}

\arguments{
\item{cols}{those components within \code{\link{srvy.dat}} corresponding to temporal and spatial data, a concatenation of column type and units.}
\item{vars}{the components corresponding to data/time, x, y, and z data.}
\item{tt1}{a Tk toplevel widget.}
}

\details{
The \code{vars} component is updated in \code{\link{srvy.dat}}.
}

\author{Fisher, J. C.}

\examples{
\donttest{
data(confluence)
srvy.vars(srvy.dat("cols"), srvy.dat("vars"))
}

}

\keyword{misc}

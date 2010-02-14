\name{srvy.vars}

\alias{srvy.vars}

\title{Set Data Variables}

\description{
A \acronym{GUI} for specifying the data's temporal and spatial variables.
}

\usage{
srvy.vars(cols.name, cols.class, vars.old, parent)
}

\arguments{
\item{cols.name}{character; a vector of column names}
\item{cols.class}{character; a vector of column units}
\item{vars.old}{list; see the \code{vars} parameter in \code{\link{srvy.dat}}}
\item{parent}{tkwin; the parent window (optional)}
}

\value{
Returns an updated \code{vars} list; see \code{\link{srvy.dat}}
}

\author{Fisher, J. C.}

\examples{
\donttest{
data(project)
cols <- srvy.dat("cols")
cols.name  <- unlist(lapply(1:length(cols), function(i) cols[[i]]$name))
cols.class <- unlist(lapply(1:length(cols), function(i) cols[[i]]$class))
vars.old <- srvy.dat("vars")
srvy.vars(cols.name, cols.class, vars.old)
}
}

\keyword{misc}

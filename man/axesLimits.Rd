\name{axesLimits}

\alias{axesLimits}

\title{Set Axes Limits}

\description{
A \acronym{GUI} for setting plotting limits.
}

\usage{
axesLimits(parent, old = NULL, addt = FALSE)
}

\arguments{
\item{parent}{tkwin; the parent window (optional)}
\item{old}{list; contains the current plotting limits, see Value section}
\item{addt}{logical; if \code{TRUE} the temporal axis limits are provided within the \acronym{GUI}}
}

\value{
Returns a list containing the following components:
  \item{x1}{numeric; the minimum x-axis value}
  \item{x1.chk}{logical; if \code{TRUE} a default value will be used for the minimum x-axis}
  \item{x2}{numeric; the maximum x-axis value}
  \item{x2.chk}{logical; if \code{TRUE} a default value will be used for the maximum x-axis}
  \item{y1}{numeric; the minimum y-axis value}
  \item{y1.chk}{logical; if \code{TRUE} a default value will be used for the minimum y-axis}
  \item{y2}{numeric; the maximum y-axis value}
  \item{y2.chk}{logical; if \code{TRUE} a default value will be used for the maximum y-axis}
  \item{z1}{numeric; the minimum z-axis value}
  \item{z1.chk}{logical; if \code{TRUE} the default value will be used for the minimum z-axis}
  \item{z2}{numeric; the maximum z-axis value}
  \item{z2.chk}{logical; if \code{TRUE} a default value will be used for the maximum z-axis}
  \item{t1}{POSIXlt; the minimum t-axis value}
  \item{t1.chk}{logical; if \code{TRUE} a default value will be used for the minimum temporal axis}
  \item{t2}{POSIXlt; the maximum t-axis value}
  \item{t2.chk}{logical; if \code{TRUE} a default value will be used for the maximum temporal axis}
}

\author{Fisher, J. C.}

\examples{
\donttest{axesLimits()}
}

\keyword{misc}

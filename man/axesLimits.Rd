\name{axesLimits}

\alias{axesLimits}

\title{Set Limits on Raster Data}

\description{
A \acronym{GUI} for setting plotting limits associated with raster data.
}

\usage{
axesLimits(tt2 = NULL, old = list())
}

\arguments{
\item{tt2}{a Tk toplevel widget.}
\item{old}{the current plotting limits, see Value section.}
}

\value{
A list containing 6 components:
  \item{z.min}{the minimum z-axis value, default \code{NA}.}
  \item{z.min.auto}{the default value for the minimum z-axis, default \code{TRUE}.}
  \item{z.max}{the maximum z-axis value, default \code{NA}.}
  \item{z.max.auto}{the default value for the maximum z-axis, default \code{TRUE}.}
  \item{z.lev}{the number of contour levels, default \code{NA}.}
  \item{z.lev.auto}{the default value for the number of contour levels, default \code{TRUE}.} 
}

\author{Fisher, J. C.}

\examples{
\donttest{axesLimits()}
}

\keyword{misc}

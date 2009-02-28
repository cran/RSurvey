\name{tran.velocity}

\alias{tran.velocity}

\title{Velocity Components for Raster Data}

\description{
A \acronym{GUI} for identifying raster fields corresponding to velocity components.
}

\usage{
tran.velocity(rasterFields, velocityFields = NULL, tt2 = NULL)
}

\arguments{
\item{rasterFields}{an array containing all raster field names.}
\item{velocityFields}{current raster fields corresponding to the x, y, z, and principle velocity components.}
\item{tt2}{a Tk toplevel widget.}
}

\value{
A character array of field names associated with velocity components in the 
x \code{[1]}, y \code{[2]}, z \code{[3]}, and principle \code{[4]} directions.
}

\author{Fisher, J. C.}

\examples{
\donttest{tran.velocity(c("VelE (m/s)", "VelN (m/s)", "VelU (m/s)", "VelM (m/s)"))}
}

\keyword{misc}

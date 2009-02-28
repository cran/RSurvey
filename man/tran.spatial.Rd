\name{tran.spatial}

\alias{tran.spatial}

\title{Spatial Fields for Raster Data}

\description{
Identify raster fields corresponding to spatial coordinates.
}

\usage{
tran.spatial(rasterFields, spatialFields = NULL, tt2 = NULL)
}

\arguments{
\item{rasterFields}{an array giving all raster fields.}
\item{spatialFields}{current raster fields corresponding to the spatial coordinates \code{(x,y)}.}
\item{tt2}{a Tk toplevel widget.}
}

\value{
A character array containing field names associated with the local x-axis \code{[1]} and local z-axis \code{[2]} coordinates.
}

\author{Fisher, J. C.}

\examples{
\donttest{tran.spatial(c("X (m)", "Z (m)", "T (degC"), c("X (m)", "Z (m)"))}
}

\keyword{misc}

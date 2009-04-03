\name{plotTransect}

\alias{plotTransect}

\title{Plot Profile and Raster Scan Data}

\description{
The profile along a transect and/or an interpolated raster distribution within the cross-sectional area of the transect is drawn.
}

\usage{
plotTransect(id, rasterField = NULL, velPlot = FALSE, flow = "magnitude")
}

\arguments{
\item{id}{the name of the transect.}
\item{rasterField}{the raster field of interest, see the \code{type} component in \code{\link{tran.dat}}.}
\item{velPlot}{a logical; if \code{TRUE} velocity raster data is plotted.}
\item{flow}{the velocity component types to use where \code{"magnitude"} is the principle direction of flow and \code{"longTran"} is the longitudinal and transverse directions.}
}

\details{
If the \code{rasterField} argument is missing a profile is plotted along the transect. 
The horizontal axis is representative of the distance between transect vertices and 
the vertical axis the distance from the vertex origin, \code{v.origin}, positive upward. 
The profile along a transect is by default based on the interpolated surface, \code{data.tin}. 
If the \code{data.tin} component of \code{\link{srvy.dat}} is \code{NULL} the function will attempt to 
access imported profile data in \code{prof}.
}

\author{Fisher, J. C.}

\seealso{
\code{mba.points}, \code{\link{filled.contour}}, \code{\link{tran.profile}}
}

\examples{
data(confluence)
plotTransect("T1")
plotTransect("T1", rasterField = "SpCond (uS/cm)")
plotTransect("T1", velPlot = TRUE, flow = "longTran")
}

\keyword{hplot}

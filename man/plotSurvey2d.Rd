\name{plotSurvey2d}

\alias{plotSurvey2d}

\title{Plot Points or Interpolated Surface}

\description{
Draws a scatter plot with arrows or contour plot with arrows. A key showing how the colors map to state variable values is shown to the right of the plot.
}

\usage{
plotSurvey2d(x = NULL, y = NULL, z = NULL, vx = NULL, vy = NULL, 
type = "p", xlim = NULL, ylim = NULL, zlim = NULL, xlab = NULL, 
ylab = NULL, zlab = NULL, asp = NA, csi = NA, width = 7, 
pointsize = 12, cex.pts = 1, nlevels = 20, rkey = FALSE, 
color.palette = terrain.colors, vuni = FALSE, vmax = NULL, 
vxby = NULL, vyby = NULL)
}

\arguments{
\item{x}{numeric; a vector of \code{x} coordinates for the plot. If \code{x} is a list, its components \code{x$x}, \code{x$y}, \code{x$z}, \code{x$vx}, and \code{x$vy} are used for \code{x}, \code{y}, \code{z}, \code{vx}, and \code{vy}, respectively.}
\item{y}{numeric; a vector of \code{y} coordinates for the plot}
\item{z}{matrix; the state variable values to be plotted, NAs allowed (optional if \code{type = "p"})}
\item{vx}{numeric; a vector of arrow component lengths in the \code{x} direction (optional)}
\item{vy}{numeric; a vector of arrow component lengths in the \code{y} direction (optional)}
\item{type}{character; a 1-character string giving the type of plot desired. The following values are possible: "p" for points, "l" for level contour, "g" for grid contour.}
\item{xlim}{numeric; a vector of \code{x} limits \code{(x1,x2)} for the plot}
\item{ylim}{numeric; a vector of \code{y} limits \code{(y1,y2)} for the plot}
\item{zlim}{numeric; a vector of \code{z} limits \code{(z1,z2)} for the plot}
\item{xlab}{character; the label for the \code{x} axis}
\item{ylab}{character; the label for the \code{y} axis}
\item{zlab}{character; the label for the \code{z} legend}
\item{asp}{numeric; the \code{y/x} aspect ratio}
\item{csi}{numeric; height of text characters in inches}
\item{width}{numeric; the width of the plotting window canvas in inches}
\item{pointsize}{integer; the point size of plotted text}
\item{cex.pts}{numeric; the amount by which point symbols should be magnified relative to the default.}
\item{nlevels}{integer; number of contour levels desired}
\item{rkey}{logical; if \code{TRUE} the legend key is reversed with \code{z} values descending from top to bottom}
\item{color.palette}{function; a color \link{palette} to be used to assign colors in the plot}
\item{vuni}{logical; if \code{TRUE} all arrow lengths are set equal}
\item{vmax}{numeric; the maximum length of arrows in inches}
\item{vxby}{integer; increment for the sequence of arrows in the \code{x} direction}
\item{vyby}{integer; increment for the sequence of arrows in the \code{y} direction}
}

\author{Fisher, J. C.}

\seealso{
\code{\link{filled.contour}}, \code{\link{image}}, \code{\link{arrows}}
}

\examples{
data(project)

d <- srvy.dat("data.pts")
plotSurvey2d(d, type = "p")

d <- srvy.dat("data.grd")
plotSurvey2d(d, type = "l")
plotSurvey2d(d, type = "g")
}

\keyword{hplot}

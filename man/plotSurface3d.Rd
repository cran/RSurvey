\name{plotSurface3d}

\alias{plotSurface3d}

\title{Plot 3D Interpolated Surface using OpenGL}

\description{
A three-dimensional (\acronym{3D}) surface plot of the processed survey data is drawn.
}

\usage{
plotSurface3d()
}

\details{
The interpolated survey data is rendered using \pkg{rgl}, a \acronym{3D} 
visualization device system (OpenGL) for \R. 
The mouse is used for interactive viewpoint navigation where
the left, right, and center mouse buttons rotate the scene, rotate the scene around 
the x-axis, and zooms the display, respectively.
}

\author{Fisher, J. C.}

\seealso{
\code{surface3d}
}

\examples{
data(confluence)
plotSurface3d()

rgl.quit()
}

\keyword{hplot}

\name{srvy.process}

\alias{srvy.process}

\title{Process Data}

\description{
This function processes the imported data, \code{data.raw}, by reducing its spatial and temporal range, applying offsets, 
adjusting for depth readings, correcting for erroneous measurements, and interpolating values.
}

\usage{srvy.process(const.3d = FALSE)}

\arguments{
\item{const.3d}{logical; if \code{TRUE} an interpoled surface is constructed}
}

\details{
The spatial range for point data is reduced using: \code{min.x}, \code{max.x}, \code{min.y}, and \code{max.y} the 
minimum \code{x}, maximum \code{x}, minimum \code{y}, and maximum \code{y} spatial range values, respectively; 
\code{min.t} and \code{max.t} the minimum and maximum temporal range values, respectively; and 
\code{polyRange}, a polygon that defines the maximum spatial extent for the \code{(x,y)} data (see \code{\link{srvy.dat}}).

State variable and temporal offsets are applied using \code{off.z} and \code{off.t}, respectively. 
Note that \code{off.t} is specified in seconds (see \code{\link{srvy.dat}}).

If \code{depth} is \code{TRUE} the state variable is designated as the depth below water surface and  
\code{z} values will be subtracted from the user specified water surface elevation, \code{wtr.elev}; 
depth values will then be plotted as elevations (see \code{\link{srvy.dat}}).

Erroneous state variable measurements are identified by calculating the change in \code{z} over 
the change in time for sequential data records. Those gradients exceeding a user defined tolerance, 
\code{grad.tol}, are identified as erroneous measurements and excluded from the data-set. 
The process is then repeated until all gradients are less than the user specified tolerance. 
An exception to the tolerance constraint is made for sequential records whose time difference is greater than 
a specified time gap, \code{time.gap} (see \code{\link{srvy.dat}}).

If \code{const.3d} is \code{TRUE} an interpolated grid of \code{z} values in constructed using a 
Multilevel B-spline approximation. The spatial extent of the interpolated surface is constrained using 
\code{polyLimit}, a polygon that sets spatial limits for grid data; where \code{z} values cooresponding to 
grid nodes located outside this polygon are set to \code{NA} (see \code{\link{srvy.dat}}).
}

\value{
Returns the \code{data.pts} data frame and \code{data.grd} list, see \code{\link{srvy.dat}}.
}

\author{Fisher, J. C.}

\seealso{
\code{mba.surf}
}

\examples{
data(project)
srvy.process(const.3d = TRUE)
}

\keyword{manip}

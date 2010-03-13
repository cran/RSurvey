\name{srvy.dat}

\alias{srvy.dat}

\docType{data}

\title{Set or Query Data and Parameters}

\description{
A function used to set or query data and parameters.
}

\usage{
srvy.dat(option, value, clearAll = FALSE)
}

\arguments{
\item{option}{character; the parameter name, described in the Parameters section}
\item{value}{a parameter value specified for \code{option}.}
\item{clearAll}{logical; if \code{TRUE} all parameters are cleared from \code{srvy.dat}.}
}

\section{Data}{
  Imported data from \acronym{ASCII} files is stored in \code{data.raw}, an object of class data.frame with at least the following components:
  \describe{
    \item{\code{[,1]}}{numeric; a vector of \code{x} coordinates}
    \item{\code{[,2]}}{numeric; a vector of \code{y} coordinates}
  }
  Processed point data is saved in \code{data.pts}, an object of class data.frame with components:
  \describe{
    \item{\code{x, y}}{numeric; a vector of \code{x} and \code{y} coordinates}
    \item{\code{z}}{numeric; a vector of state variable values (optional)}
    \item{\code{t}}{POSIXlt; a vector of time stamps (optional)}
    \item{\code{vx, vy, vz}}{numeric; a vector of velocity components in the \code{x}, \code{y}, and \code{z} directions, respectively (optional)}
  }
  Interpolated surface data is saved in \code{data.grd}, an object of class list with components: 
  \describe{
    \item{\code{x, y}}{numeric; a vector of grid line locations at which the values in \code{z} are measured.}
    \item{\code{z}}{matrix; interplated surface of state variable with rows and columns corresponding to grid lines in the \code{x} and \code{y} directions, respectively.}
    \item{\code{vx, vy, vz}}{matrix; interpolated surface of velocity components with rows and columns corresponding to grid lines in the \code{x} and \code{y} directions, respectively (optional).}
  }
}

\section{Parameters}{
  \describe{
    \item{\code{asp.yx}}{numeric; the \code{y/x} aspect ratio}
    \item{\code{asp.zx}}{numeric; the \code{z/x} aspect ratio}
    \item{\code{cex.pnt}}{numeric; a value giving the amount by which plotting point symbols should be magnified relative to the default.}
    \item{\code{cols}}{list; with list components for each column in \code{data.raw} having the following character string components: \code{name}, the column name; \code{unit}, the units associated with the data in the column; \code{class}, the class of data in the column.}
    \item{\code{csi}}{numeric; the height of plotted characters in inches}
    \item{\code{data.file}}{character; the pathname of the imported text file used to populate \code{data.raw}}
    \item{\code{date.fmt}}{character; the export format for calendar dates and times, see \code{\link{strptime}} for details; its default is \code{"\%Y-\%m-\%d \%H:\%M:\%OS"}}
    \item{\code{default.dir}}{character; the pathname of the default directory when saving or opening files; the default is the current working directory, \code{\link{getwd}()}.}
    \item{\code{depth}}{logical; if \code{TRUE} the state variable will be treated as a depth below water surface and converted to elevations using \code{wtr.elev}; its default is \code{FALSE}.}
    \item{\code{encoding}}{character; the encoding to be assumed for input strings. If the value is \code{"latin1"} or \code{"UTF-8"} it is used to mark character strings as known to be in Latin-1 or UTF-8: it is not used to re-encode the input.}
    \item{\code{font.gui}}{character; the text font, family and size, for all \acronym{GUI}s; the default is \code{"arial 8"}}
    \item{\code{grad.tol}}{numeric; the gradient tolerance \code{(dz/dt)} for temporal data correction}
    \item{\code{grid.dx, grid.dy}}{numeric; the grid spacing in the interpolated surface, \code{data.grd}}
    \item{\code{img.contour}}{logical; if \code{TRUE} the \code{image} function will be used to plot the interpolated surface; if \code{FALSE} (the default) the \code{filled.contour} function will be used.}
    \item{\code{limits}}{list; with the following components: \code{x1}, a placeholder for the minimum x-axis limit; \code{x2}, a placeholder for the maximum x-axis limit; \code{x1.chk}, a logical value when \code{TRUE} \code{x1} will be used; \code{x2.chk}, a logical value when \code{TRUE} \code{x2} will be used; \code{xlim}, the \code{x} limits for plotting; \code{y1}; \code{y2}; \code{y1.chk}; \code{y2.chk}; \code{ylim}; \code{z1}; \code{z2}; \code{z1.chk}; \code{z2.chk}; \code{zlim}; \code{t1}; \code{t2}; \code{t1.chk}; \code{t2.chk}; \code{tlim}}
    \item{\code{max.t}}{POSIXlt; the maximum range for date-time values}
    \item{\code{max.x}}{numeric; maximum range for \code{x} values}
    \item{\code{max.y}}{numeric; maximum range for \code{y} values}
    \item{\code{max.z}}{numeric; maximum range for \code{z} values}
    \item{\code{min.t}}{POSIXlt; the minimum range for date-time values}
    \item{\code{min.x}}{numeric; minimum range for \code{x} values}
    \item{\code{min.y}}{numeric; minimum range for \code{y} values}
    \item{\code{nlevels}}{integer; number of contour levels desired; the default is \code{20}.}
    \item{\code{off.t}}{numeric; temporal offset in seconds}
    \item{\code{off.z}}{numeric; state variable offset}
    \item{\code{poly}}{list; with components of class \code{"gpc.poly"}}
    \item{\code{polyRange}}{character; name of the polygon component in \code{poly} that defines the data range}
    \item{\code{polyLimit}}{character; name of the polygon component in \code{poly} that defines the data limits}
    \item{\code{proj.file}}{character; the pathname of the binary project file}
    \item{\code{projection}}{character; a character string describing the geographical projection}
    \item{\code{rkey}}{logical; if \code{TRUE} the values within the plotted state variable key are reversed; its default is \code{FALSE}.}
    \item{\code{sep}}{character; the field separator string. Values on each line of the file are separated by this string. The default separator is the \code{TAB} character.}
    \item{\code{show.arrows}}{logical; if \code{TRUE} the vector arrows will be plotted; its default is \code{FALSE}.}
    \item{\code{show.lines}}{logical; if \code{TRUE} the line contours will be plotted on the interpolated surface; the default is \code{FALSE}.}
    \item{\code{show.points}}{logical; if \code{TRUE} the points associated with \code{(x,y)} will be plotted on the interpolated surface; its default is \code{FALSE}.}
    \item{\code{show.poly}}{logical; if \code{TRUE} the range polygon is added to the points plot and the limits polygon added to the surface plot; its default is \code{FALSE}.}
    \item{\code{time.gap}}{numeric; the time gap exceedance level between sequential data records in seconds}
    \item{\code{vars}}{list; with the following components: \code{x} is the \code{cols} component index specifying the column of \code{x} values; \code{y} is the index specifying \code{y} values; \code{z} is the index specifying \code{z} values; \code{vx} is the index specifying \code{vx} values; and \code{vy} is the index specifying \code{vy} values.}
    \item{\code{vmax}}{numeric; maximum length of plotted vector arrows in inches}
    \item{\code{ver}}{character; the package version number}
    \item{\code{vuni}}{logical; if \code{TRUE} a constant vector arrow length specified by \code{vmax} is used; its default is \code{FALSE}.}
    \item{\code{vxby}}{numeric; increment vector arrows in the x-axis by this amount}
    \item{\code{vyby}}{numeric; increment vector arrows in the y-axis by this amount}
    \item{\code{width}}{numeric; the default width for windows devices in inches; the default is \code{7} inches.}
    \item{\code{win.loc}}{character; the default horizontal and vertical location for \acronym{GUI} placement in pixels}
    \item{\code{wtr.elev}}{numeric; the water surface elevation}
  }
}

\value{
If \code{value} is given the object specified by \code{option} is returned. A \code{NULL} value is returned for objects 
not yet assigned a value and where no default value is available.
}

\author{Fisher, J. C.}

\examples{
# To set a parameter
srvy.dat("test1", 3.14159265)
srvy.dat("test2", list(id = "PI", val = 3.14159265))
# To retrieve a parameter value
srvy.dat("test1")
srvy.dat("test2")
srvy.dat(c("test2", "id"))
srvy.dat(c("test2", "val"))
# To get all parameter values
d <- srvy.dat()
# To clear all parameters, use at your own risk
\dontrun{srvy.dat(clearAll = TRUE)}
}

\keyword{sysdata}

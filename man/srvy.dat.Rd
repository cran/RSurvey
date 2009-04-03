\name{srvy.dat}

\alias{srvy.dat}

\docType{data}

\title{Set or Query Survey Data}

\description{
A function used to set or query survey parameters and data.
}

\usage{
srvy.dat(option, value, clearAll = FALSE)
}

\arguments{
\item{option}{the parameter name, described in the Survey Parameters section.}
\item{value}{a parameter value specified for \code{option}.}
\item{clearAll}{a logical; if \code{TRUE} all parameters are cleared from \code{srvy.dat}.}
}

\details{
The data frame \code{data.raw} has the following components:
  \tabular{lll}{
    \code{[,1]}  \tab numeric \tab date/time, \code{"\%Y-\%m-\%d \%H:\%M:\%OS"} (optional).\cr
    \code{[,2]}  \tab numeric \tab locations in the x-direction.\cr
    \code{[,3]}  \tab numeric \tab locations in the y-direction.\cr
    \code{[,4:]} \tab numeric \tab state variables.
  }
The data frame \code{data.mod} has the following components:
  \tabular{llll}{
    \code{[,1]} \tab \code{x} \tab numeric \tab locations in the x-direction.\cr
    \code{[,2]} \tab \code{y} \tab numeric \tab locations in the y-direction.\cr
    \code{[,3]} \tab \code{z} \tab numeric \tab state variable.
  }
The list \code{data.tin} contains the following components: 
  \tabular{lll}{
    \code{x} \tab numeric \tab locations in the x-direction.\cr
    \code{y} \tab numeric \tab locations in the y-direction.\cr
    \code{z} \tab matrix  \tab rows and columns corresponding to grid lines in the x and y directions, respectively.
  }
}

\section{Survey Parameters}{
  \tabular{lll}{
    \code{cols}        \tab character \tab fields within the imported survey data, a concatenation of column type and units.\cr
    \code{csi}         \tab numeric   \tab the height of plotted characters (in).\cr
    \code{data.file}   \tab character \tab the imported text file containing survey data.\cr
    \code{data.raw}    \tab data.frame\tab raw survey data, see Details section.\cr
    \code{data.mod}    \tab data.frame\tab processed survey data, see Details.\cr
    \code{data.tin}    \tab list      \tab the interpolated surface derived from processed survey data, see Details.\cr
    \code{date.fmt}    \tab character \tab the date/time format for exported data, see \code{\link{srvy.export}}\cr
    \code{default.dir} \tab character \tab the default directory for saving and opening files.\cr
    \code{depth}       \tab logical   \tab if \code{TRUE} depths are converted to elevations during the interpolation.\cr 
    \code{ed}          \tab character \tab end date\cr
    \code{eh}          \tab numeric   \tab end hour\cr
    \code{em}          \tab numeric   \tab end minute\cr
    \code{encoding}    \tab character \tab encoding to be assumed for input strings. Mark character strings as Latin-1 or UTF-8.\cr
    \code{es}          \tab numeric   \tab end second\cr
    \code{font.gui}    \tab character \tab the \acronym{GUI} font, family and size.\cr
    \code{grad.tol}    \tab numeric   \tab tolerance value for state variable correction.\cr 
    \code{grid.res}    \tab numeric   \tab the grid resolution for interpolation.\cr
    \code{max.vect}    \tab numeric   \tab the maximum length for plotting velocity vector arrows.\cr
    \code{max.x}       \tab numeric   \tab maximum x value\cr
    \code{max.y}       \tab numeric   \tab maximum y value\cr 
    \code{max.z}       \tab numeric   \tab maximum z value\cr
    \code{min.x}       \tab numeric   \tab minimum x value\cr
    \code{min.y}       \tab numeric   \tab minimum y value\cr 
    \code{n.levels}    \tab numeric   \tab the approximate number of contour intervals.\cr 
    \code{off.t}       \tab numeric   \tab time offset in seconds.\cr 
    \code{off.z}       \tab numeric   \tab state variable offset\cr
    \code{poly}        \tab list      \tab a polygon describing the spatial domain, class \code{"gpc.poly"}.\cr
    \code{poly.file}   \tab character \tab the imported polygon file.\cr
    \code{proj.file}   \tab character \tab the binary project file.\cr
    \code{projection}  \tab character \tab a description of the geographical projection in use.\cr
    \code{sd}          \tab character \tab start date\cr
    \code{sep}         \tab character \tab the field separator character. Values on each line of the file are separated by this character.\cr
    \code{sh}          \tab numeric   \tab start hour\cr
    \code{sm}          \tab numeric   \tab start minute\cr
    \code{ss}          \tab numeric   \tab start second\cr
    \code{time.gap}    \tab numeric   \tab the time gap exceedance level between sequential data records (sec).\cr
    \code{vars}        \tab character \tab field names corresponding to data/time, x, y, and z.\cr
    \code{ver}         \tab character \tab the version of \pkg{RSurvey}.\cr
    \code{win.loc}     \tab character \tab the default horizontal and vertical location for \acronym{GUI} placement (pixels).\cr
    \code{win.width}   \tab numeric   \tab the default width for windows devices (in).\cr
    \code{wtr.elev}    \tab numeric   \tab the water surface elevation.\cr
    \code{yx.ratio}    \tab numeric   \tab the the y/x aspect ratio.\cr
    \code{zx.ratio}    \tab numeric   \tab the the z/x aspect ratio.
  }
}

\author{Fisher, J. C.}

\examples{
# To set a parameter
srvy.dat("grad.tol", 0.01)
srvy.dat("projection", "UTM")
# To get a parameter value
srvy.dat("grad.tol")
# To get all parameter values
srvy.dat()
# To clear all parameters
srvy.dat(clearAll = TRUE)
}

\keyword{sysdata}

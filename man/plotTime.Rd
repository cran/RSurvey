\name{plotTime}

\alias{plotTime}

\title{Plot Temporal Data}

\description{
A time series plot is drawn with points and connecting lines.
}

\usage{
plotTime(x, y = NULL, xlim = NULL, ylim = NULL, ylab = NULL, gap = NULL, 
width = 7, pointsize = 10, fmt = "\%H:\%M:\%OS\\n\%Y-\%m-\%d")
}

\arguments{
\item{x}{POSIXlt; a vector specifying \code{x} values}
\item{y}{numeric; a vector specifying \code{y} values}
\item{xlim}{POSIXlt; the \code{x} limits \code{(x1,x2)} of the plot}
\item{ylim}{numeric; the \code{y} limits \code{(y1,y2)} of the plot}
\item{ylab}{character; the label for the \code{y} axis}
\item{gap}{numeric; time gap exceedance level in seconds}
\item{width}{numeric; the width of the plotting window canvas in inches}
\item{pointsize}{integer; the point size of plotted text}
\item{fmt}{character; date-time format for \code{x} axis tic mark labels, see \code{\link{strptime}}}
}

\details{
Line segments will not be drawn where time differences between consecutive points are greater than \code{gap}.
}

\author{Fisher, J. C.}

\seealso{
\code{\link{plot}}
}

\examples{
data(project)
d <- srvy.dat("data.pts")
plotTime(x = d$t, y = d$z)
plotTime(x = d$t, y = d$z, gap = 3000)
}

\keyword{hplot}

\name{srvy.config}

\alias{srvy.config}

\title{Window and Plotting Parameters}

\description{
A \acronym{GUI} used to define window geometry and universal plotting parameters.
}

\usage{
srvy.config(tt1 = NULL)
}

\arguments{
\item{tt1}{a Tk toplevel widget.}
}

\details{
The output date/time format is based on character representations described in 
\code{\link{strptime}}, for example \code{"\%d/\%m/\%Y \%H:\%M:\%OS"}.
}

\value{
The following components within \code{\link{srvy.dat}} are set: 
\code{n.levels}, \code{win.windth}, \code{yx.ratio}, \code{zx.ratio}, \code{date.fmt}.
}

\author{Fisher, J. C.}

\examples{
\donttest{srvy.config()}
}

\keyword{misc}

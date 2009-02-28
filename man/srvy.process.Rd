\name{srvy.process}

\alias{srvy.process}

\title{Process Survey Data}

\description{
This function limits the spatial and temporal domains of raw data and corrects for erroneous 
state variable measurements.
}

\usage{srvy.process(const.tin = FALSE)}

\arguments{
\item{const.tin}{a logical; if \code{TRUE} interpolation is used to construct a surface map of the state variable.}
}

\details{
Erroneous state variable measurements are identified by calculating the change in \code{z} over 
the change in time for sequential data records. Those gradients exceeding a user defined tolerance, 
\code{grad.tol} (see \code{\link{srvy.dat}}), are identified as erroneous measurements. After excluding these measurements 
the process is repeated until all gradients are less than the user specified tolerance.  An exception to the tolerance 
constraint is made for sequential records whose time difference is greater than 
a specified time gap, \code{time.gap} (see \code{\link{srvy.dat}}).
}

\value{
A data frame \code{data.mod} and list \code{data.tin} are stored within \code{srvy.dat}.
}

\author{Fisher, J. C.}

\seealso{
\code{mba.surf}
}

\examples{
data(confluence)
srvy.process(const.tin = TRUE)
}

\keyword{manip}

\name{srvy.config}

\alias{srvy.config}

\title{Set Window and Plotting Parameters}

\description{
A \acronym{GUI} for specifying window geometry and universal plotting parameters.
}

\usage{
srvy.config(parent)
}

\arguments{
\item{parent}{tkwin; the parent window (optional)}
}

\details{
The output date-time format is based on a character string representation described in 
\code{\link{strptime}}, for example \code{"02/26/2010 17:05:39.61"} is represented using 
\code{"\%d/\%m/\%Y \%H:\%M:\%OS"}.
}

\author{Fisher, J. C.}

\examples{
\donttest{srvy.config()}
}

\keyword{misc}

\name{mergeData}

\alias{mergeData}

\title{Merge Time-stamped Data}

\description{
This function merges temporal data with the current time-stamped survey data. 
}

\usage{
mergeData(file = NULL)
}

\arguments{
\item{file}{either a character string naming a file or a connection.}
}

\format{
The tab delineated temporal file, \file{*.txt}, contains the following variables:
  \tabular{lll}{
    \code{[,1]}  \tab character \tab date/time\cr
    \code{[,2:]} \tab numeric   \tab state variables
  }
The first \code{[1,]} and second \code{[2,]} rows are reserved for column descriptors and units, respectively. 
Units associated with date/time values are based on format character strings described in \code{\link{strptime}}.
}

\value{
The \code{cols} and \code{data.raw} components of \code{\link{srvy.dat}} are replaced by the 
merged data set.
}

\note{
This function is not currently accessible through a \acronym{GUI}.
}

\author{Fisher, J. C.}

\seealso{
\code{\link{approx}}
}

\examples{
f <- system.file("RSurvey-ex/river.txt", package = "RSurvey")
con <- file(f, open = "r", encoding = "latin1")
d <- readFile(con)
srvy.dat("cols", d$cols)
srvy.dat("vars", d$vars)
srvy.dat("data.raw", d$dat)

f <- system.file("RSurvey-ex/river-temperature.txt", package = "RSurvey")
con <- file(f, open = "r", encoding = "latin1")
mergeData(con)
srvy.dat("cols")
srvy.dat("data.raw")
}

\keyword{misc}

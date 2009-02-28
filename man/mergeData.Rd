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
    \code{[,2:]} \tab numeric   \tab state variable(s)
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
srvy.import(file = con)
srvy.dat("cols")
summary(srvy.dat("data.raw"))

## A sample temporal file including a header and two rows of data.

## Date / Time	Temperature
## %d/%m/%Y %I:%M:%OS%p	F
## 07/05/2006 12:32:00.000PM	79.85
## 07/05/2006 12:32:00.000PM	NA

f <- system.file("RSurvey-ex/river-temperature.txt", package = "RSurvey")
con <- file(f, open = "r", encoding = "latin1")
mergeData(file = con)
srvy.dat("cols")
summary(srvy.dat("data.raw"))
}

\keyword{misc}

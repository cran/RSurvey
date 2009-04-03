\name{readFile}

\alias{readFile}

\title{Data Input}

\description{
Reads a file in table format.
}

\usage{
readFile(file, fields = TRUE, units = TRUE)
}

\arguments{
\item{file}{either a character string naming a file or a connection.}
\item{fields}{a logical value indicating whether the file contains the names of the variables as its first line.}
\item{units}{a logical value indicating whether the file contains the units of the variables as its second line.}
}

\format{
A tab delineated file, \file{*.txt}, containing the following variables:
  \tabular{lll}{
    \code{[,1]}  \tab character \tab date/time (optional)\cr
    \code{[,2]}  \tab numeric   \tab x values\cr
    \code{[,3]}  \tab numeric   \tab y values\cr
    \code{[,4:]} \tab numeric   \tab z values, state variable(s)
  }
The first \code{[1,]} and second \code{[2,]} rows are reserved for column descriptors and units, respectively. 
Units associated with date/time values are based on format character strings described in \code{\link{strptime}}, 
for example \code{"\%d/\%m/\%Y \%H:\%M:\%OS"}. You can embed comments in the data file by using the 
\dQuote{\code{#}} character. Anything after a \dQuote{\code{#}} on a line will be ignored.
}

\value{
A list with the following components: \code{cols}, a character array of column names. 
\code{vars}, a character array of column names associated with the  
date/time \code{[1]}, x \code{[2]}, y \code{[3]}, and z \code{[4]} variables. 
\code{data.raw}, a data frame of raw survey data.
}

\author{Fisher, J. C.}

\seealso{
\code{\link{read.table}}
}

\examples{
## A sample file including a header and two rows of data.

## Date / Time	Easting	Northing	Depth
## \%d/\%m/\%Y \%H:\%M:\%OS	m	m	m
## 06/01/2001 01:01:59.000	727972.52	4138308.65	3.12
## 06/01/2001 01:01:59.200	727972.51	4138308.68	3.12

f <- system.file("RSurvey-ex/lake.txt", package = "RSurvey")
con <- file(f, open = "r", encoding = "latin1")
d <- readFile(con)
srvy.dat("cols", d$cols)
srvy.dat("vars", d$vars)
srvy.dat("data.raw", d$dat)
}

\keyword{file}

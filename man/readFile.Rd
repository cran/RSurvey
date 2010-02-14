\name{readFile}

\alias{readFile}

\title{Read Data File}

\description{
Reads an \acronym{ASCII} file in table format and creates a data frame from it.
}

\usage{
readFile(file, fields = TRUE, units = TRUE, sep = "\t", 
encoding = getOption("encoding"))
}

\arguments{
\item{file}{character; the name of the file which the data will be read from.  Alternatively, \code{file} can be a readable text-mode \code{\link{connection}} (optional).}
\item{fields}{logical; if \code{TRUE} the \code{file} contains the names of the variables as its first line.}
\item{units}{logical; if \code{TRUE} the \code{file} contains the units of the variables as its second line.}
\item{sep}{character; the field separator string. Values on each line of the file are separated by this string.}
\item{encoding}{character; encoding to be assumed for input strings. If the value is \code{"latin1"} or \code{"UTF-8"} it is used to mark character strings as known to be in Latin-1 or UTF-8: it is not used to re-encode the input.}
}

\format{
A tab delineated text file containing at least the following variables:
  \describe{
    \item{\code{[,1]}}{numeric; a vector of \code{x} coordinates}
    \item{\code{[,2]}}{numeric; a vector of \code{y} coordinates}
  }
The first \code{[1,]} and second \code{[2,]} rows are reserved for column descriptors and units, respectively. 
Units associated with date-time values are based on format character strings described in \code{\link{strptime}}, 
for example \code{"\%d/\%m/\%Y \%H:\%M:\%OS"}. You may embed comments in the data file by using the 
\dQuote{\code{#}} character. Anything after a \dQuote{\code{#}} on a line will be ignored.
}

\value{
Returns a list with the following components: 
\item{dat}{data.frame; a table of data}
\item{cols}{character; a vector of column names}
\item{vars}{character; a vector giving the initial guesses of column names associated with the date-time, \code{x}, \code{y}, and \code{z} variables.}
}

\author{Fisher, J. C.}

\seealso{
\code{\link{read.table}}
}

\examples{
# A sample of the tab delineated formatting is provided below and includes 
# a header and two rows of data. Note that the '##' are not include.

## Date Time	X	Y	Z
## %Y-%m-%d %H:%M:%OS	m	m	m
## 2009-04-24 10:43:38.275	 7.8	-2.3	-0.2650
## 2009-04-24 11:20:43.757	 5.0	-4.7	-0.3790

f <- system.file("RSurvey-ex/DataExample.dat", package = "RSurvey")
con <- file(f, open = "r", encoding = "latin1")
d <- readFile(con)
close(con)
}

\keyword{file}

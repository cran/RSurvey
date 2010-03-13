\name{readFile}

\alias{readFile}

\title{Read Data File}

\description{
Reads an table formatted \acronym{ASCII} file and creates a data frame from it.
}

\usage{
readFile(file, fields = TRUE, units = TRUE, sep = "\t", 
encoding = getOption("encoding"))
}

\arguments{
\item{file}{character; the name of the file which data will be read from.  Alternatively, \code{file} can be a readable text-mode \code{\link{connection}} (optional).}
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
for example "02/26/2010 02:05:39 PM" is represented using "\%d/\%m/\%Y \%I:\%M:\%S \%p".
You may embed comments in the data file by using the \dQuote{\code{#}} 
character string; anything after a \dQuote{\code{#}} on a line will be ignored. 
The \dQuote{\code{NA}} character string is interpreted as a \code{\link{NA}} value. 

For example, a data file containing a header and three rows of data might look like:
\tabular{lrrrrl}{
# Ignored character string \tab \tab \tab \tab \tab \cr
Date Time \tab X \tab Y  \tab Z \tab Concentration \tab # Column names\cr
\%Y-\%m-\%d \%H:\%M:\%OS \tab m \tab m \tab m \tab mg/L \tab # Column units\cr
2008-02-03 10:43:38.275 \tab 7.83 \tab -2.3 \tab 0.2650 \tab 6.211E-6 \tab \cr
2008-04-24 11:20:43.757 \tab 15.01 \tab 8.1 \tab NA \tab 3.135E-5 \tab # Comment\cr
2009-04-25 13:02:11.123 \tab -4.73 \tab 23.7 \tab 0.3790 \tab 1.729E-6 \tab 
}
}

\value{
Returns a list with the following components: 
\item{dat}{data.frame; a table of data}
\item{cols}{character; a vector of column names}
\item{vars}{character; a vector giving an initial guess for column names associated with the date-time, \code{x}, \code{y}, and \code{z} variables.}
}

\author{Fisher, J. C.}

\seealso{
\code{\link{read.table}}
}

\examples{
f <- system.file("RSurvey-ex/DataExample.dat", package = "RSurvey")
con <- file(f, open = "r", encoding = "latin1")
d <- readFile(con)
close(con)
}

\keyword{file}

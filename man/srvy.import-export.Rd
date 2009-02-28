\name{srvy.import-export}

\alias{srvy.import}
\alias{srvy.export}

\title{Import and Export Spatially Distributed Data}

\description{
This function imports and exports survey data.
}

\usage{
srvy.import(file = NULL)
srvy.export(file = NULL)
}

\arguments{
\item{file}{either a character string naming a file or a connection.}
}

\format{
The tab delineated file, \file{*.txt}, contains the following variables:
  \tabular{lll}{
    \code{[,1]}  \tab character \tab date/time (optional)\cr
    \code{[,2]}  \tab numeric   \tab x values\cr
    \code{[,3]}  \tab numeric   \tab y values\cr
    \code{[,4:]} \tab numeric   \tab z values, state variable(s)
  }
The first \code{[1,]} and second \code{[2,]} rows are reserved for column descriptors and units, respectively. 
Units associated with date/time values are based on format character strings described in \code{\link{strptime}}, 
for example \code{"\%d/\%m/\%Y \%H:\%M:\%OS"}.
}

\value{
The \code{srvy.import} function populates the \code{data.raw}, \code{data.file}, \code{cols}, 
and \code{vars} components of \code{\link{srvy.dat}}. 
The output of \code{srvy.export} is dependent on the file type 
chosen within the file management pop up dialog box.  Choosing a file type of \file{*.txt} 
writes the contents of \code{data.mod} to a file, whereas a choice of \file{*.tin} writes 
the contents of \code{data.tin} to a file.
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

f <- system.file("RSurvey-ex/confluence.txt", package = "RSurvey")
con <- file(f, open = "r", encoding = "latin1")
srvy.import(file = con)
\donttest{
f <- paste(getwd(), "/test.txt", sep="")
con <- file(f, open = "w", encoding = "latin1")
srvy.export(file = con)
}

}

\keyword{file}

\name{tran.import-export}

\alias{tran.import}
\alias{tran.export}

\title{Import and Export Transect Data}

\description{
This function imports and exports transect data.
}

\usage{
tran.import(type, id, file = NULL)
tran.export(type, id, file = NULL)
}

\arguments{
\item{type}{a character string giving the desired data type (\var{transect}, \var{profile}, and \var{raster}).  See Format section.}
\item{id}{the name of the transect.}
\item{file}{either a character string naming a file or a connection.}
}

\format{
For \var{transect} data the text file, \file{*.txt}, contains the deparsed \code{\link{tran.dat}} object.

For \var{profile} data the tab delineated text file, \file{*.txt}, reserves the first \code{[1,]} 
row for column descriptors and contains the following components:
\tabular{lll}{
  \code{[,1]}  \tab numeric   \tab the distance along the local x-axis measured from the vertex origin.\cr
  \code{[,2]}  \tab numeric   \tab z values along the profile.
}

For \var{raster} data the tab delineated text file, \file{*.txt}, reserves the first \code{[1,]} 
and second \code{[2,]} rows for column descriptors and contains the following components:
\tabular{lll}{
  \code{[,1]}  \tab numeric \tab the distance along the local x-axis measured from the vertex origin.\cr
  \code{[,2]}  \tab numeric \tab local z values.\cr
  \code{[,3:]} \tab numeric \tab raster state variable(s).
}
}

\value{
The \var{transect} option replaces or creates the \code{id} component in \code{\link{tran.dat}}.  The \var{profile} and \var{raster} 
options replace the \code{prof} and \code{data.ras} components in \code{\link{tran.dat}}, respectively.
}

\author{Fisher, J. C.}

\seealso{
\code{\link{dput}}, \code{\link{read.table}}
}

\examples{
## A sample transect file:

## structure(list(id = "T1", vertices = structure(c(679163.534, 679238.561, 
## 4135735.518, 4135730.43), .Dim = c(2L, 2L), .Dimnames = list(c("1", "2"), 
## c("x", "y"))), fix.zero = "R", h.offset = 21, grid.dx = 0.05, grid.dy = 0.005, 
## asp.ratio = 5, v.origin = 11.379, v.offset = -0.18, data.file = "T1.txt"), 
## .Names = c("id", "vertices", "fix.zero", "h.offset", "grid.dx", "grid.dy", 
## "asp.ratio", "v.origin", "v.offset", "data.file"))

f <- system.file("RSurvey-ex/confluence-T1.txt", package = "RSurvey")
tran.import("transect", "T1", f)
\donttest{
f <- paste(getwd(), "/test.txt", sep="")
tran.export("transect", "T1", f)
}

## A sample profile file including a header and two rows of data.

## Distance	Elevation
## 14	10.499
## 16	10.429
\donttest{
f <- system.file("RSurvey-ex/confluence-profile-T1.txt", package = "RSurvey")
con <- file(f, open = "r", encoding = "latin1")
tran.import(type = "profile", id = "T1", file = con)

f <- paste(getwd(), "/test.txt", sep="")
con <- file(f, open = "w", encoding = "latin1")
tran.export(type = "profile", id = "T1", file = con)
}

## A sample raster file including a header and two rows of data.

## motoX	motoY	temp	pH	SpCond
## m	m	degC	units	uS/cm
## 0	-0.11	24.55	8.00	569.38
## 2	0	24.58	7.99	646.01

f <- system.file("RSurvey-ex/confluence-raster-T1.txt", package = "RSurvey")
con <- file(f, open = "r", encoding = "latin1")
tran.import(type = "raster", id = "T1", file = con)
\donttest{
f <- paste(getwd(), "/test.txt", sep="")
con <- file(f, open = "w", encoding = "latin1")
tran.export("T1", type="raster", file = con)
}
}

\keyword{file}

\name{polyImport}

\alias{polyImport}

\title{Import Spatial Domain Polygon}

\description{
This function imports a polygon from a text file.
}

\usage{
polyImport(file = NULL, dl = NULL)
}

\arguments{
\item{file}{either a character string naming a file or a connection.}
\item{dl}{the maximum distance between polygon vertices.  If \code{NULL}, no vertices are added.}
}

\format{
There are two possible formats for the tab delineated text file, \file{*.txt}. 
The first adheres to the \code{polyfile} format and 
the second adheres to the following 4 column format:
  \tabular{lll}{
    \code{[,1]} \tab integer \tab an index for vertices within a polygon.\cr
    \code{[,2]} \tab numeric \tab vertex location in the x-direction.\cr
    \code{[,3]} \tab numeric \tab vertex location in the y-direction.\cr
    \code{[,4]} \tab integer \tab a code, see Details.
  }
}

\details{
The \code{code} argument is used to identify polygons and vertex types. 
Letting \code{code = 0} for two sequential vertices will override \code{dl} and 
prevent additional vertices from being added between them. 
A \code{code < 2} indicates a vertex point is within the outer polygon. 
Inner polygons or holes are specified with a \code{code > 1}.
}

\value{
A polygon of class \code{gpc.poly-class}.
}

\note{
The \code{dl} argument will only work with the 4 column input file format.
}

\author{Fisher, J. C.}

\seealso{
\code{polyfile}
}

\examples{

## An example using the polyfile file format.
f <- system.file("RSurvey-ex/lake-poly.txt", package = "RSurvey")
con <- file(f, open = "r", encoding = "latin1")
ply <- polyImport(file = con)
pts <- get.pts(ply)
plot(ply)
points(pts[[1]], col = "red")

## An example using the 4 column file format.
f <- system.file("RSurvey-ex/confluence-poly.txt", package = "RSurvey")
con <- file(f, open = "r", encoding = "latin1")
ply <- polyImport(file = con)
pts <- get.pts(ply)
plot(ply)
for(i in 1:length(pts)) points(get.pts(ply)[[i]], col = "red")

con <- file(f, open = "r", encoding = "latin1")
ply <- polyImport(file = con, dl = 1)
pts <- get.pts(ply)
for(i in 1:length(pts)) points(get.pts(ply)[[i]], col = "green")
}

\keyword{misc}

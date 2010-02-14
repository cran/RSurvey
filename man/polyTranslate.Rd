\name{polyTranslate}

\alias{polyTranslate}

\title{Translate Alternate Polygon File Format}

\description{
Reads polygon information from a \acronym{ASCII} file.
}

\usage{
polyTranslate(file = NULL, dl = NULL)
}

\arguments{
\item{file}{character; the name of the file which the data are to be read from.  Alternatively, \code{file} can be a readable text-mode \code{\link{connection}} (optional).}
\item{dl}{numeric; the maximum distance between polygon vertices; if \code{NULL}, no vertices are added.}
}

\format{
The tab delineated text file adheres to the following 4 column format:
  \describe{
    \item{\code{[,1]}}{integer; an index for vertices within a polygon}
    \item{\code{[,2]}}{numeric; the vertex location in the x-direction}
    \item{\code{[,3]}}{numeric; the vertex location in the y-direction}
    \item{\code{[,4]}}{integer; a data code, see Details section}
  }
}

\details{
The code argument is used to identify polygons and vertex types. 
Letting code = 0 for two sequential vertices will override \code{dl} and 
prevent additional vertices from being added between them. 
A code < 2 indicates a vertex point is within the outer polygon. 
Inner polygons or holes are specified with a code > 1.
}

\value{
Returns a polygon of class \code{gpc.poly}.
}

\author{Fisher, J. C.}

\seealso{
\code{polyfile}
}

\examples{
f <- system.file("RSurvey-ex/PolygonAlternativeFormat.txt", package = "RSurvey")

con <- file(f, open = "r", encoding = "latin1")
ply <- polyTranslate(file = con)
pts <- get.pts(ply)
plot(ply)
for(i in 1:length(pts)) points(get.pts(ply)[[i]], col = "red")
close(con)

con <- file(f, open = "r", encoding = "latin1")
ply <- polyTranslate(file = con, dl = 1)
pts <- get.pts(ply)
for(i in 1:length(pts)) points(get.pts(ply)[[i]], col = "green")
close(con)
}

\keyword{misc}

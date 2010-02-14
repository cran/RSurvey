\name{writeFile}

\alias{writeFile}

\title{Write Data File}

\description{
Writes point and surface data to a file.
}

\usage{
writeFile()
}

\value{
The format and type of data written is based on the file type chosen within the file 
management pop up dialog box. 
A selection of \emph{Table data files} (\file{*.dat}) writes the contents of \code{data.pts} to a \acronym{ASCII} file; 
selecting \emph{Interpolated grid files} (\file{*.grd}) writes the contents of \code{data.grd} to a \acronym{ASCII} file; 
and a selection of \emph{ESRI shapfiles} (\file{*.shp}) writes the contents of \code{data.pts} to a binary file.
}

\author{Fisher, J. C.}

\seealso{
\code{\link{getFile}}, \code{\link{write.table}}
}

\examples{
\donttest{
data(project)
writeFile()
}
}

\keyword{file}

\name{getFile}

\alias{getFile}

\title{Select a File to Open or Save.}

\description{
A \acronym{GUI} for file management.
}

\usage{getFile(cmd = "open", exts = NULL, directory = NULL, file = NULL)}

\arguments{
\item{cmd}{determines if an \code{"open"} or \code{"save"} file management pop up dialog box is implemented.}
\item{exts}{an array of file extensions}
\item{directory}{the default directory}
\item{file}{either a character string naming a file or a connection.}
}

\value{
A list containing 4 values:
  \item{dir}{the directory containing the file.}
  \item{path}{the path associated with the file.}
  \item{name}{the name of the file.}
  \item{ext}{the extension associated with the file.}
}

\author{Fisher, J. C.}

\examples{
\donttest{getFile()}
}

\keyword{misc}

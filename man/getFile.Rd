\name{getFile}

\alias{getFile}

\title{Select a File to Open or Save.}

\description{
A pop up a dialog box for the user to select a file to open or save.
}

\usage{getFile(cmd = "Open", exts = NULL, directory = NULL, file = NULL, titleStr = cmd)}

\arguments{
\item{cmd}{determines if an \code{"Open"} or \code{"Save As"} file management pop up dialog box is implemented.}
\item{exts}{an array of default file extensions.}
\item{directory}{specifies that the files in \code{directory} should be displayed when the dialog pops up.}
\item{file}{either a character string naming a file or a connection.}
\item{titleStr}{a character string to display as the title of the dialog box.}
}

\value{
A list containing 4 components:
  \item{dir}{the directory containing the file.}
  \item{path}{the file path}
  \item{name}{the file name}
  \item{ext}{the file extension}
}

\author{Fisher, J. C.}

\examples{
\donttest{getFile()}
}

\keyword{misc}

\name{restoreSession}

\alias{restoreSession}

\title{Restore Session from Source Code Files}

\description{
This function restores local objects within the current \R session.
}

\usage{restoreSession(path, save.objs, fun.call)}

\arguments{
\item{path}{a full path name; if missing a folder browser pop up dialog box is implemented.}
\item{save.objs}{the name of local objects to preserve during the recompiling process.}
\item{fun.call}{the name of the function to call after restoring the session (optional).}
}

\value{
An updated \R session compiled from \file{.R} source code files located in \code{path} and 
its recursive directories.
}

\note{
Restoring \code{\link{srvy.dat}} and \code{\link{tran.dat}} clears their components.
}

\author{Fisher, J. C.}

\seealso{
\code{\link{source}}
}

\examples{
\dontrun{restoreSession()}
}

\keyword{programming}

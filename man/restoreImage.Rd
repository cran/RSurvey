\name{restoreImage}

\alias{restoreImage}

\title{Restore Image from Package Source Files}

\description{
This function is for package developers only.  The function recompiles the binary \file{.RData} file 
from \R source code files located within the working directory.  
It has only been tested on a Windows XP Professional \acronym{OS} and must be run within the package source directory.
}

\usage{restoreImage(fun.call = NULL, save.objs = NULL)}

\arguments{
\item{fun.call}{the name of the function to call after restoring the image.}
\item{save.objs}{the names of objects that will be unaffected by the recompiling process.}
}

\value{
A recompiled \file{.RData} file.
}

\details{
All components within \code{\link{srvy.dat}} and \code{\link{tran.dat}} 
are cleared if run within the \R command prompt; however, 
components are preserved when activated from the \acronym{GUI}.
}

\author{Fisher, J. C.}

\examples{
\dontrun{restoreImage()}
}

\keyword{programming}

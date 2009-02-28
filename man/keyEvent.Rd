\name{keyEvent}

\alias{keyEvent}

\title{Content Control within Tk Entry Widgets}

\description{
Controls the character string content within a Tk entry widget.
}

\usage{
keyEvent(ent.typ, ent.str = "", rm.data = FALSE)
}

\arguments{
\item{ent.typ}{the entry type}
\item{ent.str}{the entry value}
\item{rm.data}{a logical; if \code{TRUE} the existing processed data is removed.}
}

\details{
The entry types include: \var{real}, \var{second}, \var{integer}, \var{hour}, 
\var{minute}, and \var{date}.  If \code{rm.data} is \code{TRUE} 
then both the \code{data.mod} and \code{data.tin} components of \code{\link{srvy.dat}} 
are removed.
}

\value{
A character string with strict adherence to the entry type.
}

\author{Fisher, J. C.}

\seealso{
\code{\link{tkentry}}
}

\examples{
keyEvent("date", "07/12/1971")
## [1] "07/12/1971"

keyEvent("date", "7/2/19uq")
## [1] "7/2/19"

keyEvent("hour", "13")
## [1] "13"

keyEvent("hour", "25")
## [1] "23"
}

\keyword{misc}

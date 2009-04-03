\name{srvy.export}

\alias{srvy.export}

\title{Export Spatially Distributed Data}

\description{
This function exports survey data.
}

\usage{
srvy.export(file = NULL)
}

\arguments{
\item{file}{either a character string naming a file or a connection.}
}

\value{
The output of \code{srvy.export} is dependent on the file type chosen within the file 
management pop up dialog box.  Choosing a file type of \file{*.txt} writes the contents 
of \code{data.mod} to a file, whereas a choice of \file{*.tin} writes the contents of 
\code{data.tin} to a file.
}

\author{Fisher, J. C.}

\seealso{
\code{\link{write.table}}
}

\examples{
\donttest{
data(confluence)
f <- paste(getwd(), "/test.txt", sep="")
con <- file(f, open = "w", encoding = "latin1")
srvy.export(file = con)
}
}

\keyword{file}

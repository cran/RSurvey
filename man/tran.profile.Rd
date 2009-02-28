\name{tran.profile}

\alias{tran.profile}

\title{Calculate the Profile Along a Transect}

\description{
This function determines the profile spatial coordinates along a transect.
}

\usage{tran.profile(vertices)}

\arguments{
\item{vertices}{a matrix with rows corresponding to the transect vertices and columns to spatial coordinates.}
}

\value{
A matrix with rows corresponding to locations along a profile.  Variables associated with each location include:
  \tabular{llll}{
    \code{[,1]} \tab \code{x} \tab numeric \tab locations on the x-axis\cr
    \code{[,2]} \tab \code{y} \tab numeric \tab locations on the y-axis\cr
    \code{[,3]} \tab \code{h} \tab numeric \tab locations on the local x-axis.\cr
    \code{[,4]} \tab \code{z} \tab numeric \tab locations on the z-axis
  }
}

\author{Fisher, J. C.}

\examples{
data(confluence)
vert <- matrix(c(679164, 4135733, 679239, 4135730), nrow = 2, ncol = 2, 
        byrow = TRUE, dimnames = list(c("1", "2"), c("x", "y")))
prof <- tran.profile(vert)
prof
}

\keyword{hplot}

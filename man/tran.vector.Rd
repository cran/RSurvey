\name{tran.vector}

\alias{tran.vector}

\title{Longitudinal and Transverse Velocity Calculations}

\description{
Transform velocity components form x and y directions to longitudinal and transverse directions.
}

\usage{
tran.vector(vertices, vel.vect, arrow.max)
}

\arguments{
\item{vertices}{a matrix with rows corresponding to the transect vertices and columns to x and y values.}
\item{vel.vect}{a data frame with rows corresponding to the raster point records and columns to vector components, see Format section for further details.}
\item{arrow.max}{the maximum transverse velocity arrow symbol length.}
}

\format{
The components associated with \code{vel.vect} are as follows:
  \tabular{llll}{
    \code{[,1]} \tab \code{x} \tab numeric \tab velocity component in the x-direction.\cr
    \code{[,2]} \tab \code{y} \tab numeric \tab velocity component in the y-direction.\cr
    \code{[,3]} \tab \code{z} \tab numeric \tab velocity component in the z-direction, positive upward.
  }
}

\value{
A data frame with rows corresponding to points along a profile.  Components associated with each point include:
  \tabular{llll}{
    \code{[,1]} \tab \code{magn}     \tab numeric \tab the magnitude of the velocity within the principle direction.\cr
    \code{[,2]} \tab \code{long}     \tab numeric \tab longitudinal velocity component, perpendicular to the local x-direction.\cr
    \code{[,3]} \tab \code{arrow.x}  \tab numeric \tab the velocity component in the local x-direction.\cr
    \code{[,4]} \tab \code{arrow.y}  \tab numeric \tab the velocity component in the local z-direction.
  }
}

\author{Fisher, J. C.}

\examples{
vert <- matrix(c(0, 0, 10, 10), nrow = 2, ncol = 2, 
        byrow = TRUE, dimnames = list(c("1", "2"), c("x", "y")))
vect <- data.frame(cbind(x = c(1, -2, -3), y = c(-1, 2, -3), z = c(-1, 1, -1)))
tran.vector(vert, vect, 20)

##          magn      long       arrow.x   arrow.y
## [1,] 1.732051 -1.414214  2.809440e-16 -4.588315
## [2,] 3.000000  2.828427 -1.299754e-15  4.588315
## [3,] 4.358899  0.000000 -1.946657e+01 -4.588315
}

\keyword{misc}

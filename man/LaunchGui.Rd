% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/LaunchGui.R
\name{LaunchGui}
\alias{LaunchGui}
\title{GUI: Main Graphical User Interface}
\usage{
LaunchGui()
}
\value{
Quaries and sets the \code{vars} list component of \code{\link{Data}}.
  The components of \code{vars} include:
    \item{x, y, z}{index number for the corresponding coordinate-dimension variable in \code{cols},
      see \code{\link{ManageVariables}} function for details.}
}
\description{
Launches the main graphical user interface (\acronym{GUI}) for the \pkg{RSurvey} package.
May be used to specify coordinate variables, render plots, and access all other package functionality.
}
\examples{
\dontrun{
  LaunchGui()
}

}
\author{
J.C. Fisher, U.S. Geological Survey, Idaho Water Science Center
}
\keyword{misc}

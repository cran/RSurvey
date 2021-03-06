% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/DefineGrid.R
\name{DefineGrid}
\alias{DefineGrid}
\title{GUI: Define Interpolation Grid}
\usage{
DefineGrid(grid = NULL, parent = NULL)
}
\arguments{
\item{grid}{list.
Interpolation grid object, see \sQuote{Value} section.}

\item{parent}{tkwin.
\acronym{GUI} parent window}
}
\value{
Returns an object of class list with the following components:
  \item{opt}{an integer indicating the option that will be used to define the interpolation grid. Where
    \code{opt = 1} indicates grid boundaries based on the extent of point data
      and a resolution of 100 rows and 100 columns;
    \code{opt = 2} indicates grid boundaries based on the extent of point data
      and a cell resolution defined by the \code{res} component;
    \code{opt = 3} indicates that the grid geometry is explicitly defined by the \code{geo} component.}
  \item{res}{numeric vector of length 2 with components \code{x} and \code{y} giving the grid spacing
    along the \emph{x}- and \emph{y}-axis, respectively.}
  \item{geo}{numeric vector of length 6 with components
    \code{nrows} and \code{ncols} giving the number of rows and columns, and
    \code{xmn}, \code{xmx}, \code{ymn}, and \code{ymx} giving the limits of the grid boundary
    along the \emph{x}- and \emph{y}-axis.}
}
\description{
A graphical user interface (\acronym{GUI}) for defining the interpolation grid.
}
\examples{
\dontrun{
  DefineGrid()
}

}
\author{
J.C. Fisher, U.S. Geological Survey, Idaho Water Science Center
}
\keyword{misc}

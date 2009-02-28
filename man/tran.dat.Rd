\name{tran.dat}

\alias{tran.dat}

\docType{data}

\title{Set or Query Transect Data}

\description{
This function is used to set or query transect data.
}

\usage{
tran.dat(id, option, value, clearId = FALSE, clearAll = FALSE)
}

\arguments{
\item{id}{the transect name.}
\item{option}{the parameter name, see Transect Parameters section.}
\item{value}{a parameter value specified for \code{option}.}
\item{clearId}{a logical; if \code{TRUE} all data associated with transect \code{id} is cleared.}
\item{clearAll}{a logical; if \code{TRUE} all components are cleared from \code{tran.dat}.}
}

\section{Transect Parameters}{
  \tabular{lll}{
    \code{asp.ratio} \tab numeric    \tab the local z/x aspect ratio.\cr
    \code{data.file} \tab character  \tab the file name associated with the raster data.\cr
    \code{data.ras}  \tab data.frame \tab rows corresponding to raster point records and columns to the raster fields.\cr
    \code{fix.zero}  \tab character  \tab \code{"R"} for establishing the right vertex as the origin and \code{"L"} for the left.\cr
    \code{grid.dx}   \tab numeric    \tab the spatial resolution of the raster interpolation grid along the local x-axis.\cr
    \code{grid.dy}   \tab numeric    \tab the spatial resolution of the raster interpolation grid along the local z-axis.\cr
    \code{h.offset}  \tab numeric    \tab the local x-axis offset measured from the vertex origin.\cr
    \code{id}        \tab character  \tab the name of the transect.\cr
    \code{limits}    \tab data.frame \tab rows corresponding to the raster types and columns to the raster plotting limits.\cr
    \code{prof}      \tab data.frame \tab rows corresponding to points along the transect and columns to local x and z values.\cr
    \code{type}      \tab character  \tab fields within the imported raster data, a concatenation of column type and units.\cr
    \code{v.offset}  \tab numeric    \tab the local z-axis offset, positive upward.\cr
    \code{v.origin}  \tab numeric    \tab the z value corresponding to the local z-axis origin.\cr
    \code{vel.vect}  \tab character  \tab fields corresponding to velocity vector components: x, y, z, and principle direction.\cr
    \code{vertices}  \tab matrix     \tab rows corresponding to the transect vertices and columns to x and y values.\cr
    \code{hv.fields} \tab character  \tab raster fields corresponding to the local-x and local-z directions.
  }
}

\author{Fisher, J. C.}

\examples{
# To set a parameter within a transect
tran.dat("T1", "h.offset", 10)
tran.dat("T1", "hrz.dist", 3.14)
tran.dat("T2", "h.offset", 9)
# To get a parameter value
tran.dat("T1", "h.offset")
# To get all transect data
tran.dat()
# To get transect data for T1
tran.dat("T1")
# To clear all data associated with a transect
tran.dat("T1", clearId = TRUE)
# To clear all transect data
tran.dat(clearAll = TRUE)
}

\keyword{sysdata}

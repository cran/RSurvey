\name{RSurvey-package}

\alias{RSurvey-package}

\docType{package}

\title{
Analysis of Spatially Distributed Data
}

\description{
This package is a processing program for spatially distributed data.  The application is capable of 
error corrections, polygon manipulation, and data visualization.  A graphical user interface 
(\acronym{GUI}) is provided.
}

\details{
  \tabular{ll}{
    Package: \tab RSurvey\cr
    Type: \tab Package\cr
    Version: \tab 0.4.9\cr
    Date: \tab 2010-02-14\cr
    License: \tab GPL version 2 or newer
  }
}

\note{
The \pkg{RSurvey} \acronym{GUI}s require that 
\R operate as an \acronym{SDI} application with multiple top-level 
windows for the console, graphics, and pager. 
Preservation of unit consistency within this package is tasked to the user. 
Parameter dimensions are included in the \acronym{GUI} entry labels to 
assist with unit consistency.

Filename extensions are used to infer information about the way data is stored in the file. 
The extensions \file{*.dat}, \file{*.grd}, and \file{*.ply} indicate \acronym{ASCII} files; the 
\file{*.dat} extension is reserved for table formated data, 
\file{*.grd} for grided data describing an interpolated surface, and 
\file{*.ply} for polygon data.
The extensions \file{*.shp} and \file{*.rda} indicate binary files; the 
\file{*.shp} extension is reserved for the \acronym{ESRI} shapefile format, and 
\file{*.rda} for \pkg{RSurvey} project files.
}

\author{
Jason C. Fisher

Maintainer: Jason C. Fisher <\email{jfisher@usgs.gov}>
}

\examples{
\donttest{library(RSurvey)}
}

\keyword{package}


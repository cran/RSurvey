RSurvey
=======

This [R](http://www.r-project.org/ "R") package is a processing program for
spatially distributed data.
[**RSurvey**](http://cran.r-project.org/web/packages/RSurvey/index.html "RSurvey")
features graphing, data management, query building, and
polygon clipping tools. A graphical user interface (GUI) is provided and
requires R operate as an SDI application, using multiple
top-level windows for the console, graphics, and pager.
The set of standards used for coding **RSurvey** is documented in
[Google's R Style Guide](http://google-styleguide.googlecode.com/svn/trunk/google-r-style.html "Google's R Style Guide").

Install
-------

If R is not already installed on your
computer, download and install the latest binary distribution from
[CRAN](http://cran.r-project.org/ "The Comprehensive R Archive Network").
Windows users should set R to operate as an SDI application during installation
by choosing to customize the start-up options and specifying the SDI interface
(not the default).

Open an R session and install **RSurvey** and its dependent packages from CRAN:

    > install.packages("RSurvey")

In addition to the required packages, **RSurvey** uses functions in a number of
recommended packages:
[**rgl**](http://cran.r-project.org/web/packages/rgl/index.html "rgl")
for 3D visualization,
[**rgdal**](http://cran.r-project.org/web/packages/rgdal/index.html "rgdal")
for Esri shapefile support,
[**colorspace**](http://cran.r-project.org/web/packages/colorspace/index.html "colorspace")
for color palettes based on HCL colors,
[**dichromat**](http://cran.r-project.org/web/packages/dichromat/index.html "dichromat")
for color-blind safe color palettes, and
[**tripack**](http://cran.r-project.org/web/packages/tripack/index.html "tripack")
for auto-cropping. Note that the license for **tripack** explicitly forbids
commercial use. If any of these recommended packages are missing, **RSurvey**
will offer to install them when it first starts up.

Support for displaying table data is provided by
[tktable](http://tktable.sourceforge.net/ "tktable"),
a spreadsheet-like [Tcl/Tk](http://www.tcl.tk/ "Tcl/Tk") widget
(typically included with the binary distribution of R).
The following call will indicate whether tktable is available for use:

    > tcltk::tclRequire("Tktable", warn = TRUE)

Run
---

Load **RSurvey** in the current R session to activate the main GUI:

    > library(RSurvey)

Example data sets are provided in the following directory:

    > system.file("extdata", package = "RSurvey")


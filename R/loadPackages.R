LoadPackages <- function() {
  # This function loads R packages required by RSurvey. If a required 
  # package is unavailable on the local computer an attempt is made to 
  # acquire the package from CRAN using an existing network connection.
  
  r.packages <- c("tcltk", "sp", "rgdal", "gpclib", "rgl", "MBA", "tripack")
  tcl.packages <- c("Tktable")
  
  # Packages that may be useful for future work: 
  #   r.packages <- append(r.packages, c("udunits", "mgcv", "RColorBrewer"))
  
  i.p <- r.packages[!(r.packages %in% .packages(all.available=TRUE))]
  
  if (length(i.p) > 0) 
    install.packages(i.p)
  
  for (i in r.packages) 
    require(i, character.only=TRUE)
  
  # Additional Tcl packages
  
  pkg <- tryCatch(tcl("package", "require", "Tktable"), error=identity)
  if (inherits(pkg, "error")) {
    txt <- paste("Tcl package Tktable not found and is strongly recommended",
                 "for full functionality (http://tktable.sourceforge.net/).")
    warning(txt, domain=NA)
  }
}

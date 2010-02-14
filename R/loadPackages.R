"loadPackages" <- function() {
    
    required.packages <- c("tcltk", "rgl", "MBA", "gpclib", "sp", "tripack", "rgdal")
    
### PACKAGES THAT MAY BE USEFUL FOR FUTURE WORK: 
### required.packages <- append(required.packages, c("udunits", "mgcv"))
    
    i.p <- required.packages[!(required.packages %in% .packages(all.available=TRUE))]
    
    if(length(i.p) > 0) 
        install.packages(i.p)
    
    for(i in required.packages) 
        require(i, character.only=TRUE)
}

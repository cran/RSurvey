"plotSurvey3d" <- function(x=NULL, y=NULL, z=NULL, px=NULL, py=NULL, pz=NULL, 
                   vasp=NA, hasp=NA, width=7, ppi=96, cex.pts=1, 
                   mouseMode=c("trackball", "zAxis", "zoom"), bg="white") {
    
  # account for missing arguments
    
    if(is.null(z)) {
        if(!is.null(x) && is.list(x)) {
            z <- x$z
            y <- x$y
            x <- x$x
        }
        else
            stop("No 'z' specified")
    }
    else if(!is.null(x) && is.list(x)) {
        y <- x$y
        x <- x$x
    }
    if(is.null(x)) x <- seq(0, 1, length.out=nrow(z))
    if(is.null(y)) y <- seq(0, 1, length.out=ncol(z))
    if(any(diff(x) <= 0) || any(diff(y) <= 0)) 
        stop("Increasing 'x' and 'y' values expected")
    if(!is.matrix(z) || nrow(z) <= 1 || ncol(z) <= 1) 
        stop("No proper 'z' matrix specified")
    
    showpoints <- !is.null(px)
    if(showpoints) {
        if(is.list(px)) {
            pz <- px$z
            py <- px$y
            px <- px$x
        }
        showpoints <- !(is.null(px) | is.null(py) | is.null(pz))
    }
    
  # scale
    
    yscale <- zscale <- 1
    
    if(!is.null(hasp) && !is.na(hasp)) 
        yscale <- hasp
    else 
        yscale <- (diff(range(x, na.rm=TRUE)) / diff(range(y, na.rm=TRUE)))
        
    if(!is.null(vasp) && !is.na(vasp)) 
        zscale <- vasp
    else {
        maxdiff <- max(diff(range(x, na.rm=TRUE)), diff(range(y, na.rm=TRUE))) * 0.15
        zscale <- (maxdiff / diff(range(z, na.rm=TRUE)))
    }
    
    y <- y * yscale
    z <- z * zscale
    
    if(showpoints) {
        py <- py * yscale
        pz <- pz * zscale
    }
    
  # color
    
    zlim <- range(z, na.rm=TRUE)
    cols <- terrain.colors(11)[((z - zlim[1]) / (zlim[2] - zlim[1])) * 10 + 1]
    
  # open RGL device
    
    open3d()
    
  # size of plot window
    
    winDim <- ppi + width * ppi
    winRec <- c(ppi, ppi, winDim, winDim * 0.75)
    
    par3d(windowRect=winRec, mouseMode=mouseMode)
    
  # add terrain surface shape
    
    bg3d(color=bg)
    
    surface3d(x, y, z, color=cols, back="fill")
    view3d(theta=0, phi=-55, fov=60, zoom=0.6)
    
    if(showpoints) 
        points3d(x=px, y=py, z=pz, size=cex.pts * 3, point_antialias=TRUE)
}

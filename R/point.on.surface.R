"point.on.surface" <- function(x, y=NULL, z=NULL, px, py=NULL) {
    
  # account for missing arguments
    
    if(is.null(z)) {
        if(is.list(x)) {
            z  <- x$z
            y  <- x$y
            x  <- x$x
        }
    }
    else {
        if(is.list(x)) {
            y <- x$y
            x <- x$x
        }
    }
    
    if(!is.matrix(z)) stop(call.=FALSE, "a matrix of z values is not available")
    
    if(is.null(py)) {
        if(is.list(px)) {
            py <- px$py
            px <- px$px
        }
    }
    pz <- rep(NA, length(px))
    
  # reduce size of z matrix using full extent of point coordinates
    
    pxran <- if(length(unique(px)) > 1) range(px) else c(px[1] - 1, px[1] + 1)
    pyran <- if(length(unique(py)) > 1) range(py) else c(py[1] - 1, py[1] + 1)
    
    id.x <- (1:length(x))[x >= pxran[1] & x <= pxran[2]]
    id.y <- (1:length(y))[y >= pyran[1] & y <= pyran[2]]
    
    id.x <- (min(id.x) - 10):(max(id.x) + 10)
    id.y <- (min(id.y) - 10):(max(id.y) + 10)
    
    id.x <- id.x[id.x > 0 & id.x < length(x)]
    id.y <- id.y[id.y > 0 & id.y < length(y)]
    
    x <- x[id.x]
    y <- y[id.y]
    z <- z[id.x, id.y]
    
  # determine z values at point locations (x,y) within the interpolated surface
    
    for(i in 1:(length(x) - 1)) {
        for(j in 1:(length(y) - 1)) {
            hld <- point.in.polygon(px, py, c(x[i], x[i + 1], x[i + 1], x[i]), c(y[j], y[j], y[j + 1], y[j + 1]))
            pz[hld != 0] <- mean(c(z[i, j], z[i + 1, j], z[i + 1, j + 1], z[i, j + 1]), na.rm=TRUE)
        }
    }
    
    rtn <- cbind(px, py, pz)
    colnames(rtn) <- c("px", "py", "pz")
    
    rtn
}

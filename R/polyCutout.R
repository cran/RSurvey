"polyCutout" <- function(dat, ply=NULL) {
    
  # exclude interpolated data outside of the polygon
    
    if(class(dat) == "matrix") {
        hld <- list(x=unique(dat[,1]), y=unique(dat[,2]), z=NULL)
        nrows <- length(hld$x)
        ncols <- length(hld$y)
        hld$z <- matrix(dat[,3], nrow=nrows, ncol=ncols, byrow=FALSE)
        x <- dat[,1]
        y <- dat[,2]
        dat <- hld
    }
    else {
        nrows <- length(dat$x)
        ncols <- length(dat$y)
        x <- as.vector(matrix(rep(dat$x, ncols), nrow=nrows, ncol=ncols, byrow=FALSE))
        y <- as.vector(matrix(rep(dat$y, nrows), nrow=nrows, ncol=ncols, byrow=TRUE))
    }
    
    if(is.null(ply)) return(dat)
    
    pnt.in.ply <- matrix(0, nrow=length(dat$x), ncol=length(dat$y))
    if(is.null(dat$z)) 
        dat$z <- pnt.in.ply
    
    d <- get.pts(ply)
    holes <- sapply(d, function(x) x$hole)
    d <- append(d[!holes], d[holes])
    
    for(i in 1:length(d)) {
        pts <- d[[i]]
        hld <- point.in.polygon(point.x=x, point.y=y, pol.x=pts$x, pol.y=pts$y)
        hld <- matrix(hld, nrow=nrows, ncol=ncols, byrow=FALSE)
        if(pts$hole) 
            pnt.in.ply[hld != 0] <- 0
        else 
            pnt.in.ply <- pnt.in.ply + hld
    }
    
    dat$z[pnt.in.ply == 0] <- NA 
    
  # remove rows and columns consisting of all NA values
    
    rmcols <- rmrows <- NULL
    
    cols <- 1:ncol(dat$z)
    rows <- 1:nrow(dat$z)
    
    for(i in rows) {if(all(is.na(dat$z[i,]))) rmrows <- c(rmrows, i)}
    for(i in cols) {if(all(is.na(dat$z[,i]))) rmcols <- c(rmcols, i)}
    
    dat$x <- dat$x[!(rows %in% rmrows)]
    dat$y <- dat$y[!(cols %in% rmcols)]
    dat$z <- dat$z[!(rows %in% rmrows), !(cols %in% rmcols)]
    
    dat
}

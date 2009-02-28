"polyCutout" <- function(dat, ply) {
    
  # exclude interpolated data outside of the polygon
    
    if(class(dat) == "matrix") {
        tmp <- list(x=unique(dat[,1]), y=unique(dat[,2]), z=NULL)
        n.row <- length(tmp$x)
        n.col <- length(tmp$y)
        tmp$z <- matrix(dat[,3], nrow=n.row, ncol=n.col, byrow=FALSE)
        x <- dat[,1]
        y <- dat[,2]
        dat <- tmp
    }
    else {
        n.row <- length(dat$x)
        n.col <- length(dat$y)
        x <- as.vector(matrix(rep(dat$x, n.col), nrow=n.row, ncol=n.col, byrow=FALSE))
        y <- as.vector(matrix(rep(dat$y, n.row), nrow=n.row, ncol=n.col, byrow=TRUE))
    }
    
    pnt.in.ply <- matrix(0, nrow=length(dat$x), ncol=length(dat$y))
    if(is.null(dat$z)) 
        dat$z <- pnt.in.ply
    
    d <- get.pts(ply)
    
    for(i in 1:length(d)) {
        pts <- d[[i]]
        hld <- point.in.polygon(point.x=x, point.y=y, pol.x=pts$x, pol.y=pts$y)
        hld <- matrix(hld, nrow=n.row, ncol=n.col, byrow=FALSE)
        if(pts$hole) 
            pnt.in.ply[hld != 0] <- 0
        else 
            pnt.in.ply <- pnt.in.ply + hld
    }
    
    dat$z[pnt.in.ply == 0] <- NA 
    
  # eliminate rows and columns consisting of all NA values
    
    elim.col <- elim.row <- NULL
    
    cols <- 1:ncol(dat$z)
    rows <- 1:nrow(dat$z)
    
    for(i in rows) {if(all(is.na(dat$z[i,]))) elim.row <- c(elim.row, i)}
    for(i in cols) {if(all(is.na(dat$z[,i]))) elim.col <- c(elim.col, i)}
    
    dat$x <- dat$x[!(rows %in% elim.row)]
    dat$y <- dat$y[!(cols %in% elim.col)]
    dat$z <- dat$z[!(rows %in% elim.row), !(cols %in% elim.col)]
    
    dat
}

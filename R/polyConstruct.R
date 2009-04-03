"polyConstruct" <- function(file=NULL, dl=NULL) {
    
# additional functions (subroutines)
    
  # determine points on a line seperated by equal intervals
    
    pts.on.line <- function(pt1, pt2, dl) {
        x1 <- pt1[1]
        y1 <- pt1[2]
        z1 <- pt1[3]
        x2 <- pt2[1]
        y2 <- pt2[2]
        z2 <- pt2[3]
        if(is.na(z1)) 
            z1 <- z2 <- 0
        l <- sqrt((x1 - x2)^2 + (y1 - y2)^2 + (z1 - z2)^2)
        k <- rev(seq(l, 0, -dl)) / l
        x <- x1 + k * (x2 - x1)
        y <- y1 + k * (y2 - y1)
        z <- z1 + k * (z2 - z1)
        pts <- if(is.na(pt1[3])) cbind(x, y) 
               else cbind(x, y, z)
        as.data.frame(pts)
    }
    
# main program
    
  # import data from polygon file
    
    f <- getFile(cmd="Open", exts="txt", file=file)
    if(is.null(f)) return()
    
    con <- if("connection" %in% class(file)) file 
           else file(f$path, "r", encoding=srvy.dat("encoding"))
    
    dat <- readFile(con, fields=TRUE, units=FALSE)$dat
    names(dat) <- c("index", "x", "y", "code")
    
  # outer polygon (code <= 1), where code = 0 defines an inlet or outlet
    
    ply <- dat[dat$code <= 1,]
    ply <- unique(ply[order(ply$index), c("x", "y", "code")])
    
    if(tail(ply$code, 1) != 0) 
        ply <- rbind(ply, ply[1,])
    
    x <- ply[1, 1]
    y <- ply[1, 2]
    
    for(i in 2:nrow(ply)) {
        if(any(ply[seq(i-1, i), "code"] != 0) && !is.null(dl)) {
            pt1 <- as.numeric(ply[i-1, 1:2])
            pt2 <- as.numeric(ply[i,   1:2])
            pts <- pts.on.line(pt1, pt2, dl)
            x <- c(x, pts[,1])
            y <- c(y, pts[,2])
        }
        else {
            x <- c(x, ply[i, 1])
            y <- c(y, ply[i, 2])
        }
    }
    
    n <- 1:length(x)
    poly.dat <- as(c(1, length(n), 0, as.vector(t(cbind(x[n], y[n])))), "gpc.poly")
    
  # inner polygons (code > 1)
    
    hld <- dat[dat$code > 1,]
    for(i in sort(unique(hld$code))) {
        ply <- hld[hld$code == i,]
        ply <- unique(ply[order(ply$index), c("x", "y")])
        ply <- rbind(ply, ply[1,])
        
        x <- ply[1, 1]
        y <- ply[1, 2]
        
        for(j in 2:nrow(ply)) {
            if(!is.null(dl)) {
                pt1 <- as.numeric(ply[j-1, 1:2])
                pt2 <- as.numeric(ply[j,   1:2])
                pts <- pts.on.line(pt1, pt2, dl)
                x <- c(x, pts[,1])
                y <- c(y, pts[,2])
            }
            else {
                x <- c(x, ply[j, 1])
                y <- c(y, ply[j, 2])
            }
        }
        
        n <- 1:length(x)
        inner.poly <- as(c(1, length(n), 1, as.vector(t(cbind(x[n], y[n])))), "gpc.poly")
        poly.dat <- append.poly(poly.dat, inner.poly)
    }
    close(con)
    poly.dat
}

"plotTime" <- function(x, y=NULL, xlim=NULL, ylim=NULL, ylab=NULL, gap=NULL, 
              width=7, pointsize=10, fmt="%H:%M:%OS\n%Y-%m-%d") {
    
# additional functions (subroutines)
    
  # modify x- and y-axis limits
    
    xylim <- function(x, y, xlim, ylim, ext=0.02) {
        i <- rep(TRUE, length(x))
        
        if(!is.na(xlim[1])) i <- i & x >= xlim[1]
        if(!is.na(xlim[2])) i <- i & x <= xlim[2]
        if(!is.na(ylim[1])) i <- i & y >= ylim[1]
        if(!is.na(ylim[2])) i <- i & y <= ylim[2]
        
        xlim.new <- range(x[i], na.rm=TRUE)
        ylim.new <- range(y[i], na.rm=TRUE)
        
        xminf <- xmaxf <- ext
        yminf <- ymaxf <- ext * 2
        
        if(!is.na(xlim[1])) {xlim.new[1] <- xlim[1]; xminf <- 0}
        if(!is.na(xlim[2])) {xlim.new[2] <- xlim[2]; xmaxf <- 0}
        if(!is.na(ylim[1])) {ylim.new[1] <- ylim[1]; yminf <- 0}
        if(!is.na(ylim[2])) {ylim.new[2] <- ylim[2]; ymaxf <- 0}
        
        xran <- diff(xlim.new)
        yran <- diff(ylim.new)
        
        xlim.new[1] <- xlim.new[1] - xran * xminf
        xlim.new[2] <- xlim.new[2] + xran * xmaxf
        
        ylim.new[1] <- ylim.new[1] - yran * yminf
        ylim.new[2] <- ylim.new[2] + yran * ymaxf
        
        list(x=xlim.new, y=ylim.new)
    }
    
  # plot line segments seperated by gap
    
    lineseg <- function(x, y, gap, col="black") {
        if(!is.null(gap)) {
            idxs <- (1:(length(x) - 1))[diff(x) > gap]
            for(i in idxs) {
                x <- append(x, (x[i+1] + x[i]) / 2)
                y <- append(y, NA)
            }
            idxs <- order(x)
            x <- x[idxs]
            y <- y[idxs]
        }
        lines(x, y, col=col)
        points(x, y, pch=21, cex=0.75, col=col, bg="white")
    }
    
    
    
# main program
    
  # account for missing arguments
    
    if(is.list(x)) {
        y <- x$y
        x <- x$x
    }
    
  # window setup
    
    x11(width=width, height=width / 2, pointsize=pointsize)
    
    op <- par(mfrow=c(1, 1), bg="white", mar=c(3, 4, 1, 3) + 0.1)
    on.exit(par(op))
    
  # remove NA values
    
    logic <- !is.na(x)
    x <- x[logic]
    y <- y[logic]
    
  # sort on datetime
    
    idxs <- order(x)
    x <- x[idxs]
    y <- y[idxs]
    
  # transfrom x-values from datetime to seconds
    
    origin <- strptime("1970-01-01 00:00:00.0", "%Y-%m-%d %H:%M:%OS")
    
    datetime <- as.POSIXct(x, origin=origin)
    
    x <- unclass(datetime)
    
  # initialize axes limits
    
    if(is.null(xlim)) xlim <- c(NA, NA)
    if(is.null(ylim)) ylim <- c(NA, NA)
    
    xmin <- if(is.na(xlim[1])) NA else unclass(as.POSIXct(xlim[1], origin=origin))
    xmax <- if(is.na(xlim[2])) NA else unclass(as.POSIXct(xlim[2], origin=origin))
    
    xlim <- c(xmin, xmax)
    
    lim <- xylim(x, y, xlim, ylim)
    
  # plot
    
    plot(NA, xlim=lim$x, ylim=lim$y, type="n", xaxs="i", yaxs="i", ann=FALSE, cex.axis=0.7, xaxt="n")
    
    lineseg(x, y, gap)
    
    loc.tics <- unclass(axis.POSIXct(side=1, datetime, format=fmt, labels=TRUE, cex.axis=0.7))
    
    if(!is.null(ylab)) title(ylab=ylab, line=2.5)
    minorTics(2)
    minorTics(1, loc.tics)
}

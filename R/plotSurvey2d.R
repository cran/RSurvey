"plotSurvey2d" <- function(x=NULL, y=NULL, z=NULL, vx=NULL, vy=NULL, type="p", 
                  xlim=NULL, ylim=NULL, zlim=NULL, xlab=NULL, ylab=NULL, zlab=NULL, 
                  asp=NA, csi=NA, width=7, pointsize=12, cex.pts=1, nlevels=20, rkey=FALSE, 
                  color.palette=terrain.colors, vuni=FALSE, vmax=NULL, vxby=NULL, vyby=NULL) {
    
  # account for missing arguments
    
    if(is.null(z)) {
        if(is.list(x)) {
            vx <- x$vx
            vy <- x$vy
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
    
    if(is.matrix(z)) {
        zdim <- dim(z)
        if(is.null(x)) x <- seq(0, 1, length.out=zdim[1])
        if(is.null(y)) y <- seq(0, 1, length.out=zdim[1])
        if(any(diff(x) <= 0) || any(diff(y) <= 0)) 
            stop("increasing 'x' and 'y' values expected")
        if(zdim[1] <= 1 || zdim[2] <= 1) 
            stop("no proper 'z' matrix specified")
    }
    
    if(is.null(x) || is.null(y)) 
        stop("missing 'x' or 'y' values")
    if(is.null(z) & (type %in% c("l", "g"))) 
        stop("filled contour requires 'z' value")
    
    if(is.null(asp)) asp <- NA
    
    if(is.na(csi)) {
        x11(pointsize=pointsize)
        csi <- par("csi") # height of characters and width of margin line (in)
        dev.off()
    }
    
    if(is.null(xlim)) {xmin <- xmax <- NA} else {xmin <- xlim[1]; xmax <- xlim[2]}
    if(is.null(ylim)) {ymin <- ymax <- NA} else {ymin <- ylim[1]; ymax <- ylim[2]}
    if(is.null(zlim)) {zmin <- zmax <- NA} else {zmin <- zlim[1]; zmax <- zlim[2]}
    
  # apply limits on z
    
    if(type %in% c("l", "g")) {
        if(!is.na(zmin)) z[apply(z, c(1,2), function(x) {!is.na(x) && x < zmin})] <- NA
        if(!is.na(zmax)) z[apply(z, c(1,2), function(x) {!is.na(x) && x > zmax})] <- NA
    }
    else if(!is.null(z)) {
        if(!is.na(zmin)) z[z < zmin] <- NA
        if(!is.na(zmax)) z[z > zmax] <- NA
    }
    
  # axes ranges
    
    if(type %in% c("l", "g")) {
        i <- rep(TRUE, length(x))
        j <- rep(TRUE, length(y))
        
        if(!is.na(xmin)) i <- i & x >= xmin
        if(!is.na(xmax)) i <- i & x <= xmax
        if(!is.na(ymin)) j <- j & y >= ymin
        if(!is.na(ymax)) j <- j & y <= ymax
        
        rows.na <- sapply(1:nrow(z), function(idx) !all(is.na(z[idx,])))
        cols.na <- sapply(1:ncol(z), function(idx) !all(is.na(z[,idx])))
        
        i <- i & if(nrow(z) < length(x)) c(FALSE, rows.na) | c(rows.na, FALSE) else rows.na
        j <- j & if(ncol(z) < length(y)) c(FALSE, cols.na) | c(cols.na, FALSE) else cols.na
        
        xran <- range(x[i])
        yran <- range(y[j])
        zran <- range(z[rows.na, cols.na], finite=TRUE)
    }
    else {
        xran <- range(x)
        yran <- range(y)
        if(!is.null(z)) {
            k <- rep(TRUE, length(z))
            if(!is.na(xmin)) k <- k & x >= xmin
            if(!is.na(xmax)) k <- k & x <= xmax
            if(!is.na(ymin)) k <- k & y >= ymin
            if(!is.na(ymax)) k <- k & y <= ymax
            zran <- range(z[k], finite=TRUE)
        }
    }
    
  # axes limits
    
    xminf <- xmaxf <- yminf <- ymaxf <- 0
    
    if(is.na(xmin)) {xmin <- xran[1]; xminf <- 0.02}
    if(is.na(xmax)) {xmax <- xran[2]; xmaxf <- 0.02}
    if(is.na(ymin)) {ymin <- yran[1]; yminf <- 0.02}
    if(is.na(ymax)) {ymax <- yran[2]; ymaxf <- 0.02}
    
    xdif <- diff(c(xmin, xmax))
    ydif <- diff(c(ymin, ymax))
    
    xlim <- c(xmin - xdif * xminf, xmax + xdif * xmaxf)
    ylim <- if(is.na(asp)) c(ymin - ydif * yminf, ymax + ydif * ymaxf) 
            else c(ymin - xdif * yminf / asp, ymax + xdif * ymaxf / asp)
    
  # canvas setup
    
    mar.plot   <- c(4, 4, 2, 0.75)
    mar.legend <- c(4, 0, 2, 4)
    
    if(is.null(z)) 
        legend.width <- 0
    else 
        legend.width <- (1 + mar.legend[2] + mar.legend[4]) * csi # legend width (in)
    
    if(is.na(asp)) 
        height <- width
    else {
        xmar <- (mar.plot[2] + mar.plot[4]) * csi # plot margin width (in)
        ymar <- (mar.plot[1] + mar.plot[3]) * csi # plot margin height (in)
        xin <- width - xmar - legend.width # plot width (in)
        yin <- xin * diff(ylim) / diff(xlim) * asp # plot height (in)
        height <- yin + ymar # canvas height (in)
    }
    
    x11(width=width, height=height, pointsize=pointsize)
    
  # legend
    
    if(!is.null(z)) {
        layout(matrix(c(2, 1), nc=2), widths=c(1, lcm(legend.width * 2.54)))
        
        levels <- pretty(zran, nlevels)
        col <- color.palette(length(levels) - 1)
        
        par(mar=mar.legend, las=1)
        plot.new()
        
        zlim <- if(rkey) rev(range(levels)) else range(levels)
        
        plot.window(xlim=c(0, 1), ylim=zlim, xaxs="i", yaxs="i")
        rect(0, levels[-length(levels)], 1, levels[-1], col=col)
        
        axis(4, at=pretty(levels, n=8), las=3, cex.axis=0.7, padj=-1)
        
        mtext(zlab, side=4, line=2, cex=0.9, las=3)
        
        box()
    }
    
  # plot template
    
    par(mar=mar.plot)
    plot.new()
    plot.window(xlim=xlim, ylim=ylim, xaxs="i", yaxs="i", asp=asp)
    axis(1, las=1, cex.axis=0.7, padj=-1)
    axis(2, las=0, cex.axis=0.7)
    minorTics(1:2)
    
    title(xlab=xlab, cex.main=0.9, cex.lab=0.9, line=2.0)
    title(ylab=ylab, cex.main=0.9, cex.lab=0.9, line=2.5)
    
  # plot interpolated surface
    
    if(type == "l") {
        if(!is.double(z)) storage.mode(z) <- "double"
        .Internal(filledcontour(as.double(x), as.double(y), z, as.double(levels), col=col))
    }
    if(type == "g") 
        image(as.double(x), as.double(y), z, col=col, add=TRUE, breaks=as.double(levels))
    
  # plot vector arrows
    
    if(!is.null(vx) | !is.null(vy)) {
        
        if(is.matrix(vx) | is.matrix(vy)) {
            m <- length(y)
            n <- length(x)
            
            vdim <- if(is.null(vx)) dim(vy) else dim(vx)
            
            if(vdim[1] == n - 1 && vdim[2] == m - 1) {
                x <- x[1:(n - 1)] + diff(x) / 2
                y <- y[1:(m - 1)] + diff(y) / 2
                
                m <- length(y)
                n <- length(x)
            }
            v <- data.frame(cbind(x=rep(x, m), y=as.vector(matrix(rep(y, n), nrow=n, ncol=m, byrow=TRUE))))
        }
        else 
            v <- data.frame(cbind(x=x, y=y))
        
        v$vx <- if(is.null(vx)) NA else as.vector(vx)
        v$vy <- if(is.null(vy)) NA else as.vector(vy)
        
        v <- v[!(is.na(v$vx) & is.na(v$vy)),]
        
        if(is.na(asp)) asp <- diff(ylim) / diff(xlim)
        ran <- range(abs(c(v$vx / asp, v$vy)), na.rm=TRUE)
        
        if(is.null(vmax) || !is.numeric(vmax)) 
            len <- sqrt((diff(xlim) / asp)^2 + diff(ylim)^2) * 0.03
        else {
            xpin <- abs(diff(par("usr")[1:2])) / par("pin")[1]
            len <- vmax * xpin
        }
        
        if(vuni) {
            v$vx <- sign(v$vx) * len
            v$vy <- sign(v$vy) * len
        }
        else {
            v$vx <- sign(v$vx) * len * ((abs(v$vx) - ran[1]) / (ran[2] - ran[1]))
            v$vy <- sign(v$vy) * len * ((abs(v$vy) - ran[1]) / (ran[2] - ran[1]))
        }
        
        if(type %in% c("l", "g")) {
            vxUnique <- sort(unique(v$x))
            vyUnique <- sort(unique(v$y))
            
            if(is.null(vxby)) vxseq <- as.integer(seq(1, length(vxUnique), length.out=20))
            else              vxseq <- seq(1, length(vxUnique), by=vxby)
            
            if(is.null(vyby)) vyseq <- as.integer(seq(1, length(vyUnique), length.out=20))
            else              vyseq <- seq(1, length(vyUnique), by=vyby)
            
            v <- v[v$x %in% vxUnique[vxseq] & v$y %in% vyUnique[vyseq],]
        }
        
        v$vx[is.na(v$vx)] <- 0
        v$vy[is.na(v$vy)] <- 0
        
        v <- v[!(v$vx == 0 & v$vy == 0),]
        
        arrows(v$x, v$y, v$x + v$vx, v$y + v$vy, length=0.05, angle=30)
    }
    
  # plot points
    
    if(type == "p") {
        if(is.null(z) | is.matrix(z)) 
            points(x, y, pch=21, cex=cex.pts, col="black", bg="white")
        else {
            for(i in 1:length(col)) {
                logic <- z >= levels[i] & z <= levels[i + 1]
                points(x[logic], y[logic], pch=21, cex=cex.pts, col="black", bg=col[i])
            }
        }
    }
    
    box()
    
    invisible()
}

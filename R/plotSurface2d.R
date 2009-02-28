"plotSurface2d" <- function(const.poly=FALSE) {
    
  # simplify data
    
    data.tin <- srvy.dat("data.tin")
    
  # incorporate spatial domain using polygon
    
    ply <- srvy.dat("poly")
    if(!is.null(ply)) 
        data.tin <- polyCutout(data.tin, ply)
    
  # simplify data notation
    
    x <- data.tin$x
    y <- data.tin$y
    z <- data.tin$z
    
  # limits
    
    x.min <- min(x)
    x.max <- max(x)
    y.min <- min(y)
    y.max <- max(y)
    x.ext <- diff(range(x)) * 0.04
    x.lim <- c(x.min - x.ext, x.max + x.ext)
    y.lim <- c(y.min - x.ext, y.max + x.ext)
    z.lim <- range(z, na.rm=TRUE)
    
  # window setup
    
    win.width <- srvy.dat("win.width")
    asp.ratio <- srvy.dat("yx.ratio")
    
    mar <- c(5.1, 4.1, 4.1, 8.1)
    
    if(is.null(asp.ratio)) {
        win.height <- win.width
        asp.ratio <- NA
    }
    else {
        mar <- c(5.1, 4.1 / asp.ratio, 4.1, 8.1 / asp.ratio)
        mar.width <- (mar[2] + mar[4]) * srvy.dat("csi")
        mar.height <- mar.width * ((mar[1] + mar[3]) / (mar[2] + mar[4]))
        ratio <- diff(y.lim) / diff(x.lim) * asp.ratio
        win.height <- ((win.width - mar.width) * ratio) + mar.height
    }
    x11(width=win.width, height=win.height, pointsize=10)
    
  # plot labels
    
    vars <- srvy.dat("vars")
    x.lab    <- vars[2]
    y.lab    <- vars[3]
    state.id <- vars[4]
    
    if(!is.null(srvy.dat("depth")) & !is.null(srvy.dat("wtr.elev"))) {
        if(as.logical(srvy.dat("depth"))) 
            state.id <- gsub("Depth", "Elevation", state.id, ignore.case=TRUE)
    }
    
    m.lab <- paste(c("Level Contours", state.id), collapse=" of ")
    
  # filled contour plot
    
    fun <- function() {
        axis(1, las=1, cex.axis=0.7)
        axis(2, las=0, cex.axis=0.7)
        minorTics(1:2)
        title(main=m.lab, xlab=x.lab, ylab=y.lab, cex.main=0.9)
        box()
        
###     contour(x, y, z, col="red", lty="solid", add=TRUE, drawlabels=T, levels=level, lwd=0.1) # contour lables
        
        ids <- names(tran.dat())
        if(length(ids) > 0) {
            for(id in ids) {
                vertices <- tran.dat(id, "vertices")
                lines(vertices, col="black")
                points(vertices, col="black", pch=19)
                text(vertices, pos=1, cex=0.7, labels=paste(id, c("L", "R"), sep="-"))
            }
        }
        
        if(const.poly) {
            xy <- locator(type="o", col="blue", cex=1, pch=19)
            
            lines(cbind(c(xy$x, xy$x[1]), c(xy$y, xy$y[1])), col="blue")
            
            new.ply <- as(as.data.frame(xy), "gpc.poly")
            
            if(!is.null(ply) & !is.null(new.ply)) 
                srvy.dat("poly", intersect(ply, new.ply))
            if( is.null(ply) & !is.null(new.ply)) 
                srvy.dat("poly", new.ply)
            
            ply <- srvy.dat("poly")
            if(!is.null(ply)) 
                data.tin <- polyCutout(data.tin, ply)
            
            x <- data.tin$x
            y <- data.tin$y
            z <- data.tin$z
            
            z.lim <- range(z, na.rm=TRUE)
            
            levels <- pretty(z.lim, srvy.dat("n.levels"))
            col.palette <- colorRampPalette(c("blue", "white", "red"))
            cols <- col.palette(length(levels) - 1)
            
            .Internal(filledcontour(as.double(x), as.double(y), z, as.double(levels), col=cols))
        }
    }
    
    level <- pretty(z.lim, srvy.dat("n.levels"))
    num <- length(level) - 1
    add.on <- as.integer(num * 0.2)
    
#   colrs <- rev(gray(1:(num + add.on) / (num + add.on))[(add.on + 1):(num + add.on)]) # gray contours
    
    colrs <- rev(terrain.colors(num)) # color contours
    
    filled.contour(x, y, z, xlim=x.lim, ylim=y.lim, zlim=z.lim, levels=level, asp=asp.ratio, 
        col=colrs, key.axes=axis(4, las=1, cex.axis=0.7), plot.axes=fun())
    
  # margin text
    
    data.file <- srvy.dat("data.file")
    if(nchar(data.file) > 40) 
        data.file <- paste(substr(data.file, 1, 40), "...", sep="")
    txt <- paste("Raw File(s):", data.file, sep=" ")
    mtext(txt, side=3, line=-0.6, cex=0.7, outer=TRUE, adj=0)
    
    txt <- NULL
    if(!is.null(srvy.dat("wtr.elev"))) 
        txt <- append(txt, paste("Water Surface Elev: ", srvy.dat("wtr.elev"), "m", sep=""))
    
    if(!is.null(ply)) {
        area <- area.poly(ply)
        vol  <- sum(na.omit(as.vector(z))) * (mean(diff(x)) * mean(diff(y)))
        txt <- append(txt, paste("Surface Area: ", round(area, 0), "m^2", sep=""))
        txt <- append(txt, paste("Volume: ",       round(vol,  0), "m^3", sep=""))
    }
    
    if(!is.null(txt)) 
        mtext(paste(na.omit(txt), collapse="; "), side=3, line=-0.6, cex=0.7, outer=TRUE, adj=1)
    
    txt <- srvy.dat("projection")
    if(!is.null(srvy.dat("off.z"))) 
        txt <- append(txt, paste("State Variable Offset:", srvy.dat("off.z"), sep=" "))
    mtext(paste(na.omit(txt), collapse="; "), side=1, line=-1.1, cex=0.7, outer=TRUE, adj=0)
    
    txt <- paste(srvy.dat("ver"), date(), sep=", ")
    mtext(txt, side=1, line=-1.1, cex=0.7, outer=TRUE, adj=1)
}


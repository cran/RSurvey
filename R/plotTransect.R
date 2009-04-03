"plotTransect" <- function(id, rasterField=NULL, velPlot=FALSE, flow="magnitude") {
    
# additional functions (subroutines)
    
  # mean value at sampling point
    
    meanDwell <- function(dat) {
        xy <- uniquecombs(as.matrix(dat[,1:2]))
        xyz <- cbind(xy, matrix(data=NA, nrow=nrow(xy), ncol=ncol(dat) - 2))
        colnames(xyz) <- names(dat)
        for(i in 1:nrow(xyz)) {
            logic <- dat$x == xyz[i,"x"] & dat$y == xyz[i,"y"]
            for(j in 3:(ncol(dat))) 
                xyz[i,j] <- mean(dat[logic, j])
        }
        as.data.frame(xyz)
    }
    
  # Point approximation from bivariate scattered data using multilevel B-splines
    
    interp.fun <- function(dat, grid.pts, poly.domain) {
        m <- n <- 1
        k <- diff(range(dat$y)) / diff(range(dat$x))
        if(k < 1) 
            m <- 2 
        else 
            n <- 2
        hld <- mba.points(xyz=as.matrix(dat), xy.est=grid.pts, n=n, m=m, h=11, extend=TRUE)$xyz.est
        ans <- polyCutout(hld, poly.domain)
        ans
    }
    
  # points in polygon
    
    pts.in.poly <- function(x, y, poly.domain) {
        idx <- 1:length(x)
        pts <- get.pts(poly.domain)
        ans <- NULL
        for(i in 1:length(pts)) {
            hld <- point.in.polygon(point.x=x, point.y=y, pol.x=pts[[i]]$x, pol.y=pts[[i]]$y)
            ans <- c(ans, idx[hld != 0])
        }
        ans
    }
    
# main program
    
    if(length(tran.dat()) == 0) stop(call.=FALSE, "No profiles exist.")
    
    if(!is.null(rasterField)) 
        n.2 <- match(rasterField, tran.dat(id, "type"))
    
  # simplify data notation
    
    win.width <- srvy.dat("win.width")
    n.levels  <- srvy.dat("n.levels")
    
    vertices  <- tran.dat(id, "vertices")
    fix.zero  <- tran.dat(id, "fix.zero")
    v.origin  <- tran.dat(id, "v.origin")
    v.offset  <- tran.dat(id, "v.offset")
    h.offset  <- tran.dat(id, "h.offset")
    grid.dx   <- tran.dat(id, "grid.dx")
    grid.dy   <- tran.dat(id, "grid.dy")
    zero.int  <- tran.dat(id, "zero.int")
    asp.ratio <- tran.dat(id, "asp.ratio")
    
  # total distance between vertices
    
    total.dist <- sqrt((vertices[2,1] - vertices[1,1])^2 + 
                  (vertices[2,2] - vertices[1,2])^2)
    
  # text describing vertices
    
    s.lab <- paste("(", paste(as.character(vertices[1,]), collapse=", "), ") (", 
             paste(as.character(vertices[2,]), collapse=", "), ")", sep="")
    
  # profile source is either taken from interpolated surface or imported
    
    d <- int.d <- pro.d <- NULL
    
    if(!is.null(srvy.dat("data.tin"))) {
        hld <- tran.profile(vertices)[,c("h", "z")]
        colnames(hld) <- c("x", "y")
        d <- int.d <- hld
    }
    
    pro.d <- tran.dat(id, "prof")
    if(!is.null(pro.d)) {
        colnames(pro.d) <- c("x", "y")
        d <- pro.d
    }
    
  # plot depth profile
    
    if(is.null(rasterField) && !velPlot) {
        if(is.null(d)) stop(call.=FALSE, "No profiles exist.")
        
        lim.x <- range(c(int.d[,"x"], pro.d[,"x"]))
        lim.y <- range(c(int.d[,"y"], pro.d[,"y"]))
        
        if(is.null(asp.ratio)) {
            win.height <- win.width
            asp.ratio <- NA
        }
        else {
            mar <- c(5.1, 4.1 / asp.ratio, 4.1, 2.1 / asp.ratio)
            mar.width <- (mar[2] + mar[4]) * srvy.dat("csi")
            mar.height <- mar.width * ((mar[1] + mar[3]) / (mar[2] + mar[4]))
            ratio <- diff(lim.y) / diff(lim.x) * asp.ratio
            win.height <- ((win.width - mar.width) * ratio) + mar.height
        }
        x11(width=win.width, height=win.height, pointsize=10)
        
        plot(NA, xlim=lim.x, ylim=lim.y, type="n", axes=FALSE, ann=FALSE, asp=asp.ratio)
        
        cols <- typs <- NULL
        if(!is.null(int.d)) {
            lines(int.d[,"x"], int.d[,"y"], col="dark green")
            cols <- "dark green"
            typs <- "2D Surface"
        }
        if(!is.null(pro.d)) {
            lines(pro.d[,"x"], pro.d[,"y"], col="red")
            cols <- append(cols, "red")
            typs <- append(typs, "Imported")
        }
        
        x.lab <- "Local x [L]"
        y.lab <- "z"
        
        title(main=paste("Profile Along Transect", id), xlab=x.lab, ylab=y.lab, cex.main=0.9)
        axis(1, las=1, cex.axis=0.7)
        axis(2, las=0, cex.axis=0.7)
        minorTics(1:2)
        
        legend(x="bottomright", legend=typs, col=cols, lty=c(1, 1), cex=0.7)
        
        box()
        
        txt <- paste(srvy.dat("ver"), date(), sep=", ")
        mtext(txt, side=1, line=-1.1, cex=0.7, outer=TRUE, adj=1)
        
        mtext(s.lab, line=1, cex=0.7)
        
        return()
    }
    
  # simplify x and y values
    
    hv.fields <- tran.dat(id, "hv.fields")
    if(is.null(hv.fields)) 
        stop(call.=FALSE, "Spatial fields have not been specified.")
    hv.fields <- make.names(hv.fields)
    dat <- as.data.frame(tran.dat(id, "data.ras")[,hv.fields])
    names(dat) <- c("x", "y")
    
  # add local x offset to the local x values
    
    dat$x <- dat$x + h.offset
    
  # correct for downstream view
    
    if(fix.zero == "R") 
        dat$x <- total.dist - dat$x
    
  # translate local z values into the z coordinate system
    
    dat$y <- v.origin + v.offset + dat$y
    
  # spatial domain
    
    poly.domain <- NULL
    if(is.null(d)) 
        poly.domain <- as(dat[chull(dat),], "gpc.poly")
    else {
        xy <- d[,c("x", "y")]
        
        if(v.origin <= min(d[,"y"])) 
            stop(call.=FALSE, "Local z-axis origin less than or equal to minium z value.")
        else if(v.origin == max(d[,"y"])) 
            poly.domain <- as(xy, "gpc.poly")
        else if(v.origin >  max(d[,"y"])) {
            hld <- rbind(c(d[1, "x"], v.origin), xy, c(d[nrow(d), "x"], v.origin))
            poly.domain <- as(hld, "gpc.poly")
        }
        else {
            hld <- c(range(d[,"x"]), rev(range(d[,"x"])), rep(min(d[,"y"]) - 1, 2), rep(v.origin, 2))
            hld <- structure(hld, .Dim=c(4, 2))
            poly.domain <- intersect(as(xy, "gpc.poly"), as(hld, "gpc.poly"))
        }
    }
    
  # cross-section
    
    area <- area.poly(poly.domain)
    
    poly.points <- get.pts(poly.domain)
    hyd.radius <- 0
    for(i in 1:length(poly.points)) 
        hyd.radius <- hyd.radius + sum(sqrt(diff(poly.points[[i]]$x)^2 + diff(poly.points[[i]]$y)^2))
    
  # interpolation grid
    
    lim.x <- range(dat$x, get.bbox(poly.domain)$x)
    lim.y <- range(min(dat$y), max(dat$y) + v.offset * 2, get.bbox(poly.domain)$y)
    
    no.x <- as.integer(diff(lim.x) / grid.dx) + 1
    no.y <- as.integer(diff(lim.y) / grid.dy) + 1
    x.o <- seq(lim.x[1], lim.x[2], length=no.x)
    y.o <- seq(lim.y[1], lim.y[2], length=no.y)
    
    grid.pts <- as.data.frame(cbind(
                x=as.vector(matrix(rep(x.o, no.y), nrow=no.x, ncol=no.y, byrow=FALSE)),
                y=as.vector(matrix(rep(y.o, no.x), nrow=no.x, ncol=no.y, byrow=TRUE))))
    
  # calculate flow, align velocity vectors with profile
    
    components <- tran.dat(id, "vel.vect")
    vel <- NULL
    
    if(!is.null(components)) {
        
        if(!any(is.na(components[1:3]))) {
            idx <- c(match(components[1:3], tran.dat(id, "type")))
            tmp <- as.data.frame(tran.dat(id, "data.ras")[,idx])
            names(tmp) <- c("x", "y", "z")
            
            asp <- if(is.null(asp.ratio)) diff(lim.x) / diff(lim.y) else asp.ratio
            arrow.max <- sqrt((diff(lim.x) / asp)^2 + diff(lim.y)^2) * 0.2
            
            tmp <- tran.vector(tran.dat(id, "vertices"), tmp, arrow.max)
            
            vel <- cbind(dat, tmp)
        }
        
        if(!is.na(components[4]) & flow=="magnitude") {
            idx <- c(match(components[4], tran.dat(id, "type")))
            tmp <- as.data.frame(tran.dat(id, "data.ras")[,idx])
            names(tmp) <- "magn"
            
            if(is.null(vel)) 
                vel <- cbind(dat, tmp)
            else 
                vel$magn <- tmp$magn
        }
    }
    
  # process data
    
    if(!velPlot) {
        
      # identify state variable
        
        dat$z <- tran.dat(id, "data.ras")[,n.2]
        
      # remove NA values
        
        dat.mod <- dat[!is.na(dat$z),]
        
      # dwell time averaging
        
        dat.mod <- meanDwell(dat.mod)
        
      # sampling points within polygon domain
        
        dat.mod <- dat.mod[pts.in.poly(dat.mod$x, dat.mod$y, poly.domain),]
        
      # locations of sampling points
        
        pts <- dat.mod[,c("x","y")]
        
      # interpolation
        
        tin <- interp.fun(dat.mod, grid.pts, poly.domain)
    }
    
    if(!is.null(vel)) {
        
      # identify state variable
        
        dat$z <- if(flow == "longTran") vel$long else vel$magn
        
      # remove NA values
        
        idx <- !is.na(dat$z)
        dat.mod <- dat[idx,]
        vel.mod <- vel[idx,]
        
      # dwell time averaging
        
        dat.mod <- meanDwell(dat.mod)
        vel.mod <- meanDwell(vel.mod)
        
      # flow processing
        
        if(!is.null(d)) {
            
          # sampling points within polygon domain
            
            idx <- pts.in.poly(dat.mod$x, dat.mod$y, poly.domain)
            dat.mod <- dat.mod[idx,]
            vel.mod <- vel.mod[idx,]
            
          # zero gradient across local y origin
            
            hld <- dat.mod[dat.mod$y == max(dat.mod$y),]
            hld$y <- hld$y + v.offset * 2
            dat.mod <- rbind(dat.mod, hld)
            
          # zero velocity along hydraulic radius
            
            if(!is.null(zero.int)) {
                d.spl <- smooth.spline(d[,c("x", "y")])
                
                no <- 100000
                dx <- seq(min(d[,"x"]), max(d[,"x"]), length=no)
                xy <- predict(d.spl, dx)
                p1 <- list(x=xy$x[1:(no - 1)], y=xy$y[1:(no - 1)])
                p2 <- list(x=xy$x[2:no], y=xy$y[2:no])
                
                dl <- sqrt((p2$x - p1$x)^2 + (p2$y - p1$y)^2)
                logic <- cumsum(dl) %% zero.int < mean(diff(dx))
                
                hld <- as.data.frame(cbind(x=xy$x[logic], y=xy$y[logic]))
                hld$z <- rep(0, nrow(hld))
                dat.mod <- rbind(dat.mod, hld)
            }
        }
        
      # interpolation
        
        hld <- interp.fun(dat.mod, grid.pts, poly.domain)
        
      # area and velocity of elements
        
        elem.area <- mean(diff(hld$x)) * mean(diff(hld$y))
        elem.vel  <- as.vector(hld$z, mode="numeric")
        
      # velocity plot data
        
        if(velPlot) {
            tin <- hld
            pts <- dat.mod[pts.in.poly(dat.mod$x, dat.mod$y, poly.domain), c("x","y")]
            vel.mod <- vel.mod[pts.in.poly(vel.mod$x, vel.mod$y, poly.domain),]
        }
        
      # other calculations
        
        vol.flow.rate <- sum(elem.vel * elem.area, na.rm=TRUE)
        
        if(!velPlot) {
            tds.elem <- as.vector(tin$z, mode="numeric") * 0.65 # convert SC from uS/cm to mg/L to g/m^3
            tds.flux.rate <- sum(tds.elem * elem.area * elem.vel, na.rm=TRUE)
        }
    }
    
    
# contour plot
    
  # limits
    
    x.min <- min(tin$x)
    x.max <- max(tin$x)
    y.min <- min(tin$y)
    y.max <- max(tin$y)
    z.min <- min(tin$z, na.rm=TRUE)
    z.max <- max(tin$z, na.rm=TRUE)
    z.lev <- n.levels
    
    limits <- tran.dat(id, "limits")
    if(!is.null(limits)) {
        if(velPlot) {
            if(flow == "magnitude") 
                limits <- as.list(limits["velocity.principle.dir",])
            else 
                limits <- as.list(limits["velocity.long.tran",])
        }
        else 
            limits <- as.list(limits[n.2,])
    }
    
    if(!is.null(limits)) {
        if(!limits$z.min.auto) z.min <- limits$z.min
        if(!limits$z.max.auto) z.max <- limits$z.max
        if(!limits$z.lev.auto) z.lev <- limits$z.lev
    }
    
    x.ext <- diff(c(x.min, x.max)) * 0.04
    y.ext <- diff(c(y.min, y.max)) * 0.04
    if(!is.null(asp.ratio)) 
        y.ext <- y.ext / asp.ratio
    lim.x <- c(x.min - x.ext, x.max + x.ext) 
    lim.y <- c(y.min - y.ext, y.max + y.ext)
    z.lim <- c(z.min, z.max)
    
  # window setup
    
    if(is.null(asp.ratio)) {
        win.height <- win.width
        asp.ratio <- NA
    }
    else {
        mar <- c(5.1, 1.1 / asp.ratio, 4.1, 4.1 / asp.ratio)
        mar.width <- (mar[2] + mar[4]) * srvy.dat("csi")
        mar.height <- mar.width * ((mar[1] + mar[3]) / (mar[2] + mar[4]))
        ratio <- (diff(range(tin$y)) / diff(range(tin$x))) * asp.ratio
        win.height <- ((win.width - mar.width) * ratio) + mar.height
    }
    x11(width=win.width, height=win.height, pointsize=10)
    
  # plot labels
    
    x.lab <- "Local x [L]"
    y.lab <- "z"
    
    txt <- "Spatial Distribution of"
    if(velPlot) {
        if(flow == "magnitude") 
            txt <- append(txt, "Magnitude Velocities")
        else 
            txt <- append(txt, "Logitudinal & Transverse Velocities")
    }
    else 
        txt <- append(txt, tran.dat(id, "type")[n.2])
    txt <- append(txt, paste("within the", id, "Profile"))
    m.lab <- paste(txt, collapse=" ")
    
  # levels and colors
    
    cols <- if(velPlot) c("blue", "white", "red") else c("#007FFF", "#FFDB58", "#E32636")
    col.palette <- colorRampPalette(cols) 
    
    level <- pretty(z.lim, z.lev)
    num <- length(level) - 1
    
    colrs <- col.palette(num)
    
### add.on <- as.integer(num * 0.2)
### colrs <- gray(1:(num + add.on) / (num + add.on))[(add.on + 1):(num + add.on)]
    
  # contour plot
    
    fun <- function() {
        points(pts$x, pts$y, col="black", pch=20, cex=0.5)
        
###     contour(tin$x, tin$y, tin$z, col="black", lty="solid", add=TRUE, drawlabels=FALSE, levels=level, lwd=0.1) # contour lables
        
        if(velPlot && flow == "longTran") 
            arrows(vel.mod$x, vel.mod$y, vel.mod$x + vel.mod$arrow.x, vel.mod$y + vel.mod$arrow.y, 
                length=0.05, angle=20)
        
        axis(1, las=1, cex.axis=1)
        axis(2, las=0, cex.axis=1)
        minorTics(1:2)
        title(main=m.lab, xlab=x.lab, ylab=y.lab, cex.main=0.9)
        mtext(s.lab, line=1, cex=0.7)
        box()
    }
    filled.contour(tin$x, tin$y, tin$z, xlim=lim.x, ylim=lim.y, zlim=z.lim, 
        levels=level, asp=asp.ratio, col=colrs, key.axes=axis(4, las=1, cex.axis=1), 
        plot.axes=fun())
    
    txt <- paste("File:", tran.dat(id, "data.file"))
    mtext(txt, side=3, line=-0.6, cex=0.7, outer=TRUE, adj=0)
    
    txt <- paste("Area:", round(area, digits=2), sep=" ")
    txt <- append(txt, paste("Hydraulic Radius:", round(hyd.radius, digits=2), sep=" "))
    mtext(paste(txt, collapse="; "), side=3, line=-0.6, cex=0.7, outer=TRUE, adj=1)
    
    txt <- paste("Surface Elev:", round(v.origin, digits=2), sep=" ")
    txt <- append(txt, paste("Below Surf", round(v.offset, digits=2), sep=" "))
    txt <- append(txt, paste("Horiz. Offset:", round(h.offset, digits=2), sep=" "))
    txt <- append(txt, paste("Grid dx:", round(grid.dx, digits=2), sep=" "))
    txt <- append(txt, paste("Grid dy:", round(grid.dy, digits=2), sep=" "))
    if(velPlot & !is.null(zero.int)) 
        txt <- append(txt, paste("Zero Vel Interval:", round(zero.int, digits=2), sep=" "))
    mtext(paste(txt, collapse="; "), side=3, line=-1.4, cex=0.7, outer=TRUE, adj=1)
    
    txt <- NULL
    if(exists("vol.flow.rate")) 
        txt <- append(txt, paste("Vol. Flow Rate:", round(vol.flow.rate, digits=3), sep=" "))
    if(exists("tds.flux.rate")) 
        txt <- append(txt, paste("TDS Flux Rate:", round(tds.flux.rate, digits=3), sep=" "))
    mtext(paste(txt, collapse="; "), side=1, line=-1.1, cex=0.7, outer=TRUE, adj=0)
    
    txt <- paste(srvy.dat("ver"), date(), sep=", ")
    mtext(txt, side=1, line=-1.1, cex=0.7, outer=TRUE, adj=1)
}

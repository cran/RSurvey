"srvy.process" <- function(const.3d=FALSE) {
    
  # simplify data
    
    data.raw <- srvy.dat("data.raw")
    data.pts <- srvy.dat("data.pts")
    data.grd <- srvy.dat("data.grd")
    
  # process data
    
    if(is.null(data.pts)) {
        
        vars <- srvy.dat("vars")
        
        dat <- NULL
        
        dat$x <- data.raw[[vars$x]]
        dat$y <- data.raw[[vars$y]]
        
        dat$z <- if(is.null(vars$z)) NULL else data.raw[[vars$z]]
        dat$t <- if(is.null(vars$t)) NULL else data.raw[[vars$t]]
        
        dat$vx <- if(is.null(vars$vx)) NULL else data.raw[[vars$vx]]
        dat$vy <- if(is.null(vars$vy)) NULL else data.raw[[vars$vy]]
        
      # constrain spatial data
        
        dat <- as.data.frame(dat)[!is.na(dat$x) & !is.na(dat$y),]
        dat <- dat[dat$x >= srvy.dat("min.x") & dat$x <= srvy.dat("max.x"),]
        dat <- dat[dat$y >= srvy.dat("min.y") & dat$y <= srvy.dat("max.y"),]
        
      # constrain state variable
        
        if(!is.null(vars$z)) {
            dat <- as.data.frame(dat)[!is.na(dat$z),]
            dat <- dat[dat$z >= srvy.dat("min.z") & dat$z <= srvy.dat("max.z"),]
            
          # offset
            
            if(!is.null(srvy.dat("off.z"))) 
                dat$z <- dat$z + srvy.dat("off.z")
            
          # correct for water surface elevation
            
            if(!is.null(srvy.dat("depth")) && !is.null(srvy.dat("wtr.elev"))) 
                if(as.logical(srvy.dat("depth"))) 
                    dat$z <- srvy.dat("wtr.elev") - dat$z
        }
        
      # constrain temporal data
        
        if(!is.null(vars$t)) {
            datetime <- strptime(dat$t, "%Y-%m-%d %H:%M:%OS")
            dat <- as.data.frame(dat)[!is.na(datetime) & datetime >= srvy.dat("min.t") & datetime <= srvy.dat("max.t"),]
            
          # remove data spikes, gradient correction ([change in state]/[change in time] < grad.tol)
            
            if(!is.null(srvy.dat("grad.tol"))) {
                datetime <- strptime(dat$t, "%Y-%m-%d %H:%M:%OS")
                
                s.1 <- s.0 <- as.numeric(difftime(datetime, min(datetime), units="secs"))
                z.1 <- dat$z
                
                while(TRUE) {
                    grad <- abs(diff(z.1) / diff(s.1))
                    
                    if(!is.null(srvy.dat("time.gap")))
                        grad[diff(s.1) > srvy.dat("time.gap")] <- 0
                    
                    grad <- c(0, grad)
                    
                    if(all(grad < srvy.dat("grad.tol"), na.rm=TRUE)) break
                    
                    logic <- grad < srvy.dat("grad.tol")
                    logic[is.na(grad)] <- FALSE
                    
                    s.1 <- s.1[logic]
                    z.1 <- z.1[logic]
                }
                dat <- dat[(s.0 %in% s.1),]
            }
            
          # offset
            
            if(!is.null(srvy.dat("off.t"))) {
                tmp <- strptime(dat$t, "%Y-%m-%d %H:%M:%OS") + srvy.dat("off.t")
                dat$t <- format(tmp, format="%Y-%m-%d %H:%M:%OS")
            }
        }
        
      # incorporate polygon spatial domain
        
        ply <- if(is.null(srvy.dat("polyRange"))) NULL else srvy.dat("poly")[[srvy.dat("polyRange")]]
        
        if(class(ply) == "gpc.poly") {
            d <- get.pts(ply)
            for(i in 1:length(d)) {
                pts <- d[[i]]
                hld <- point.in.polygon(point.x=dat$x, point.y=dat$y, pol.x=pts$x, pol.y=pts$y)
                poly.logic <- if(pts$hole) hld != 1 else hld != 0
                dat <- dat[poly.logic,]
            }
        }
        
      # save modified data
        
        srvy.dat("data.pts", dat)
    }
    
  # construct 3-D surface
    
    if(is.null(data.grd) & const.3d) {
        
        data.pts <- srvy.dat("data.pts")
        
      # simplify data notation
        
        x <- data.pts$x
        y <- data.pts$y
        z <- data.pts$z
        
        vx <- data.pts$vx
        vy <- data.pts$vy
        
        if(is.null(z)) return()
        
      # limit polygon
        
        ply <- if(is.null(srvy.dat("polyLimit"))) NULL else srvy.dat("poly")[[srvy.dat("polyLimit")]]
        
      # define the grid and characteristics for the interpolated values
        
        if(is.null(ply)) {
            xlim <- range(x, na.rm=TRUE)
            ylim <- range(y, na.rm=TRUE)
        }
        else {
            bb <- get.bbox(ply)
            xlim <- bb$x
            ylim <- bb$y
        }
        
        xnum <- 100
        ynum <- 100
        
        if(!is.null(srvy.dat("grid.dx"))) 
            xnum <- as.integer(diff(xlim) / srvy.dat("grid.dx")) + 1
        
        if(!is.null(srvy.dat("grid.dy"))) 
            ynum <- as.integer(diff(ylim) / srvy.dat("grid.dy")) + 1
        
        if(xnum < 1 | ynum < 1) stop("grid resolution equal to zero")
        
      # bivariate interpolation onto grid for irregularly spaced data
        
        xo <- seq(xlim[1], xlim[2], length=xnum)
        yo <- seq(ylim[1], ylim[2], length=ynum)
        
        pts <- as.data.frame(cbind(
               x=as.vector(matrix(rep(xo, ynum), nrow=xnum, ncol=ynum, byrow=FALSE)),
               y=as.vector(matrix(rep(yo, xnum), nrow=xnum, ncol=ynum, byrow=TRUE)))
        )
        
        m <- n <- 1
        k <- diff(range(y)) / diff(range(x))
        if(k < 1) m <- 2 else n <- 2
        
        surf <- function(x, y, z, pts, n, m) {
            xyz <- matrix(data=c(x, y, z), ncol=3)
            ans <- mba.points(xyz=xyz, xy.est=pts, n=n, m=m, h=11, extend=TRUE, verbose=FALSE)$xyz.est
            xy <- cbind(x,y)
            domain <- if(is.null(ply)) as(xy[chull(xy),], "gpc.poly") else ply
            polyCutout(ans, domain)
        }
        
        dat <- surf(x, y, z, pts, n, m)
        
        if(!is.null(vx)) dat$vx <- surf(x, y, vx, pts, n, m)$z
        if(!is.null(vy)) dat$vy <- surf(x, y, vy, pts, n, m)$z
        
      # save surface data
        
        srvy.dat("data.grd", dat)
    }
    
}

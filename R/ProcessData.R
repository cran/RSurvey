"ProcessData" <- function() {
  # This function performs data processing on the state variables.
  
  # Additional functions (subroutines)
  
  # Evaluate function
  
  Eval <- function(v) {
    if (is.null(v)) {
      rtn <- NULL
    } else {
      rtn <- EvalFunction(cols[[v]]$fun, cols)
    }
    rtn
  }
  
  
  # Main program
  
  # Process point data
  
  if (is.null(Data("data.pts"))) {
    
    Data("data.grd", NULL)
    
    # Simplify data notation
    
    cols <- Data("cols")
    vars <- Data("vars")
    
    if (is.null(Data("data.raw"))) {
      tmp  <- Eval(vars$x)
      d <- data.frame(index=1:length(tmp))
      d$x  <- tmp
    } else {
      d <- data.frame(index=1:nrow(Data("data.raw")))
      d$x  <- Eval(vars$x)
    }
    
    d$y  <- Eval(vars$y)
    d$z  <- Eval(vars$z)
    d$t  <- Eval(vars$t)
    d$vx <- Eval(vars$vx)
    d$vy <- Eval(vars$vy)
    d$vz <- Eval(vars$vz)
    
    # Remove NA's
    
    if (!is.null(vars$x)) 
      d <- d[!is.na(d$x), ]
    if (!is.null(vars$y)) 
      d <- d[!is.na(d$y), ]
    if (!is.null(vars$z)) 
      d <- d[!is.na(d$z), ]
    if (!is.null(vars$t)) 
      d <- d[!is.na(d$t), ]
    
    # Range limits
    
    lim <- Data("lim.data")
    
    if (!is.null(lim)) {
      if (!is.null(vars$x)) {
        if (!is.na(lim$xlim[1])) 
          d <- d[d$x >= lim$xlim[1], ]
        if (!is.na(lim$xlim[2])) 
          d <- d[d$x <= lim$xlim[2], ]
      }
      if (!is.null(vars$y)) {
        if (!is.na(lim$ylim[1])) 
          d <- d[d$y >= lim$ylim[1], ]
        if (!is.na(lim$ylim[2])) 
          d <- d[d$y <= lim$ylim[2], ]
      }
      if (!is.null(vars$z)) {
        if (!is.na(lim$zlim[1])) 
          d <- d[d$z >= lim$zlim[1], ]
        if (!is.na(lim$zlim[2])) 
          d <- d[d$z <= lim$zlim[2], ]
      }
      if (!is.null(vars$t)) {
        if (!is.na(lim$tlim[1])) 
          d <- d[d$t >= lim$tlim[1], ]
        if (!is.na(lim$tlim[2])) 
          d <- d[d$t <= lim$tlim[2], ]
      }
    }
    
    # Incorporate polygon spatial domain
    
    if (!is.null(vars$x) & !is.null(vars$y)) {
      ply <- Data("poly.data")
      if (!is.null(ply)) 
        ply <- Data(c("poly", ply))
      if (inherits(ply, "gpc.poly")) {
        all.pts <- get.pts(ply)
        for (i in seq(along=all.pts)) {
          pts <- all.pts[[i]]
          tmp <- point.in.polygon(point.x=d$x, point.y=d$y, 
                                  pol.x=pts$x, pol.y=pts$y)
          d <- d[if (pts$hole) tmp != 1 else tmp != 0, ]
        }
      }
    }
    
    # Save point data
    
    if (nrow(d) == 0) 
      stop("Range excludes all data")
    Data("data.pts", d)
  }
  
  # Construct 3-D surface
  
  if (is.null(Data("data.grd"))) {
    
    # Simplify data notation
    
    x <- Data(c("data.pts", "x"))
    y <- Data(c("data.pts", "y"))
    z <- Data(c("data.pts", "z"))
    
    if (is.null(x) | is.null(y) | is.null(z)) 
      return()
    
    vx <- Data(c("data.pts", "vx"))
    vy <- Data(c("data.pts", "vy"))
    vz <- Data(c("data.pts", "vz"))
    
    # Limit polygon
    
    ply <- Data("poly.crop")
    if (!is.null(ply)) 
      ply <- Data("poly")[[ply]]
    
    # Define the grid and characteristics for the interpolated values
    
    if (is.null(ply)) {
      xlim <- range(x, na.rm=TRUE)
      ylim <- range(y, na.rm=TRUE)
    } else {
      bb <- get.bbox(ply)
      xlim <- bb$x
      ylim <- bb$y
    }
    
    dx <- Data("grid.dx")
    dy <- Data("grid.dy")
    
    xnum <- ynum <- 100
    if (!is.null(dx)) 
      xnum <- as.integer(diff(xlim) / dx) + 1
    if (!is.null(dy)) 
      ynum <- as.integer(diff(ylim) / dy) + 1
    
    if (xnum < 1 | ynum < 1) 
      stop("Grid resolution equal to zero")
    
    # Bivariate interpolation onto grid for irregularly spaced data
    
    xo <- seq(xlim[1], xlim[2], length=xnum)
    yo <- seq(ylim[1], ylim[2], length=ynum)
    
    x1 <- as.vector(matrix(rep(xo, ynum), nrow=xnum, ncol=ynum, byrow=FALSE))
    y1 <- as.vector(matrix(rep(yo, xnum), nrow=xnum, ncol=ynum, byrow=TRUE))
    
    pts <- as.data.frame(cbind(x=x1, y=y1))
    
    m <- n <- 1
    x.diff <- diff(range(x))
    y.diff <- diff(range(y))
    if (x.diff == 0 || y.diff == 0) {
      Data("data.grd", NULL)
      warning("Interpolation failed due to data range of zero")
    } else {
      k <- y.diff / x.diff
    }
    if (k < 1) {
      m <- 2 
    } else {
      n <- 2
    }
    if (!is.null(Data("mba.m"))) 
      m <- Data("mba.m")
    if (!is.null(Data("mba.n"))) 
      n <- Data("mba.n")
    h <- Data("mba.h")
    
    GetSurface <- function(x, y, z, pts, n, m) {
      xyz <- matrix(data=c(x, y, z), ncol=3)
      ans <- mba.points(xyz=xyz, xy.est=pts, n=n, m=m, h=h, 
                        extend=TRUE, verbose=FALSE)$xyz.est
      xy <- cbind(x, y)
      if (is.null(ply)) {
        domain <- as(xy[chull(xy), ], "gpc.poly")
      } else {
        domain <- ply
      }
      CutoutPolygon(ans, domain)
    }
    
    d <- GetSurface(x, y, z, pts, n, m)
    
    if (!is.null(vx)) 
      d$vx <- GetSurface(x, y, vx, pts, n, m)$z
    if (!is.null(vy)) 
      d$vy <- GetSurface(x, y, vy, pts, n, m)$z
    if (!is.null(vz)) {
      d$vz <- GetSurface(x, y, vz, pts, n, m)$z
      
      # Volumetrix flux
      
      GetArcLength <- function(x) {
        diff(c(x[1], x[-1] - (diff(x) / 2), x[length(x)]))
      } # returns arc length
      
      m <- length(d$x)
      n <- length(d$y)
      area <- matrix(rep(GetArcLength(d$x), n), nrow=m, ncol=n, byrow=FALSE) * 
              matrix(rep(GetArcLength(d$y), m), nrow=m, ncol=n, byrow=TRUE)
      vol.flux <- sum(d$vz * area, na.rm=TRUE) # vol. flux = velocity * area
      if (is.numeric(vol.flux)) 
        d$vf <- vol.flux
    }
    
    # Save grid data
    
    Data("data.grd", d)
  }
}

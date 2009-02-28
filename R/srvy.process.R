"srvy.process" <- function(const.tin=FALSE) {
    
  # simplify data
    
    data.raw <- srvy.dat("data.raw")
    data.mod <- srvy.dat("data.mod")
    data.tin <- srvy.dat("data.tin")
    
  # process data
    
    if(is.null(data.mod)) {
        dat <- NULL
        
        vars <- srvy.dat("vars")
        vars[!is.na(vars)] <- make.names(vars[!is.na(vars)])
        
        dat$datetime <- data.raw[[vars[1]]]
        dat$x        <- data.raw[[vars[2]]]
        dat$y        <- data.raw[[vars[3]]]
        dat$z        <- data.raw[[vars[4]]]
        
      # constrain temporal data
        
        if(!is.null(dat$datetime)) {
            
            datetime <- strptime(dat$datetime, "%Y-%m-%d %H:%M:%OS")
            
            s.t <- paste(srvy.dat("sd"), " ", srvy.dat("sh"), ":", srvy.dat("sm"), ":", srvy.dat("ss"), sep="")
            s.t <- strptime(s.t, "%Y-%m-%d %H:%M:%OS")
            
            e.t <- paste(srvy.dat("ed"), " ", srvy.dat("eh"), ":", srvy.dat("em"), ":", srvy.dat("es"), sep="")
            e.t <- strptime(e.t, "%Y-%m-%d %H:%M:%OS")
            
            dat <- as.data.frame(dat)[!is.na(datetime) & datetime >= s.t & datetime <= e.t,]
        }
        
      # remove NA values from dataframe
        
        dat <- as.data.frame(dat)
        dat <- dat[!is.na(dat$x) & !is.na(dat$y) & !is.na(dat$z),]
        
      # constrain spatial data and the state variable
        
        dat <- dat[dat$x >= srvy.dat("min.x") & dat$x <= srvy.dat("max.x"),]
        dat <- dat[dat$y >= srvy.dat("min.y") & dat$y <= srvy.dat("max.y"),]
        
        if(!is.null(srvy.dat("min.z"))) 
            dat <- dat[dat$z >= srvy.dat("min.z"),]
        if(!is.null(srvy.dat("max.z"))) 
            dat <- dat[dat$z <= srvy.dat("max.z"),]
        
      # remove data spikes with gradient correction ([change in state]/[change in time] < grad.tol)
        
        if(!is.null(dat$datetime) & !is.null(srvy.dat("grad.tol"))) {
            datetime <- strptime(dat$datetime, "%Y-%m-%d %H:%M:%OS")
            
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
        
      # state variable offset
        
        if(!is.null(srvy.dat("off.z"))) 
            dat$z <- dat$z + srvy.dat("off.z")
        
      # time offset
        
        if(!is.null(srvy.dat("off.t"))) {
            tmp <- strptime(dat$datetime, "%Y-%m-%d %H:%M:%OS") + srvy.dat("off.t")
            dat$datetime <- format(tmp, format="%Y-%m-%d %H:%M:%OS")
        }
        
      # correct for water surface elevation
        
        if(!is.null(srvy.dat("depth")) && !is.null(srvy.dat("wtr.elev"))) {
            if(as.logical(srvy.dat("depth"))) 
                dat$z <- srvy.dat("wtr.elev") - dat$z
        }
        
      # incorporate polygon spatial domain
        
        if(!is.null(srvy.dat("poly"))) {
            d <- get.pts(srvy.dat("poly"))
            for(i in 1:length(d)) {
                pts <- d[[i]]
                hld <- point.in.polygon(point.x=dat$x, point.y=dat$y, pol.x=pts$x, pol.y=pts$y)
                poly.logic <- if(pts$hole) hld != 1 else hld != 0
                dat <- dat[poly.logic,]
            }
        }
        
      # save modified data
        
        srvy.dat("data.mod", dat)
    }
    
  # TIN construction
    
    if(is.null(data.tin) & const.tin) {
        
        data.mod <- srvy.dat("data.mod")
        
      # simplify data notation
        
        x <- data.mod$x
        y <- data.mod$y
        z <- data.mod$z
        
      # define the grid and characteristics for the interpolated values
        
        lim.x <- range(x)
        lim.y <- range(y)
        no.x <- as.integer(diff(lim.x) / srvy.dat("grid.res")) + 1
        no.y <- as.integer((diff(lim.x) / srvy.dat("grid.res")) * (diff(lim.y) / diff(lim.x))) + 1
        
        no <- max(c(no.x, no.y))
        
      # bivariate interpolation onto grid for irregularly spaced data
        
        m <- n <- 1
        k <- diff(lim.y) / diff(lim.x)
        if(k < 1) 
            m <- as.integer(1 / k)
        else 
            n <- as.integer(k)
        
        dat <- suppressWarnings(mba.surf(xyz=matrix(data=c(x, y, z), ncol=3), 
               no.X=no, no.Y=no, n=n, m=m, h=11, extend=FALSE)$xyz.est)
        
      # save surface data
        
        srvy.dat("data.tin", dat)
    }
}

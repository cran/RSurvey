"polyImport" <- function(file=NULL, dl=NULL) {
    
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
        pts <- if(is.na(pt1[3])) cbind(x, y) else cbind(x, y, z)
        as.data.frame(pts)
    }
    
# main program
    
  # import data from polygon file
    
    f <- getFile(cmd="open", exts="txt", file=file)
    if(is.null(f)) return()
    
    con <- if("connection" %in% class(file)) file 
           else file(f$path, "r", encoding=srvy.dat("encoding"))
    
    dat1 <- scan(file=con, what="raw", sep=srvy.dat("sep"), nlines=1, quiet=TRUE)
    
    if(length(dat1) == 1) {
        dat <- append(as.numeric(dat1), scan(con, quiet=TRUE))
        poly.dat <- as(dat, "gpc.poly")
    }
    else {
        dat <- read.table(file=con, sep=srvy.dat("sep"), 
               col.names=c("Index", "X", "Y", "Code"))
               
      # input file requires header, Code 0 for defining inlets/outlets
        
        hld <- dat[dat$Code <= 1,]
        hld <- unique(hld[order(hld$Index), c("X", "Y", "Code")])
        
        if(hld$Code[nrow(hld)] != 0) 
            hld <- rbind(hld, hld[1,])
        
        x <- hld[1, 1]
        y <- hld[1, 2]
        
        for(i in 2:nrow(hld)) {
            if(!(hld[i - 1, "Code"] == 0 && hld[i, "Code"] == 0) && !is.null(dl)) {
                pts <- pts.on.line(as.numeric(hld[i - 1, 1:2]), as.numeric(hld[i, 1:2]), dl)
                x <- c(x, pts[,1])
                y <- c(y, pts[,2])
            }
            else {
                x <- c(x, hld[i, 1])
                y <- c(y, hld[i, 2])
            }
        }
        
        n <- 1:length(x)
        poly.dat <- as(c(1, length(n), 0, as.vector(t(cbind(x[n], y[n])))), "gpc.poly")
        
        tmp <- dat[dat$Code > 1,]
        for(i in sort(unique(tmp$Code))) {
            hld <- tmp[tmp$Code == i,]
            hld <- unique(hld[order(hld$Index), c("X", "Y")])
            hld <- rbind(hld, hld[1,])
            
            x <- hld[1, 1]
            y <- hld[1, 2]
            
            for(i in 2:nrow(hld)) {
                if(!is.null(dl)) {
                    pts <- pts.on.line(as.numeric(hld[i - 1, 1:2]), as.numeric(hld[i, 1:2]), dl)
                    x <- c(x, pts[,1])
                    y <- c(y, pts[,2])
                }
                else {
                    x <- c(x, hld[i, 1])
                    y <- c(y, hld[i, 2])
                }
            }
            n <- 1:length(x)
            inner.poly <- as(c(1, length(n), 1, as.vector(t(cbind(x[n], y[n])))), "gpc.poly")
            poly.dat <- append.poly(poly.dat, inner.poly)
        }
    }
    close(con)
    poly.dat
}

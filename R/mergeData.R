"mergeData" <- function(file=NULL) {
    
    old.dat <- srvy.dat("data.raw")
    old.col <- srvy.dat("cols")
    vars    <- srvy.dat("vars")
    
    f <- getFile(cmd="open", exts="txt", file=file)
    if(is.null(f)) return()
    
    con <- if("connection" %in% class(file)) file 
           else file(f$path, "r", encoding=srvy.dat("encoding"))
    
    new.col <- as.character(as.vector(t(read.table(file=con, row.names=NULL, 
               sep=srvy.dat("sep"), nrows=1))))
    new.unt <- as.character(as.vector(t(read.table(file=con, row.names=NULL, 
               sep=srvy.dat("sep"), nrows=1))))
    new.dat <- read.table(file=con, row.names=NULL, sep=srvy.dat("sep"))
    
    close(con)
    
    tmp <- rep("", length(new.unt))
    tmp[!is.na(new.unt)] <- paste(" (", new.unt[!is.na(new.unt)], ")", sep="")
    new.col <- paste(new.col, tmp, sep="")
    
    colnames(new.dat) <- make.names(new.col)
    
    tbl.info <- cbind(c(names(old.dat), names(new.dat)), c(old.col, new.col))
    
    xname <- make.names(vars[2])
    yname <- make.names(vars[3])
    
    for(i in 1:ncol(new.dat)) {
        if(any(!is.na(strptime(new.dat[,i], new.unt[i])))) {
            colnames(new.dat) <- replace(names(new.dat), i, make.names(vars[1]))
            dt1 <- strptime(new.dat[,i], new.unt[i])
            new.dat[,i] <- format(dt1, format="%Y-%m-%d %H:%M:%OS")
            break
        }
    }
    
    dt0 <- strptime(old.dat[,make.names(vars[1])], "%Y-%m-%d %H:%M:%OS")
    
    sec0 <- as.numeric(difftime(dt0, min(dt0, na.rm=TRUE), units="sec"))
    sec1 <- as.numeric(difftime(dt1, min(dt0, na.rm=TRUE), units="sec"))
    
    new.dat[,xname] <- approx(sec0, old.dat[,xname], xout=sec1, ties=mean)$y
    new.dat[,yname] <- approx(sec0, old.dat[,yname], xout=sec1, ties=mean)$y
    
    new.dat <- new.dat[!is.na(new.dat[,xname]),]
    new.dat <- new.dat[!is.na(new.dat[,yname]),]
    
    dat <- merge(old.dat, new.dat, all=TRUE)
    
    cols <- NULL
    for(i in 1:ncol(dat)) 
        cols[i] <- tbl.info[tbl.info[,1] %in% names(dat)[i], 2]
    
    srvy.dat("cols", cols)
    srvy.dat("data.raw", dat)
}

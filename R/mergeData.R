"mergeData" <- function(file=NULL) {
    
    old.dat  <- srvy.dat("data.raw")
    old.cols <- srvy.dat("cols")
    old.vars <- srvy.dat("vars")
    
    if(is.na(old.vars[1])) stop(call.=FALSE, "No temporal data is available to merge with.")
    old.vars <- make.names(old.vars)
    
    id.t <- old.vars[1]
    id.x <- old.vars[2]
    id.y <- old.vars[3]
    
    f <- getFile(cmd="Open", exts="txt", file=file)
    if(is.null(f)) return()
    
    con <- if("connection" %in% class(file)) file 
           else file(f$path, "r", encoding=srvy.dat("encoding"))
    on.exit(close(con))
    
    new <- readFile(con)
    if(is.na(new$vars[1])) stop(call.=FALSE, "No temporal data is available within the new file.")
    new$vars <- make.names(new$vars)
    
    tbl.info <- cbind(c(names(old.dat), names(new$dat)), c(old.cols, new$cols))
    
    idx <- match(new$vars[1], names(new$dat))
    colnames(new$dat) <- replace(names(new$dat), idx, id.t)
    
    dt0 <- strptime(old.dat[,id.t], "%Y-%m-%d %H:%M:%OS")
    dt1 <- strptime(new$dat[,id.t], "%Y-%m-%d %H:%M:%OS")
    
    sec0 <- as.numeric(difftime(dt0, min(dt0, na.rm=TRUE), units="sec"))
    sec1 <- as.numeric(difftime(dt1, min(dt0, na.rm=TRUE), units="sec"))
    
    new$dat[,id.x] <- approx(sec0, old.dat[,id.x], xout=sec1, ties=mean)$y
    new$dat[,id.y] <- approx(sec0, old.dat[,id.y], xout=sec1, ties=mean)$y
    
    new$dat <- new$dat[!is.na(new$dat[,id.x]),]
    new$dat <- new$dat[!is.na(new$dat[,id.y]),]
    
    dat <- merge(old.dat, new$dat, all=TRUE)
    
    cols <- NULL
    for(i in 1:ncol(dat)) 
        cols[i] <- tbl.info[tbl.info[,1] %in% names(dat)[i], 2]
    
    srvy.dat("data.raw", dat)
    srvy.dat("cols", cols)
}

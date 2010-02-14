"writeFile" <- function() {
    
# additional functions (subroutines)
    
  # file header info
    
    headerInfo <- function(typs) {
        idxs <- as.vector(unlist(vars[typs]))
        
        cols <- srvy.dat("cols")
        cols.name <- unlist(lapply(1:length(cols), function(i) cols[[i]]$name))[idxs]
        cols.unit <- unlist(lapply(1:length(cols), function(i) cols[[i]]$unit))[idxs]
        
        if(!is.null(vars$t)) cols.unit[1] <- date.fmt
        
        list(nams=cols.name, unts=cols.unit)
    }
    
  # file connection
    
    fileCon <- function(path) {
        if("connection" %in% class(file)) 
            con <- file
        else 
            con <- file(path, "w", encoding=srvy.dat("encoding"))
    }
    
    
# main program
    
  # simplify data
    
    data.pts <- srvy.dat("data.pts")
    data.grd <- srvy.dat("data.grd")
    vars     <- srvy.dat("vars")
    date.fmt <- srvy.dat("date.fmt")
    sep      <- srvy.dat("sep")
    
  # determine file name and save
    
    ext <- "dat"
    if(!is.null(data.grd)) 
        ext <- append(ext, "grd")
    if(!is.null(data.pts)) 
        ext <- append(ext, "shp")
    
    f <- getFile(cmd="Save As", exts=ext, file=NULL, caption="Save Survey Data As", defaultextension="dat")
    if(is.null(f)) return()
    
    if(f$ext == "dat") {
        con <- fileCon(f$path)
        
        if(!is.null(data.pts$t)) {
            datetime <- strptime(data.pts$t, "%Y-%m-%d %H:%M:%OS")
            data.pts$t <- format(datetime, format=date.fmt)
        }
        
        data.pts$x <- format(data.pts$x, nsmall=4)
        data.pts$y <- format(data.pts$y, nsmall=4)
        data.pts$z <- format(data.pts$z, nsmall=4)
        
        header <- headerInfo(c("t", "x", "y", "z"))
        
        dat <- rbind(header$nams, header$unts, data.pts)
        
        write.table(dat, file=con, append=FALSE, quote=FALSE, 
            row.names=FALSE, col.names=FALSE, sep=sep)
    }
    
    if(f$ext == "grd") {
        con <- fileCon(f$path)
        
        x <- rep(data.grd$x, ncol(data.grd$z))
        y <- sort(rep(data.grd$y, nrow(data.grd$z)))
        z <- as.vector(data.grd$z, mode="numeric")
        
        dat <- cbind(x, y, z)
        dat <- dat[!is.na(dat[,3]),]
        
        header <- headerInfo(c("x", "y", "z"))
        
        dat <- rbind(header$nams, header$unts, dat)
        
        write.table(dat, file=con, quote=FALSE, row.names=FALSE, col.names=FALSE, sep=sep)
    }
    
    if(f$ext == "shp") {
        coordinates(data.pts) <- c("x", "y")
        
        hld <- strsplit(f$name, ".", fixed=TRUE)[[1]]
        fname <- paste(hld[-length(hld)], collapse=".")
        
        writeOGR(data.pts, f$dir, fname, driver="ESRI Shapefile", verbose=TRUE)
    }
    
    if(exists("con")) close(con)
}

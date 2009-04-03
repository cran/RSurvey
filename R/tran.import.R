"tran.import" <- function(type, id, file=NULL) {
    
  # determine input file
    
    f <- getFile(cmd="Open", exts="txt", file=file)
    if(is.null(f)) return()
    
    con <- if("connection" %in% class(file)) file 
           else file(f$path, "r", encoding=srvy.dat("encoding"))
    
  # import transect
    
    if(type == "transect") {
        dat <- dget(con)
        if(is.list(dat)) {
            if(!is.null(dat$id)) 
                id <- dat$id
            tran.dat(id, value=dat)
        }
    }
    
  # import profile
    
    if(type == "profile") {
        d <- readFile(con, fields=TRUE, units=FALSE)
        names(d$dat) <- c("h", "z")
        tran.dat(id, "prof", d$dat)
    }
    
  # import raster data
    
    if(type == "raster") {
        
      # read file
        
        d <- readFile(con)
        
      # establish placeholder for plot limits
        
        na <- rep(NA, length(d$cols) + 2)
        on <- rep( 1, length(d$cols) + 2)
        val <- cbind(z.min=na, z.min.auto=on, z.max=na, z.max.auto=on, z.lev=na, z.lev.auto=on)
        rowNames <- append(colnames(d$dat), c("velocity.principle.dir", "velocity.long.tran"))
        limits <- data.frame(val, row.names=rowNames)
        
      # save data in tran.dat
        
        tran.dat(id, "type", d$cols)
        tran.dat(id, "hv.fields", d$vars[2:3])
        tran.dat(id, "data.ras", d$dat)
        tran.dat(id, "data.file", f$name)
        tran.dat(id, "limits", limits)
    }
    
    close(con)
}

"tran.import" <- function(type, id, file=NULL) {
    
  # determine input file
    
    f <- getFile(cmd="open", exts="txt", file=file)
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
        dat <- read.table(file=con, header=TRUE, row.names=NULL)
        names(dat) <- c("h", "z")
        tran.dat(id, "prof", dat)
    }
    
  # import raster data
    
    if(type == "raster") {
        
      # read data into a data frame
        
        col <- as.character(as.vector(t(read.table(file=con, row.names=NULL, 
               sep=srvy.dat("sep"), nrows=1))))
        unt <- as.character(as.vector(t(read.table(file=con, row.names=NULL, 
               sep=srvy.dat("sep"), nrows=1))))
        dat <- read.table(file=con, row.names=NULL, sep=srvy.dat("sep"))
        
      # establish column names for data frame
        
        tmp <- rep("", length(unt))
        tmp[!is.na(unt)] <- paste(" (", unt[!is.na(unt)], ")", sep="")
        col <- paste(col, tmp, sep="")
        
        colnames(dat) <- make.names(col, unique=TRUE)
        
      # check for valid numeric data
        
        num.logic <- NULL
        for(j in 1:ncol(dat)) 
            num.logic <- append(num.logic, is.numeric(dat[,j]))
        col <- col[ num.logic]
        unt <- unt[ num.logic]
        dat <- dat[,num.logic]
        
      # establish placeholder for plot limits
        
        na <- rep(NA, length(col) + 2)
        on <- rep( 1, length(col) + 2)
        val <- cbind(z.min=na, z.min.auto=on, z.max=na, z.max.auto=on, z.lev=na, z.lev.auto=on)
        rowNames <- append(colnames(dat), c("velocity.principle.dir", "velocity.long.tran"))
        limits <- data.frame(val, row.names=rowNames)
        
      # save data in tran.dat
        
        tran.dat(id, "type", col)
        tran.dat(id, "hv.fields", col[1:2])
        tran.dat(id, "data.ras", dat)
        tran.dat(id, "data.file", f$name)
        tran.dat(id, "limits", limits)
    }
    
    close(con)
}

"srvy.export" <- function(file=NULL) {
    
  # simplify data
    
    data.mod <- srvy.dat("data.mod")
    data.tin <- srvy.dat("data.tin")
    
  # determine file name and save
    
    ext <- "txt"
    if(!is.null(data.tin)) 
        ext <- append(ext, "tin")
    
    f <- getFile(cmd="Save As", exts=ext, file=file)
    if(is.null(f)) return()
    
    con <- if("connection" %in% class(file)) file 
           else file(f$path, "w", encoding=srvy.dat("encoding"))
    
    if(f$ext == "txt") {
        if(!is.null(data.mod$datetime)) {
            datetime <- strptime(data.mod$datetime, "%Y-%m-%d %H:%M:%OS")
            data.mod$datetime <- format(datetime, format=srvy.dat("date.fmt"))
        }
        data.mod$x <- format(data.mod$x, nsmall=4)
        data.mod$y <- format(data.mod$y, nsmall=4)
        data.mod$z <- format(data.mod$z, nsmall=4)
        
        vars <- na.omit(srvy.dat("vars"))
        vars <- srvy.dat("cols")[srvy.dat("cols") %in% vars]
        
        cols <- unts <- NULL
        hld <- gregexpr("[(]", vars)
        for(i in 1:length(hld)) {
            pos <- hld[[i]][length(hld[[i]])]
            cols[i] <- ifelse(pos < 0, vars[i], substr(vars[i], 1, pos - 2))
            unts[i] <- ifelse(pos < 0, '', substr(vars[i], pos + 1, nchar(vars[i]) - 1))
            
            if(!is.null(data.mod$datetime) && i == 1) 
                unts[i] <- srvy.dat("date.fmt")
        }
        write.table(rbind(cols, unts), file=con, append=FALSE, quote=FALSE, 
            row.names=FALSE, col.names=FALSE, sep=srvy.dat("sep"))
        write.table(data.mod, file=con, quote=FALSE, 
            row.names=FALSE, col.names=FALSE, sep=srvy.dat("sep"))
    }
    
    if(f$ext == "tin") {
        x <- rep(data.tin$x, ncol(data.tin$z))
        y <- sort(rep(data.tin$y, nrow(data.tin$z)))
        z <- as.vector(data.tin$z, mode="numeric")
        dat <- cbind(x, y, z)
        dat <- dat[!is.na(dat[,3]),]
        write.table(dat, file=con, quote=FALSE, row.names=FALSE, 
            sep=srvy.dat("sep"), col.names=c("x", "y", "z"))
    }
    
    close(con)
}

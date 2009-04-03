"tran.export" <- function(type, id, file=NULL) {
    
  # determine export file
    
    f <- getFile(cmd="Save As", exts="txt", file=file)
    if(is.null(f)) return()
    
    con <- if("connection" %in% class(file)) file 
           else file(f$path, "w", encoding=srvy.dat("encoding"))
    
  # export transect
    
    if(type == "transect") 
        dput(tran.dat(id), file=con, 
            control=c("keepNA", "keepInteger", "showAttributes"))
    
  # export profile
    
    if(type == "profile") {
        dat <- tran.dat(id, "prof")
        
        isTin <- !is.null(srvy.dat("data.tin"))
        isPro <- !is.null(dat)
        
        if(!isTin & !isPro) return()
        
        if(!isPro & isTin) 
            dat <- tran.profile(tran.dat(id, "vertices"))
        
        write.table(dat, file=con, sep=srvy.dat("sep"), 
            row.names=FALSE, col.names=TRUE)
    }
    
  # export raster data
    
    if(type == "raster") {
        dat <- tran.dat(id, "data.ras")
        typ <- tran.dat(id, "type")
        
        header.1 <- substr(typ, 1, regexpr("[ ][(]", typ) - 1)
        header.2 <- substr(typ, regexpr("[(]", typ) + 1, regexpr("[)]", typ) - 1)
        write(c(header.1, header.2), file=con, ncolumns=length(header.1), 
            sep=srvy.dat("sep"))
        
        write.table(dat, file=con, quote=FALSE, row.names=FALSE, 
            col.names=FALSE, sep=srvy.dat("sep"))
    }
    
    close(con)
}

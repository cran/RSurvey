"srvy.import" <- function(file=NULL) {
    
    f <- getFile(cmd="open", exts="txt", file=file)
    if(is.null(f)) return()
    
    con <- if("connection" %in% class(file)) file 
           else file(f$path, "r", encoding=srvy.dat("encoding"))
    
    col <- as.character(as.vector(t(read.table(file=con, row.names=NULL, 
           sep=srvy.dat("sep"), nrows=1))))
    unt <- as.character(as.vector(t(read.table(file=con, row.names=NULL, 
           sep=srvy.dat("sep"), nrows=1))))
    dat <- read.table(file=con, row.names=NULL, sep=srvy.dat("sep"))
    
    close(con)
    
    tmp <- rep("", length(unt))
    tmp[!is.na(unt)] <- paste(" (", unt[!is.na(unt)], ")", sep="")
    col <- paste(col, tmp, sep="")
    
    colnames(dat) <- make.names(col)
    
    vars <- rep(NA, 4)
    for(i in 1:ncol(dat)) {
        chk <- strptime(dat[,i], unt[i])
        if(any(!is.na(chk))) {
            dat[,i] <- format(chk, format="%Y-%m-%d %H:%M:%OS")
            vars[1] <- col[i]
            break
        }
    }
    
    j <- 1
    for(i in 1:ncol(dat)) {
        if(is.numeric(dat[,i])) {
            j <- j + 1
            vars[j] <- col[i]
            if(j == 4) break
        }
    }
    
    if(is.na(vars[4])) stop(call.=FALSE, "Error associated with input file.")
    
    srvy.dat("data.file", paste(f$name, collapse="; "))
    srvy.dat("cols", col)
    srvy.dat("vars", vars)
    
    srvy.dat("data.raw", dat)
}


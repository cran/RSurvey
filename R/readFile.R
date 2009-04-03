"readFile" <- function(file, fields=TRUE, units=TRUE) {
    
  # read file
    
    dat <- read.table(file, header=FALSE, sep=srvy.dat("sep"), na.strings=c("", "NA"),
           row.names=NULL, colClasses="character", fill=TRUE, strip.white=TRUE,
           blank.lines.skip=TRUE, comment.char="#", flush=TRUE, 
           encoding=srvy.dat("encoding"))
    
    n <- ncol(dat)
    
  # header fields
    
    fld <- NULL
    fld.str <- rep("", n)
    if(fields) {
        fld <- as.character(dat[1,])
        dat <- dat[-1,]
        fld.str[!is.na(fld)] <- fld[!is.na(fld)]
    }
    
  # header units
    
    unt <- NULL
    unt.str <- rep("", n)
    if(units) {
        unt <- as.character(dat[1,])
        dat <- dat[-1,]
        unt.str[!is.na(unt)] <- paste(" (", unt[!is.na(unt)], ")", sep="")
    }
    
  # column names
    
    cols <- paste(fld.str, unt.str, sep="")
    colnames(dat) <- make.names(cols, unique=TRUE)
    
  # column classes
    
    vars <- NA
    for(j in 1:n) {
        
        d <- dat[,j]
        
      # date/time
        if(!is.null(unt)) {
            chk <- strptime(d, unt[j])
            yes <- !is.na(chk)
            if(any(yes)) {
                dat[ yes, j] <- format(chk[yes], format="%Y-%m-%d %H:%M:%OS")
                dat[!yes, j] <- NA
                if(is.na(vars[1])) 
                    vars[1] <- cols[j]
                next
            }
        }
        
      # numeric
        chk <- as.numeric(d)
        if(any(!is.na(chk))) {
            dat[,j] <- chk
            vars <- append(vars, cols[j])
            next
        }
        
    }
    
    list(cols=cols, vars=vars[1:4], dat=dat)
}

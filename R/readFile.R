"readFile" <- function(file, fields=TRUE, units=TRUE, sep="\t", encoding=getOption("encoding")) {
    
  # read file
    
    dat <- read.table(file, header=FALSE, sep=sep, na.strings=c("", "NA"),
           row.names=NULL, colClasses="character", fill=TRUE, strip.white=TRUE,
           blank.lines.skip=TRUE, comment.char="#", flush=TRUE, 
           encoding=encoding)
    
    n <- ncol(dat)
    
  # header fields
    
    if(fields) {
        fld <- as.character(dat[1,])
        dat <- dat[-1,]
    }
    else 
        fld <- rep("", n)
    
  # header units
    
    if(units) {
        unt <- as.character(dat[1,])
        dat <- dat[-1,]
    }
    else 
        unt <- rep(NA, n)
    
  # reset row names
    
    row.names(dat) <- 1:nrow(dat)
    
  # initialize lists
    
    cols <- list()
    vars <- list()
    
  # column classes
    
    for(i in 1:n) {
        
        cols[[i]] <- list()
        cols[[i]]$name <- fld[i]
        cols[[i]]$unit <- unt[i]
        
        d <- na.omit(dat[,i])
        
      # date/time
        
        if(!is.na(unt[i])) {
            val <- strptime(d, unt[i])
            if(all(!is.na(val))) {
                cols[[i]]$class <- "POSIXt"
                dat[!is.na(dat[,i]), i] <- format(val, format="%Y-%m-%d %H:%M:%OS")
                if(is.null(vars$t)) 
                    vars$t <- i
                next
            }
        }
        
      # numeric
        
        if(all(!is.na(as.numeric(d)))) {
            cols[[i]]$class <- "numeric"
            dat[,i] <- as.numeric(dat[,i])
            if(is.null(vars$x)) vars$x <- i else
            if(is.null(vars$y)) vars$y <- i
            next
        }
        
      # character
        
        cols[[i]]$class <- "character"
        dat[,i] <- as.character(dat[,i])
        warning(paste("Column", i, "read as a character vector.\n"))
    }
    
    if(length(c(vars$x, vars$y)) < 2) 
        stop("Insufficient spatial data provided")
    
    list(data=dat, cols=cols, vars=vars)
}

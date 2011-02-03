ReadData <- function(con, headers=c(FALSE, FALSE, FALSE), sep="\t", 
                     quote="\"'", nrows=-1, na.strings=c("", "NA"), skip=0, 
                     comment.char="#", encoding=getOption("encoding"), 
                     parent=NULL) {
  # Reads table formatted data from a connection and creates a 
  # data frame from it.
  
  # Check for previous data
  
  if (!is.null(Data("cols")) & !is.null(parent)) {
    msg <- "This action will delete existing data?"
    ans <- tkmessageBox(icon="question", message=msg, title="Warning", 
                        type="okcancel", parent=parent)
    ans <- as.character(ans)
    if (ans == "ok") {
      Data(clear.data=TRUE)
    } else {
      return()
    }
  }
  
  # Track computational time
  
  elapsed.time <- system.time({
    tclServiceMode(FALSE)
    
    # Connection (included for backward compatibility)
    
    if (!inherits(con, "connection")) {
      con <- file(description=con, open="r", encoding=encoding)
      on.exit(close(con))
    }
    
    # Load data
    
    d <- try(read.table(con, header=FALSE, sep=sep, quote=quote, 
                        row.names=NULL, na.strings=na.strings, 
                        colClasses="character", nrows=nrows, 
                        skip=skip, check.names=TRUE, fill=TRUE, 
                        strip.white=TRUE, blank.lines.skip=TRUE, 
                        comment.char=comment.char, 
                        allowEscapes=TRUE, flush=TRUE, 
                        fileEncoding="", encoding=encoding), silent=TRUE)
    
    tclServiceMode(TRUE)
    
    if (inherits(d, "try-error") && !is.null(parent)) {
      msg <- "There was a problem during data import."
      tkmessageBox(icon="error", message=msg, detail=d, title="Error", 
                   type="ok", parent=parent)
      return()
    }
    
    # Remove columns containing all NA values
    
    d <- d[, sapply(seq(along=d), function(i) !all(is.na(d[, i])))]
    
    # Determine the number of columns
    
    n <- ncol(d)
    
    # Address file header
    
    if (headers[1]) {
      nams <- as.character(d[1, ])
      nams[is.na(nams)] <- "Unknown"
      d <- d[-1, ]
    } else {
        nams <- rep("Unknown", n)
    }
    
    if (headers[2]) {
      unts <- as.character(d[1, ])
      d <- d[-1, ]
    } else {
      unts <- rep(NA, n)
    }
    
    if (headers[3]) {
      digs <- suppressWarnings(as.integer(d[1, ]))
      digs[is.na(digs) | (digs < 0 | digs > 20)] <- NA
      d <- d[-1, ]
    } else {
      digs <- rep(NA, n)
    }
    
    # Reset row names
    
    rownames(d) <- 1:nrow(d)
    
    # Initialize variables
    
    cols <- list()
    vars <- list()
    ids <- NULL
    
    # Establish column types
    
    for (idx in 1:n) {
      val <- d[, idx]
      unt <- if (is.na(unts[idx])) NULL else unts[idx]
      dig <- if (is.na(digs[idx])) NULL else digs[idx]
      
      is.date <- FALSE
      if (!is.null(unt) && !all(is.na(val))) {
        date.time <- as.POSIXct(val, format=unt)
        is.date <- all(!is.na(date.time[!is.na(val)]))
      }
      
      # Convert value to assumed format
      
      if (is.date) {
        val <- date.time
      } else {
        val <- type.convert(d[, idx], as.is=FALSE)
      }
      
      # Class POSIXct
      
      if (inherits(val, "POSIXct")) {
        if (is.null(vars$t)) 
          vars$t <- idx 
      
      # Class integer or numeric
      
      } else if (inherits(val, c("integer", "numeric"))) {
        val[!is.finite(val)] <- NA
        
        if (is.null(vars$x)) {
          vars$x <- idx
        } else if (is.null(vars$y)) {
          vars$y <- idx
        } else if (is.null(vars$z)) {
          vars$z <- idx 
        }
      }
      
      # Additional attributes
      
      nam <- nams[idx]
      
      id <- paste(c(nam, unt), collapse=", ")
      i <- 1
      
      hld <- id
      while (id %in% ids) {
        id <- paste(hld, " (", i, ")", sep="")
        i <- i + 1
      }
      ids <- c(ids, id)
      
      cols[[idx]] <- list()
      cols[[idx]]$id <- id
      cols[[idx]]$name <- nam
      cols[[idx]]$unit <- unt
      cols[[idx]]$digits <- dig
      cols[[idx]]$class <- class(val)[1]
      cols[[idx]]$index <- idx
      cols[[idx]]$summary <- SummarizeData(val, dig, unt)
      cols[[idx]]$fun <- paste("DATA[[\"", id, "\"]]", sep="")
      
      d[, idx] <- val
    }
    
    # Error checks
    
    if (length(c(vars$x, vars$y)) < 2) {
      msg <- "Insufficient number of numeric fields, try revising."
      if (is.null(parent)) {
        stop(msg, call.=FALSE)
      } else {
        tkmessageBox(icon="error", message=msg, title="Error", type="ok", 
                     parent=parent)
        return()
      }
    }
    
    # Store data
    
    Data("data.raw", d)
    Data("cols", cols)
    Data("vars", vars)
  })
  
  msg <- paste("\nTime required to import data:", 
               format(elapsed.time['elapsed']), "seconds\n", "\n")
  msg
}

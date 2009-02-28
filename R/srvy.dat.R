"srvy.dat" <- local({
    
    dat <- list()
    
    default <- list("grid.res" = 0.5, 
                    "depth"    = 0, 
                    "n.levels" = 20, 
                    "win.width"= 7, 
                    "date.fmt" = "%Y-%m-%d %H:%M:%OS",
                    "font.gui"="arial 8",
                    "default.dir"=getwd(),
                    "sep"="\t",
                    "encoding"=getOption("encoding")
                )
    
    function(option, value, clearAll=FALSE) {
        
      # to clear all values
        if(clearAll) {
            dat <<- list()
            return(dat)
        }
        
        if(missing(option)) return(dat)
        if(missing(value)) {
            val <- dat[[option]]
            if(is.null(val) & option %in% names(default)) 
                val <- default[[option]]
            return(val)
        }
        else 
            dat[[option]] <<- value
    }
})


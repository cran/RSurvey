"srvy.dat" <- local({
    
    dat <- list()
    
    default <- list("depth" = 0, 
                    "nlevels" = 20, 
                    "width" = 7, 
                    "date.fmt" = "%Y-%m-%d %H:%M:%OS",
                    "font.gui" = "arial 8",
                    "default.dir" = getwd(),
                    "sep" = "\t",
                    "cex.pts" = 1,
                    "rkey" = 0,
                    "show.poly" = 0,
                    "img.contour" = 0,
                    "show.arrows" = 0,
                    "show.lines" = 0,
                    "show.points" = 0,
                    "vuni" = 0,
                    "encoding" = getOption("encoding")
                )
    
    function(option, value, clearAll=FALSE) {
        
      # clear all values
        if(clearAll) {
            dat <<- list()
            return(dat)
        }
        
        if(missing(option)) 
            return(dat)
        
        chk <- function(opt) if(is.na(suppressWarnings(as.integer(opt)))) opt else as.integer(opt)
        
        component1 <- chk(option[1])
        component2 <- chk(option[2])
        component3 <- chk(option[3])
        
        if(missing(value)) {
            if(!is.na(component3)) value <- dat[[component1]][[component2]][[component3]] else
            if(!is.na(component2)) value <- dat[[component1]][[component2]] else 
            value <- if(is.null(dat[[component1]]) & component1 %in% names(default)) default[[component1]] else dat[[component1]]
            return(value)
        }
        else {
            if(!is.na(component3)) dat[[component1]][[component2]][[component3]] <<- value else
            if(!is.na(component2)) dat[[component1]][[component2]] <<- value else
            dat[[component1]] <<- value
        }
    }
})


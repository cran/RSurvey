"tran.dat" <- local({
    dat <- list()
    default <- list("fix.zero" = "L", 
                    "h.offset" = 0, 
                    "grid.dx"  = 0.05, 
                    "grid.dy"  = 0.005)
    function(id, option, value, clearId=FALSE, clearAll=FALSE) {
        
      # clear all transects
        if(clearAll) {
            dat <<- list()
            return(dat)
        }
        
        if(missing(id)) return(dat)
        
      # clear individual transect
        if(clearId) {
            dat[[id]] <<- NULL
            return()
        }
        
      # return or replace transect data
        if(missing(option)) {
            if(missing(value)) 
                return(dat[[id]])
            else {
                if(is.list(value)) 
                    dat[[id]] <<- value
                return()
            }
        }
        
        if(missing(value)) {
            if(is.null(id)) {
                if(option %in% names(default)) 
                    return(default[[option]])
                else 
                    return()
            }
            else 
                return(dat[[id]][[option]])
        }
        else {
            if(is.null(dat[[id]])) 
                dat[[id]] <<- list()
            dat[[id]][[option]] <<- value
        }
    }
})

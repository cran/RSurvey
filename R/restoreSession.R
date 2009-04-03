"restoreSession" <- function(path, save.objs, fun.call) {
    
# this function restores local objects within the current R session.
    
    if(missing(path)) {
        require(tcltk)
        path <- tclvalue(tkchooseDirectory(initialdir=srvy.dat("default.dir"), 
                title="Choose R Source Directory..."))
        if(path == "") return()
        srvy.dat("default.dir", path)
    }
    
    if(missing(save.objs)) 
        save.objs <- NULL
    
    hld <- ls(all=FALSE, envir=as.environment(1))
    cur.objs <- hld[!hld %in% save.objs]
    
    graphics.off()
    
    rm(list=cur.objs, envir=as.environment(1))
    
    r.files <- list.files(path, pattern="[.][R]$", 
               full.names=TRUE, recursive=TRUE, ignore.case=TRUE)
    
    for(i in r.files) {
        tmp <- tail(unlist(strsplit(i, "/")), 1)
        obj <- substr(tmp, 1, nchar(tmp) - 2)
        if(!obj %in% save.objs) {
            try(source(i))
            cat(i, "\n")
        }
    }
    
    if(!missing(fun.call)) 
        eval(parse(text=fun.call))()
}

"restoreImage" <- function(fun.call=NULL, save.objs=NULL) {
    
# refresh image (restoreImage) will update .RData from *.R source code
# located in .../working_directory/R.
    
    pkg <- rev(unlist(strsplit(getwd(), "/")))[1]
    
    if(pkg %in% .packages()) 
        stop(call.=FALSE, "This function is for package developers only.")
    
    if(is.null(fun.call)) {
        ans <- readline(prompt="This action will delete existing data. Would you like to continue (yes/no)? ")
        if(substr(tolower(ans), 1, 1) != "y") return()
    }
    
    graphics.off()
    
    cur.objs <- ls(all=FALSE, envir=as.environment(1))
    if(!is.null(save.objs)) 
        cur.objs <- cur.objs[!(cur.objs %in% save.objs)]
    
    rm(list=cur.objs, envir=as.environment(1))
    
    dir.path <- paste(getwd(), "/R", sep="")
    
    r.files <- list.files(path=dir.path, pattern="[.][R]$", full.names=TRUE)
    
    for(i in r.files) {
        tmp <- unlist(strsplit(i, "/"))
        tmp <- tmp[length(tmp)]
        obj <- substr(tmp, 1, nchar(tmp) - 2)
        if(!(obj %in% save.objs)) 
            try(source(i))
    }
    
    if(!is.null(fun.call)) 
        eval(parse(text=fun.call))()
}

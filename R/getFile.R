"getFile" <- function(parent, cmd="Open", file=NULL, exts=NULL, initialdir=NULL, initialfile=NULL, 
             defaultextension=NULL, caption=cmd, multi=FALSE) {

# additional functions (subroutines)
    
  # determine file extension
    
    fileExt <- function(x) {
        brk <- unlist(strsplit(basename(x), "\\."))
        ext <- if(length(brk) == 1) "" else tail(brk, 1)
        tolower(ext)
    }
    
# main program
    
    allFilters <- list(ply  = "Polygon files", 
                       dat  = "Table data files", 
                       grd  = "Interpolated grid files", 
                       rda  = "RSurvey project files", 
                       r    = "R source files", 
                       png  = "Png files", 
                       jpg  = "Jpeg files", 
                       jpeg = "Jpeg files", 
                       ps   = "Postscript files", 
                       eps  = "Encapsulated postscript files", 
                       pdf  = "PDF files", 
                       bmp  = "Windows bitmap files", 
                       shp  = "ESRI shapefiles", 
                       tif  = "TIFF files", 
                       tiff = "TIFF files", 
                       txt  = "Text files"
                   )
    
    if(!is.null(file)) {
        if("connection" %in% class(file)) 
            file <- summary.connection(file)$description
        dir <- dirname(file)
        nam <- basename(file)
        ext <- fileExt(file)
        typ <- allFilters[[ext]]
        f <- list(path=file, dir=dir, name=nam, ext=ext, type=typ)
        srvy.dat("default.dir", dir)
        return(f)
    }
    
    if(is.null(initialdir)) 
        initialdir <- srvy.dat("default.dir")
    
    filters <- matrix(nrow=0, ncol=2)
    
    if(!is.null(exts)) {
        for(ext in tolower(exts)) {
            typ <- allFilters[[ext]]
            
            if(is.null(typ)) typ <- toupper(ext)
            
            filters <- rbind(filters, c(typ, paste(".", ext, sep="")))
        }
    }
    
    
    
    
    filters   <- rbind(filters, c("All files", "*"))
    filters[] <- paste("{", filters, "}", sep="")
    filters   <- apply(filters, 1, paste, collapse=" ")
    filters   <- paste(paste("{", filters, "}", sep=""), collapse=" ")
    
    if(tolower(substr(cmd, 1, 4)) == "open") 
        args <- list("tk_getOpenFile", title=caption, multiple=multi)
    else 
        args <- list("tk_getSaveFile", title=caption)
    
    if(!missing(parent)) args[["parent"]] <- parent
    
    if(!is.null(defaultextension)) 
        args <- c(args, defaultextension=defaultextension)
    if(!is.null(initialdir)) 
        args <- c(args, initialdir=initialdir)
    if(!is.null(initialfile)) 
        args <- c(args, initialfile=initialfile)
    
    args <- c(args, filetypes=filters)
    
    res <- tclvalue(do.call(tcl, args))
    
    if(!nzchar(res)) return()
    
    if(multi) {
        ans <- character()
        pat <- "([^{])*\\{([^}]*)\\}(.*)"
        while (grepl(pat, res)) {
            ans <- c(ans, sub(pat, "\\2", res))
            res <- sub(pat, "\\1\\3", res)
        }
        ans <- c(ans, strsplit(res, " ", fixed = TRUE)[[1]])
        ans <- ans[nzchar(ans)]
    }
    else 
        ans <- res
    
    n <- length(ans)
    if(n > 1) f <- list()
    for(i in 1:n) {
        
        pth <- ans[i]
        dir <- dirname(pth)
        nam <- basename(pth)
        ext <- fileExt(pth)
        typ <- allFilters[[ext]]
        
        val <- list(path=pth, dir=dir, name=nam, ext=ext, type=typ)
        if(n > 1) f[[i]] <- val else f <- val
    }
    
    if(is.null(f)) 
        warning("No file was selected")
    else 
        srvy.dat("default.dir", dir)
    
    f
}

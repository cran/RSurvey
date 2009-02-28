"getFile" <- function(cmd="open", exts=NULL, directory=NULL, file=NULL) {

# additional functions (subroutines)
    
  # determine file extension
    
    fileExt <- function(x) sub(".*\\.", "", x)
    
# main program
    
    FILE <- list()
    
    if(!is.null(file)) {
        
        filename <- if("connection" %in% class(file)) summary.connection(file)$description else file
        
        hld <- unlist(strsplit(filename, "/"))
        FILE$path <- filename
        FILE$dir  <- paste(hld[1:(length(hld) - 1)], collapse="/")
        FILE$name <- hld[length(hld)]
        FILE$ext  <- fileExt(filename)
        
        srvy.dat("default.dir", FILE$dir)
        return(FILE)
    }
    
    types <- '{{All files} *}'
    ini.file <- def.ext <- ''
    
    if(!is.null(exts)) {
        for(i in rev(exts)) {
            ext <- tolower(fileExt(i))
            type <- NULL
            
            if(ext == "txt") {type <- '{{Text Files} {.txt}}'} else
            if(ext == "dat") {type <- '{{Text Files} {.dat}}'} else
            if(ext == "csv") {type <- '{{Text Files} {.csv}}'} else
            if(ext == "rda") {type <- '{{R Project File} {.rda}}'} else
            if(ext == "tin") {type <- '{{TIN Files} {.tin}}'} else
            if(ext == "png") {type <- '{{PNG Files} {.png}}'} else
            if(ext == "jpg") {type <- '{{JPEG Files} {.jpg .jpeg}}'} else
            if(ext == "ps" ) {type <- '{{PostScript Files} {.ps}}'} else
            if(ext == "eps") {type <- '{{Encapsulated PostScript Files} {.eps}}'} else
            if(ext == "tex") {type <- '{{Latex Files} {.tex}}'} else
            if(ext == "pdf") {type <- '{{PDF Files} {.pdf}}'} else
            if(ext == "bmp") {type <- '{{Bitmap Files} {.bmp}}'}
            
            types <- c(type, types)
        }
        ini.file <- paste('*.', ext, sep="")
        def.ext <- ext
    }
    
    types <- paste(types, collapse=" ")
    
    if(is.null(directory)) 
        directory <- srvy.dat("default.dir")
    
    if(cmd == "open") 
        f <- tclvalue(tkgetOpenFile(filetypes=types, initialdir=directory))
    if(cmd == "save") { 
        f <- tclvalue(tkgetSaveFile(defaultextension=def.ext, filetypes=types,
             initialdir=directory, initialfile=ini.file))
    }
    
    if(f == "") return()
    
    hld <- unlist(strsplit(f[1], "/"))
    FILE$dir <- paste(hld[1:(length(hld) - 1)], collapse="/")
    srvy.dat("default.dir", FILE$dir)
    
    for(i in 1:length(f)) {
        FILE$path[i] <- f[i]
        
        hld <- unlist(strsplit(f[i], "/"))
        FILE$name[i] <- hld[length(hld)]
        
        hld <- unlist(strsplit(f[i], "\\."))
        FILE$ext[i] <- if(length(hld) == 1) "" else hld[length(hld)]
    }
    
    FILE
}


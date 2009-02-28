"polyAutocrop" <- function(tt1=NULL) {
    
# additional functions (subroutines)
    
    poly.mesh <- function() {
        maxLength <- as.numeric(tclvalue(maxLength.var))
        maxItr    <- as.integer(tclvalue(maxItr.var))
        
        if(!is.na(maxLength) & !is.na(maxItr)) {
            ply <<- autocrop(mesh, maxLength, maxItr)
            pts <- get.pts(ply)[[1]]
            x <- c(pts$x, pts$x[1])
            y <- c(pts$y, pts$y[1])
            lines(x, y, col=col.pal[idx])
            idx <<- idx + 1
            srvy.dat("data.mod", NULL)
            srvy.dat("data.tin", NULL)
        }
    }
    
# main program
    
  # GUI font
    
    fnt <- srvy.dat("font.gui")
    
  # initialize the polygon
    
    ply <- NULL
    
  # line colors
    
    idx <- 1
    col.pal <- c("blue", "red", "green4", "brown", "orange2", "purple3", 
                 "salmon2", "#008080", "#808080", "#808000")
    
  # construct mesh
    
    data.raw <- srvy.dat("data.raw")
    mesh <- tri.mesh(data.raw[,1], data.raw[,2], duplicate="remove")
    
  # convex hull and maximum outer arc length
    
    hull <- convex.hull(mesh)
    x1 <- hull$x
    x2 <- c(hull$x[-1], hull$x[1])
    y1 <- hull$y
    y2 <- c(hull$y[-1], hull$y[1])
    len <- max(sqrt((x2 - x1)^2 + (y2 - y1)^2))
    
  # limits
    
    x.min <- min(x1)
    x.max <- max(x1)
    y.min <- min(y1)
    y.max <- max(y1)
    x.ext <- diff(range(x1)) * 0.04
    x.lim <- c(x.min - x.ext, x.max + x.ext)
    y.lim <- c(y.min - x.ext, y.max + x.ext)
    
  # window setup
    
    win.width <- srvy.dat("win.width")
    asp.ratio <- srvy.dat("yx.ratio")
    
    if(is.null(asp.ratio)) {
        win.height <- win.width
        asp.ratio <- NA
    }
    else {
        mar <- c(5.1, 4.1 / asp.ratio, 4.1, 2.1 / asp.ratio)
        mar.width <- (mar[2] + mar[4]) * srvy.dat("csi")
        mar.height <- mar.width * ((mar[1] + mar[3]) / (mar[2] + mar[4]))
        ratio <- diff(y.lim) / diff(x.lim) * asp.ratio
        win.height <- ((win.width - mar.width) * ratio) + mar.height
    }
    x11(width=win.width, height=win.height, pointsize=10)
    
  # plot mesh
    
    tnabor <- integer(mesh$tlnew)
    nnabs  <- integer(mesh$n)
    nptr   <- integer(mesh$n)
    nptr1  <- integer(mesh$n)
    nbnos  <- integer(mesh$n)
    ans <- .Fortran("troutq", as.integer(mesh$nc), as.integer(mesh$lc), 
           as.integer(mesh$n), as.double(mesh$x), as.double(mesh$y), as.integer(mesh$tlist), 
           as.integer(mesh$tlptr), as.integer(mesh$tlend), as.integer(6), 
           nnabs = as.integer(nnabs), nptr = as.integer(nptr), nptr1 = as.integer(nptr1), 
           tnabor = as.integer(tnabor), nbnos = as.integer(nbnos), 
           na = as.integer(0), nb = as.integer(0), nt = as.integer(0), PACKAGE = "tripack")
    plot.new()
    plot.window(xlim = x.lim, ylim = y.lim, asp=asp.ratio)
    for (i in 1:mesh$n) {
        inb <- ans$tnabor[ans$nptr[i]:ans$nptr1[i]]
        for (j in inb) 
            lines(c(mesh$x[i], mesh$x[j]), c(mesh$y[i], mesh$y[j]))
    }
    
    axis(1, las=1, cex.axis=0.7)
    axis(2, las=0, cex.axis=0.7)
    minorTics(1:2)
    vars <- srvy.dat("vars")
    title(main="Autocrop Polygon Construction", xlab=vars[2], ylab=vars[3], cex.main=0.9)
    box()
    
  # assign the variables linked to Tk widgets
    
    maxLength.var <- tclVar(format(len, nsmall=3))
    maxItr.var    <- tclVar(100000)
    
    tt2.done.var <- tclVar(0)
    
  # open gui
    
    tt2 <- tktoplevel()
    if(!is.null(tt1)) {
        tkwm.transient(tt2, tt1)
        tmp <- unlist(strsplit(as.character(tkwm.geometry(tt1)), "\\+"))
        tkwm.geometry(tt2, paste("+", as.integer(tmp[2]) + 25, "+", as.integer(tmp[3]) + 25, sep=""))
    }
    tktitle(tt2) <- "Autocrop"
    
    if(!is.null(srvy.dat("icon")) && file.exists(srvy.dat("icon"))) 
        tkwm.iconbitmap(tt2, srvy.dat("icon"))
    
    tkwm.resizable(tt2, 0, 0)
    
  # frame 1 contains parameters
    
    frame1 <- tkframe(tt2, relief="flat", borderwidth=2)
    
    frame1.lab.1.1 <- tklabel(frame1, font=fnt, text="Maximum arc length")
    frame1.lab.2.1 <- tklabel(frame1, font=fnt, text="Maximum num. of iterations")
    
    frame1.ent.1.2 <- tkentry(frame1, font=fnt, width=10, textvariable=maxLength.var)
    frame1.ent.2.2 <- tkentry(frame1, font=fnt, width=10, textvariable=maxItr.var)
    
    tkbind(frame1.ent.1.2, "<KeyRelease>", function() {tclvalue(maxLength.var) <- keyEvent("real", tclvalue(maxLength.var), rm.data=FALSE)})
    tkbind(frame1.ent.1.2, "<KeyRelease>", function() {tclvalue(maxItr.var) <- keyEvent("integer", tclvalue(maxItr.var), rm.data=FALSE)})
    
    tkgrid(frame1.lab.1.1, frame1.ent.1.2, padx=2, pady=2)
    tkgrid(frame1.lab.2.1, frame1.ent.2.2, padx=2, pady=2)
    
    tkgrid.configure(frame1.lab.1.1, sticky="e")
    tkgrid.configure(frame1.lab.2.1, sticky="e")
    
    tkpack(frame1, fill="both")
    
  # frame 2 contains buttons
    
    frame2 <- tkframe(tt2, relief="flat", borderwidth=2)
    
    frame2.but.2.1 <- tkbutton(frame2, font=fnt, width=10, text="BUILD",  command=function() {poly.mesh()})
    frame2.but.2.2 <- tkbutton(frame2, font=fnt, width=10, text="CANCEL", command=function() {tclvalue(tt2.done.var) <- 1})
    
    tkgrid(frame2.but.2.1, frame2.but.2.2, padx=2)
    
    tkpack(frame2, padx=2, pady=5)
    
  # gui control
    
    tkfocus(tt2)
    tkgrab(tt2)
    tkbind(tt2, "<Destroy>", function() {tclvalue(tt2.done.var) <- 1})
    tkwait.variable(tt2.done.var)
    tkgrab.release(tt2)
    tkdestroy(tt2)
    
    ply
}

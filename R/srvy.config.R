"srvy.config" <- function(tt1=NULL) {
    
# additional functions (subroutines)
    
    update.par <- function() {
        hld <- as.integer(tclvalue(n.levels.var))
        srvy.dat("n.levels", if(is.na(hld)) NULL else hld)
        hld <- as.numeric(tclvalue(win.width.var))
        srvy.dat("win.width", if(is.na(hld)) NULL else hld)
        hld <- as.numeric(tclvalue(yx.ratio.var))
        srvy.dat("yx.ratio", if(is.na(hld)) NULL else hld)
        hld <- as.numeric(tclvalue(zx.ratio.var))
        srvy.dat("zx.ratio", if(is.na(hld)) NULL else hld)
        hld <- as.character(tclvalue(date.fmt.var))
        srvy.dat("date.fmt", if(hld == "") NULL else hld)
    }
    
# main program
    
  # GUI font
    
    fnt <- srvy.dat("font.gui")
    
  # assign the variables linked to Tk widgets
    
    n.levels.var  <- tclVar()
    win.width.var <- tclVar()
    yx.ratio.var  <- tclVar()
    zx.ratio.var  <- tclVar()
    date.fmt.var  <- tclVar()
    
    if(!is.null(srvy.dat("n.levels")))  tclvalue(n.levels.var)  <- srvy.dat("n.levels")
    if(!is.null(srvy.dat("win.width"))) tclvalue(win.width.var) <- srvy.dat("win.width")
    if(!is.null(srvy.dat("yx.ratio")))  tclvalue(yx.ratio.var)  <- srvy.dat("yx.ratio")
    if(!is.null(srvy.dat("zx.ratio")))  tclvalue(zx.ratio.var)  <- srvy.dat("zx.ratio")
    if(!is.null(srvy.dat("date.fmt")))  tclvalue(date.fmt.var)  <- srvy.dat("date.fmt")
    
    tt2.done.var <- tclVar(0)
    
  # open gui
    
    tt2 <- tktoplevel()
    if(!is.null(tt1)) {
        tkwm.transient(tt2, tt1)
        tmp <- unlist(strsplit(as.character(tkwm.geometry(tt1)), "\\+"))
        tkwm.geometry(tt2, paste("+", as.integer(tmp[2]) + 25, "+", as.integer(tmp[3]) + 25, sep=""))
    }
    tktitle(tt2) <- "Configuration"
    
    tkwm.resizable(tt2, 0, 0)
    
  # frame 1 contains parameters
    
    frame1 <- tkframe(tt2, relief="flat", borderwidth=2)
    
    frame1.lab.1.1 <- tklabel(frame1, font=fnt, text="Approximate num. of contour intervals")
    frame1.lab.2.1 <- tklabel(frame1, font=fnt, text="Width of windows device (in)")
    frame1.lab.3.1 <- tklabel(frame1, font=fnt, text="Aspect ratio, y / x")
    frame1.lab.4.1 <- tklabel(frame1, font=fnt, text="Aspect ratio, z / x")
    
    frame1.ent.1.2 <- tkentry(frame1, font=fnt, width=11, textvariable=n.levels.var)
    frame1.ent.2.2 <- tkentry(frame1, font=fnt, width=11, textvariable=win.width.var)
    frame1.ent.3.2 <- tkentry(frame1, font=fnt, width=11, textvariable=yx.ratio.var)
    frame1.ent.4.2 <- tkentry(frame1, font=fnt, width=11, textvariable=zx.ratio.var)
    
    tkbind(frame1.ent.1.2, "<KeyRelease>", function() {tclvalue(n.levels.var)  <- keyEvent("integer", tclvalue(n.levels.var))})
    tkbind(frame1.ent.2.2, "<KeyRelease>", function() {tclvalue(win.width.var) <- keyEvent("real",    tclvalue(win.width.var))})
    tkbind(frame1.ent.3.2, "<KeyRelease>", function() {tclvalue(yx.ratio.var)  <- keyEvent("real",    tclvalue(yx.ratio.var))})
    tkbind(frame1.ent.4.2, "<KeyRelease>", function() {tclvalue(zx.ratio.var)  <- keyEvent("real",    tclvalue(zx.ratio.var))})
    
    tkgrid(frame1.lab.1.1, frame1.ent.1.2, padx=1, pady=1)
    tkgrid(frame1.lab.2.1, frame1.ent.2.2, padx=1, pady=1)
    tkgrid(frame1.lab.3.1, frame1.ent.3.2, padx=1, pady=1)
    tkgrid(frame1.lab.4.1, frame1.ent.4.2, padx=1, pady=1)
    
    tkgrid.configure(frame1.lab.1.1, sticky="e")
    tkgrid.configure(frame1.lab.2.1, sticky="e")
    tkgrid.configure(frame1.lab.3.1, sticky="e")
    tkgrid.configure(frame1.lab.4.1, sticky="e")
    
    tkpack(frame1, fill="both")
    
  # frame 2 contains output date/time format
    
    frame2 <- tkframe(tt2, relief="flat", borderwidth=2)
    
    frame2.lab.1.1 <- tklabel(frame2, font=fnt, text="Output date/time format")
    
    frame2.ent.1.2 <- tkentry(frame2, font=fnt, width=23, textvariable=date.fmt.var)
    
    tkbind(frame2.ent.1.2, "<KeyRelease>", function() {tclvalue(date.fmt.var) <- keyEvent("date", tclvalue(date.fmt.var))})
    
    tkgrid(frame2.lab.1.1, frame2.ent.1.2, padx=1, pady=1)
    
    tkgrid.configure(frame2.lab.1.1, sticky="e")
    
    tkpack(frame2, fill="both")
    
  # gui control
    
    tkfocus(tt2)
    tkgrab(tt2)
    tkbind(tt2, "<Destroy>", function() tclvalue(tt2.done.var) <- 1)
    tkwait.variable(tt2.done.var)
    
    update.par()
    
    tkgrab.release(tt2)
    tkdestroy(tt2)
}

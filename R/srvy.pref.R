"srvy.pref" <- function(tt1=NULL) {
    
# additional functions (subroutines)
    
    update.par <- function() {
        hld <- as.numeric(tclvalue(grad.tol.var))
        srvy.dat("grad.tol", if(is.na(hld)) NULL else hld)
        hld <- as.numeric(tclvalue(time.gap.var))
        srvy.dat("time.gap", if(is.na(hld)) NULL else hld)
        hld <- as.numeric(tclvalue(grid.res.var))
        srvy.dat("grid.res", if(is.na(hld)) NULL else hld)
        hld <- as.numeric(tclvalue(wtr.elev.var))
        srvy.dat("wtr.elev", if(is.na(hld)) NULL else hld)
        hld <- as.integer(tclvalue(depth.var))
        srvy.dat("depth", hld)
    }
    
# main program
    
  # GUI font
    
    fnt <- srvy.dat("font.gui")
    
  # assign the variables linked to Tk widgets 
    
    grad.tol.var <- tclVar()
    time.gap.var <- tclVar()
    grid.res.var <- tclVar()
    wtr.elev.var <- tclVar()
    depth.var    <- tclVar()
    
    if(!is.null(srvy.dat("grad.tol"))) tclvalue(grad.tol.var) <- srvy.dat("grad.tol")
    if(!is.null(srvy.dat("time.gap"))) tclvalue(time.gap.var) <- srvy.dat("time.gap")
    if(!is.null(srvy.dat("grid.res"))) tclvalue(grid.res.var) <- srvy.dat("grid.res")
    if(!is.null(srvy.dat("wtr.elev"))) tclvalue(wtr.elev.var) <- srvy.dat("wtr.elev")
    if(!is.null(srvy.dat("depth")))    tclvalue(depth.var)    <- srvy.dat("depth")
    
    tt2.done.var <- tclVar(0)
    
  # open gui
    
    tt2 <- tktoplevel()
    if(!is.null(tt1)) {
        tkwm.transient(tt2, tt1)
        tmp <- unlist(strsplit(as.character(tkwm.geometry(tt1)), "\\+"))
        tkwm.geometry(tt2, paste("+", as.integer(tmp[2]) + 25, "+", as.integer(tmp[3]) + 25, sep=""))
    }
    tktitle(tt2) <- "Preferences"
    
    if(!is.null(srvy.dat("icon")) && file.exists(srvy.dat("icon"))) 
        tkwm.iconbitmap(tt2, srvy.dat("icon"))
    
    tkwm.resizable(tt2, 0, 0)
    
  # frame 1 contains temporal data correction parameteres
    
    frame1 <- ttklabelframe(tt2, relief="flat", borderwidth=5, padding=3, text="Temporal data correction")
    
    frame1.lab.1.1 <- tklabel(frame1, font=fnt, text="Gradient tolerance, dz / dt [L/T]")
    frame1.lab.2.1 <- tklabel(frame1, font=fnt, text="Time gap exceedence level (sec)")
    
    frame1.ent.1.2 <- tkentry(frame1, font=fnt, width=11, textvariable=grad.tol.var)
    frame1.ent.2.2 <- tkentry(frame1, font=fnt, width=11, textvariable=time.gap.var)
    
    tkbind(frame1.ent.1.2, "<KeyRelease>", function() {tclvalue(grad.tol.var) <- keyEvent("real", tclvalue(grad.tol.var), rm.data=TRUE)})
    tkbind(frame1.ent.2.2, "<KeyRelease>", function() {tclvalue(time.gap.var) <- keyEvent("real", tclvalue(time.gap.var), rm.data=TRUE)})
    
    tkgrid(frame1.lab.1.1, frame1.ent.1.2, padx=1, pady=1)
    tkgrid(frame1.lab.2.1, frame1.ent.2.2, padx=1, pady=1)
    
    tkgrid.configure(frame1.lab.1.1, sticky="e")
    tkgrid.configure(frame1.lab.2.1, sticky="e")
    
    tkpack(frame1, fill="both", ipadx=2, ipady=2, padx=2, pady=2)


  # frame 2 contains interpolation parameteres
    
    frame2 <- ttklabelframe(tt2, relief="flat", borderwidth=5, padding=3, text="Spatial interpolation")
    
    frame2.lab.1.1 <- tklabel(frame2, font=fnt, text="Grid resoltion for interpolation [L]")
    
    frame2.ent.1.2 <- tkentry(frame2, font=fnt, width=11, textvariable=grid.res.var)
    
    tkbind(frame2.ent.1.2, "<KeyRelease>", function() {tclvalue(grid.res.var) <- keyEvent("real", tclvalue(grid.res.var), rm.data=TRUE)})
    
    tkgrid(frame2.lab.1.1, frame2.ent.1.2, padx=1, pady=1)
    
    tkpack(frame2, fill="both", ipadx=2, ipady=2, padx=2, pady=2)
    
  # frame 3 contains water parameters
    
    frame3 <- ttklabelframe(tt2, relief="flat", borderwidth=5, padding=3, text="Water")
    
    frame3.lab.2.1 <- tklabel(frame3, font=fnt, text="     Water surface elevation [L]")
    
    frame3.chk.1 <- tkcheckbutton(frame3, variable=depth.var, text="state variable is depth below water surface")
    
    frame3.ent.2.2 <- tkentry(frame3, font=fnt, width=11, textvariable=wtr.elev.var)
    
    tkbind(frame3.ent.2.2, "<KeyRelease>", function() {tclvalue(wtr.elev.var) <- keyEvent("real", tclvalue(wtr.elev.var), rm.data=TRUE)})
    
    tkgrid(frame3.chk.1, columnspan=2, padx=1, pady=1)
    tkgrid(frame3.lab.2.1, frame3.ent.2.2, padx=1, pady=1)
    
    tkpack(frame3, fill="both", ipadx=2, ipady=2, padx=2, pady=2)
    
  # gui control
    
    tkfocus(tt2)
    tkgrab(tt2)
    tkbind(tt2, "<Destroy>", function() tclvalue(tt2.done.var) <- 1)
    tkwait.variable(tt2.done.var)
    
    update.par()
    
    tkgrab.release(tt2)
    tkdestroy(tt2)
}

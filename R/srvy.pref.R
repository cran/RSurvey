"srvy.pref" <- function(parent) {
    
# additional functions (subroutines)
    
    update.par <- function() {
        hld <- as.numeric(tclvalue(grad.tol.var))
        srvy.dat("grad.tol", if(is.na(hld)) NULL else hld)
        hld <- as.numeric(tclvalue(time.gap.var))
        srvy.dat("time.gap", if(is.na(hld)) NULL else hld)
        hld <- as.numeric(tclvalue(grid.dx.var))
        srvy.dat("grid.dx", if(is.na(hld)) NULL else hld)
        hld <- as.numeric(tclvalue(grid.dy.var))
        srvy.dat("grid.dy", if(is.na(hld)) NULL else hld)
        hld <- as.numeric(tclvalue(wtr.elev.var))
        srvy.dat("wtr.elev", if(is.na(hld)) NULL else hld)
        hld <- as.integer(tclvalue(depth.var))
        srvy.dat("depth", hld)
        hld <- as.numeric(tclvalue(off.z.var))
        srvy.dat("off.z", if(is.na(hld)) NULL else hld)
        hld <- as.numeric(tclvalue(off.t.var))
        srvy.dat("off.t", if(is.na(hld)) NULL else hld)
    }
    
# main program
    
  # assign the variables linked to Tk widgets 
    
    grad.tol.var <- tclVar()
    time.gap.var <- tclVar()
    grid.dx.var  <- tclVar()
    grid.dy.var  <- tclVar()
    wtr.elev.var <- tclVar()
    depth.var    <- tclVar()
    off.z.var    <- tclVar()
    off.t.var    <- tclVar()
    
    if(!is.null(srvy.dat("grad.tol"))) tclvalue(grad.tol.var) <- srvy.dat("grad.tol")
    if(!is.null(srvy.dat("time.gap"))) tclvalue(time.gap.var) <- srvy.dat("time.gap")
    if(!is.null(srvy.dat("grid.dx")))  tclvalue(grid.dx.var)  <- srvy.dat("grid.dx")
    if(!is.null(srvy.dat("grid.dy")))  tclvalue(grid.dy.var)  <- srvy.dat("grid.dy")
    if(!is.null(srvy.dat("wtr.elev"))) tclvalue(wtr.elev.var) <- srvy.dat("wtr.elev")
    if(!is.null(srvy.dat("depth")))    tclvalue(depth.var)    <- srvy.dat("depth")
    if(!is.null(srvy.dat("off.z")))    tclvalue(off.z.var)    <- srvy.dat("off.z")
    if(!is.null(srvy.dat("off.t")))    tclvalue(off.t.var)    <- srvy.dat("off.t")
    
    tt.done.var <- tclVar(0)
    
  # open gui
    
    tt <- tktoplevel(padx=5, pady=5)
    if(!missing(parent)) {
        tkwm.transient(tt, parent)
        tmp <- unlist(strsplit(as.character(tkwm.geometry(parent)), "\\+"))
        tkwm.geometry(tt, paste("+", as.integer(tmp[2]) + 25, "+", as.integer(tmp[3]) + 25, sep=""))
    }
    tktitle(tt) <- "Preferences"
        
    tkwm.resizable(tt, 0, 0)
    
  # frame 1 contains temporal data correction parameteres
    
    frame1 <- ttklabelframe(tt, relief="flat", borderwidth=5, padding=3, text="Temporal data correction")
    
    frame1.lab.1.1 <- ttklabel(frame1, text="Gradient tolerance, dz / dt [L/T]")
    frame1.lab.2.1 <- ttklabel(frame1, text="Time gap exceedence level (sec)")
    
    frame1.ent.1.2 <- ttkentry(frame1, width=12, textvariable=grad.tol.var)
    frame1.ent.2.2 <- ttkentry(frame1, width=12, textvariable=time.gap.var)
    
    tkbind(frame1.ent.1.2, "<KeyRelease>", function() {tclvalue(grad.tol.var) <- keyEvent("real", tclvalue(grad.tol.var), rm.data=TRUE)})
    tkbind(frame1.ent.2.2, "<KeyRelease>", function() {tclvalue(time.gap.var) <- keyEvent("real", tclvalue(time.gap.var), rm.data=TRUE)})
    
    tkgrid(frame1.lab.1.1, frame1.ent.1.2, padx=1, pady=1)
    tkgrid(frame1.lab.2.1, frame1.ent.2.2, padx=1, pady=1)
    
    tkgrid.configure(frame1.lab.1.1, sticky="e")
    tkgrid.configure(frame1.lab.2.1, sticky="e")
    
    tcl("grid", "anchor", frame1, "ne")
    
    tkpack(frame1, fill="both", ipadx=2, ipady=2, padx=2, pady=2)


  # frame 2 contains interpolation parameteres
    
    frame2 <- ttklabelframe(tt, relief="flat", borderwidth=5, padding=3, text="Interpolated Surface")
    
    frame2.lab.1.1 <- ttklabel(frame2, text="x-axis grid spacing [L]")
    frame2.lab.2.1 <- ttklabel(frame2, text="y-axis grid spacing [L]")
    
    frame2.ent.1.2 <- ttkentry(frame2, width=12, textvariable=grid.dx.var)
    frame2.ent.2.2 <- ttkentry(frame2, width=12, textvariable=grid.dy.var)
    
    tkbind(frame2.ent.1.2, "<KeyRelease>", function() {tclvalue(grid.dx.var) <- keyEvent("real", tclvalue(grid.dx.var), rm.data=TRUE)})
    tkbind(frame2.ent.1.2, "<KeyRelease>", function() {tclvalue(grid.dy.var) <- keyEvent("real", tclvalue(grid.dy.var), rm.data=TRUE)})
    
    tkgrid(frame2.lab.1.1, frame2.ent.1.2, padx=1, pady=1)
    tkgrid(frame2.lab.2.1, frame2.ent.2.2, padx=1, pady=1)
    
    tkgrid.configure(frame2.lab.1.1, sticky="e")
    tkgrid.configure(frame2.lab.2.1, sticky="e")
    
    tcl("grid", "anchor", frame2, "ne")
    
    tkpack(frame2, fill="both", ipadx=2, ipady=2, padx=2, pady=2)
    
  # frame 3 contains water parameters
    
    frame3 <- ttklabelframe(tt, relief="flat", borderwidth=5, padding=3, text="Bathymetry")
    
    frame3.chk.1 <- ttkcheckbutton(frame3, variable=depth.var, text="set state variable as depth below water surface")
    
    frame3.lab.2.1 <- ttklabel(frame3, text="     Water surface elevation [L]")
    
    frame3.ent.2.2 <- ttkentry(frame3, width=12, textvariable=wtr.elev.var)
    
    tkbind(frame3.ent.2.2, "<KeyRelease>", function() {tclvalue(wtr.elev.var) <- keyEvent("real", tclvalue(wtr.elev.var), rm.data=TRUE)})
    
    tkgrid(frame3.chk.1, columnspan=2, pady=1)
    tkgrid(frame3.lab.2.1, frame3.ent.2.2, padx=1, pady=1)
    
    tkgrid.configure(frame3.lab.2.1, sticky="e")
    
    tcl("grid", "anchor", frame3, "ne")
    
    tkpack(frame3, fill="both", ipadx=2, ipady=2, padx=2, pady=2)
    
  # frame 4 contains offsets
    
    frame4 <- ttklabelframe(tt, relief="flat", borderwidth=5, padding=3, text="Offsets")
    
    frame4.lab.1.1 <- ttklabel(frame4, text="State variable offset [L]")
    frame4.lab.2.1 <- ttklabel(frame4, text="Temporal offset (sec)")
    
    frame4.ent.1.2 <- ttkentry(frame4, width=12, textvariable=off.z.var)
    frame4.ent.2.2 <- ttkentry(frame4, width=12, textvariable=off.t.var)
    
    tkbind(frame4.ent.1.2, "<KeyRelease>", function() {tclvalue(off.z.var) <- keyEvent("real", tclvalue(off.z.var), rm.data=TRUE)})
    tkbind(frame4.ent.2.2, "<KeyRelease>", function() {tclvalue(off.t.var) <- keyEvent("real", tclvalue(off.t.var), rm.data=TRUE)})
    
    tkgrid(frame4.lab.1.1, frame4.ent.1.2, padx=1, pady=1)
    tkgrid(frame4.lab.2.1, frame4.ent.2.2, padx=1, pady=1)
    
    tkgrid.configure(frame4.lab.1.1, sticky="e")
    tkgrid.configure(frame4.lab.2.1, sticky="e")
    
    tcl("grid", "anchor", frame4, "ne")
    
    tkpack(frame4, fill="both", ipadx=2, ipady=2, padx=2, pady=2)
    
  # gui control
    
    tkfocus(tt)
    tkgrab(tt)
    tkbind(tt, "<Destroy>", function() tclvalue(tt.done.var) <- 1)
    tkwait.variable(tt.done.var)
    
    update.par()
    
    tkgrab.release(tt)
    tkdestroy(tt)
}

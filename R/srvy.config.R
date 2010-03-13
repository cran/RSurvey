"srvy.config" <- function(parent) {
    
# additional functions (subroutines)
    
    update.par <- function() {
        hld <- as.integer(tclvalue(nlevels.var))
        srvy.dat("nlevels", if(is.na(hld)) NULL else hld)
        hld <- as.numeric(tclvalue(width.var))
        srvy.dat("width", if(is.na(hld)) NULL else hld)
        hld <- as.numeric(tclvalue(cex.pts.var))
        srvy.dat("cex.pts", if(is.na(hld)) NULL else hld)
        hld <- as.numeric(tclvalue(asp.yx.var))
        srvy.dat("asp.yx", if(is.na(hld)) NULL else hld)
        hld <- as.numeric(tclvalue(asp.zx.var))
        srvy.dat("asp.zx", if(is.na(hld)) NULL else hld)
        hld <- as.numeric(tclvalue(vmax.var))
        srvy.dat("vmax", if(is.na(hld)) NULL else hld)
        hld <- as.numeric(tclvalue(vxby.var))
        srvy.dat("vxby", if(is.na(hld)) NULL else hld)
        hld <- as.numeric(tclvalue(vyby.var))
        srvy.dat("vyby", if(is.na(hld)) NULL else hld)
        hld <- as.character(tclvalue(date.fmt.var))
        srvy.dat("date.fmt", if(hld == "") NULL else hld)
        
        srvy.dat("rkey", as.integer(tclvalue(rkey.var)))
        srvy.dat("img.contour", as.integer(tclvalue(img.contour.var)))
        srvy.dat("show.arrows", as.integer(tclvalue(show.arrows.var)))
        srvy.dat("show.lines", as.integer(tclvalue(show.lines.var)))
        srvy.dat("show.points", as.integer(tclvalue(show.points.var)))
        srvy.dat("show.poly", as.integer(tclvalue(show.poly.var)))
        srvy.dat("vuni", as.integer(tclvalue(vuni.var)))
    }
    
# main program
    
  # assign the variables linked to Tk widgets
    
    nlevels.var     <- tclVar()
    width.var       <- tclVar()
    cex.pts.var     <- tclVar()
    asp.yx.var      <- tclVar()
    asp.zx.var      <- tclVar()
    vmax.var        <- tclVar()
    vxby.var        <- tclVar()
    vyby.var        <- tclVar()
    date.fmt.var    <- tclVar()
    rkey.var        <- tclVar()
    show.poly.var   <- tclVar()
    img.contour.var <- tclVar()
    show.lines.var  <- tclVar()
    show.points.var <- tclVar()
    show.arrows.var <- tclVar()
    vuni.var        <- tclVar()
    
    if(!is.null(srvy.dat("nlevels")))     tclvalue(nlevels.var)     <- srvy.dat("nlevels")
    if(!is.null(srvy.dat("width")))       tclvalue(width.var)       <- srvy.dat("width")
    if(!is.null(srvy.dat("cex.pts")))     tclvalue(cex.pts.var)     <- srvy.dat("cex.pts")
    if(!is.null(srvy.dat("asp.yx")))      tclvalue(asp.yx.var)      <- srvy.dat("asp.yx")
    if(!is.null(srvy.dat("asp.zx")))      tclvalue(asp.zx.var)      <- srvy.dat("asp.zx")
    if(!is.null(srvy.dat("vmax")))        tclvalue(vmax.var)        <- srvy.dat("vmax")
    if(!is.null(srvy.dat("vxby")))        tclvalue(vxby.var)        <- srvy.dat("vxby")
    if(!is.null(srvy.dat("vyby")))        tclvalue(vyby.var)        <- srvy.dat("vyby")
    if(!is.null(srvy.dat("date.fmt")))    tclvalue(date.fmt.var)    <- srvy.dat("date.fmt")
    if(!is.null(srvy.dat("rkey")))        tclvalue(rkey.var)        <- srvy.dat("rkey")
    if(!is.null(srvy.dat("show.poly")))   tclvalue(show.poly.var)   <- srvy.dat("show.poly")
    if(!is.null(srvy.dat("img.contour"))) tclvalue(img.contour.var) <- srvy.dat("img.contour")
    if(!is.null(srvy.dat("show.lines")))  tclvalue(show.lines.var)  <- srvy.dat("show.lines")
    if(!is.null(srvy.dat("show.points"))) tclvalue(show.points.var) <- srvy.dat("show.points")
    if(!is.null(srvy.dat("show.arrows"))) tclvalue(show.arrows.var) <- srvy.dat("show.arrows")
    if(!is.null(srvy.dat("vuni")))        tclvalue(vuni.var)        <- srvy.dat("vuni")
    
    tt.done.var <- tclVar(0)
    
  # open gui
    
    tt <- tktoplevel(padx=5, pady=5)
    if(!missing(parent)) {
        tkwm.transient(tt, parent)
        tmp <- unlist(strsplit(as.character(tkwm.geometry(parent)), "\\+"))
        tkwm.geometry(tt, paste("+", as.integer(tmp[2]) + 25, "+", as.integer(tmp[3]) + 25, sep=""))
    }
    tktitle(tt) <- "Configuration"
    
    tkwm.resizable(tt, 0, 0)
    
  # frame 1 contains parameters
    
    frame1 <- ttklabelframe(tt, relief="flat", borderwidth=5, padding=8, text="Plot setup")
    
    frame1.lab.1.1 <- ttklabel(frame1, text="Approximate number of contour levels")
    frame1.lab.2.1 <- ttklabel(frame1, text="Width of plotting window canvas (in)")
    frame1.lab.3.1 <- ttklabel(frame1, text="Scaling for point symbols")
    frame1.lab.4.1 <- ttklabel(frame1, text="Aspect ratio, y / x")
    frame1.lab.5.1 <- ttklabel(frame1, text="Aspect ratio, z / x")
    frame1.lab.6.1 <- ttklabel(frame1, text="Maximum arrow length (in)")
    frame1.lab.7.1 <- ttklabel(frame1, text="Increment for sequence of arrows in x direction")
    frame1.lab.8.1 <- ttklabel(frame1, text="Increment for sequence of arrows in y direction")
    
    frame1.ent.1.2 <- ttkentry(frame1, width=6, textvariable=nlevels.var)
    frame1.ent.2.2 <- ttkentry(frame1, width=6, textvariable=width.var)
    frame1.ent.3.2 <- ttkentry(frame1, width=6, textvariable=cex.pts.var)
    frame1.ent.4.2 <- ttkentry(frame1, width=6, textvariable=asp.yx.var)
    frame1.ent.5.2 <- ttkentry(frame1, width=6, textvariable=asp.zx.var)
    frame1.ent.6.2 <- ttkentry(frame1, width=6, textvariable=vmax.var)
    frame1.ent.7.2 <- ttkentry(frame1, width=6, textvariable=vxby.var)
    frame1.ent.8.2 <- ttkentry(frame1, width=6, textvariable=vyby.var)
    
    tkbind(frame1.ent.1.2, "<KeyRelease>", function() {tclvalue(nlevels.var) <- keyEvent("integer", tclvalue(nlevels.var))})
    tkbind(frame1.ent.2.2, "<KeyRelease>", function() {tclvalue(width.var)   <- keyEvent("real",    tclvalue(width.var))})
    tkbind(frame1.ent.3.2, "<KeyRelease>", function() {tclvalue(cex.pts.var) <- keyEvent("real",    tclvalue(cex.pts.var))})
    tkbind(frame1.ent.4.2, "<KeyRelease>", function() {tclvalue(asp.yx.var)  <- keyEvent("real",    tclvalue(asp.yx.var))})
    tkbind(frame1.ent.5.2, "<KeyRelease>", function() {tclvalue(asp.zx.var)  <- keyEvent("real",    tclvalue(asp.zx.var))})
    tkbind(frame1.ent.6.2, "<KeyRelease>", function() {tclvalue(vmax.var)    <- keyEvent("real",    tclvalue(vmax.var))})
    tkbind(frame1.ent.7.2, "<KeyRelease>", function() {tclvalue(vxby.var)    <- keyEvent("integer", tclvalue(vxby.var))})
    tkbind(frame1.ent.8.2, "<KeyRelease>", function() {tclvalue(vyby.var)    <- keyEvent("integer", tclvalue(vyby.var))})
    
    tkgrid(frame1.lab.1.1, frame1.ent.1.2, padx=1, pady=1)
    tkgrid(frame1.lab.2.1, frame1.ent.2.2, padx=1, pady=1)
    tkgrid(frame1.lab.3.1, frame1.ent.3.2, padx=1, pady=1)
    tkgrid(frame1.lab.4.1, frame1.ent.4.2, padx=1, pady=1)
    tkgrid(frame1.lab.5.1, frame1.ent.5.2, padx=1, pady=1)
    tkgrid(frame1.lab.6.1, frame1.ent.6.2, padx=1, pady=1)
    tkgrid(frame1.lab.7.1, frame1.ent.7.2, padx=1, pady=1)
    tkgrid(frame1.lab.8.1, frame1.ent.8.2, padx=1, pady=1)
    
    tkgrid.configure(frame1.lab.1.1, sticky="e")
    tkgrid.configure(frame1.lab.2.1, sticky="e")
    tkgrid.configure(frame1.lab.3.1, sticky="e")
    tkgrid.configure(frame1.lab.4.1, sticky="e")
    tkgrid.configure(frame1.lab.5.1, sticky="e")
    tkgrid.configure(frame1.lab.6.1, sticky="e")
    tkgrid.configure(frame1.lab.7.1, sticky="e")
    tkgrid.configure(frame1.lab.8.1, sticky="e")
    
    tcl("grid", "anchor", frame1, "center")
    
    tkpack(frame1, fill="both", padx=2, pady=2, expand="yes", side="left")
    
  # frame 2 contains plot features
    
    frame2 <- ttklabelframe(tt, relief="flat", borderwidth=5, padding=3, text="Plot features")
    
    frame2.chk.1.1 <- ttkcheckbutton(frame2, variable=rkey.var,        text="reverse legend")
    frame2.chk.2.1 <- ttkcheckbutton(frame2, variable=show.poly.var,   text="show range and limit polygons")
    frame2.chk.3.1 <- ttkcheckbutton(frame2, variable=img.contour.var, text="use image contour")
    frame2.chk.4.1 <- ttkcheckbutton(frame2, variable=show.lines.var,  text="show contour lines")
    frame2.chk.5.1 <- ttkcheckbutton(frame2, variable=show.points.var, text="show points on surface")
    frame2.chk.6.1 <- ttkcheckbutton(frame2, variable=show.arrows.var, text="show arrows")
    frame2.chk.7.1 <- ttkcheckbutton(frame2, variable=vuni.var,        text="use uniform arrow lengths")
    
    tkgrid(frame2.chk.1.1, pady=1, sticky="w")
    tkgrid(frame2.chk.2.1, pady=1, sticky="w")
    tkgrid(frame2.chk.3.1, pady=1, sticky="w")
    tkgrid(frame2.chk.4.1, pady=1, sticky="w")
    tkgrid(frame2.chk.5.1, pady=1, sticky="w")
    tkgrid(frame2.chk.6.1, pady=1, sticky="w")
    tkgrid(frame2.chk.7.1, pady=1, sticky="w")
    
    tcl("grid", "anchor", frame2, "center")
    
    tkpack(frame2, fill="both", padx=2, pady=2, side="top")
    
  # frame 3 contains output date/time format
    
    frame3 <- ttklabelframe(tt, relief="flat", borderwidth=5, padding=3, text="Export formats")
    
    frame3.lab.1.1 <- ttklabel(frame3, text="Date/time")
    
    frame3.ent.1.2 <- ttkentry(frame3, width=23, textvariable=date.fmt.var)
    
    tkbind(frame3.ent.1.2, "<KeyRelease>", function() {tclvalue(date.fmt.var) <- keyEvent("date", tclvalue(date.fmt.var))})
    
    tkgrid(frame3.lab.1.1, frame3.ent.1.2, padx=2, pady=1)
    
    tcl("grid", "anchor", frame3, "center")
    
    tkpack(frame3, fill="both", padx=2, pady=2, side="bottom")
    
  # gui control
    
    tkfocus(tt)
    tkgrab(tt)
    tkbind(tt, "<Destroy>", function() tclvalue(tt.done.var) <- 1)
    tkwait.variable(tt.done.var)
    
    update.par()
    
    tkgrab.release(tt)
    tkdestroy(tt)
}

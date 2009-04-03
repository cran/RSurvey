"tran.edit" <- function(id=NULL, tt2=NULL) {
    
# additional functions (subroutines)
    
  # closing commands
    
    update.par <- function() {
        id.old <- id
        id.new <- as.character(tclvalue(id.var))
        if(id.new == "") return()
        
        ids <- names(tran.dat())
        if(is.null(id.old)) {
            while(id.new %in% ids) 
                id.new <- paste(id.new, "NEW")
        }
        else if(id.old != id.new) {
            if(id.old %in% ids) {
                tran.dat(id.new, value=tran.dat(id.old))
                tran.dat(id.old, clearId=TRUE)
            }
        }
        
        x1 <- as.numeric(tclvalue(x1.var))
        x2 <- as.numeric(tclvalue(x2.var))
        y1 <- as.numeric(tclvalue(y1.var))
        y2 <- as.numeric(tclvalue(y2.var))
        f0 <- as.character(tclvalue(fix.zero.var))
        zo <- as.numeric(tclvalue(v.origin.var))
        vo <- as.numeric(tclvalue(v.offset.var))
        ho <- as.numeric(tclvalue(h.offset.var))
        dx <- as.numeric(tclvalue(grid.dx.var))
        dy <- as.numeric(tclvalue(grid.dy.var))
        zi <- as.numeric(tclvalue(zero.int.var))
        ar <- as.numeric(tclvalue(asp.ratio.var))
        
        if(!(any(is.na(c(x1, x2, y1, y2))))) {
            tran.dat(id.new, "id", id.new)
            
            vert <- matrix(c(x1, x2, y1, y2), nrow=2, ncol=2, dimnames=list(1:2, c("x", "y")))
            tran.dat(id.new, "vertices", vert)
            
            tran.dat(id.new, "fix.zero", f0)
            
            tran.dat(id.new, "v.origin",  if(is.na(zo)) NULL else zo)
            tran.dat(id.new, "v.offset",  if(is.na(vo)) NULL else vo)
            tran.dat(id.new, "h.offset",  if(is.na(ho)) NULL else ho)
            tran.dat(id.new, "grid.dx",   if(is.na(dx)) NULL else dx)
            tran.dat(id.new, "grid.dy",   if(is.na(dy)) NULL else dy)
            tran.dat(id.new, "zero.int",  if(is.na(zi)) NULL else zi)
            tran.dat(id.new, "asp.ratio", if(is.na(ar)) NULL else ar)
        }
    }
    
    
    
# main program
    
  # GUI font
    
    fnt <- srvy.dat("font.gui")
    
  # assign the variables linked to Tk widgets
    
    id.var <- tclVar()
    x1.var <- tclVar()
    x2.var <- tclVar()
    y1.var <- tclVar()
    y2.var <- tclVar()
    fix.zero.var  <- tclVar()
    h.offset.var  <- tclVar()
    grid.dx.var   <- tclVar()
    grid.dy.var   <- tclVar()
    asp.ratio.var <- tclVar()
    v.origin.var  <- tclVar()
    v.offset.var  <- tclVar()
    zero.int.var  <- tclVar()
    
    if(!is.null(id)) tclvalue(id.var) <- id
    
    vert <- tran.dat(id, "vertices")
    if(!is.null(vert)) {
        tclvalue(x1.var) <- vert[[1,1]]
        tclvalue(y1.var) <- vert[[1,2]]
        tclvalue(x2.var) <- vert[[2,1]]
        tclvalue(y2.var) <- vert[[2,2]]
    }
    
    f0 <- tran.dat(id, "fix.zero")
    if(!is.null(f0)) tclvalue(fix.zero.var) <- f0
    zo <- tran.dat(id, "v.origin")
    if(!is.null(zo)) tclvalue(v.origin.var) <- zo
    vo <- tran.dat(id, "v.offset")
    if(!is.null(vo)) tclvalue(v.offset.var) <- vo
    ho <- tran.dat(id, "h.offset")
    if(!is.null(ho)) tclvalue(h.offset.var) <- ho
    dx <- tran.dat(id, "grid.dx")
    if(!is.null(dx)) tclvalue(grid.dx.var)  <- dx
    dy <- tran.dat(id, "grid.dy")
    if(!is.null(dy)) tclvalue(grid.dy.var)  <- dy
    zi <- tran.dat(id, "zero.int")
    if(!is.null(zi)) tclvalue(zero.int.var) <- zi
    ar <- tran.dat(id, "asp.ratio")
    if(!is.null(ar)) tclvalue(asp.ratio.var)<- ar
    
    tt3.done.var <- tclVar(0)
    
  # open gui
    
    tt3 <- tktoplevel()
    if(!is.null(tt2)) {
        tkwm.transient(tt3, tt2)
        tmp <- unlist(strsplit(as.character(tkwm.geometry(tt2)), "\\+"))
        tkwm.geometry(tt3, paste("+", as.integer(tmp[2]) + 25, "+", as.integer(tmp[3]) + 25, sep=""))
    }
    tktitle(tt3) <- "Edit Transect"
    
    tkwm.resizable(tt3, 0, 0)
    
  # frame 1 contains the profile name and vertices
    
    frame1 <- tkframe(tt3, relief="flat", borderwidth=5)
    
    frame1.lab.1.1 <- tklabel(frame1, font=fnt, justify="right",  text="Name")
    
    frame1.ent.1.2 <- tkentry(frame1, font=fnt, width=26, textvariable=id.var)
    
    tkgrid(frame1.lab.1.1, frame1.ent.1.2)
    
    tkgrid.configure(frame1.lab.1.1, row=0, column=0, sticky="e")
    tkgrid.configure(frame1.ent.1.2, row=0, column=1, columnspan=2)
    
    tkpack(frame1)
    
  # frame 2 contains vertices
    
    frame2 <- ttklabelframe(tt3, relief="flat", borderwidth=5, padding=3, text="Vertices")
    
    frame2.lab.1.2 <- tklabel(frame2, font=fnt, justify="center", text="x [L]")
    frame2.lab.1.3 <- tklabel(frame2, font=fnt, justify="center", text="y [L]")
    frame2.lab.1.4 <- tklabel(frame2, font=fnt, justify="center", text="Origin")
    frame2.lab.2.1 <- tklabel(frame2, font=fnt, justify="right",  text="Left")
    frame2.lab.3.1 <- tklabel(frame2, font=fnt, justify="right",  text="Right")
    
    frame2.ent.2.2 <- tkentry(frame2, font=fnt, width=13, textvariable=x1.var)
    frame2.ent.2.3 <- tkentry(frame2, font=fnt, width=13, textvariable=y1.var)
    frame2.ent.3.2 <- tkentry(frame2, font=fnt, width=13, textvariable=x2.var)
    frame2.ent.3.3 <- tkentry(frame2, font=fnt, width=13, textvariable=y2.var)
    
    frame2.rad.2.4 <- tkradiobutton(frame2, variable=fix.zero.var, value="L", justify="center")
    frame2.rad.3.4 <- tkradiobutton(frame2, variable=fix.zero.var, value="R", justify="center")
    
    tkbind(frame2.ent.2.2, "<KeyRelease>", function() {tclvalue(x1.var) <- keyEvent("real", tclvalue(x1.var))})
    tkbind(frame2.ent.2.3, "<KeyRelease>", function() {tclvalue(y1.var) <- keyEvent("real", tclvalue(y1.var))})
    tkbind(frame2.ent.3.2, "<KeyRelease>", function() {tclvalue(x2.var) <- keyEvent("real", tclvalue(x2.var))})
    tkbind(frame2.ent.3.3, "<KeyRelease>", function() {tclvalue(y2.var) <- keyEvent("real", tclvalue(y2.var))})
    
    tkgrid(frame2.lab.1.2, frame2.lab.1.3, frame2.lab.1.4)
    tkgrid(frame2.lab.2.1, frame2.ent.2.2, frame2.ent.2.3, frame2.rad.2.4)
    tkgrid(frame2.lab.3.1, frame2.ent.3.2, frame2.ent.3.3, frame2.rad.3.4)
    
    tkgrid.configure(frame2.lab.1.2, row=1, column=1)
    tkgrid.configure(frame2.lab.1.3, row=1, column=2)
    tkgrid.configure(frame2.lab.1.4, row=1, column=3)
    tkgrid.configure(frame2.lab.2.1, row=2, column=0, sticky="e")
    tkgrid.configure(frame2.lab.3.1, row=3, column=0, sticky="e")
    
    tkgrid.configure(frame2.ent.2.2, row=2, column=1)
    tkgrid.configure(frame2.ent.2.3, row=2, column=2)
    tkgrid.configure(frame2.ent.3.2, row=3, column=1)
    tkgrid.configure(frame2.ent.3.3, row=3, column=2)
    
    tkgrid.configure(frame2.rad.2.4, row=2, column=3)
    tkgrid.configure(frame2.rad.3.4, row=3, column=3)
    
    tkpack(frame2)
    
  # frame 3 contains other parameters
    
    frame3 <- tkframe(tt3, relief="flat", borderwidth=5)
    
    frame3.lab.1.1 <- tklabel(frame3, font=fnt, text="z value for the local z-axis origin [L]")
    frame3.lab.2.1 <- tklabel(frame3, font=fnt, text="Local z-axis offset, positive upward [L]")
    frame3.lab.3.1 <- tklabel(frame3, font=fnt, text="Local x-axis offset, vertex origin [L]")
    frame3.lab.4.1 <- tklabel(frame3, font=fnt, text="Local x-axis grid resoltion [L]")
    frame3.lab.5.1 <- tklabel(frame3, font=fnt, text="Local z-axis grid resoltion [L]")
    frame3.lab.6.1 <- tklabel(frame3, font=fnt, text="Zero velocity along profile, interval [L]")
    frame3.lab.7.1 <- tklabel(frame3, font=fnt, text="Aspect ratio, local z / x")
    
    frame3.ent.1.2 <- tkentry(frame3, font=fnt, width=8, textvariable=v.origin.var)
    frame3.ent.2.2 <- tkentry(frame3, font=fnt, width=8, textvariable=v.offset.var)
    frame3.ent.3.2 <- tkentry(frame3, font=fnt, width=8, textvariable=h.offset.var)
    frame3.ent.4.2 <- tkentry(frame3, font=fnt, width=8, textvariable=grid.dx.var)
    frame3.ent.5.2 <- tkentry(frame3, font=fnt, width=8, textvariable=grid.dy.var)
    frame3.ent.6.2 <- tkentry(frame3, font=fnt, width=8, textvariable=zero.int.var)
    frame3.ent.7.2 <- tkentry(frame3, font=fnt, width=8, textvariable=asp.ratio.var)
    
    tkbind(frame3.ent.1.2, "<KeyRelease>", function() {tclvalue(v.origin.var)  <- keyEvent("real", tclvalue(v.origin.var))})
    tkbind(frame3.ent.2.2, "<KeyRelease>", function() {tclvalue(v.offset.var)  <- keyEvent("real", tclvalue(v.offset.var))})
    tkbind(frame3.ent.3.2, "<KeyRelease>", function() {tclvalue(h.offset.var)  <- keyEvent("real", tclvalue(h.offset.var))})
    tkbind(frame3.ent.4.2, "<KeyRelease>", function() {tclvalue(grid.dx.var)   <- keyEvent("real", tclvalue(grid.dx.var))})
    tkbind(frame3.ent.5.2, "<KeyRelease>", function() {tclvalue(grid.dy.var)   <- keyEvent("real", tclvalue(grid.dy.var))})
    tkbind(frame3.ent.6.2, "<KeyRelease>", function() {tclvalue(zero.int.var)  <- keyEvent("real", tclvalue(zero.int.var))})
    tkbind(frame3.ent.7.2, "<KeyRelease>", function() {tclvalue(asp.ratio.var) <- keyEvent("real", tclvalue(asp.ratio.var))})
    
    tkgrid(frame3.lab.1.1, frame3.ent.1.2, padx=1, pady=1)
    tkgrid(frame3.lab.2.1, frame3.ent.2.2, padx=1, pady=1)
    tkgrid(frame3.lab.3.1, frame3.ent.3.2, padx=1, pady=1)
    tkgrid(frame3.lab.4.1, frame3.ent.4.2, padx=1, pady=1)
    tkgrid(frame3.lab.5.1, frame3.ent.5.2, padx=1, pady=1)
    tkgrid(frame3.lab.6.1, frame3.ent.6.2, padx=1, pady=1)
    tkgrid(frame3.lab.7.1, frame3.ent.7.2, padx=1, pady=1)
    
    tkgrid.configure(frame3.lab.1.1, sticky="e")
    tkgrid.configure(frame3.lab.2.1, sticky="e")
    tkgrid.configure(frame3.lab.3.1, sticky="e")
    tkgrid.configure(frame3.lab.4.1, sticky="e")
    tkgrid.configure(frame3.lab.5.1, sticky="e")
    tkgrid.configure(frame3.lab.6.1, sticky="e")
    tkgrid.configure(frame3.lab.7.1, sticky="e")
    
    tkpack(frame3, fill="both")
    
  # gui control
    
    tkgrab(tt3)
    tkfocus(tt3)
    tkbind(tt3, "<Destroy>", function() {tclvalue(tt3.done.var) <- 1})
    tkwait.variable(tt3.done.var)
    
    update.par()
    
    tkgrab.release(tt3)
    tkdestroy(tt3)
}

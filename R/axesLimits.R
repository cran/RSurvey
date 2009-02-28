"axesLimits" <- function(tt2=NULL, old=list()) {

# additional functions (subroutines)
    
    update.axes <- function() {
        new <- list()
        
        z.min <- as.numeric(tclvalue(z.min.var))
        z.max <- as.numeric(tclvalue(z.max.var))
        z.lev <- as.numeric(tclvalue(z.lev.var))
        
        z.min.auto <- as.integer(tclvalue(z.min.auto.var))
        z.max.auto <- as.integer(tclvalue(z.max.auto.var))
        z.lev.auto <- as.integer(tclvalue(z.lev.auto.var))
        
        if(is.na(z.min) && !z.min.auto) z.min.auto <- 1
        if(is.na(z.max) && !z.max.auto) z.max.auto <- 1
        if(is.na(z.lev) && !z.lev.auto) z.lev.auto <- 1
        
        new <- list(z.min=z.min, z.min.auto=z.min.auto, z.max=z.max, z.max.auto=z.max.auto, 
                    z.lev=z.lev, z.lev.auto=z.lev.auto)
        new
    }
    
# main program
    
  # GUI font
    
    fnt <- srvy.dat("font.gui")
    
  # universal parameters
    
  # assign the variables linked to Tk widgets 
    
    z.min.var      <- tclVar()
    z.max.var      <- tclVar()
    z.min.auto.var <- tclVar()
    z.max.auto.var <- tclVar()
    z.lev.var      <- tclVar()
    z.lev.auto.var <- tclVar()
    
    if(!is.null(old$z.min) && !is.na(old$z.min)) tclvalue(z.min.var) <- old$z.min
    if(!is.null(old$z.max) && !is.na(old$z.max)) tclvalue(z.max.var) <- old$z.max
    if(!is.null(old$z.lev) && !is.na(old$z.lev)) tclvalue(z.lev.var) <- old$z.lev
    
    if(is.null(old$z.min.auto)) old$z.min.auto <- 1
    if(is.null(old$z.max.auto)) old$z.max.auto <- 1
    if(is.null(old$z.lev.auto)) old$z.lev.auto <- 1
    
    tclvalue(z.min.auto.var) <- old$z.min.auto
    tclvalue(z.max.auto.var) <- old$z.max.auto
    tclvalue(z.lev.auto.var) <- old$z.lev.auto
    
    tt3.done.var <- tclVar(0)
    
  # open gui
    
    tt3 <- tktoplevel(padx=5, pady=5)
    if(!is.null(tt2)) {
        tkwm.transient(tt3, tt2)
        tmp <- unlist(strsplit(as.character(tkwm.geometry(tt2)), "\\+"))
        tkwm.geometry(tt3, paste("+", as.integer(tmp[2]) + 25, "+", as.integer(tmp[3]) + 25, sep=""))
    }
    tktitle(tt3) <- "Axes Limits"
    
    if(!is.null(srvy.dat("icon")) && file.exists(srvy.dat("icon"))) 
        tkwm.iconbitmap(tt3, srvy.dat("icon"))
    
    tkwm.resizable(tt3, 0, 0)
    
  # frame 1 contains z-axis paramters
    
    frame1 <- ttklabelframe(tt3, relief="flat", padding=3, text="Raster Data")
    
    frame1.lab.1.1 <- tklabel(frame1, font=fnt, text="Minimum")
    frame1.lab.2.1 <- tklabel(frame1, font=fnt, text="Maximum")
    frame1.lab.3.1 <- tklabel(frame1, font=fnt, text="Num. levels")
    
    frame1.ent.1.2 <- tkentry(frame1, font=fnt, width=15, textvariable=z.min.var)
    frame1.ent.2.2 <- tkentry(frame1, font=fnt, width=15, textvariable=z.max.var)
    frame1.ent.3.2 <- tkentry(frame1, font=fnt, width=15, textvariable=z.lev.var)
    
    tkbind(frame1.ent.1.2, "<KeyRelease>", function() {tclvalue(z.min.var) <- keyEvent("real", tclvalue(z.min.var))})
    tkbind(frame1.ent.2.2, "<KeyRelease>", function() {tclvalue(z.max.var) <- keyEvent("real", tclvalue(z.max.var))})
    tkbind(frame1.ent.3.2, "<KeyRelease>", function() {tclvalue(z.lev.var) <- keyEvent("integer", tclvalue(z.lev.var))})
    
    frame1.chk.1.3 <- tkcheckbutton(frame1, variable=z.min.auto.var, text="Automatic")
    frame1.chk.2.3 <- tkcheckbutton(frame1, variable=z.max.auto.var, text="Automatic")
    frame1.chk.3.3 <- tkcheckbutton(frame1, variable=z.lev.auto.var, text="Automatic")
    
    tkgrid(frame1.lab.1.1, frame1.ent.1.2, frame1.chk.1.3, padx=1, pady=1)
    tkgrid(frame1.lab.2.1, frame1.ent.2.2, frame1.chk.2.3, padx=1, pady=1)
    tkgrid(frame1.lab.3.1, frame1.ent.3.2, frame1.chk.3.3, padx=1, pady=1)
    
    tkgrid.configure(frame1.lab.1.1, sticky="e")
    tkgrid.configure(frame1.lab.2.1, sticky="e")
    tkgrid.configure(frame1.lab.3.1, sticky="e")
    
    tkpack(frame1, fill="both", ipadx=2, ipady=2, padx=2, pady=2)
    
  # gui control
    
    tkfocus(tt3)
    tkgrab(tt3)
    tkbind(tt3, "<Destroy>", function() tclvalue(tt3.done.var) <- 1)
    tkwait.variable(tt3.done.var)
    
    new <- update.axes()
    
    tkgrab.release(tt3)
    tkdestroy(tt3)
    
    new
}


"geoProj" <- function(tt1=NULL) {
    
# additional functions (subroutines)
    
    update.par <- function() {
        if(tclvalue(projection.var) != "") 
            srvy.dat("projection", tclvalue(projection.var))
    }
    
# main program
    
  # GUI font
    
    fnt <- srvy.dat("font.gui")
    
  # assign the variables linked to Tk widgets 
    
    projection.var <- tclVar()
    
    if(!is.null(srvy.dat("projection"))) 
        tclvalue(projection.var) <- srvy.dat("projection")
    
    tclvalue(projection.var) <- ifelse(is.null(srvy.dat("projection")), "", srvy.dat("projection"))
    
    tt2.done.var <- tclVar(0)
    
  # open gui
    
    tt2 <- tktoplevel(padx=5, pady=5)
    if(!is.null(tt1)) {
        tkwm.transient(tt2, tt1)
        tmp <- unlist(strsplit(as.character(tkwm.geometry(tt1)), "\\+"))
        tkwm.geometry(tt2, paste("+", as.integer(tmp[2]) + 25, "+", as.integer(tmp[3]) + 25, sep=""))
    }
    tktitle(tt2) <- "Geographical Projection"
    
    tkwm.resizable(tt2, 0, 0)
    
  # frame 1 contains parameters
    
    frame1 <- tkframe(tt2, relief="flat", borderwidth=2)
    
    frame1.lab.1.1 <- tklabel(frame1, font=fnt, text="Projection")
    
    frame1.lab.1.3 <- tklabel(frame1, font=fnt, text="")
    
    frame1.ent.1.2 <- tkentry(frame1, font=fnt, width=30, textvariable=projection.var)
    
    tkgrid(frame1.lab.1.1, frame1.ent.1.2, frame1.lab.1.3, padx=1, pady=1)
    
    tkgrid.configure(frame1.lab.1.1, sticky="e")
    
    tkgrid.configure(frame1.lab.1.3, sticky="w")
    
    tkpack(frame1, fill="both")
    
  # gui control
    
    tkfocus(tt2)
    tkgrab(tt2)
    tkbind(tt2, "<Destroy>", function() tclvalue(tt2.done.var) <- 1)
    tkwait.variable(tt2.done.var)
    
    update.par()
    
    tkgrab.release(tt2)
    tkdestroy(tt2)
}

"geoproj" <- function(parent, old=NULL) {
    
# main program
    
  # assign the variables linked to Tk widgets 
    
    projection.var <- tclVar()
    
    if(!is.null(old)) 
        tclvalue(projection.var) <- old
    
    tt.done.var <- tclVar(0)
    
  # open gui
    
    tt <- tktoplevel(padx=5, pady=5)
    if(!missing(parent)) {
        tkwm.transient(tt, parent)
        tmp <- unlist(strsplit(as.character(tkwm.geometry(parent)), "\\+"))
        tkwm.geometry(tt, paste("+", as.integer(tmp[2]) + 25, "+", as.integer(tmp[3]) + 25, sep=""))
    }
    tktitle(tt) <- "Geographical Projection"
    
    tkwm.resizable(tt, 0, 0)
    
  # frame 0 contains parameters
    
    frame0 <- ttkframe(tt, relief="flat", borderwidth=2)
    
    frame0.lab.1.1 <- ttklabel(frame0, text="Projection")
    
    frame0.lab.1.3 <- ttklabel(frame0, text="")
    
    frame0.ent.1.2 <- ttkentry(frame0, width=30, textvariable=projection.var)
    
    tkgrid(frame0.lab.1.1, frame0.ent.1.2, frame0.lab.1.3, padx=1, pady=1)
    
    tkgrid.configure(frame0.lab.1.1, sticky="e")
    
    tkgrid.configure(frame0.lab.1.3, sticky="w")
    
    tkpack(frame0, fill="both")
    
  # gui control
    
    tkfocus(tt)
    tkgrab(tt)
    tkbind(tt, "<Destroy>", function() tclvalue(tt.done.var) <- 1)
    tkwait.variable(tt.done.var)
    
    tkgrab.release(tt)
    tkdestroy(tt)
    
    rtn <- as.character(tclvalue(projection.var))
    if(rtn == "") return(NULL) else return(rtn)
}

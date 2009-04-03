"tran.velocity" <- function(rasterFields, velocityFields=NULL, tt2=NULL) {
    
# additional functions (subroutines)
    
  # close GUI
    
    gui.close <- function() {
        if(as.integer(tclvalue(tt3.done.var)) != 0) return()
        
        num.chk <- function(n) {ifelse(length(n) == 0, NA, n)}
        num <- NULL
        num <- append(num, num.chk(as.integer(tkcurselection(frame1.lst.1)) + 1))
        num <- append(num, num.chk(as.integer(tkcurselection(frame1.lst.2)) + 1))
        num <- append(num, num.chk(as.integer(tkcurselection(frame1.lst.3)) + 1))
        num <- append(num, num.chk(as.integer(tkcurselection(frame1.lst.4)) + 1))
        
        velocityFields <<- rasterFields[num]
        
        tclvalue(tt3.done.var) <- 1
    }
    
# main program
    
  # GUI font
    
    fnt <- srvy.dat("font.gui")
    
  # assign variables
    
    tt3.done.var <- tclVar(0)
    
  # open gui
    
    tt3 <- tktoplevel()
    if(!is.null(tt2)) {
        tkwm.transient(tt3, tt2)
        tmp <- unlist(strsplit(as.character(tkwm.geometry(tt2)), "\\+"))
        tkwm.geometry(tt3, paste("+", as.integer(tmp[2]) + 25, "+", as.integer(tmp[3]) + 25, sep=""))
    }
    tktitle(tt3) <- "Velocity Vector Components"
    
    tkwm.resizable(tt3, 0, 0)
    
  # frame 1 contains velocity vector identification
    
    frame1 <- tkframe(tt3, relief="flat", borderwidth=2) 
    
    frame1.lab.1 <- tklabel(frame1, justify="center", font=fnt, text="x-direction")
    frame1.lab.2 <- tklabel(frame1, justify="center", font=fnt, text="y-direction")
    frame1.lab.3 <- tklabel(frame1, justify="center", font=fnt, text="z-direction")
    frame1.lab.4 <- tklabel(frame1, justify="center", font=fnt, text="Principle")
    
    frame1.lst.1 <- tklistbox(frame1, selectmode="browse", font=fnt, width=19, height=8, exportselection=FALSE)
    frame1.lst.2 <- tklistbox(frame1, selectmode="browse", font=fnt, width=19, height=8, exportselection=FALSE)
    frame1.lst.3 <- tklistbox(frame1, selectmode="browse", font=fnt, width=19, height=8, exportselection=FALSE)
    frame1.lst.4 <- tklistbox(frame1, selectmode="browse", font=fnt, width=19, height=8, exportselection=FALSE)
    
    frame1.ysc.1 <- tkscrollbar(frame1, orient="vertical")
    frame1.ysc.2 <- tkscrollbar(frame1, orient="vertical")
    frame1.ysc.3 <- tkscrollbar(frame1, orient="vertical")
    frame1.ysc.4 <- tkscrollbar(frame1, orient="vertical")
    
    frame1.xsc.1 <- tkscrollbar(frame1, orient="horizontal")
    frame1.xsc.2 <- tkscrollbar(frame1, orient="horizontal")
    frame1.xsc.3 <- tkscrollbar(frame1, orient="horizontal")
    frame1.xsc.4 <- tkscrollbar(frame1, orient="horizontal")
    
    tkconfigure(frame1.lst.1, background="white", 
        yscrollcommand=paste(.Tk.ID(frame1.ysc.1), "set"), 
        xscrollcommand=paste(.Tk.ID(frame1.xsc.1), "set"))
    tkconfigure(frame1.lst.2, background="white", 
        yscrollcommand=paste(.Tk.ID(frame1.ysc.2), "set"), 
        xscrollcommand=paste(.Tk.ID(frame1.xsc.2), "set"))
    tkconfigure(frame1.lst.3, background="white", 
        yscrollcommand=paste(.Tk.ID(frame1.ysc.3), "set"), 
        xscrollcommand=paste(.Tk.ID(frame1.xsc.3), "set"))
    tkconfigure(frame1.lst.4, background="white", 
        yscrollcommand=paste(.Tk.ID(frame1.ysc.4), "set"), 
        xscrollcommand=paste(.Tk.ID(frame1.xsc.4), "set"))
    
    tkconfigure(frame1.ysc.1, command=paste(.Tk.ID(frame1.lst.1), "yview"))
    tkconfigure(frame1.ysc.2, command=paste(.Tk.ID(frame1.lst.2), "yview"))
    tkconfigure(frame1.ysc.3, command=paste(.Tk.ID(frame1.lst.3), "yview"))
    tkconfigure(frame1.ysc.4, command=paste(.Tk.ID(frame1.lst.4), "yview"))
    
    tkconfigure(frame1.xsc.1, command=paste(.Tk.ID(frame1.lst.1), "xview"))
    tkconfigure(frame1.xsc.2, command=paste(.Tk.ID(frame1.lst.2), "xview"))
    tkconfigure(frame1.xsc.3, command=paste(.Tk.ID(frame1.lst.3), "xview"))
    tkconfigure(frame1.xsc.4, command=paste(.Tk.ID(frame1.lst.4), "xview"))
    
    clr.sel.1 <- function() tkselection.clear(frame1.lst.1, as.integer(tkcurselection(frame1.lst.1)))
    tkbind(frame1.lst.1, "<Control-ButtonRelease-1>", clr.sel.1)
    clr.sel.2 <- function() tkselection.clear(frame1.lst.2, as.integer(tkcurselection(frame1.lst.2)))
    tkbind(frame1.lst.2, "<Control-ButtonRelease-1>", clr.sel.2)
    clr.sel.3 <- function() tkselection.clear(frame1.lst.3, as.integer(tkcurselection(frame1.lst.3)))
    tkbind(frame1.lst.3, "<Control-ButtonRelease-1>", clr.sel.3)
    clr.sel.4 <- function() tkselection.clear(frame1.lst.4, as.integer(tkcurselection(frame1.lst.4)))
    tkbind(frame1.lst.4, "<Control-ButtonRelease-1>", clr.sel.4)
    
    tkgrid(frame1.lab.1, frame1.lab.2, frame1.lab.3, frame1.lab.4, pady=0)
    tkgrid(frame1.lst.1, frame1.ysc.1, frame1.xsc.1, 
           frame1.lst.2, frame1.ysc.2, frame1.xsc.2, 
           frame1.lst.3, frame1.ysc.3, frame1.xsc.3, 
           frame1.lst.4, frame1.ysc.4, frame1.xsc.4)
    
    tkgrid.configure(frame1.lab.1, column=0, row=0)
    tkgrid.configure(frame1.lab.2, column=2, row=0)
    tkgrid.configure(frame1.lab.3, column=4, row=0)
    tkgrid.configure(frame1.lab.4, column=6, row=0)
    
    tkgrid.configure(frame1.lst.1, column=0, row=1, sticky="w e")
    tkgrid.configure(frame1.ysc.1, column=1, row=1, sticky="n s")
    tkgrid.configure(frame1.xsc.1, column=0, row=2, sticky="w e")
    tkgrid.configure(frame1.lst.2, column=2, row=1, sticky="w e")
    tkgrid.configure(frame1.ysc.2, column=3, row=1, sticky="n s")
    tkgrid.configure(frame1.xsc.2, column=2, row=2, sticky="w e")
    tkgrid.configure(frame1.lst.3, column=4, row=1, sticky="w e")
    tkgrid.configure(frame1.ysc.3, column=5, row=1, sticky="n s")
    tkgrid.configure(frame1.xsc.3, column=4, row=2, sticky="w e")
    tkgrid.configure(frame1.lst.4, column=6, row=1, sticky="w e")
    tkgrid.configure(frame1.ysc.4, column=7, row=1, sticky="n s")
    tkgrid.configure(frame1.xsc.4, column=6, row=2, sticky="w e")
    
    for(i in rasterFields) {
        tkinsert(frame1.lst.1, "end", i)
        tkinsert(frame1.lst.2, "end", i)
        tkinsert(frame1.lst.3, "end", i)
        tkinsert(frame1.lst.4, "end", i)
    }
    
    idx <- rep(NA, 4)
    if(!is.null(velocityFields)) {
        for(i in 1:4) {
            if(velocityFields[i] %in% rasterFields) 
                idx[i] <- match(velocityFields[i], rasterFields) - 1
        }
    }
    
    if(!is.na(idx[1])) tkselection.set(frame1.lst.1, idx[1])
    if(!is.na(idx[2])) tkselection.set(frame1.lst.2, idx[2])
    if(!is.na(idx[3])) tkselection.set(frame1.lst.3, idx[3])
    if(!is.na(idx[4])) tkselection.set(frame1.lst.4, idx[4])
    
    tkgrid(frame1)
    
  # gui control
    
    tkfocus(tt3)
    tkgrab(tt3)
    tkbind(tt3, "<Destroy>", function() {gui.close()})
    tkwait.variable(tt3.done.var)
    
    tkgrab.release(tt3)
    tkdestroy(tt3)
    
    velocityFields
}

"srvy.vars" <- function(cols, vars, tt1=NULL) {
    
# additional functions (subroutines)
    
  # close GUI
    
    gui.close <- function() {
        if(as.integer(tclvalue(tt2.done.var)) != 0) return()
        
        n.1 <- tkcurselection(frame1.lst.1)
        n.2 <- tkcurselection(frame1.lst.2)
        n.3 <- tkcurselection(frame1.lst.3)
        n.4 <- tkcurselection(frame1.lst.4)
        
        hld <- NULL
        hld <- append(hld, ifelse(tclvalue(n.1) == "", NA, tclvalue(tkget(frame1.lst.1, n.1))))
        hld <- append(hld, ifelse(tclvalue(n.2) == "", NA, tclvalue(tkget(frame1.lst.2, n.2))))
        hld <- append(hld, ifelse(tclvalue(n.3) == "", NA, tclvalue(tkget(frame1.lst.3, n.3))))
        hld <- append(hld, ifelse(tclvalue(n.4) == "", NA, tclvalue(tkget(frame1.lst.4, n.4))))
        
        srvy.dat("vars", hld)
        
        tclvalue(tt2.done.var) <- 1
    }
    
# main program
    
  # GUI font
    
    fnt <- srvy.dat("font.gui")
    
  # assign variables
    
    tt2.done.var <- tclVar(0)
    
  # open gui
    
    tt2 <- tktoplevel()
    if(!is.null(tt1)) {
        tkwm.transient(tt2, tt1)
        tmp <- unlist(strsplit(as.character(tkwm.geometry(tt1)), "\\+"))
        tkwm.geometry(tt2, paste("+", as.integer(tmp[2]) + 25, "+", as.integer(tmp[3]) + 25, sep=""))
    }
    tktitle(tt2) <- "Variables"
    
    if(!is.null(srvy.dat("icon")) && file.exists(srvy.dat("icon"))) 
        tkwm.iconbitmap(tt2, srvy.dat("icon"))
    
    tkwm.resizable(tt2, 0, 0)
    
  # frame 1 contains velocity vector identification
    
    frame1 <- tkframe(tt2, relief="flat", borderwidth=2) 
    
    frame1.lab.1 <- tklabel(frame1, justify="center", font=fnt, text="Date/Time")
    frame1.lab.2 <- tklabel(frame1, justify="center", font=fnt, text="x-axis")
    frame1.lab.3 <- tklabel(frame1, justify="center", font=fnt, text="y-axis")
    frame1.lab.4 <- tklabel(frame1, justify="center", font=fnt, text="z-axis")
    
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
    
    if(!is.na(vars[1])) {
        tkgrid.configure(frame1.lab.1, column=0, row=0)
        tkgrid.configure(frame1.lst.1, column=0, row=1, sticky="w e")
        tkgrid.configure(frame1.ysc.1, column=1, row=1, sticky="n s")
        tkgrid.configure(frame1.xsc.1, column=0, row=2, sticky="w e")
        for(i in cols) {
            if(!is.na(strptime(srvy.dat("data.raw")[1, make.names(i)], "%Y-%m-%d %H:%M:%OS"))) 
                tkinsert(frame1.lst.1, "end", i)
        }
    }
    
    tkgrid.configure(frame1.lab.2, column=2, row=0)
    tkgrid.configure(frame1.lst.2, column=2, row=1, sticky="w e")
    tkgrid.configure(frame1.ysc.2, column=3, row=1, sticky="n s")
    tkgrid.configure(frame1.xsc.2, column=2, row=2, sticky="w e")
    for(i in cols) tkinsert(frame1.lst.2, "end", i)
    
    tkgrid.configure(frame1.lab.3, column=4, row=0)
    tkgrid.configure(frame1.lst.3, column=4, row=1, sticky="w e")
    tkgrid.configure(frame1.ysc.3, column=5, row=1, sticky="n s")
    tkgrid.configure(frame1.xsc.3, column=4, row=2, sticky="w e")
    for(i in cols) tkinsert(frame1.lst.3, "end", i)
    
    tkgrid.configure(frame1.lab.4, column=6, row=0)
    tkgrid.configure(frame1.lst.4, column=6, row=1, sticky="w e")
    tkgrid.configure(frame1.ysc.4, column=7, row=1, sticky="n s")
    tkgrid.configure(frame1.xsc.4, column=6, row=2, sticky="w e")
    for(i in cols) tkinsert(frame1.lst.4, "end", i)
    
    idx <- rep(NA, 4)
    for(i in 1:4) 
        if(vars[i] %in% cols) idx[i] <- match(vars[i], cols) - 1
    
    if(!is.na(idx[1])) tkselection.set(frame1.lst.1, idx[1])
    if(!is.na(idx[2])) tkselection.set(frame1.lst.2, idx[2])
    if(!is.na(idx[3])) tkselection.set(frame1.lst.3, idx[3])
    if(!is.na(idx[4])) tkselection.set(frame1.lst.4, idx[4])
    
    tkgrid(frame1)
    tkpack(frame1, fill="both")
    
  # gui control
    
    tkfocus(tt2)
    tkgrab(tt2)
    tkbind(tt2, "<Destroy>", function() {gui.close()})
    tkwait.variable(tt2.done.var)
    
    tkgrab.release(tt2)
    tkdestroy(tt2)
}

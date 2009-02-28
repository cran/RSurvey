"tran.spatial" <- function(rasterFields, spatialFields=NULL, tt2=NULL) {
    
# additional functions (subroutines)
    
  # close GUI
    
    gui.close <- function() {
        if(as.integer(tclvalue(tt3.done.var)) != 0) return()
        
        num.chk <- function(n) {ifelse(length(n) == 0, NA, n)}
        
        num <- NULL
        num <- append(num, num.chk(as.integer(tkcurselection(frame1.lst.1)) + 1))
        num <- append(num, num.chk(as.integer(tkcurselection(frame1.lst.2)) + 1))
        
        spatialFields <<- rasterFields[num]
        
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
    tktitle(tt3) <- "Spatial Fields"
    
    if(!is.null(srvy.dat("icon")) && file.exists(srvy.dat("icon"))) 
        tkwm.iconbitmap(tt3, srvy.dat("icon"))
    
    tkwm.resizable(tt3, 0, 0)
    
  # frame 1 contains spatial fields identification
    
    frame1 <- tkframe(tt3, relief="flat", borderwidth=2) 
    
    frame1.lab.1 <- tklabel(frame1, justify="center", font=fnt, text="Local x-axis")
    frame1.lab.2 <- tklabel(frame1, justify="center", font=fnt, text="Local z-axis")
    
    frame1.lst.1 <- tklistbox(frame1, selectmode="browse", font=fnt, width=19, height=8, exportselection=FALSE)
    frame1.lst.2 <- tklistbox(frame1, selectmode="browse", font=fnt, width=19, height=8, exportselection=FALSE)
    
    frame1.ysc.1 <- tkscrollbar(frame1, orient="vertical")
    frame1.ysc.2 <- tkscrollbar(frame1, orient="vertical")
    
    frame1.xsc.1 <- tkscrollbar(frame1, orient="horizontal")
    frame1.xsc.2 <- tkscrollbar(frame1, orient="horizontal")
    
    tkconfigure(frame1.lst.1, background="white", 
        yscrollcommand=paste(.Tk.ID(frame1.ysc.1), "set"), 
        xscrollcommand=paste(.Tk.ID(frame1.xsc.1), "set"))
    tkconfigure(frame1.lst.2, background="white", 
        yscrollcommand=paste(.Tk.ID(frame1.ysc.2), "set"), 
        xscrollcommand=paste(.Tk.ID(frame1.xsc.2), "set"))
    
    tkconfigure(frame1.ysc.1, command=paste(.Tk.ID(frame1.lst.1), "yview"))
    tkconfigure(frame1.ysc.2, command=paste(.Tk.ID(frame1.lst.2), "yview"))
    
    tkconfigure(frame1.xsc.1, command=paste(.Tk.ID(frame1.lst.1), "xview"))
    tkconfigure(frame1.xsc.2, command=paste(.Tk.ID(frame1.lst.2), "xview"))
    
    clr.sel.1 <- function() tkselection.clear(frame1.lst.1, as.integer(tkcurselection(frame1.lst.1)))
    tkbind(frame1.lst.1, "<Control-ButtonRelease-1>", clr.sel.1)
    clr.sel.2 <- function() tkselection.clear(frame1.lst.2, as.integer(tkcurselection(frame1.lst.2)))
    tkbind(frame1.lst.2, "<Control-ButtonRelease-1>", clr.sel.2)
    
    tkgrid(frame1.lab.1, frame1.lab.2, pady=0)
    tkgrid(frame1.lst.1, frame1.ysc.1, frame1.xsc.1, 
           frame1.lst.2, frame1.ysc.2, frame1.xsc.2)
    
    tkgrid.configure(frame1.lab.1, column=0, row=0)
    tkgrid.configure(frame1.lab.2, column=2, row=0)
    
    tkgrid.configure(frame1.lst.1, column=0, row=1, sticky="w e")
    tkgrid.configure(frame1.ysc.1, column=1, row=1, sticky="n s")
    tkgrid.configure(frame1.xsc.1, column=0, row=2, sticky="w e")
    tkgrid.configure(frame1.lst.2, column=2, row=1, sticky="w e")
    tkgrid.configure(frame1.ysc.2, column=3, row=1, sticky="n s")
    tkgrid.configure(frame1.xsc.2, column=2, row=2, sticky="w e")
    
    for(i in rasterFields) {
        tkinsert(frame1.lst.1, "end", i)
        tkinsert(frame1.lst.2, "end", i)
    }
    
    idx <- rep(NA, 2)
    if(!is.null(spatialFields)) {
        for(i in 1:2) {
            if(spatialFields[i] %in% rasterFields) 
                idx[i] <- match(spatialFields[i], rasterFields) - 1
        }
    }
    
    if(!is.na(idx[1])) 
        tkselection.set(frame1.lst.1, idx[1])
    if(!is.na(idx[2])) 
        tkselection.set(frame1.lst.2, idx[2])
    
    tkgrid(frame1)
    
  # gui control
    
    tkfocus(tt3)
    tkgrab(tt3)
    tkbind(tt3, "<Destroy>", function() {gui.close()})
    tkwait.variable(tt3.done.var)
    
    tkgrab.release(tt3)
    tkdestroy(tt3)
    
    spatialFields
}

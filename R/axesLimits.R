"axesLimits" <- function(parent, old=NULL, addt=FALSE) {

# additional functions (subroutines)
    
    update.axes <- function() {
        d <- list()
        
        d$x1 <- as.numeric(tclvalue(x1.var))
        d$x2 <- as.numeric(tclvalue(x2.var))
        d$y1 <- as.numeric(tclvalue(y1.var))
        d$y2 <- as.numeric(tclvalue(y2.var))
        d$z1 <- as.numeric(tclvalue(z1.var))
        d$z2 <- as.numeric(tclvalue(z2.var))
        
        if(is.na(d$x1)) d$x1 <- d$x1.chk <- NULL else d$x1.chk <- as.integer(tclvalue(x1.chk.var))
        if(is.na(d$x2)) d$x2 <- d$x2.chk <- NULL else d$x2.chk <- as.integer(tclvalue(x2.chk.var))
        if(is.na(d$y1)) d$y1 <- d$y1.chk <- NULL else d$y1.chk <- as.integer(tclvalue(y1.chk.var))
        if(is.na(d$y2)) d$y2 <- d$y2.chk <- NULL else d$y2.chk <- as.integer(tclvalue(y2.chk.var))
        if(is.na(d$z1)) d$z1 <- d$z1.chk <- NULL else d$z1.chk <- as.integer(tclvalue(z1.chk.var))
        if(is.na(d$z2)) d$z2 <- d$z2.chk <- NULL else d$z2.chk <- as.integer(tclvalue(z2.chk.var))
        
        d$xlim <- c(if(!is.null(d$x1) && !d$x1.chk) d$x1 else NA,
                    if(!is.null(d$x2) && !d$x2.chk) d$x2 else NA)
        d$ylim <- c(if(!is.null(d$y1) && !d$y1.chk) d$y1 else NA,
                    if(!is.null(d$y2) && !d$y2.chk) d$y2 else NA)
        d$zlim <- c(if(!is.null(d$z1) && !d$z1.chk) d$z1 else NA,
                    if(!is.null(d$z2) && !d$z2.chk) d$z2 else NA)
        
        if(addt) {
            t1.str <- paste(tclvalue(t1d.var), " ", as.integer(tclvalue(t1h.var)), ":", 
                      as.integer(tclvalue(t1m.var)), ":", as.numeric(tclvalue(t1s.var)), sep="")
            t2.str <- paste(tclvalue(t2d.var), " ", as.integer(tclvalue(t2h.var)), ":", 
                      as.integer(tclvalue(t2m.var)), ":", as.numeric(tclvalue(t2s.var)), sep="")
            
            t1 <- strptime(t1.str, "%Y-%m-%d %H:%M:%OS")
            t2 <- strptime(t2.str, "%Y-%m-%d %H:%M:%OS")
            
            d$t1 <- if("POSIXt" %in% class(t1)) t1 else NA
            d$t2 <- if("POSIXt" %in% class(t2)) t2 else NA
            
            if(is.na(d$t1)) d$t1 <- d$t1.chk <- NULL else d$t1.chk <- as.integer(tclvalue(t1.chk.var))
            if(is.na(d$t2)) d$t2 <- d$t2.chk <- NULL else d$t2.chk <- as.integer(tclvalue(t2.chk.var))
            
            d$tlim <- strptime(c(if(!is.null(d$t1) && !d$t1.chk) t1.str else NA, 
                                 if(!is.null(d$t2) && !d$t2.chk) t2.str else NA), "%Y-%m-%d %H:%M:%OS")
        }
        
        d
    }
    
    
    
# main program
    
  # assign the variables linked to Tk widgets 
    
    if(is.null(old)) old <- list()
    
    x1.var <- if(is.null(old$x1)) tclVar() else tclVar(old$x1)
    x2.var <- if(is.null(old$x2)) tclVar() else tclVar(old$x2)
    y1.var <- if(is.null(old$y1)) tclVar() else tclVar(old$y1)
    y2.var <- if(is.null(old$y2)) tclVar() else tclVar(old$y2)
    z1.var <- if(is.null(old$z1)) tclVar() else tclVar(old$z1)
    z2.var <- if(is.null(old$z2)) tclVar() else tclVar(old$z2)
    
    x1.chk <- if(is.null(old$x1)) 1 else old$x1.chk
    x2.chk <- if(is.null(old$x2)) 1 else old$x2.chk
    y1.chk <- if(is.null(old$y1)) 1 else old$y1.chk
    y2.chk <- if(is.null(old$y2)) 1 else old$y2.chk
    z1.chk <- if(is.null(old$z1)) 1 else old$z1.chk
    z2.chk <- if(is.null(old$z2)) 1 else old$z2.chk
    
    x1.chk.var <- tclVar(x1.chk)
    x2.chk.var <- tclVar(x2.chk)
    y1.chk.var <- tclVar(y1.chk)
    y2.chk.var <- tclVar(y2.chk)
    z1.chk.var <- tclVar(z1.chk)
    z2.chk.var <- tclVar(z2.chk)
    
    x1.sta.var <- if(x1.chk) tclVar("disabled") else tclVar("normal")
    x2.sta.var <- if(x2.chk) tclVar("disabled") else tclVar("normal")
    y1.sta.var <- if(y1.chk) tclVar("disabled") else tclVar("normal")
    y2.sta.var <- if(y2.chk) tclVar("disabled") else tclVar("normal")
    z1.sta.var <- if(z1.chk) tclVar("disabled") else tclVar("normal")
    z2.sta.var <- if(z2.chk) tclVar("disabled") else tclVar("normal")
    
    if(addt) {
        if(is.null(old$t1)) {
            t1d.var <- tclVar()
            t1h.var <- tclVar()
            t1m.var <- tclVar()
            t1s.var <- tclVar()
        }
        else {
            t1d.var <- tclVar(format(old$t1, format="%Y-%m-%d"))
            t1h.var <- tclVar(as.character(as.integer(format(old$t1, format="%H" ))))
            t1m.var <- tclVar(as.character(as.integer(format(old$t1, format="%M" ))))
            t1s.var <- tclVar(as.character(as.numeric(format(old$t1, format="%OS"))))
        }
        if(is.null(old$t2)) {
            t2d.var <- tclVar()
            t2h.var <- tclVar()
            t2m.var <- tclVar()
            t2s.var <- tclVar()
        }
        else {
            t2d.var <- tclVar(format(old$t2, format="%Y-%m-%d"))
            t2h.var <- tclVar(as.character(as.integer(format(old$t2, format="%H" ))))
            t2m.var <- tclVar(as.character(as.integer(format(old$t2, format="%M" ))))
            t2s.var <- tclVar(as.character(as.numeric(format(old$t2, format="%OS"))))
        }
        
        t1.chk <- if(is.null(old$t1)) 1 else old$t1.chk
        t2.chk <- if(is.null(old$t2)) 1 else old$t2.chk
        
        t1.chk.var <- tclVar(t1.chk)
        t2.chk.var <- tclVar(t2.chk)
        
        t1.sta.var <- if(t1.chk) tclVar("disabled") else tclVar("normal")
        t2.sta.var <- if(t2.chk) tclVar("disabled") else tclVar("normal")
    }
    
    tt.done.var <- tclVar(0)
    
  # open gui
    
    tt <- tktoplevel(padx=5, pady=5)
    
    if(!missing(parent)) {
        tkwm.transient(tt, parent)
        tmp <- unlist(strsplit(as.character(tkwm.geometry(parent)), "\\+"))
        tkwm.geometry(tt, paste("+", as.integer(tmp[2]) + 25, "+", as.integer(tmp[3]) + 25, sep=""))
    }
    tktitle(tt) <- "Axes Limits"
    tkwm.resizable(tt, 0, 0)
    
  # notebook with tabs
    
    nb <- ttknotebook(tt)
    
  # frame 0 contains z-axis limits
    
    frame0 <- ttkframe(nb, relief="flat", padding=10, borderwidth=2)
    tkadd(nb, frame0, text="      x      ")
    
    frame0.lab.1.1 <- ttklabel(frame0, text="Minimum")
    frame0.lab.2.1 <- ttklabel(frame0, text="Maximum")
    
    frame0.ent.1.2 <- ttkentry(frame0, width=18, textvariable=x1.var, state=tclvalue(x1.sta.var))
    frame0.ent.2.2 <- ttkentry(frame0, width=18, textvariable=x2.var, state=tclvalue(x2.sta.var))
    
    tkbind(frame0.ent.1.2, "<KeyRelease>", function() {tclvalue(x1.var) <- keyEvent("real", tclvalue(x1.var))})
    tkbind(frame0.ent.2.2, "<KeyRelease>", function() {tclvalue(x2.var) <- keyEvent("real", tclvalue(x2.var))})
    
    frame0.chk.1.3 <- ttkcheckbutton(frame0, variable=x1.chk.var, text="Auto", 
                      command=function() {
                          tclvalue(x1.sta.var) <- if(as.integer(tclvalue(x1.chk.var))) "disabled" else "normal"
                          tkconfigure(frame0.ent.1.2, state=tclvalue(x1.sta.var))
                          tkfocus(frame0.ent.1.2)
                      })
    frame0.chk.2.3 <- ttkcheckbutton(frame0, variable=x2.chk.var, text="Auto", 
                      command=function() {
                          tclvalue(x2.sta.var) <- if(as.integer(tclvalue(x2.chk.var))) "disabled" else "normal"
                          tkconfigure(frame0.ent.2.2, state=tclvalue(x2.sta.var))
                          tkfocus(frame0.ent.2.2)
                      })
    
    tkgrid(frame0.lab.1.1, frame0.ent.1.2, frame0.chk.1.3, padx=1, pady=1)
    tkgrid(frame0.lab.2.1, frame0.ent.2.2, frame0.chk.2.3, padx=1, pady=1)
    
    tkgrid.configure(frame0.lab.1.1, sticky="e")
    tkgrid.configure(frame0.lab.2.1, sticky="e")
    
    tcl("grid", "anchor", frame0, "center")
    
  # frame 1 contains y-axis limits
    
    frame1 <- ttkframe(nb, relief="flat", padding=10, borderwidth=2)
    tkadd(nb, frame1, text="      y      ")
    
    frame1.lab.1.1 <- ttklabel(frame1, text="Minimum")
    frame1.lab.2.1 <- ttklabel(frame1, text="Maximum")
    
    frame1.ent.1.2 <- ttkentry(frame1, width=18, textvariable=y1.var, state=tclvalue(y1.sta.var))
    frame1.ent.2.2 <- ttkentry(frame1, width=18, textvariable=y2.var, state=tclvalue(y2.sta.var))
    
    tkbind(frame1.ent.1.2, "<KeyRelease>", function() {tclvalue(y1.var) <- keyEvent("real", tclvalue(y1.var))})
    tkbind(frame1.ent.2.2, "<KeyRelease>", function() {tclvalue(y2.var) <- keyEvent("real", tclvalue(y2.var))})
    
    frame1.chk.1.3 <- ttkcheckbutton(frame1, variable=y1.chk.var, text="Auto", 
                      command=function() {
                          tclvalue(y1.sta.var) <- if(as.integer(tclvalue(y1.chk.var))) "disabled" else "normal"
                          tkconfigure(frame1.ent.1.2, state=tclvalue(y1.sta.var))
                          tkfocus(frame1.ent.1.2)
                      })
    frame1.chk.2.3 <- ttkcheckbutton(frame1, variable=y2.chk.var, text="Auto", 
                      command=function() {
                          tclvalue(y2.sta.var) <- if(as.integer(tclvalue(y2.chk.var))) "disabled" else "normal"
                          tkconfigure(frame1.ent.2.2, state=tclvalue(y2.sta.var))
                          tkfocus(frame1.ent.2.2)
                      })
    
    tkgrid(frame1.lab.1.1, frame1.ent.1.2, frame1.chk.1.3, padx=1, pady=1)
    tkgrid(frame1.lab.2.1, frame1.ent.2.2, frame1.chk.2.3, padx=1, pady=1)
    
    tkgrid.configure(frame1.lab.1.1, sticky="e")
    tkgrid.configure(frame1.lab.2.1, sticky="e")
    
    tcl("grid", "anchor", frame1, "center")
    
  # frame 2 contains z-axis limits
    
    frame2 <- ttkframe(nb, relief="flat", padding=10, borderwidth=2)
    tkadd(nb, frame2, text="      z      ")
    
    frame2.lab.1.1 <- ttklabel(frame2, text="Minimum")
    frame2.lab.2.1 <- ttklabel(frame2, text="Maximum")
    
    frame2.ent.1.2 <- ttkentry(frame2, width=18, textvariable=z1.var, state=tclvalue(z1.sta.var))
    frame2.ent.2.2 <- ttkentry(frame2, width=18, textvariable=z2.var, state=tclvalue(z2.sta.var))
    
    tkbind(frame2.ent.1.2, "<KeyRelease>", function() {tclvalue(z1.var) <- keyEvent("real", tclvalue(z1.var))})
    tkbind(frame2.ent.2.2, "<KeyRelease>", function() {tclvalue(z2.var) <- keyEvent("real", tclvalue(z2.var))})
    
    frame2.chk.1.3 <- ttkcheckbutton(frame2, variable=z1.chk.var, text="Auto", 
                      command=function() {
                          tclvalue(z1.sta.var) <- if(as.integer(tclvalue(z1.chk.var))) "disabled" else "normal"
                          tkconfigure(frame2.ent.1.2, state=tclvalue(z1.sta.var))
                          tkfocus(frame2.ent.1.2)
                      })
    frame2.chk.2.3 <- ttkcheckbutton(frame2, variable=z2.chk.var, text="Auto", 
                      command=function() {
                          tclvalue(z2.sta.var) <- if(as.integer(tclvalue(z2.chk.var))) "disabled" else "normal"
                          tkconfigure(frame2.ent.2.2, state=tclvalue(z2.sta.var))
                          tkfocus(frame2.ent.2.2)
                      })
    
    tkgrid(frame2.lab.1.1, frame2.ent.1.2, frame2.chk.1.3, padx=1, pady=1)
    tkgrid(frame2.lab.2.1, frame2.ent.2.2, frame2.chk.2.3, padx=1, pady=1)
    
    tkgrid.configure(frame2.lab.1.1, sticky="e")
    tkgrid.configure(frame2.lab.2.1, sticky="e")
    
    tcl("grid", "anchor", frame2, "center")
    
  # frame 3 contains t-axis limits
    
    if(addt) {
        frame3 <- ttkframe(nb, relief="flat", padding=10, borderwidth=2)
        tkadd(nb, frame3, text="      t      ")
        
        frame3.lab.1.1 <- ttklabel(frame3, text="Minimum")
        frame3.lab.2.1 <- ttklabel(frame3, text="Maximum")
        
        frame3.ent.1.2 <- ttkentry(frame3, width=11, textvariable=t1d.var, state=tclvalue(t1.sta.var))
        frame3.ent.1.3 <- ttkentry(frame3, width= 3, textvariable=t1h.var, state=tclvalue(t1.sta.var))
        frame3.ent.1.4 <- ttkentry(frame3, width= 3, textvariable=t1m.var, state=tclvalue(t1.sta.var))
        frame3.ent.1.5 <- ttkentry(frame3, width= 5, textvariable=t1s.var, state=tclvalue(t1.sta.var))
        
        frame3.ent.2.2 <- ttkentry(frame3, width=11, textvariable=t2d.var, state=tclvalue(t2.sta.var))
        frame3.ent.2.3 <- ttkentry(frame3, width= 3, textvariable=t2h.var, state=tclvalue(t2.sta.var))
        frame3.ent.2.4 <- ttkentry(frame3, width= 3, textvariable=t2m.var, state=tclvalue(t2.sta.var))
        frame3.ent.2.5 <- ttkentry(frame3, width= 5, textvariable=t2s.var, state=tclvalue(t2.sta.var))
        
        tkbind(frame3.ent.1.2, "<KeyRelease>", function() {tclvalue(t1d.var) <- keyEvent("date",   tclvalue(t1d.var))})
        tkbind(frame3.ent.1.3, "<KeyRelease>", function() {tclvalue(t1h.var) <- keyEvent("hour",   tclvalue(t1h.var))})
        tkbind(frame3.ent.1.4, "<KeyRelease>", function() {tclvalue(t1m.var) <- keyEvent("minute", tclvalue(t1m.var))})
        tkbind(frame3.ent.1.5, "<KeyRelease>", function() {tclvalue(t1s.var) <- keyEvent("second", tclvalue(t1s.var))})
        
        tkbind(frame3.ent.2.2, "<KeyRelease>", function() {tclvalue(t2d.var) <- keyEvent("date",   tclvalue(t2d.var))})
        tkbind(frame3.ent.2.3, "<KeyRelease>", function() {tclvalue(t2h.var) <- keyEvent("hour",   tclvalue(t2h.var))})
        tkbind(frame3.ent.2.4, "<KeyRelease>", function() {tclvalue(t2m.var) <- keyEvent("minute", tclvalue(t2m.var))})
        tkbind(frame3.ent.2.5, "<KeyRelease>", function() {tclvalue(t2s.var) <- keyEvent("second", tclvalue(t2s.var))})
        
        frame3.chk.1.6 <- ttkcheckbutton(frame3, variable=t1.chk.var, text="Auto", 
                          command=function() {
                              tclvalue(t1.sta.var) <- if(as.integer(tclvalue(t1.chk.var))) "disabled" else "normal"
                              tkconfigure(frame3.ent.1.2, state=tclvalue(t1.sta.var))
                              tkconfigure(frame3.ent.1.3, state=tclvalue(t1.sta.var))
                              tkconfigure(frame3.ent.1.4, state=tclvalue(t1.sta.var))
                              tkconfigure(frame3.ent.1.5, state=tclvalue(t1.sta.var))
                              tkfocus(frame3.ent.1.2)
                          })
        frame3.chk.2.6 <- ttkcheckbutton(frame3, variable=t2.chk.var, text="Auto", 
                          command=function() {
                              tclvalue(t2.sta.var) <- if(as.integer(tclvalue(t2.chk.var))) "disabled" else "normal"
                              tkconfigure(frame3.ent.2.2, state=tclvalue(t2.sta.var))
                              tkconfigure(frame3.ent.2.3, state=tclvalue(t2.sta.var))
                              tkconfigure(frame3.ent.2.4, state=tclvalue(t2.sta.var))
                              tkconfigure(frame3.ent.2.5, state=tclvalue(t2.sta.var))
                              tkfocus(frame3.ent.2.2)
                          })
        
        tkgrid(frame3.lab.1.1, frame3.ent.1.2, frame3.ent.1.3, frame3.ent.1.4, frame3.ent.1.5, frame3.chk.1.6, padx=1, pady=1)
        tkgrid(frame3.lab.2.1, frame3.ent.2.2, frame3.ent.2.3, frame3.ent.2.4, frame3.ent.2.5, frame3.chk.2.6, padx=1, pady=1)
        
        tkgrid.configure(frame3.lab.1.1, sticky="e")
        tkgrid.configure(frame3.lab.2.1, sticky="e")
        
        tcl("grid", "anchor", frame3, "center")
    }
    
  # insert notebook
    
    tkpack(nb)
    
  # gui control
    
    tkfocus(tt)
    tkgrab(tt)
    tkbind(tt, "<Destroy>", function() tclvalue(tt.done.var) <- 1)
    tkwait.variable(tt.done.var)
    
    new <- update.axes()
    
    tkgrab.release(tt)
    tkdestroy(tt)
    
    new
}

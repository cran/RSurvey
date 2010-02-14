"srvy.vars" <- function(cols.name, cols.class, vars.old, parent) {
    
# additional functions (subroutines)
    
  # close GUI
    
    gui.close <- function() {
        if(as.integer(tclvalue(tt.done.var)) != 0) return()
        
        idx.11 <- as.integer(tcl(frame0.box.1, "current"))
        idx.21 <- as.integer(tcl(frame1.box.1, "current")) + 1
        idx.22 <- as.integer(tcl(frame1.box.2, "current")) + 1
        idx.23 <- as.integer(tcl(frame1.box.3, "current"))
        idx.31 <- as.integer(tcl(frame2.box.1, "current"))
        idx.32 <- as.integer(tcl(frame2.box.2, "current"))
        idx.33 <- as.integer(tcl(frame2.box.3, "current"))
        
        vars$t  <<- if(idx.11 <= 0) NULL else idx.dt[idx.11]
        vars$x  <<- if(idx.21 <= 0) NULL else idx.no[idx.21]
        vars$y  <<- if(idx.22 <= 0) NULL else idx.no[idx.22]
        vars$z  <<- if(idx.23 <= 0) NULL else idx.no[idx.23]
        vars$vx <<- if(idx.31 <= 0) NULL else idx.no[idx.31]
        vars$vy <<- if(idx.32 <= 0) NULL else idx.no[idx.32]
        vars$vz <<- if(idx.33 <= 0) NULL else idx.no[idx.33]
        
        tclvalue(tt.done.var) <- 1
    }
    
# main program
    
  # assign variables
    
    n <- length(cols.name)
    
    idx.dt <- (1:n)[cols.class == "POSIXt"]
    idx.no <- (1:n)[cols.class == "numeric"]
    
    val.dt <- c("", cols.name[idx.dt])
    val.no <- c("", cols.name[idx.no])
    
    nd <- length(idx.dt)
    nn <- length(idx.no)
    
    vars <- list()
    
    tt.done.var <- tclVar(0)
    
  # open gui
    
    tt <- tktoplevel(padx=5, pady=5)
    if(!missing(parent)) {
        tkwm.transient(tt, parent)
        tmp <- unlist(strsplit(as.character(tkwm.geometry(parent)), "\\+"))
        tkwm.geometry(tt, paste("+", as.integer(tmp[2]) + 25, "+", as.integer(tmp[3]) + 25, sep=""))
    }
    tktitle(tt) <- "Variables"
    
    tkwm.resizable(tt, 0, 0)
    
  # frame 0 contains temporal field identification
    
    frame0 <- ttklabelframe(tt, relief="flat", borderwidth=5, padding=3, text="Temporal Field")
    
    frame0.lab.1 <- ttklabel(frame0, justify="center", text="Date/Time")
    
    frame0.box.1 <- ttkcombobox(frame0, state="readonly", width=25, values=val.dt)
    
    if(!is.null(vars.old$t)) tcl(frame0.box.1, "current", (1:nd)[vars.old$t == idx.dt])
    
    tkgrid(frame0.lab.1, frame0.box.1, padx=1, pady=3)
    
    tkgrid.configure(frame0.lab.1, sticky="e")
    
    tcl("grid", "anchor", frame0, "ne")
    tkpack(frame0, fill="both", ipadx=2, ipady=2, padx=2, pady=2)
    
  # frame 1 contains spatial field identification
    
    frame1 <- ttklabelframe(tt, relief="flat", borderwidth=5, padding=3, text="Spatial Fields")
    
    frame1.lab.1 <- ttklabel(frame1, justify="center", text="x-axis")
    frame1.lab.2 <- ttklabel(frame1, justify="center", text="y-axis")
    frame1.lab.3 <- ttklabel(frame1, justify="center", text="z-axis")
    
    frame1.box.1 <- ttkcombobox(frame1, state="readonly", width=25, values=val.no[-1])
    frame1.box.2 <- ttkcombobox(frame1, state="readonly", width=25, values=val.no[-1])
    frame1.box.3 <- ttkcombobox(frame1, state="readonly", width=25, values=val.no)
    
    if(!is.null(vars.old$x)) tcl(frame1.box.1, "current", (1:nn)[vars.old$x == idx.no] - 1)
    if(!is.null(vars.old$y)) tcl(frame1.box.2, "current", (1:nn)[vars.old$y == idx.no] - 1)
    if(!is.null(vars.old$z)) tcl(frame1.box.3, "current", (1:nn)[vars.old$z == idx.no])
    
    tkgrid(frame1.lab.1, frame1.box.1, padx=1, pady=3)
    tkgrid(frame1.lab.2, frame1.box.2, padx=1, pady=3)
    tkgrid(frame1.lab.3, frame1.box.3, padx=1, pady=3)
    
    tkgrid.configure(frame1.lab.1, sticky="e")
    tkgrid.configure(frame1.lab.2, sticky="e")
    tkgrid.configure(frame1.lab.3, sticky="e")
    
    tcl("grid", "anchor", frame1, "ne")
    tkpack(frame1, fill="both", ipadx=2, ipady=2, padx=2, pady=2)
    
  # frame 2 contains velocity vector field identification
    
    frame2 <- ttklabelframe(tt, relief="flat", borderwidth=5, padding=3, text="Vector Components")
    
    frame2.lab.1 <- ttklabel(frame2, justify="center", text="x-axis")
    frame2.lab.2 <- ttklabel(frame2, justify="center", text="y-axis")
    frame2.lab.3 <- ttklabel(frame2, justify="center", text="z-axis")
    
    frame2.box.1 <- ttkcombobox(frame2, state="readonly", width=25, values=val.no)
    frame2.box.2 <- ttkcombobox(frame2, state="readonly", width=25, values=val.no)
    frame2.box.3 <- ttkcombobox(frame2, state="readonly", width=25, values=val.no)
    
    if(!is.null(vars.old$vx)) tcl(frame2.box.1, "current", (1:nn)[vars.old$vx == idx.no])
    if(!is.null(vars.old$vy)) tcl(frame2.box.2, "current", (1:nn)[vars.old$vy == idx.no])
    if(!is.null(vars.old$vz)) tcl(frame2.box.3, "current", (1:nn)[vars.old$vz == idx.no])
    
    tkgrid(frame2.lab.1, frame2.box.1, padx=1, pady=3)
    tkgrid(frame2.lab.2, frame2.box.2, padx=1, pady=3)
    tkgrid(frame2.lab.3, frame2.box.3, padx=1, pady=3)
    
    tkgrid.configure(frame2.lab.1, sticky="e")
    tkgrid.configure(frame2.lab.2, sticky="e")
    tkgrid.configure(frame2.lab.3, sticky="e")
    
    tcl("grid", "anchor", frame2, "ne")
    tkpack(frame2, fill="both", ipadx=2, ipady=2, padx=2, pady=2)
    
  # gui control
    
    tkfocus(tt)
    tkgrab(tt)
    tkbind(tt, "<Destroy>", function() {gui.close()})
    tkwait.variable(tt.done.var)
    
    tkgrab.release(tt)
    tkdestroy(tt)
    
    vars
}

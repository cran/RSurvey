"polyAutocrop" <- function(dat, xlab=NULL, ylab=NULL, zlab=NULL, parent) {
    
# additional functions (subroutines)
    
    polymesh <- function() {
        
        maxLen <- as.numeric(tclvalue(maxLen.var))
        maxItr <- as.integer(tclvalue(maxItr.var))
        
        if(is.na(maxLen) || is.na(maxItr)) return()
        
        ply <<- autocrop(mesh, maxLen, maxItr)
        
        if(is.null(ply)) {
            msg <- "Autocrop failed, try increasing the max. outer arc length."
            tkmessageBox(icon="warning", message=msg, title="Warning", type="ok", parent=tt)
        }
        else {
            if(!as.integer(tclvalue(tt.plot.var))) {
                init.plot(dat)
                tclvalue(tt.plot.var) <- 1
            }
            plot(ply, add=TRUE, poly.args=list(border="black"))
        }
        
        tkfocus(tt)
    }
    
  # initialize plot
    
    init.plot <- function(dat) {
        plotSurvey2d(dat, type="p", xlab=xlab, ylab=ylab, zlab=zlab, asp=asp, csi=csi, 
            width=width, nlevels=nlevels, cex=cex, rkey=rkey)
    }
    
  # axis label
    
    axis.label <- function(cols, variable) {
        if(is.null(variable) || is.null(cols[[variable]])) return(NULL)
        paste(cols[[variable]]$name, " (", cols[[variable]]$unit, ")", sep="")
    }
    
    
    
# main program
    
  # general plotting parameters
    
    asp     <- srvy.dat("asp.yx")
    csi     <- srvy.dat("csi")
    width   <- srvy.dat("width")
    nlevels <- srvy.dat("nlevels")
    cex     <- srvy.dat("cex.pts")
    rkey    <- srvy.dat("rkey")
    
  # initialize the polygon and index
    
    ply <- NULL
    rtn <- NULL
    
  # construct mesh
    
    mesh <- tri.mesh(dat$x, dat$y, duplicate="remove")
    
  # convex hull and maximum outer arc length
    
    hull <- convex.hull(mesh)
    x1 <- hull$x
    y1 <- hull$y
    x2 <- c(x1[-1], x1[1])
    y2 <- c(y1[-1], y1[1])
    len <- max(sqrt((x2 - x1)^2 + (y2 - y1)^2))
    
  # assign the variables linked to Tk widgets
    
    maxLen.var <- tclVar(format(len, nsmall=3))
    maxItr.var <- tclVar(100000)
    
    tt.plot.var <- tclVar(0)
    
    tt.done.var <- tclVar(0)
    
  # open gui
    
    tt <- tktoplevel(padx=5, pady=5)
    if(!missing(parent)) {
        tkwm.transient(tt, parent)
        tmp <- unlist(strsplit(as.character(tkwm.geometry(parent)), "\\+"))
        tkwm.geometry(tt, paste("+", as.integer(tmp[2]) + 25, "+", as.integer(tmp[3]) + 25, sep=""))
    }
    tktitle(tt) <- "Autocrop"
    tkwm.resizable(tt, 0, 0)
    
  # frame 1 contains parameters
    
    frame1 <- ttkframe(tt, relief="flat", borderwidth=2)
    
    frame1.lab.1.1 <- ttklabel(frame1, text="Maximum outer arc length")
    frame1.lab.2.1 <- ttklabel(frame1, text="Max. number of iterations")
    
    frame1.ent.1.2 <- ttkentry(frame1, width=10, textvariable=maxLen.var)
    frame1.ent.2.2 <- ttkentry(frame1, width=10, textvariable=maxItr.var)
    
    tkbind(frame1.ent.1.2, "<KeyRelease>", function() {tclvalue(maxLen.var) <- keyEvent("real", tclvalue(maxLen.var), rm.data=FALSE)})
    tkbind(frame1.ent.1.2, "<KeyRelease>", function() {tclvalue(maxItr.var) <- keyEvent("integer", tclvalue(maxItr.var), rm.data=FALSE)})
    
    tkgrid(frame1.lab.1.1, frame1.ent.1.2, padx=2, pady=2)
    tkgrid(frame1.lab.2.1, frame1.ent.2.2, padx=2, pady=2)
    
    tkgrid.configure(frame1.lab.1.1, sticky="e")
    tkgrid.configure(frame1.lab.2.1, sticky="e")
    
    tcl("grid", "anchor", frame1, "center")
    
    tkpack(frame1, fill="both")
    
  # frame 2 contains buttons
    
    frame2 <- ttkframe(tt, relief="flat", borderwidth=2)
    
    frame2.but.2.1 <- ttkbutton(frame2, width=10, text="BUILD",  command=polymesh)
    
    frame2.but.2.2 <- ttkbutton(frame2, width=10, text="SAVE", 
                          command=function() {
                              if(class(ply) == "gpc.poly") {
                                  rtn <<- ply
                                  plot(rtn, add=TRUE, poly.args=list(border="red"))
                                  
                              }
                          }
                      )
    
    tkgrid(frame2.but.2.1, frame2.but.2.2, padx=2)
    
    tcl("grid", "anchor", frame2, "center")
    
    tkpack(frame2, padx=2, pady=5)
    
  # gui control
    
    tkfocus(tt)
    tkgrab(tt)
    tkbind(tt, "<Destroy>", function() {tclvalue(tt.done.var) <- 1})
    tkwait.variable(tt.done.var)
    tkgrab.release(tt)
    tkdestroy(tt)
    
    rtn
}

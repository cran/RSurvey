"srvy" <- function() {
    
# additional functions (subroutines)
    
  # update all GUI parameters
    
    gui.update <- function() {
        data.raw <- srvy.dat("data.raw")
        if(!is.null(data.raw) && length(data.raw) != 0) {
            dat <- NULL
            vars <- srvy.dat("vars")
            dat$datetime <- data.raw[[make.names(vars[1])]]
            dat$x        <- data.raw[[make.names(vars[2])]]
            dat$y        <- data.raw[[make.names(vars[3])]]
            dat$z        <- data.raw[[make.names(vars[4])]]
            
            if(is.null(dat$datetime)) {
                tclvalue(sd.var) <- ""
                tclvalue(ed.var) <- ""
                tclvalue(sh.var) <- ""
                tclvalue(eh.var) <- ""
                tclvalue(sm.var) <- ""
                tclvalue(em.var) <- ""
                tclvalue(ss.var) <- ""
                tclvalue(es.var) <- ""
             }
             else {
                range.dat <- strptime(dat$datetime[c(1, length(dat$datetime))], "%Y-%m-%d %H:%M:%OS")
                
                if(tclvalue(sd.var) == "") tclvalue(sd.var) <- format(range.dat[1], format="%Y-%m-%d")
                if(tclvalue(ed.var) == "") tclvalue(ed.var) <- format(range.dat[2], format="%Y-%m-%d")
                if(tclvalue(sh.var) == "") tclvalue(sh.var) <- format(range.dat[1], format="%H")
                if(tclvalue(eh.var) == "") tclvalue(eh.var) <- format(range.dat[2], format="%H")
                if(tclvalue(sm.var) == "") tclvalue(sm.var) <- format(range.dat[1], format="%M")
                if(tclvalue(em.var) == "") tclvalue(em.var) <- format(range.dat[2], format="%M")
                if(tclvalue(ss.var) == "") tclvalue(ss.var) <- format(range.dat[1], format="%OS3")
                if(tclvalue(es.var) == "") tclvalue(es.var) <- format(range.dat[2], format="%OS3")
            }
            if(!is.null(dat$datetime)) {
                srvy.dat("sd", tclvalue(sd.var))
                srvy.dat("ed", tclvalue(ed.var))
                srvy.dat("sh", as.integer(tclvalue(sh.var)))
                srvy.dat("eh", as.integer(tclvalue(eh.var)))
                srvy.dat("sm", as.integer(tclvalue(sm.var)))
                srvy.dat("em", as.integer(tclvalue(em.var)))
                srvy.dat("ss", as.numeric(tclvalue(ss.var)))
                srvy.dat("es", as.numeric(tclvalue(es.var)))
            }
            
            if(tclvalue(min.x.var) == "") tclvalue(min.x.var) <- min(dat$x)
            if(tclvalue(max.x.var) == "") tclvalue(max.x.var) <- max(dat$x)
            if(tclvalue(min.y.var) == "") tclvalue(min.y.var) <- min(dat$y)
            if(tclvalue(max.y.var) == "") tclvalue(max.y.var) <- max(dat$y)
            
            srvy.dat("min.y", as.numeric(tclvalue(min.y.var)))
            srvy.dat("min.x", as.numeric(tclvalue(min.x.var)))
            srvy.dat("max.x", as.numeric(tclvalue(max.x.var)))
            srvy.dat("max.y", as.numeric(tclvalue(max.y.var)))
            
            hld <- as.numeric(tclvalue(min.z.var))
            srvy.dat("min.z", if(is.na(hld)) NULL else hld)
            hld <- as.numeric(tclvalue(max.z.var))
            srvy.dat("max.z", if(is.na(hld)) NULL else hld)
            hld <- as.numeric(tclvalue(off.z.var))
            srvy.dat("off.z", if(is.na(hld)) NULL else hld)
            hld <- as.numeric(tclvalue(off.t.var))
            srvy.dat("off.t", if(is.na(hld)) NULL else hld)
        }
    }
    
  # identify gui icon
    
    getIcon <- function() {
        path <- ifelse("package:RSurvey" %in% search(), system.file(package="RSurvey"), "./inst")
        srvy.dat("icon", NULL)
        if(.Platform$OS.type == "windows") {
            f <- paste(path, "/RSurvey.ico", sep="")
            if(file.exists(f)) 
                srvy.dat("icon", paste(path, "/RSurvey.ico", sep=""))
        }
    }
    
  # restore entry widgets
    
    gui.restore <- function(vars) {
        for(i in vars) {
            hld <- srvy.dat(i)
            if(is.null(hld)) next()
            if(is.na(hld)) next()
            
            tmp <- eval(parse(text=paste(i, ".var", sep="")))
            tclvalue(tmp) <- as.character(hld)
        }
    }
    
  # close GUI
    
    gui.close <- function() {
        if(as.integer(tclvalue(tt1.done.var)) != 0) return() 
        device.close()
        gui.update()
        
        tmp <- unlist(strsplit(as.character(tkwm.geometry(tt1)), "\\+"))
        srvy.dat("win.loc", paste("+", as.integer(tmp[2]), "+", as.integer(tmp[3]), sep=""))
        
        if(!(is.null(dev.list()))) 
            for(i in 1:length(dev.list())) dev.off()
        
        win <- ls(envir=.TkRoot$env, all=TRUE)
        num <- sort(suppressWarnings(as.integer(substr(win, 2, nchar(win)))), decreasing=TRUE)
        tmp <- paste(".", num, sep="")
        win <- tmp[.Tk.ID(tt1) != tmp]
        if(length(win) > 0) 
            for(i in 1:length(win)) tcl("destroy", win[i])
        
        tclvalue(tt1.done.var) <- 1
        tkdestroy(tt1)
        rm(tt1, pos=".GlobalEnv")
    } 
    
  # open R binary data file
    
    proj.new <- function() {
        ans <- clear.objects("project")
        if(ans == "cancel") return()
    }
    
  # open R binary data file
    
    proj.open <- function() {
        f <- getFile(cmd="open", exts="rda")
        if(is.null(f)) return()
        
        ans <- clear.objects("project")
        if(ans != "cancel") {
            load(file=f$path, envir=.GlobalEnv)
            srvy.dat("proj.file", f$path)
            getIcon()
            gui.restore(vars)
            gui.update()
        }
        tkfocus(tt1)
    }
    
  # save R binary data file
    
    proj.save <- function() {
        if(!is.null(srvy.dat("proj.file"))) {
            if(file.access(srvy.dat("proj.file"), mode=0) != 0) 
                srvy.dat("proj.file", NULL)
        }
        if(is.null(srvy.dat("proj.file"))) {
            f <- getFile(cmd="save", exts="rda")
            if(!is.null(f)) 
                srvy.dat("proj.file", f$path)
        }
        if(!is.null(srvy.dat("proj.file"))) {
            tmp <- unlist(strsplit(srvy.dat("proj.file"), "/"))
            hld.path <- tmp[1:(length(tmp) - 1)]
            srvy.dat("default.dir", paste(hld.path, collapse="/"))
            gui.update()
            save.objs <- c("srvy.dat", "tran.dat")
            save(list=save.objs, file=srvy.dat("proj.file"), compress=TRUE)
        }
        tkfocus(tt1)
    }
    
  # save a new R binary data file
    
    proj.save.as <- function() {
        srvy.dat("proj.file", NULL)
        proj.save()
        tkfocus(tt1)
    }
    
  # clear objects
    
    clear.objects <- function(type) {
        
        hld.var <- c("default.dir", "win.loc", "csi", "grad.tol", "time.gap", 
                   "grid.res", "depth", "date.fmt", "yx.ratio", "zx.ratio", 
                   "n.levels", "win.width", "icon", "ver", "font.gui")
        
        if(type == "data") {
            ans <- ifelse(is.null(srvy.dat("data.file")), "ok", 
                   as.character(tkmessageBox(icon="question", 
                   message=paste("This action will delete existing data?", sep=""), 
                   title="Warning", type="okcancel", parent=tt1)))
            if(ans == "cancel") return(ans)
            hld.var <- c(hld.var, "poly", "proj.file")
            srvy.dat("data.raw", NULL)
            srvy.dat("data.mod", NULL)
            srvy.dat("data.tin", NULL)
        }
        
        if(type == "project") {
            ans <- ifelse(is.null(srvy.dat("proj.file")), "ok", 
                   as.character(tkmessageBox(icon="question", 
                   message=paste("Save the existing project?", sep=""), 
                   title="Warning", type="yesnocancel", parent=tt1)))
            if(ans == "cancel") 
                return(ans)
            if(ans == "yes") 
                proj.save()
            
            if(exists("tt2", where=".GlobalEnv")) 
                tkdestroy(get("tt2", env=.GlobalEnv))
                
            tran.dat(clearAll=TRUE)
        }
        
        hold.par <- srvy.dat()
        srvy.dat(clearAll=TRUE)
        
        for(i in hld.var) 
            srvy.dat(i, hold.par[[i]])
        for(i in paste(vars, ".var", sep="")) {
            tmp <- eval(parse(text=i))
            tclvalue(tmp) <- ""
        }
        
        ans
    }
    
  # export polygon data to file
    
    polyExport <- function() {
        if(is.null(srvy.dat("poly"))) return()
        
        f <- getFile(cmd="save", exts="txt")
        if(is.null(f)) return()
        
        poly <- srvy.dat("poly")
        outfile <- file(f$path)
        
        open(outfile, open="w")
        num.contours <- length(poly@pts)
        cat(num.contours, "\n", file=outfile)
        for(i in 1:num.contours) {
            m <- as(poly[i], "matrix")
            cat(nrow(m), "\n", file=outfile, append=TRUE)
            if(!is(poly, "gpc.poly.nohole")) 
                cat(as.numeric(poly@pts[[i]]$hole), "\n", file=outfile, append=TRUE)
            write(format(t(m), nsmall=12), file=outfile, ncolumns=2, append=TRUE)
        }
        close(outfile)
        tkfocus(tt1)
    }
    
  # polygon construction
    
    polyConstruct <- function() {
        
        data.raw <- srvy.dat("data.raw")
        
        if(is.null(srvy.dat("data.file"))) 
            stop(call.=FALSE, "No data is available.")
        
        gui.update()
        srvy.process()
        
        txt <- paste("After the plot has been created, use the mouse to identify the",
               "vertices of the polygon. The identification process can be terminated",
               "by clicking the second button and selecting [Stop] from the menu,",
               "or from the [Stop] menu on the graphics window.", sep="\n")
        tkmessageBox(icon="info", message=txt, title="Polygon Construction", parent=tt1)
        
        tkconfigure(tt1, cursor="watch")
        
        srvy.process(const.tin=TRUE)
        plotSurface2d(const.poly=TRUE)
        
        srvy.dat("data.mod", NULL)
        srvy.dat("data.tin", NULL)
        
        tkconfigure(tt1, cursor="arrow")
        tkfocus(tt1)
    }
    
  # polygon delete
    
    polyClear <- function() {
        if(is.null(srvy.dat("poly"))) return()
        txt <- "Delete existing polygon?"
        ans <- as.character(tkmessageBox(icon="question", message=txt, 
               title="Clear", parent=tt1, type="yesno"))
        if(ans == "yes") {
            srvy.dat("poly", NULL)
            srvy.dat("data.mod", NULL)
            srvy.dat("data.tin", NULL)
        }
    }
    
  # close R and RGL graphic devices
    
    device.close <- function() {
        graphics.off()
        while(rgl.cur() != 0) 
            rgl.close()
    }
    
  # save R graphic devices
    
    device.save.r <- function() {
        if(is.null(dev.list())) return()
        
        f <- getFile(cmd="save", exts=c("png", "jpg", "ps", "pdf", "bmp"))
        if(is.null(f)) return()
        
        f$path <- substr(f$path, 1, nchar(f$path) - (nchar(f$ext) + 1))
        savePlot(filename=f$path, type=f$ext)
    }
    
  # save RGL graphic devices
    
    device.save.rgl <- function() {
        if(rgl.cur() == 0) return()
        
        f <- getFile(cmd="save", exts=c("png", "ps", "eps", "tex", "pdf"))
        if(is.null(f)) return()
        
        if(f$ext == "png") 
            rgl.snapshot(filename=f$path)
        else 
            rgl.postscript(filename=f$path, fmt=f$ext)
    }
    
  # about package
    
    about <- function() {
        con <- ifelse(file.access(paste(getwd(), "/DESCRIPTION", sep=""), mode=0) == 0, "DESCRIPTION", 
               paste(.path.package(package="RSurvey", quiet=FALSE), "/DESCRIPTION", sep=""))
        txt <- paste(readLines(con, n=-1), collapse="\n")
        tkmessageBox(icon="info", message=txt, title="About RSurvey", parent=tt1)
    }   
    
    
    
# main program
    
  # establish working directory
    
    if(is.null(srvy.dat("default.dir"))) {
        tmp <- ifelse("package:RSurvey" %in% search(), system.file("RSurvey-ex", package="RSurvey"), getwd())
        srvy.dat("default.dir", tmp)
    }
    
  # exit if older instance of the package is active
    
    if(exists("tt1", where=".GlobalEnv")) {
        tkwm.deiconify(tt1)
        stop("An older instance of RSurvey is active and will be brought forward.")
    }
    
  # determine graphics parameters
    
    x11(pointsize=10)
    srvy.dat("csi", par("csi"))
    graphics.off()
    
    options(digits.secs=3)
    
  # load required R packages
    
    loadPackages()
    
  # GUI font
    
    fnt <- srvy.dat("font.gui")
    
  # determine the loaction of the GUI icon
    
    getIcon()
    
  # assign the variables linked to Tk widgets
    
    vars <- c("sd", "sh", "sm", "ss", "ed", "eh", "em", "es", "max.y", "min.y", 
              "min.x", "max.x", "min.z", "max.z", "off.z", "off.t")
    
    sd.var    <- tclVar()
    sh.var    <- tclVar()
    sm.var    <- tclVar()
    ss.var    <- tclVar()
    ed.var    <- tclVar()
    eh.var    <- tclVar()
    em.var    <- tclVar()
    es.var    <- tclVar()
    max.y.var <- tclVar()
    min.y.var <- tclVar()
    max.x.var <- tclVar()
    min.x.var <- tclVar()
    max.z.var <- tclVar()
    min.z.var <- tclVar()
    off.z.var <- tclVar()
    off.t.var <- tclVar()
    
    tt1.done.var <- tclVar(0)
    
  # package version number
    
    f <- ifelse("package:RSurvey" %in% search(), 
         system.file("DESCRIPTION", package="RSurvey"), "DESCRIPTION")
    srvy.dat("ver", paste("RSurvey", scan(f, what="character", skip=3, nlines=1, quiet=TRUE)[2]))
    
  # open gui
    
    tt1 <- tktoplevel()
    tkwm.geometry(tt1, srvy.dat("win.loc"))
    tktitle(tt1) <- srvy.dat("ver")
    tkwm.resizable(tt1, 0, 0)
    
    if(!is.null(srvy.dat("icon")) && file.exists(srvy.dat("icon"))) 
        tkwm.iconbitmap(tt1, srvy.dat("icon"))
    
  # top menu
    
    top.menu <- tkmenu(tt1, tearoff=0)
    
  # project menu
    
    menu.proj <- tkmenu(tt1, tearoff=0, font=fnt)
    tkadd(top.menu, "cascade", label="Project", menu=menu.proj, underline=0)
    
    tkadd(menu.proj, "command", label="New", command=proj.new)
    tkadd(menu.proj, "separator")
    tkadd(menu.proj, "command", label="Open", command=proj.open)
    tkadd(menu.proj, "command", label="Save", command=proj.save)
    tkadd(menu.proj, "command", label="Save As", command=proj.save.as)
    tkadd(menu.proj, "separator")
    tkadd(menu.proj, "command", label="Close Plots", command=device.close)
    tkadd(menu.proj, "command", label="Save R Plot", command=device.save.r)
    tkadd(menu.proj, "command", label="Save RGL Plot", command=device.save.rgl)
    tkadd(menu.proj, "separator")
    tkadd(menu.proj, "command", label="Exit", command=gui.close)
    
  # data menu
    
    menu.data <- tkmenu(tt1, tearoff=0, font=fnt)
    tkadd(top.menu, "cascade", label="Data", menu=menu.data, underline=0)
    
    tkadd(menu.data, "command", label="Import", 
        command=function() {
            if(clear.objects("data") == "ok") {
                srvy.import()
                gui.update()
                tkfocus(tt1)
            }
        }
    )
    tkadd(menu.data, "command", label="Export", 
        command=function() {
            gui.update()
            if(is.null(srvy.dat("data.raw"))) 
                stop(call.=FALSE, "No data exists.")
            srvy.process(const.tin=TRUE)
            if(!is.null(srvy.dat("data.tin"))) 
                srvy.export()
            tkfocus(tt1)
        }
    )
    tkadd(menu.data, "separator")
    tkadd(menu.data, "command", label="Clear", 
        command=function() {
            if(clear.objects("data") == "ok") 
                gui.update() 
        }
    )
    tkadd(menu.data, "separator")
    tkadd(menu.data, "command", label="Variables", 
        command=function() {
            if(is.null(srvy.dat("vars"))) 
                stop(call.=FALSE, "No data exists.")
            
            vars.old <- srvy.dat("vars")
            srvy.vars(srvy.dat("cols"), srvy.dat("vars"), tt1)
            vars.new <- srvy.dat("vars")
            
            if(!is.na(vars.old[1]) && vars.old[1] != vars.new[1]) {
                tclvalue(sd.var) <- ""
                tclvalue(ed.var) <- ""
                tclvalue(sh.var) <- ""
                tclvalue(eh.var) <- ""
                tclvalue(sm.var) <- ""
                tclvalue(em.var) <- ""
                tclvalue(ss.var) <- ""
                tclvalue(es.var) <- ""
            }
            if(vars.old[2] != vars.new[2]) {
                tclvalue(min.x.var) <- ""
                tclvalue(max.x.var) <- ""
            }
            if(vars.old[3] != vars.new[3]) {
                tclvalue(min.y.var) <- ""
                tclvalue(max.y.var) <- ""
            }
            if(!identical(vars.old, vars.new)) {
                srvy.dat("data.mod", NULL)
                srvy.dat("data.tin", NULL)
                gui.update()
            }
        }
    )
    tkadd(menu.data, "command", label="Preferences", 
        command=function() {
            gui.update()
            srvy.pref(tt1)
        }
    )
    
  # polygon menu
    
    menu.poly <- tkmenu(tt1, tearoff=0, font=fnt)
    
    tkadd(top.menu, "cascade", label="Polygon", menu=menu.poly, underline=0)
    
    tkadd(menu.poly, "command", label="Import", 
        command=function() {
            srvy.dat("poly", polyImport())
            srvy.dat("data.mod", NULL)
            srvy.dat("data.tin", NULL)
            tkfocus(tt1)
        }
    )
    
    tkadd(menu.poly, "command", label="Export", command=polyExport)
    
    tkadd(menu.poly, "separator")
    tkadd(menu.poly, "command", label="Construct", command=polyConstruct)
    tkadd(menu.poly, "command", label="Autocrop", 
        command=function() {
            if(is.null(srvy.dat("data.file"))) 
                stop(call.=FALSE, "No data is available.")
            tkconfigure(tt1, cursor="watch")
            ply <- polyAutocrop(tt1)
            if(!is.null(ply)) {
                srvy.dat("poly", ply)
                srvy.dat("data.mod", NULL)
                srvy.dat("data.tin", NULL)
            }
            tkconfigure(tt1, cursor="arrow")
            tkfocus(tt1)
        }
    )
    
    tkadd(menu.poly, "separator")
    
    tkadd(menu.poly, "command", label="Clear", command=polyClear)
    
  # transect menu
    
    menu.transect <- tkmenu(tt1, tearoff=0, font=fnt)
    tkadd(top.menu, "cascade", label="Transect", menu=menu.transect, underline=0)
    
    tkadd(menu.transect, "command", label="Manage", 
        command=function() {
            tran(tt1)
        }
    )
    
  # advanced menu
    
    menu.advanced <- tkmenu(tt1, tearoff=0, font=fnt)
    tkadd(top.menu, "cascade", label="Advanced", menu=menu.advanced, underline=0)
    
    tkadd(menu.advanced, "command", label="Configuration", 
        command=function() {
            gui.update()
            srvy.config(tt1)
        }
    )
    tkadd(menu.advanced, "command", label="Geographical Projection", 
        command=function() {
            geoProj(tt1)
        }
    )
    if(!("RSurvey" %in% .packages())) {
        tkadd(menu.advanced, "separator")
        tkadd(menu.advanced, "command", label="Restore R Image", 
            command=function() {
                gui.close()
                restoreImage(fun.call="srvy", save.objs=c("srvy.dat", "tran.dat"))
            }
        )
    }
    
  # help menu
    
    menu.help <- tkmenu(tt1, tearoff=0, font=fnt)
    tkadd(top.menu, "cascade", label="Help", menu=menu.help, underline=0)
    
    if(.Platform$OS.type == "windows" & ("RSurvey" %in% .packages())) {
        tkadd(menu.help, "command", label="Contents", 
            command=function() {
                shell.exec(paste(.path.package(package="RSurvey", quiet=FALSE), "/chtml/RSurvey.chm", sep=""))
            }
        )
        tkadd(menu.help, "separator")
    }
    tkadd(menu.help, "command", label="About", command=about)
    
  # finalize top menu
    
    tkconfigure(tt1, menu=top.menu)
    assign("tt1", tt1, pos=1)
    
  # frame 1 contains temporal information
    
    frame1 <- tkframe(tt1, relief="flat", borderwidth=2)
    
    frame1.lab.1.1 <- tklabel(frame1, font=fnt, text=" ")
    frame1.lab.1.2 <- tklabel(frame1, font=fnt, text="Date (Y-m-d)")
    frame1.lab.1.3 <- tklabel(frame1, font=fnt, text="Hour (0-23)")
    frame1.lab.1.4 <- tklabel(frame1, font=fnt, text="Min. (0-59)")
    frame1.lab.1.5 <- tklabel(frame1, font=fnt, text="Sec. (0-59.999)")
    frame1.lab.4.1 <- tklabel(frame1, font=fnt, text=" ")
    frame1.lab.4.2 <- tklabel(frame1, font=fnt, text="Min. x [L]")
    frame1.lab.4.3 <- tklabel(frame1, font=fnt, text="Max. x [L]")
    frame1.lab.4.4 <- tklabel(frame1, font=fnt, text="Min. y [L]")
    frame1.lab.4.5 <- tklabel(frame1, font=fnt, text="Max. y [L]")
    frame1.lab.2.1 <- tklabel(frame1, font=fnt, text="Start")
    frame1.lab.3.1 <- tklabel(frame1, font=fnt, text="Finish")
    frame1.lab.5.1 <- tklabel(frame1, font=fnt, text="Limits")
    
    frame1.ent.2.2 <- tkentry(frame1, font=fnt, width=13, textvariable=sd.var)
    frame1.ent.2.3 <- tkentry(frame1, font=fnt, width=13, textvariable=sh.var)
    frame1.ent.2.4 <- tkentry(frame1, font=fnt, width=13, textvariable=sm.var)
    frame1.ent.2.5 <- tkentry(frame1, font=fnt, width=13, textvariable=ss.var)
    frame1.ent.3.2 <- tkentry(frame1, font=fnt, width=13, textvariable=ed.var)
    frame1.ent.3.3 <- tkentry(frame1, font=fnt, width=13, textvariable=eh.var)
    frame1.ent.3.4 <- tkentry(frame1, font=fnt, width=13, textvariable=em.var)
    frame1.ent.3.5 <- tkentry(frame1, font=fnt, width=13, textvariable=es.var)
    frame1.ent.5.2 <- tkentry(frame1, font=fnt, width=13, textvariable=min.x.var)
    frame1.ent.5.3 <- tkentry(frame1, font=fnt, width=13, textvariable=max.x.var)
    frame1.ent.5.4 <- tkentry(frame1, font=fnt, width=13, textvariable=min.y.var)
    frame1.ent.5.5 <- tkentry(frame1, font=fnt, width=13, textvariable=max.y.var)
    
    tkbind(frame1.ent.2.2, "<KeyRelease>", function(){tclvalue(sd.var) <- keyEvent("date",   tclvalue(sd.var), rm.data=TRUE)})
    tkbind(frame1.ent.2.3, "<KeyRelease>", function(){tclvalue(sh.var) <- keyEvent("hour",   tclvalue(sh.var), rm.data=TRUE)})
    tkbind(frame1.ent.2.4, "<KeyRelease>", function(){tclvalue(sm.var) <- keyEvent("minute", tclvalue(sm.var), rm.data=TRUE)})
    tkbind(frame1.ent.2.5, "<KeyRelease>", function(){tclvalue(ss.var) <- keyEvent("second", tclvalue(ss.var), rm.data=TRUE)})
    tkbind(frame1.ent.3.2, "<KeyRelease>", function(){tclvalue(ed.var) <- keyEvent("date",   tclvalue(ed.var), rm.data=TRUE)})
    tkbind(frame1.ent.3.3, "<KeyRelease>", function(){tclvalue(eh.var) <- keyEvent("hour",   tclvalue(eh.var), rm.data=TRUE)})
    tkbind(frame1.ent.3.4, "<KeyRelease>", function(){tclvalue(em.var) <- keyEvent("minute", tclvalue(em.var), rm.data=TRUE)})
    tkbind(frame1.ent.3.5, "<KeyRelease>", function(){tclvalue(es.var) <- keyEvent("second", tclvalue(es.var), rm.data=TRUE)})
    tkbind(frame1.ent.5.2, "<KeyRelease>", function(){tclvalue(min.x.var) <- keyEvent("real", tclvalue(min.x.var), rm.data=TRUE)})
    tkbind(frame1.ent.5.3, "<KeyRelease>", function(){tclvalue(max.x.var) <- keyEvent("real", tclvalue(max.x.var), rm.data=TRUE)})
    tkbind(frame1.ent.5.4, "<KeyRelease>", function(){tclvalue(min.y.var) <- keyEvent("real", tclvalue(min.y.var), rm.data=TRUE)})
    tkbind(frame1.ent.5.5, "<KeyRelease>", function(){tclvalue(max.y.var) <- keyEvent("real", tclvalue(max.y.var), rm.data=TRUE)})
    
    tkgrid(frame1.lab.1.1, frame1.lab.1.2, frame1.lab.1.3, frame1.lab.1.4, frame1.lab.1.5, pady=0)
    tkgrid(frame1.lab.2.1, frame1.ent.2.2, frame1.ent.2.3, frame1.ent.2.4, frame1.ent.2.5, pady=1)
    tkgrid(frame1.lab.3.1, frame1.ent.3.2, frame1.ent.3.3, frame1.ent.3.4, frame1.ent.3.5, pady=1)
    tkgrid(frame1.lab.4.1, frame1.lab.4.2, frame1.lab.4.3, frame1.lab.4.4, frame1.lab.4.5, pady=0)
    tkgrid(frame1.lab.5.1, frame1.ent.5.2, frame1.ent.5.3, frame1.ent.5.4, frame1.ent.5.5, pady=1)
    
    tkgrid.configure(frame1.lab.2.1, sticky="e")
    tkgrid.configure(frame1.lab.3.1, sticky="e")
    tkgrid.configure(frame1.lab.5.1, sticky="e")
    
    tkpack(frame1, fill="both", padx=2)
    
  # frame 2 (parameters), frame 3 (buttons), and frame 4 (contains frames 2 and 3)
    
    frame4 <- tkframe(tt1, relief="flat", borderwidth=0)
    
    frame2 <- tkframe(frame4, relief="flat", borderwidth=2)
    
    frame2.lab.1.1 <- tklabel(frame2, font=fnt, text="Minimum z")
    frame2.lab.2.1 <- tklabel(frame2, font=fnt, text="Maximum z")
    frame2.lab.3.1 <- tklabel(frame2, font=fnt, text="z-axis offset")
    frame2.lab.4.1 <- tklabel(frame2, font=fnt, text="Time offset (sec)")
    
    frame2.ent.1.2 <- tkentry(frame2, font=fnt, width=15, textvariable=min.z.var)
    frame2.ent.2.2 <- tkentry(frame2, font=fnt, width=15, textvariable=max.z.var)
    frame2.ent.3.2 <- tkentry(frame2, font=fnt, width=15, textvariable=off.z.var)
    frame2.ent.4.2 <- tkentry(frame2, font=fnt, width=15, textvariable=off.t.var)
    
    tkbind(frame2.ent.1.2, "<KeyRelease>", function(){tclvalue(min.z.var) <- keyEvent("real", tclvalue(min.z.var), rm.data=TRUE)})
    tkbind(frame2.ent.2.2, "<KeyRelease>", function(){tclvalue(max.z.var) <- keyEvent("real", tclvalue(max.z.var), rm.data=TRUE)})
    tkbind(frame2.ent.3.2, "<KeyRelease>", function(){tclvalue(off.z.var) <- keyEvent("real", tclvalue(off.z.var), rm.data=TRUE)})
    tkbind(frame2.ent.4.2, "<KeyRelease>", function(){tclvalue(off.t.var) <- keyEvent("real", tclvalue(off.t.var), rm.data=TRUE)})
    
    tkgrid(frame2.lab.1.1, frame2.ent.1.2, pady=2)
    tkgrid(frame2.lab.2.1, frame2.ent.2.2, pady=2)
    tkgrid(frame2.lab.3.1, frame2.ent.3.2, pady=2)
    tkgrid(frame2.lab.4.1, frame2.ent.4.2, pady=2)
    
    tkgrid.configure(frame2.lab.1.1, sticky="e")
    tkgrid.configure(frame2.lab.2.1, sticky="e")
    tkgrid.configure(frame2.lab.3.1, sticky="e")
    tkgrid.configure(frame2.lab.4.1, sticky="e")
    
    frame3 <- tkframe(frame4, relief="flat", borderwidth=2)
    
    frame3.but.1 <- tkbutton(frame3, font=fnt, width=15, text="TIME PLOT", 
                        command=function() {
                             hld <- srvy.dat("vars")[1]
                            if(is.null(hld)) 
                                stop(call.=FALSE, "No data is available.")
                            if(is.na(hld))
                                stop(call.=FALSE, "No temporal data available.")
                            tkconfigure(tt1, cursor="watch")
                            gui.update()
                            srvy.process()
                            plotTime()
                            tkconfigure(tt1, cursor="arrow")
                            tkfocus(tt1)
                        }
                    )
    frame3.but.2 <- tkbutton(frame3, font=fnt, width=15, text="POINT PLOT",
                        command=function() {
                            if(is.null(srvy.dat("vars"))) 
                                stop(call.=FALSE, "No data is available.")
                            tkconfigure(tt1, cursor="watch")
                            gui.update()
                            srvy.process()
                            plotPoints()
                            tkconfigure(tt1, cursor="arrow")
                            tkfocus(tt1)
                        }
                    )
    frame3.but.3 <- tkbutton(frame3, font=fnt, width=15, text="2D SURFACE", 
                        command=function() {
                            if(is.null(srvy.dat("vars"))) 
                                stop(call.=FALSE, "No data is available.")
                            tkconfigure(tt1, cursor="watch")
                            gui.update()
                            srvy.process(const.tin=TRUE)
                            plotSurface2d()
                            tkconfigure(tt1, cursor="arrow")
                            tkfocus(tt1)
                        }
                    )
    frame3.but.4 <- tkbutton(frame3, font=fnt, width=15, text="3D SURFACE", 
                        command=function() {
                            if(is.null(srvy.dat("vars"))) 
                                stop(call.=FALSE, "No data is available.")
                            tkconfigure(tt1, cursor="watch")
                            gui.update()
                            srvy.process(const.tin=TRUE)
                            plotSurface3d()
                            tkconfigure(tt1, cursor="arrow")
                            tkfocus(tt1)
                        }
                    )
                    
    tkgrid(frame3.but.1, pady=2)
    tkgrid(frame3.but.2, pady=2)
    tkgrid(frame3.but.3, pady=2)
    tkgrid(frame3.but.4, pady=2)
    
    tkpack(frame2, frame3, expand=1, side="left")
    tkpack(frame4, fill="both", pady=2, padx=2)
    
  # gui closure
    
    gui.restore(vars)
    gui.update()
    tkfocus(tt1)
    tkbind(tt1, "<Destroy>", gui.close)
}

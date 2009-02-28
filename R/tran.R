"tran" <- function(tt1=NULL) {

# additional functions (subroutines)

  # update listboxes
    
    update.listbox <- function() {
        num <- length(tran.dat())
        if(num > 0) {
            n.1 <- as.integer(tkcurselection(frame1.lst.1))
            n.2 <- as.integer(tkcurselection(frame1.lst.2))
            
            if(length(n.1) == 0) 
                n.1 <- 0
            if((n.1 + 1) > num) 
                n.1 <- n.1 - 1
            
            tkdelete(frame1.lst.1, 0, "end")
            
            for(i in names(tran.dat())) 
                tkinsert(frame1.lst.1, "end", i)
            tkselection.set(frame1.lst.1, n.1)
            tksee(frame1.lst.1, n.1)
            
            tkdelete(frame1.lst.2, 0, "end")
            cols <- tran.dat(n.1 + 1, "type")
            
            if(length(n.2) == 0) 
                n.2 <- 0
            if((n.2 + 1) > length(cols)) 
                n.2 <- n.2 - 1
            
            if(!is.null(cols)) {
                for(i in cols) 
                    tkinsert(frame1.lst.2, "end", i)
                tkselection.set(frame1.lst.2, n.2)
                tksee(frame1.lst.2, n.2)
            }
        }
        else {
            tkdelete(frame1.lst.1, 0, "end")
            tkdelete(frame1.lst.2, 0, "end")
        }
    }
    
  # clear transect
    
    transect.clear <- function(del.all) {
        
        if(length(tran.dat()) == 0) return()
        
        if(del.all) {
            txt <- "Delete all transects?"
            ans <- as.character(tkmessageBox(icon="question", message=txt, 
                   title="Clear Transects", parent=tt2, type="yesno"))
            if(ans == "yes") 
                tran.dat(clearAll=TRUE)
        }
        else {
            n.1 <- as.integer(tkcurselection(frame1.lst.1)) + 1
            txt <- paste("Do you really want to delete the", tran.dat(n.1, "id"), "transect?")
            ans <- as.character(tkmessageBox(icon="question", message=txt, 
                   title="Clear Transect", parent=tt2, type="yesno"))
            if(ans == "yes") 
                tran.dat(n.1, clearId=TRUE)
        }
        update.listbox()
    }
    
  # clear profile
    
    profile.clear <- function() {
        if(length(tran.dat()) == 0) return()
        n.1 <- as.integer(tkcurselection(frame1.lst.1)) + 1
        tran.dat(n.1, "prof", NULL)
    }
    
  # clear raster
    
    raster.clear <- function(del.all) {
        
        if(length(tran.dat()) == 0) return()
        
        n.1 <- as.integer(tkcurselection(frame1.lst.1)) + 1
        if(is.null(tran.dat(n.1, "type"))) return()
        
        if(del.all) {
            txt <- paste("Do you really want to delete all data associated with the transect?")
            ans <- as.character(tkmessageBox(icon="question", message=txt, 
                   title="Delete All Data", parent=tt2, type="yesno"))
            if(ans == "no") return()
        }
        
        n.2 <- as.integer(tkcurselection(frame1.lst.2))
        
        if(length(n.2) == 0 | del.all) {
            tran.dat(n.1, "type", NULL)
            tran.dat(n.1, "data.ras", NULL)
            tran.dat(n.1, "limits", NULL)
            tran.dat(n.1, "vel.vect", NULL)
            tran.dat(n.1, "hv.fields", NULL)
        }
        else {
            rasterSel <- paste(as.character(tkget(frame1.lst.2, n.2)), collapse=" ")
            
            if(!is.null(tran.dat(n.1, "hv.fields"))) 
                if(any(tran.dat(n.1, "hv.fields") %in% rasterSel)) return()
            
            vel.vect <- tran.dat(n.1, "vel.vect")
            if(!is.null(vel.vect)) {
                vel.vect[vel.vect %in% rasterSel] <- NA
                tran.dat(n.1, "vel.vect", vel.vect)
            }
            
            type <- tran.dat(n.1, "type")
            idxs <- !type %in% rasterSel
            tran.dat(n.1, "type", type[idxs])
            
            rdat <- tran.dat(n.1, "data.ras")
            idxs <- !names(rdat) %in% make.names(rasterSel)
            tran.dat(n.1, "data.ras", rdat[,idxs])
            
            limits <- tran.dat(n.1, "limits")
            idxs <- !row.names(limits) %in% make.names(rasterSel)
            tran.dat(n.1, "limits", limits[idxs,])
        }
        update.listbox()
    }
    
  # close gui
    
    gui.close <- function() {
        tkdestroy(tt2)
        if(exists("tt2", where=".GlobalEnv")) 
            rm(tt2, pos=".GlobalEnv")
    }
    
    
    
# main program

  # GUI font
    
    fnt <- srvy.dat("font.gui")
    
  # assign the variables linked to Tk widgets
    
    expr.var    <- tclVar()
    velType.var <- tclVar()
    
    tclvalue(velType.var) <- "magnitude"
    
  # open gui
    
    tt2 <- tktoplevel()
    if(!is.null(tt1)) {
        tkwm.transient(tt2, tt1)
        tmp <- unlist(strsplit(as.character(tkwm.geometry(tt1)), "\\+"))
        tkwm.geometry(tt2, paste("+", as.integer(tmp[2]) + 25, "+", as.integer(tmp[3]) + 25, sep=""))
    }
    tktitle(tt2) <- "Transect Management"
    
    if(!is.null(srvy.dat("icon")) && file.exists(srvy.dat("icon"))) 
        tkwm.iconbitmap(tt2, srvy.dat("icon"))
    
    tkwm.resizable(tt2, 0, 0) 
    
  # top menu
    
    top.menu <- tkmenu(tt2, tearoff=0)
    
  # transect menu
    
    menu.transect <- tkmenu(top.menu, tearoff=0, font=fnt)
    
    tkadd(top.menu, "cascade", label="Transect", menu=menu.transect, underline=0)
    
    tkadd(menu.transect, "command", label="New", 
        command=function() {
            tran.edit(tt2=tt2)
            update.listbox()
            tkselection.clear(frame1.lst.1, 0, "end")
            tkselection.set(frame1.lst.1, "end")
            tksee(frame1.lst.1, "end")
            update.listbox()
        }
    )
    
    tkadd(menu.transect, "command", label="Edit", 
        command=function() {
            if(length(tran.dat()) == 0) 
                stop(call.=FALSE, "No transect exists.")
            n.1 <- as.integer(tkcurselection(frame1.lst.1)) + 1
            tran.edit(tran.dat(n.1, "id"), tt2)
            update.listbox()
        }
    )
    
    tkadd(menu.transect, "separator")
    tkadd(menu.transect, "command", label="Import", 
        command=function() {
            tran.import("transect", "NEW")
            update.listbox()
            tkfocus(tt2)
        }
    )
    tkadd(menu.transect, "command", label="Export", 
        command=function() {
            if(length(tran.dat()) == 0) return()
            n.1 <- as.integer(tkcurselection(frame1.lst.1)) + 1
            tran.export("transect", n.1)
            tkfocus(tt2)
        }
    )
    tkadd(menu.transect, "separator")
    tkadd(menu.transect, "command", label="Clear", 
        command=function() transect.clear(del.all=FALSE)
    )
    tkadd(menu.transect, "command", label="Clear All", 
        command=function() transect.clear(del.all=TRUE)
    )
    
  # profile menu
    
    menu.profile <- tkmenu(top.menu, tearoff=0, font=fnt)
    
    tkadd(top.menu, "cascade", label="Profile", menu=menu.profile, underline=0)
    
    tkadd(menu.profile, "command", label="Import", 
        command=function() {
            if(length(tran.dat()) == 0) return()
            n.1 <- as.integer(tkcurselection(frame1.lst.1)) + 1
            tran.import("profile", n.1)
            tkfocus(tt2)
        }
    )
    tkadd(menu.profile, "command", label="Export", 
        command=function() {
            if(length(tran.dat()) == 0) return()
            n.1 <- as.integer(tkcurselection(frame1.lst.1)) + 1
            tran.export("profile", n.1)
            tkfocus(tt2)
        }
    )
    tkadd(menu.profile, "separator")
    tkadd(menu.profile, "command", label="Clear", command=profile.clear)
    tkadd(menu.profile, "separator")
    tkadd(menu.profile, "command", label="Plot", 
        command=function() {
            n.1 <- as.integer(tkcurselection(frame1.lst.1)) + 1
            plotTransect(tran.dat(n.1, "id"))
        }
    )
    
  # raster menu
    
    menu.raster <- tkmenu(tt2, tearoff=0, font=fnt)
    tkadd(top.menu, "cascade", label="Raster", menu=menu.raster, underline=0)
    
    tkadd(menu.raster, "command", label="Import", 
        command=function() {
            if(length(tran.dat()) == 0) return()
            n.1 <- as.integer(tkcurselection(frame1.lst.1)) + 1
            tran.import("raster", n.1)
            update.listbox()
            tkfocus(tt2)
        }
    )
    tkadd(menu.raster, "command", label="Export All", 
        command=function() {
            n.1 <- as.integer(tkcurselection(frame1.lst.1)) + 1
            n.2 <- as.integer(tkcurselection(frame1.lst.2)) + 1
            if(length(n.2) == 0) return()
            tran.export("raster", n.1)
            tkfocus(tt2)
        }
    )
    tkadd(menu.raster, "separator")
    tkadd(menu.raster, "command", label="Clear", 
        command=function() raster.clear(del.all=FALSE)
    )
    tkadd(menu.raster, "command", label="Clear All", 
        command=function() raster.clear(del.all=TRUE)
    )
    tkadd(menu.raster, "separator")
    tkadd(menu.raster, "command", label="Spatial Fields", 
        command=function() {
            if(length(tran.dat()) == 0) 
                stop(call.=FALSE, "No profiles exist.")
            n.1 <- as.integer(tkcurselection(frame1.lst.1)) + 1
            tmp <- tran.spatial(tran.dat(n.1, "type"), tran.dat(n.1, "hv.fields"), tt2)
            tran.dat(n.1, "hv.fields", tmp)
        }
    )
    
    tkadd(menu.raster, "separator")
    tkadd(menu.raster, "command", label="Plot",
        command=function() {
            n.1 <- as.integer(tkcurselection(frame1.lst.1)) + 1
            n.2 <- as.integer(tkcurselection(frame1.lst.2))
            if(length(n.2) == 0)
                stop(call.=FALSE, "No raster data is available.")
            tkconfigure(tt2, cursor="watch")
            rasterField <- paste(as.character(tkget(frame1.lst.2, n.2)), collapse=" ")
            plotTransect(tran.dat(n.1, "id"), rasterField, velPlot=FALSE, flow=tclvalue(velType.var))
            tkconfigure(tt2, cursor="arrow")
        }
    )
    tkadd(menu.raster, "command", label="Axes Limits",
        command=function() {
            n.1 <- as.integer(tkcurselection(frame1.lst.1)) + 1
            n.2 <- as.integer(tkcurselection(frame1.lst.2)) + 1
            if(length(n.2) == 0)
                stop(call.=FALSE, "No raster data is available.")
            limits <- tran.dat(n.1, "limits")
            old <- as.list(limits[n.2,])
            new <- axesLimits(tt2, old)
            limits[n.2,] <- as.data.frame(new)
            tran.dat(n.1, "limits", limits)
        }
    )
    
  # velocity menu
    
    menu.vel <- tkmenu(tt2, tearoff=0, font=fnt)
    tkadd(top.menu, "cascade", label="Velocity", menu=menu.vel, underline=0)
    
    tkadd(menu.vel, "command", label="Vector Components", 
        command=function() {
            if(length(tran.dat()) == 0) 
                stop(call.=FALSE, "No profiles exist.")
            n.1 <- as.integer(tkcurselection(frame1.lst.1)) + 1
            tmp <- tran.velocity(tran.dat(n.1, "type"), tran.dat(n.1, "vel.vect"), tt2)
            tran.dat(n.1, "vel.vect", tmp)
        }
    )
    
    velTypeMenu <- tkmenu(tt2, tearoff=0, font=fnt)
    
    tkadd(velTypeMenu, "radio", label="Principle Direction (Magnitude)", variable=velType.var, value="magnitude")
    tkadd(velTypeMenu, "radio", label="Longitudinal and Transverse", variable=velType.var, value="longTran")
    
    tkadd(menu.vel, "cascade", label="Type", menu=velTypeMenu)
    
    tkadd(menu.vel, "separator") 
    
    tkadd(menu.vel, "command", label="Plot",
        command=function() {
            msg <- "Missing velocity vector component(s)."
            
            n.1 <- as.integer(tkcurselection(frame1.lst.1)) + 1
            vel <- tran.dat(n.1, "vel.vect")
            
            velType <- tclvalue(velType.var)
            
            if(velType == "magnitude") {
                if(is.null(vel) || (any(is.na(vel[1:3])) & is.na(vel[4]))) 
                    stop(call.=FALSE, msg)
            }
            if(velType == "longTran") {
                if(is.null(vel) || any(is.na(vel[1:3]))) 
                    stop(call.=FALSE, msg)
            }
            
            tkconfigure(tt2, cursor="watch")
            plotTransect(tran.dat(n.1, "id"), velPlot=TRUE, flow=velType)
            tkconfigure(tt2, cursor="arrow")
            tkfocus(tt2)
        }
    )
    
    tkadd(menu.vel, "command", label="Axes Limits",
        command=function() {
            n.1 <- as.integer(tkcurselection(frame1.lst.1)) + 1
            limits <- tran.dat(n.1, "limits")
            if(is.null(limits)) 
                stop(call.=FALSE, "Raster data is required for this feature.")
            else {
                if(tclvalue(velType.var) == "magnitude") 
                    vel.type <- "velocity.principle.dir"
                else 
                    vel.type <- "velocity.long.tran"
                old <- as.list(limits[vel.type,])
                new <- axesLimits(tt2, old)
                limits[vel.type,] <- as.data.frame(new)
                tran.dat(n.1, "limits", limits)
            }
        }
    )
    
  # finalize top menu
    
    tkconfigure(tt2, menu=top.menu)
    assign("tt2", tt2, pos=1) 
    
  # frame 1 is the overall frame
      
    frame1 <- tkframe(tt2, relief="flat", borderwidth=2)
    frame1.lab.1 <- tklabel(frame1, justify="center", font=fnt, text="Transect Name")
    frame1.lab.2 <- tklabel(frame1, justify="center", font=fnt, text="Raster Data")
    
    frame1.lst.1 <- tklistbox(frame1, selectmode="browse", font=fnt, width=19, height=8,
                    exportselection=FALSE)
    frame1.lst.2 <- tklistbox(frame1, selectmode="browse", font=fnt, width=19, height=8,
                    exportselection=FALSE)
    
    frame1.ysc.1 <- tkscrollbar(frame1, orient="vertical")
    frame1.ysc.2 <- tkscrollbar(frame1, orient="vertical")
    frame1.xsc.1 <- tkscrollbar(frame1, orient="horizontal")
    frame1.xsc.2 <- tkscrollbar(frame1, orient="horizontal") 
    
    tkconfigure(frame1.lst.1, background="white", 
        yscrollcommand=paste(.Tk.ID(frame1.ysc.1), "set"), 
        xscrollcommand=paste(.Tk.ID(frame1.xsc.1), "set")
    )
    tkconfigure(frame1.lst.2, background="white", 
        yscrollcommand=paste(.Tk.ID(frame1.ysc.2), "set"), 
        xscrollcommand=paste(.Tk.ID(frame1.xsc.2), "set")
    )
    tkconfigure(frame1.ysc.1, command=paste(.Tk.ID(frame1.lst.1), "yview"))
    tkconfigure(frame1.ysc.2, command=paste(.Tk.ID(frame1.lst.2), "yview"))
    tkconfigure(frame1.xsc.1, command=paste(.Tk.ID(frame1.lst.1), "xview"))
    tkconfigure(frame1.xsc.2, command=paste(.Tk.ID(frame1.lst.2), "xview"))
    
    tkbind(frame1.lst.1, "<ButtonRelease-1>", update.listbox)
    
    tkgrid(frame1.lab.1, frame1.lab.2, pady=0)
    tkgrid(frame1.lst.1, frame1.ysc.1, frame1.xsc.1, frame1.lst.2, frame1.ysc.2, frame1.xsc.2)
    
    tkgrid.configure(frame1.lab.1,  column=0, row=0)
    tkgrid.configure(frame1.lab.2,  column=2, row=0)
    
    tkgrid.configure(frame1.lst.1,  column=0, row=1, sticky="w e")
    tkgrid.configure(frame1.ysc.1,  column=1, row=1, sticky="n s")
    tkgrid.configure(frame1.xsc.1,  column=0, row=2, sticky="w e")
    tkgrid.configure(frame1.lst.2,  column=2, row=1, sticky="w e")
    tkgrid.configure(frame1.ysc.2,  column=3, row=1, sticky="n s")
    tkgrid.configure(frame1.xsc.2,  column=2, row=2, sticky="w e")
    
    update.listbox()
    
    tkgrid(frame1)
    
  # gui control
    
    tkfocus(tt2)
    tkbind(tt2, "<Destroy>", gui.close)
}


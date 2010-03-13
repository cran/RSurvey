"polyManage" <- function(parent, ply=NULL, ply.ran=NULL, ply.lim=NULL, encoding=getOption("encoding")) {
    
# additional functions (subroutines)
    
  # plot polygon
    
    plotPolygon <- function(){
        
        drawPolygon <- function(contours, tag="", col.line="", col.fill="") {
            for(cnt in contours) {
                pts <- xy2mn(cnt$x, cnt$y)
                mn <- rep(NA, length(pts$m) * 2)
                is.odd <- !array(0:1, length(mn)) 
                mn[ is.odd] <- pts$m
                mn[!is.odd] <- pts$n
                
                tkcreate(frame0.cvs, "polygon", .Tcl.args(mn), fill=col.fill, 
                    outline=col.line, width=1, tag=tag)
            }
        }
        
        idxs <- as.integer(tkcurselection(frame1.lst)) + 1
        
        tcl(frame0.cvs, "delete", "all")
        xran <<- NULL
        yran <<- NULL
        
        tclvalue(area.var) <- "NA"
        tclvalue(poly.var) <- "NA"
        tclvalue(hole.var) <- "NA"
        tclvalue(vert.var) <- "NA"
        
        base.ply <<- NULL
        
        if(length(idxs) == 0) return()
        
        for(idx in idxs) {
            bb <- get.bbox(ply[[idx]])
            xran <- range(c(xran, bb$x))
            yran <- range(c(yran, bb$y))
        }
        xran <<- extendrange(xran, f=0.02)
        yran <<- extendrange(yran, f=0.02)
        
        cmd <- tclvalue(rb.var)
        base.ply <<- ply[[idxs[1]]]
        for(idx in idxs[-1]) {
            if(cmd == "add") base.ply <<- union(base.ply, ply[[idx]]) else 
            if(cmd == "sub") base.ply <<- setdiff(base.ply, ply[[idx]]) else 
            if(cmd == "int") base.ply <<- intersect(base.ply, ply[[idx]])
        }
        if(cmd == "exc" && length(idxs) > 1) {
            union.ply <- base.ply
            inter.ply <- base.ply
            for(idx in idxs[-1]) {
                union.ply <- union(union.ply, ply[[idx]])
                inter.ply <- intersect(inter.ply, ply[[idx]])
            }
            base.ply <<- setdiff(union.ply, inter.ply)
        }
        
        base.pts <- get.pts(base.ply)
        
        if(length(base.pts) == 0) base.ply <<- NULL
        else {
            hole <- NULL
            vert <- 0
            for(ctr in base.pts) {
                hole <- append(hole, ctr$hole)
                vert <- vert + length(ctr$x)
            }
            
            if(!is.null(hole)) {
                drawPolygon(base.pts[!hole], col.fill="#FEFE88")
                if(any(hole)) drawPolygon(base.pts[hole], col.fill="white")
            }
            
            tclvalue(area.var) <- format(area.poly(base.ply))
            tclvalue(poly.var) <- length(base.pts)
            tclvalue(hole.var) <- sum(hole)
            tclvalue(vert.var) <- vert
        }
        
        for(i in idxs) 
            drawPolygon(get.pts(ply[[i]]), tag=names(ply)[i], col.line=col.pal[i])
    }
    
  # transform coordinates from real to canvas
    
    xy2mn <- function(x, y) {
        m <- w * ((x - xran[1]) / diff(xran))
        n <- h - (h * ((y - yran[1]) / diff(yran)))
        list(m=m, n=n)
    }
    
  # transform coordinates from canvas to real
    
    mn2xy <- function(m, n) {
        x <- (m * diff(xran) + w * xran[1]) / w
        y <- (h * yran[1] - (n - h) * diff(yran)) / h
        list(x=x, y=y)
    }
    
  # scale objects in canvas based on canvas size
    
    scaleCanvas <- function() {
        w0 <- w
        h0 <- h
        w <<- as.numeric(tkwinfo("width",  frame0.cvs))
        h <<- as.numeric(tkwinfo("height", frame0.cvs))
        tcl(frame0.cvs, "scale", "all", 0, 0, w/w0, h/h0)
    }
    
  # update pointer coordinates
    
    mouseMotion <- function(x, y) {
        m <- as.numeric(x)
        n <- as.numeric(y)
        pnt <- mn2xy(m, n)
        tclvalue(xy.var) <- paste(format(pnt$x), format(pnt$y), sep=", ")
    }
    
  # default pointer coordinates after leaving canvas
    
    mouseLeave <- function() {
        tclvalue(xy.var) <- "x, y"
    }
    
  # name polygon
    
    namePolygon <- function(old=NULL, nam=NA){
        if(is.na(nam)) nam <- "New Polygon"
        idx <- 1
        chk <- nam
        while(chk %in% old) {
            chk <- paste(nam, " (", idx, ")", sep="")
            idx <- idx + 1
        }
        chk
    }
    
  # rename polygon
    
    renamePolygon <- function() {
        
        updateEntry <- function() {
            
            if(tclvalue(cur.var) != "" && !(tclvalue(new.var) %in% oldNames)) 
                newNames[oldNames %in% tclvalue(cur.var)] <<- tclvalue(new.var)
            
            tclvalue(new.var) <- newNames[oldNames %in% tclvalue(old.var)]
            
            tclvalue(cur.var) <- tclvalue(old.var)
        }
        
        saveRenames <- function() {
            
            if(length(ply) == 0) return()
            
            if(tclvalue(cur.var) != "" && !(tclvalue(new.var) %in% oldNames)) {
                newNames[oldNames %in% tclvalue(cur.var)] <<- tclvalue(new.var)
                tclvalue(cur.var) <- tclvalue(new.var)
                tclvalue(old.var) <- tclvalue(new.var)
            }
            else {
                tclvalue(new.var) <- tclvalue(old.var)
            }
            
            tkconfigure(combobox, "-values", newNames)
            
            oldNames <<- newNames
            
            names(ply) <<- oldNames
        }
        
        oldNames <- names(ply)
        newNames <- oldNames
        
        old.var <- tclVar("")
        new.var <- tclVar("")
        cur.var <- tclVar("")
        child.done.var <- tclVar(0)
        
        child <- tktoplevel(padx=25, pady=15)
        
        tkwm.transient(child, tt)
        tmp <- unlist(strsplit(as.character(tkwm.geometry(tt)), "\\+"))
        tkwm.geometry(child, paste("+", as.integer(tmp[2]) + 25, "+", as.integer(tmp[3]) + 25, sep=""))
        
        tktitle(child) <- "Rename Polygon"
        
        combobox <- ttkcombobox(child, state="readonly", width=25, textvariable=old.var, 
                    values=if(length(oldNames) == 1) paste("{", oldNames, "}", sep="") else oldNames)
        entry <- ttkentry(child, width=25, textvariable=new.var)
        button <- ttkbutton(child, width=12, text="SAVE", command=saveRenames)
        
        tkpack(combobox, entry, button, pady=5)
        
        tkbind(combobox, "<<ComboboxSelected>>", updateEntry)
        
        tkfocus(child)
        tkgrab(child)
        
        tkbind(child, "<Destroy>", function() tclvalue(child.done.var) <- 1)
        
        tkwait.variable(child.done.var)
        
        tkgrab.release(child)
        tkdestroy(child)
        tkfocus(tt)
        
        if(length(ply) == 0) return()
        
        for(i in 1:length(oldNames)) 
            tclvalue(list.var) <- tcl("lreplace", tclvalue(list.var), i-1, i-1, oldNames[i])
        
        idx1 <- as.integer(tcl(frame2c.box.1.2, "current"))
        idx2 <- as.integer(tcl(frame2c.box.2.2, "current"))
        
        vals <- c("", names(ply))
        tkconfigure(frame2c.box.1.2, "-values", vals)
        tkconfigure(frame2c.box.2.2, "-values", vals)
        
        if(idx1 >= 0) tcl(frame2c.box.1.2, "current", idx1)
        if(idx2 >= 0) tcl(frame2c.box.2.2, "current", idx2)
    }
    
  # save new polygon
    
    saveNewPolygon <- function() {
        
        if(is.null(base.ply)) return()
        
        nam <- namePolygon(old=names(ply))
        ply[[nam]] <<- base.ply
        
        tcl("lappend", list.var, nam)
        
        vals <- c("", names(ply))
        tkconfigure(frame2c.box.1.2, "-values", vals)
        tkconfigure(frame2c.box.2.2, "-values", vals)
        
        
        idx <- length(ply) - 1
        tkselection.clear(frame1.lst, 0, idx)
        tkselection.set(frame1.lst, idx, idx)
        
        tclvalue(rb.var) <- "add"
        plotPolygon()
    }
    
    
  # select polygon
    
    selectPolygon <- function(type) {
        
        n <- length(ply)
        if(n == 0) return()
        
        nams <- names(ply)
        
        idxs <- (1:n) - 1
        
        sel <- as.integer(tkcurselection(frame1.lst))
        
        if(type == "all") tkselection.set(frame1.lst, 0, max(idxs)) else
        if(type == "none" | type == "inverse") 
            tkselection.clear(frame1.lst, 0, max(idxs))
        if(type == "inverse") {
            for(i in idxs[!(idxs %in% sel)]) 
                tkselection.set(frame1.lst, i)
        }
        
        plotPolygon()
    }
    
  # polygon arrangement
    
    arrange <- function(type) {
        
        idxs <- as.integer(tkcurselection(frame1.lst)) + 1
        if(length(idxs) == 0) return()
        
        box1.idx <- as.integer(tcl(frame2c.box.1.2, "current"))
        box2.idx <- as.integer(tcl(frame2c.box.2.2, "current"))
        box1.val <- if(box1.idx == 0) "" else names(ply)[box1.idx]
        box2.val <- if(box2.idx == 0) "" else names(ply)[box2.idx]
        
        if(type == "back") {
            ply <<- append(ply[idxs], ply[-idxs]) 
            newIdxs <- 1:length(idxs)
        } else 
        if(type == "front") {
            ply <<- append(ply[-idxs], ply[idxs]) 
            newIdxs <- (length(ply) - length(idxs) + 1):length(ply)
        } else 
        if(type == "backward") {
            
            n <- length(ply)
            
            newIdxs <- idxs
            allIdxs <- 1:n
            
            for(i in 1:n) 
                if(i %in% newIdxs) allIdxs[c(i - 1, i)] <- allIdxs[c(i, i - 1)]
            
            ply <<- ply[allIdxs]
            
            newIdxs <- if(length(newIdxs) == 0) 1 else (1:n)[allIdxs %in% newIdxs]
            
        } else 
        if(type == "forward") {
            n <- length(ply)
            newIdxs <- idxs
            allIdxs <- 1:n
            for(i in rev(1:n)) 
                if(i %in% newIdxs) allIdxs[c(i, i + 1)] <- allIdxs[c(i + 1, i)]
            allIdxs <- na.omit(allIdxs)
            ply <<- ply[allIdxs]
            
            newIdxs <- if(length(newIdxs) == 0) n else (1:n)[allIdxs %in% newIdxs]
        }
        
        for(i in 1:length(ply)) 
            tclvalue(list.var) <- tcl("lreplace", tclvalue(list.var), i-1, i-1, names(ply)[i])
        
        tkselection.clear(frame1.lst, 0, max(idxs))
        for(i in newIdxs - 1) 
            tkselection.set(frame1.lst, i)
        
        vals <- c("", names(ply))
        
        idx1 <- (1:length(vals))[vals %in% box1.val] - 1
        idx2 <- (1:length(vals))[vals %in% box2.val] - 1
        
        tkconfigure(frame2c.box.1.2, "-values", vals)
        tkconfigure(frame2c.box.2.2, "-values", vals)
        
        if(idx1 >= 0) tcl(frame2c.box.1.2, "current", idx1)
        if(idx2 >= 0) tcl(frame2c.box.2.2, "current", idx2)
        
        plotPolygon()
    }
    
  # clear polygon
    
    clearPolygon <- function() {
        
        idxs <- as.integer(tkcurselection(frame1.lst)) + 1
        if(length(idxs) == 0) return()
        
        for(idx in idxs) {
            i <- as.integer(tcl("lsearch", "-exact", tclvalue(list.var), names(ply)[idx]))
            if(i < 0) next
            tclvalue(list.var) <- tcl("lreplace", tclvalue(list.var), i, i)
        }
        
        ply <<- ply[-idxs]
        
        tkselection.clear(frame1.lst, 0, length(ply) - 1)
        
        vals <- c("", names(ply))
        tkconfigure(frame2c.box.1.2, "-values", vals)
        tkconfigure(frame2c.box.2.2, "-values", vals)
        
        if(!tclvalue(cbox1.var) %in% names(ply)) tclvalue(cbox1.var) <- ""
        if(!tclvalue(cbox2.var) %in% names(ply)) tclvalue(cbox2.var) <- ""
        
        plotPolygon()
    }
    
  # open polygon
    
    openPolygon <- function() {
        f <- getFile(parent=tt, cmd="Open", exts=c("ply"), caption="Import Polygon File(s)", multi=TRUE)
        if(is.null(f)) return()
        
        if(!is.list(f[[1]])) f <- list(f)
        
        for(i in 1:length(f)) {
            
            con <- if("connection" %in% class(file)) file 
                   else file(f[[i]]$path, "r", encoding=encoding)
            new.poly <- read.polyfile(con, nohole=FALSE)
            
            close(con)
            
            nam <- namePolygon(old=names(ply), nam=unlist(strsplit(f[[i]]$name, "\\."))[1])
            
            ply[[nam]] <<- new.poly
            
            tcl("lappend", list.var, nam)
            
            vals <- c("", names(ply))
            tkconfigure(frame2c.box.1.2, "-values", vals)
            tkconfigure(frame2c.box.2.2, "-values", vals)
        }
    }
    
  # save polygon
    
    savePolygon <- function() {
        
        idxs <- as.integer(tkcurselection(frame1.lst)) + 1
        if(length(idxs) == 0) return()
        
        path <- tclvalue(tkchooseDirectory(initialdir=srvy.dat("default.dir"), parent=tt,
                title="Choose directory to save selected polygon(s) in..."))
        if(path == "") return()
        srvy.dat("default.dir", path)
        
        existingFiles <- list.files(path, pattern="*.ply", full.names=TRUE)
        
        for(i in names(ply)[idxs]) {
            outFile <- paste(path, "/", i, ".ply", sep="")
            
            if(outFile %in% existingFiles) {
                msg <- paste("This folder already contains a file named ", basename(outFile), "\n\n",
                       "Would you like to replace the existing file?\n", sep="")
                ans <- as.character(tkmessageBox(icon="question", message=msg, 
                        title="Confirm File Replace", type="yesnocancel", parent=tt))
                if(ans == "cancel") break
                if(ans == "no") next
            }
            
            outPoly <- ply[[i]]
            
            write.polyfile(outPoly, outFile)
        }
        
        tkfocus(tt)
    }
    
    
    
# main program
    
    if(is.null(ply)) ply <- list()
    
    w <- 320
    h <- 320
    
    xran <- NULL
    yran <- NULL
    
    col.pal <- c("#FA3A3A", "#3EB636", "#000000", "#227CE8", "#F060A8", "#F18D31", 
                 "#46C5BD", "#AAC704", "#E64804", "#915135", "#4F575A", "#B1CD02", 
                 "#3DBF34", "#315A5E", "#5E3831", "#FA330C", "#D45B0A", "#494012")
    
    base.ply <- NULL
    
  # assign the variables linked to Tk widgets
    
    xy.var <- tclVar("x, y")
    rb.var <- tclVar("add")
    
    area.var <- tclVar("NA")
    poly.var <- tclVar("NA")
    hole.var <- tclVar("NA")
    vert.var <- tclVar("NA")
    
    list.var <- tclVar()
    for(i in names(ply)) tcl("lappend", list.var, i)
    
    cbox1.var <- if(!is.null(ply.ran) && ply.ran %in% names(ply)) tclVar(ply.ran) else tclVar()
    cbox2.var <- if(!is.null(ply.lim) && ply.lim %in% names(ply)) tclVar(ply.lim) else tclVar()
    
    tt.done.var <- tclVar(0)
    
  # open gui
    
    tclServiceMode(FALSE)
    
    tt <- tktoplevel(padx=5, pady=5)
    
    if(!missing(parent)) {
        tkwm.transient(tt, parent)
        tmp <- unlist(strsplit(as.character(tkwm.geometry(parent)), "\\+"))
        tkwm.geometry(tt, paste("+", as.integer(tmp[2]) + 25, "+", as.integer(tmp[3]) + 25, sep=""))
    }
    
    tktitle(tt) <- "Polygon Management"
    
  # menus
    
    top.menu <- tkmenu(tt, tearoff=0)
    
    menu.file <- tkmenu(tt, tearoff=0)
    tkadd(top.menu, "cascade", label="File", menu=menu.file, underline=0)
    
    tkadd(menu.file, "command", label="Open", accelerator="Ctrl+O", command=openPolygon)
    tkadd(menu.file, "command", label="Save", accelerator="Ctrl+S", command=savePolygon)
    tkadd(menu.file, "separator")
    tkadd(menu.file, "command", label="Rename", accelerator="Ctrl-R", command=renamePolygon)
    
    menu.select <- tkmenu(tt, tearoff=0)
    tkadd(top.menu, "cascade", label="Select", menu=menu.select, underline=0)
    tkadd(menu.select, "command", label="All", accelerator="Ctrl+A", command=function() {selectPolygon("all")})
    tkadd(menu.select, "command", label="Deselect", accelerator="Shift+Ctrl+A", command=function() {selectPolygon("none")})
    tkadd(menu.select, "command", label="Inverse", command=function() {selectPolygon("inverse")})
    
    menu.arrange <- tkmenu(tt, tearoff=0)
    tkadd(top.menu, "cascade", label="Arrange", menu=menu.arrange, underline=0)
    tkadd(menu.arrange, "command", label="Bring to front", accelerator="Shift+Ctrl+]", command=function() {arrange("front")})
    tkadd(menu.arrange, "command", label="Bring forward", accelerator="Ctrl+]", command=function() {arrange("forward")})
    tkadd(menu.arrange, "command", label="Send backward", accelerator="Ctrl+[", command=function() {arrange("backward")})
    tkadd(menu.arrange, "command", label="Send to back", accelerator="Shift+Ctrl+[", command=function() {arrange("back")})
    tkadd(menu.arrange, "separator")
    tkadd(menu.arrange, "command", label="Clear", accelerator="Del", command=clearPolygon)
    
    tkconfigure(tt, menu=top.menu)
    
  # frames
    
    pw <- ttkpanedwindow(tt, orient="horizontal")
    
    frame0 <- ttkframe(pw, relief="flat", borderwidth=0, padding=0)
    
    frame0.cvs <- tkcanvas(frame0, relief="flat", width=w, height=h, background="white", 
                  confine=TRUE, borderwidth=0, closeenough=0, cursor="crosshair")
    
    frame1 <- ttkframe(pw, relief="flat", borderwidth=0, padding=0)
    frame1.lst <- tklistbox(frame1, selectmode="extended", activestyle="underline", relief="flat", 
                  borderwidth=5, width=15, exportselection=FALSE, listvariable=list.var)
    frame1.ysc <- ttkscrollbar(frame1, orient="vertical")
    tkconfigure(frame1.lst, background="white", yscrollcommand=paste(.Tk.ID(frame1.ysc), "set"))
    tkconfigure(frame1.ysc, command=paste(.Tk.ID(frame1.lst), "yview"))
    tkpack(frame1.lst, side="left",  fill="both", expand=TRUE, pady=1)
    tkpack(frame1.ysc, side="right", fill="y", anchor="w", padx=0, pady=2)
    
    tkbind(frame1.lst, "<ButtonRelease-1>", plotPolygon)
    
    frame2 <- tkframe(pw, relief="flat")
    
    
    frame2a <- ttklabelframe(frame2, relief="flat", borderwidth=3, padding=3, text="Shape modes")
    
    frame2a.rb.1 <- ttkradiobutton(frame2a, variable=rb.var, command=plotPolygon, value="add", text="Unite")
    frame2a.rb.2 <- ttkradiobutton(frame2a, variable=rb.var, command=plotPolygon, value="sub", text="Minus front")
    frame2a.rb.3 <- ttkradiobutton(frame2a, variable=rb.var, command=plotPolygon, value="int", text="Intersect")
    frame2a.rb.4 <- ttkradiobutton(frame2a, variable=rb.var, command=plotPolygon, value="exc", text="Exclude overlapping")
    
    frame2a.but <- ttkbutton(frame2a, width=12, text="BUILD", command=saveNewPolygon)
    
    tkgrid(frame2a.rb.1, sticky="w")
    tkgrid(frame2a.rb.2, sticky="w")
    tkgrid(frame2a.rb.3, sticky="w")
    tkgrid(frame2a.rb.4, sticky="w")
    tkgrid(frame2a.but, pady=2)
    
    tcl("grid", "anchor", frame2a, "center")
    
    
    frame2b <- ttklabelframe(frame2, relief="flat", borderwidth=3, padding=3, text="Attributes")
    
    frame2b.lab.1.1 <- tklabel(frame2b, text="Area =")
    frame2b.lab.2.1 <- tklabel(frame2b, text="Polygons =")
    frame2b.lab.3.1 <- tklabel(frame2b, text="Holes =")
    frame2b.lab.4.1 <- tklabel(frame2b, text="Vertices =")
    
    frame2b.lab.1.2 <- tklabel(frame2b, text=tclvalue(area.var))
    frame2b.lab.2.2 <- tklabel(frame2b, text=tclvalue(poly.var))
    frame2b.lab.3.2 <- tklabel(frame2b, text=tclvalue(hole.var))
    frame2b.lab.4.2 <- tklabel(frame2b, text=tclvalue(vert.var))
    
    tkconfigure(frame2b.lab.1.2, textvariable=area.var)
    tkconfigure(frame2b.lab.2.2, textvariable=poly.var)
    tkconfigure(frame2b.lab.3.2, textvariable=hole.var)
    tkconfigure(frame2b.lab.4.2, textvariable=vert.var)
    
    tkgrid(frame2b.lab.1.1, frame2b.lab.1.2)
    tkgrid(frame2b.lab.2.1, frame2b.lab.2.2)
    tkgrid(frame2b.lab.3.1, frame2b.lab.3.2)
    tkgrid(frame2b.lab.4.1, frame2b.lab.4.2)
    
    tkgrid.configure(frame2b.lab.1.1, sticky="e")
    tkgrid.configure(frame2b.lab.2.1, sticky="e")
    tkgrid.configure(frame2b.lab.3.1, sticky="e")
    tkgrid.configure(frame2b.lab.4.1, sticky="e")
    
    tkgrid.configure(frame2b.lab.1.2, sticky="w")
    tkgrid.configure(frame2b.lab.2.2, sticky="w")
    tkgrid.configure(frame2b.lab.3.2, sticky="w")
    tkgrid.configure(frame2b.lab.4.2, sticky="w")
    
    tcl("grid", "anchor", frame2b, "w")
    
    
    frame2c <- ttklabelframe(frame2, relief="flat", borderwidth=3, padding=3, text="Data (x,y)")
    
    frame2c.lab.1.1 <- tklabel(frame2c, text="Range")
    frame2c.lab.2.1 <- tklabel(frame2c, text="Limit")
    
    vals <- c("", names(ply))
    frame2c.box.1.2 <- ttkcombobox(frame2c, state="readonly", values=vals, textvariable=cbox1.var, width=15)
    frame2c.box.2.2 <- ttkcombobox(frame2c, state="readonly", values=vals, textvariable=cbox2.var, width=15)
    
    tkgrid(frame2c.lab.1.1, frame2c.box.1.2, pady=3)
    tkgrid(frame2c.lab.2.1, frame2c.box.2.2, pady=3)
    
    tkgrid.configure(frame2c.lab.1.1, sticky="e")
    tkgrid.configure(frame2c.lab.2.1, sticky="e")
    
    tcl("grid", "anchor", frame2c, "center")
    
    
    frame2d <- tkframe(frame2, relief="flat")
    frame2d.lab <- tklabel(frame2d, text=tclvalue(xy.var))
    tkconfigure(frame2d.lab, textvariable=xy.var)
    tkpack(frame2d.lab, side="left")
    
### frame2d.grp <- ttksizegrip(frame2d)
### tkpack(frame2d.grp, side="right")
    
    
    tkpack(frame2a, fill="both", expand=TRUE, padx=3)
    tkpack(frame2b, fill="both", expand=TRUE, padx=3, pady=3)
    tkpack(frame2c, fill="both", expand=TRUE, padx=3, pady=3)
    tkpack(frame2d, fill="both")
    
    
    tkgrid(frame0.cvs, frame1, frame2, sticky="news")
    
    tkgrid.rowconfigure(frame0, frame0.cvs, weight=1)
    tkgrid.columnconfigure(frame0, frame0.cvs, weight=1)
    
    tkadd(pw, frame0, weight=5)
    tkadd(pw, frame1, weight=1)
    tkadd(pw, frame2, weight=0)
    
    tkpack(pw, fill="both", expand="yes")
    
  # bind events
    
    tkbind(frame0.cvs, "<Motion>", function(x, y) mouseMotion(x, y))
    tkbind(frame0.cvs, "<Leave>", mouseLeave)
    tkbind(frame0.cvs, "<Configure>", scaleCanvas)
    
    tkbind(tt, "<Control-o>", openPolygon)
    tkbind(tt, "<Control-s>", savePolygon)
    
    tkbind(tt, "<Control-a>", function() selectPolygon("all"))
    tkbind(tt, "<Shift-Control-A>", function() selectPolygon("none"))
    
    tkbind(tt, "<Control-]>", function() arrange("forward"))
    tkbind(tt, "<Shift-Control-}>", function() arrange("front"))
    tkbind(tt, "<Control-[>", function() arrange("backward"))
    tkbind(tt, "<Shift-Control-{>", function() arrange("back"))
    
    tkbind(tt, "<BackSpace>", clearPolygon)
    tkbind(tt, "<Delete>", clearPolygon)
    tkbind(tt, "<Control-r>", renamePolygon)
    
    tkbind(tt, "<Up>", plotPolygon)
    tkbind(tt, "<Down>", plotPolygon)
    
    
    
    tclServiceMode(TRUE)
    
  # gui control
    
    tkfocus(tt)
    tkgrab(tt)
    tkbind(tt, "<Destroy>", function() tclvalue(tt.done.var) <- 1)
    tkwait.variable(tt.done.var)
    
    ply.ran <- tclvalue(cbox1.var)
    ply.lim <- tclvalue(cbox2.var)
    
    if(length(ply) == 0) ply <- NULL
    if(ply.ran == "") ply.ran <- NULL
    if(ply.lim == "") ply.lim <- NULL
    
    rtn <- list(ply=ply, ply.ran=ply.ran, ply.lim=ply.lim)
    
    tkgrab.release(tt)
    tkdestroy(tt)
    
    rtn
}

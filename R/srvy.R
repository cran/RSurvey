"srvy" <- function() {
    
# additional functions (subroutines)
    
  # update all GUI parameters
    
    gui.update <- function(refresh=FALSE) {
        dat <- srvy.dat("data.raw")
        if(!is.null(dat) && length(dat) != 0) {
            
            d <- NULL
            vars <- srvy.dat("vars")
            
            d$x <- dat[[vars$x]]
            d$y <- dat[[vars$y]]
            
            if(refresh) {
                tclvalue(x1.var) <- ""
                tclvalue(x2.var) <- ""
                tclvalue(y1.var) <- ""
                tclvalue(y2.var) <- ""
                srvy.dat("data.pts", NULL)
                srvy.dat("data.grd", NULL)
            }
            if(refresh | is.null(vars$z)) {
                tclvalue(z1.var) <- ""
                tclvalue(z2.var) <- ""
            }
            if(refresh | is.null(vars$t)) {
                tclvalue(sd.var) <- ""
                tclvalue(ed.var) <- ""
                tclvalue(sh.var) <- ""
                tclvalue(eh.var) <- ""
                tclvalue(sm.var) <- ""
                tclvalue(em.var) <- ""
                tclvalue(ss.var) <- ""
                tclvalue(es.var) <- ""
            }
            
            if(tclvalue(x1.var) == "") tclvalue(x1.var) <- min(d$x, na.rm=TRUE)
            if(tclvalue(x2.var) == "") tclvalue(x2.var) <- max(d$x, na.rm=TRUE)
            if(tclvalue(y1.var) == "") tclvalue(y1.var) <- min(d$y, na.rm=TRUE)
            if(tclvalue(y2.var) == "") tclvalue(y2.var) <- max(d$y, na.rm=TRUE)
            
            srvy.dat("min.x", as.numeric(tclvalue(x1.var)))
            srvy.dat("max.x", as.numeric(tclvalue(x2.var)))
            srvy.dat("min.y", as.numeric(tclvalue(y1.var)))
            srvy.dat("max.y", as.numeric(tclvalue(y2.var)))
            
            if(!is.null(vars$z)) {
                d$z <- dat[[vars$z]]
                
                if(tclvalue(z1.var) == "") tclvalue(z1.var) <- min(d$z, na.rm=TRUE)
                if(tclvalue(z2.var) == "") tclvalue(z2.var) <- max(d$z, na.rm=TRUE)
                
                srvy.dat("min.z", as.numeric(tclvalue(z1.var)))
                srvy.dat("max.z", as.numeric(tclvalue(z2.var)))
            }
            
            if(!is.null(vars$t)) {
                d$t <- dat[[vars$t]]
                range.dat <- range(strptime(d$t, "%Y-%m-%d %H:%M:%OS"), na.rm=TRUE)
                
                if(tclvalue(sd.var) == "") tclvalue(sd.var) <- format(range.dat[1], format="%Y-%m-%d")
                if(tclvalue(ed.var) == "") tclvalue(ed.var) <- format(range.dat[2], format="%Y-%m-%d")
                if(tclvalue(sh.var) == "") tclvalue(sh.var) <- as.integer(format(range.dat[1], format="%H"))
                if(tclvalue(eh.var) == "") tclvalue(eh.var) <- as.integer(format(range.dat[2], format="%H"))
                if(tclvalue(sm.var) == "") tclvalue(sm.var) <- as.integer(format(range.dat[1], format="%M"))
                if(tclvalue(em.var) == "") tclvalue(em.var) <- as.integer(format(range.dat[2], format="%M"))
                if(tclvalue(ss.var) == "") tclvalue(ss.var) <- as.numeric(format(range.dat[1], format="%OS3"))
                if(tclvalue(es.var) == "") tclvalue(es.var) <- as.numeric(format(range.dat[2], format="%OS3"))
                
                srvy.dat("min.t", strptime(paste(tclvalue(sd.var), " ", 
                    as.integer(tclvalue(sh.var)), ":", as.integer(tclvalue(sm.var)), ":", 
                    as.numeric(tclvalue(ss.var)), sep=""), "%Y-%m-%d %H:%M:%OS"))
                srvy.dat("max.t", strptime(paste(tclvalue(ed.var), " ", 
                    as.integer(tclvalue(eh.var)), ":", as.integer(tclvalue(em.var)), ":", 
                    as.numeric(tclvalue(es.var)), sep=""), "%Y-%m-%d %H:%M:%OS"))
            }
        }
    }
    
  # restore entry widgets
    
    gui.restore <- function() {
        
        hld <- srvy.dat("min.x")
        if(!(is.null(hld) || is.na(hld))) tclvalue(x1.var) <- as.numeric(hld)
        hld <- srvy.dat("max.x")
        if(!(is.null(hld) || is.na(hld))) tclvalue(x2.var) <- as.numeric(hld)
        hld <- srvy.dat("min.y")
        if(!(is.null(hld) || is.na(hld))) tclvalue(y1.var) <- as.numeric(hld)
        hld <- srvy.dat("max.y")
        if(!(is.null(hld) || is.na(hld))) tclvalue(y2.var) <- as.numeric(hld)
        hld <- srvy.dat("min.z")
        if(!(is.null(hld) || is.na(hld))) tclvalue(z1.var) <- as.numeric(hld)
        hld <- srvy.dat("max.z")
        if(!(is.null(hld) || is.na(hld))) tclvalue(z2.var) <- as.numeric(hld)
        
        hld <- srvy.dat("min.t")
        if(!(is.null(hld) || is.na(hld))) {
            tclvalue(sd.var) <- format(hld, format="%Y-%m-%d")
            tclvalue(sh.var) <- as.integer(format(hld, format="%H"))
            tclvalue(sm.var) <- as.integer(format(hld, format="%M"))
            tclvalue(ss.var) <- as.numeric(format(hld, format="%OS"))
        }
        hld <- srvy.dat("max.t")
        if(!(is.null(hld) || is.na(hld))) {
            tclvalue(ed.var) <- format(hld, format="%Y-%m-%d")
            tclvalue(eh.var) <- as.integer(format(hld, format="%H"))
            tclvalue(em.var) <- as.integer(format(hld, format="%M"))
            tclvalue(es.var) <- as.numeric(format(hld, format="%OS"))
        }
    }
    
  # close GUI
    
    gui.close <- function() {
        if(as.integer(tclvalue(tt0.done.var)) != 0) return() 
        device.close()
        gui.update()
        
        tmp <- unlist(strsplit(as.character(tkwm.geometry(tt0)), "\\+"))
        srvy.dat("win.loc", paste("+", as.integer(tmp[2]), "+", as.integer(tmp[3]), sep=""))
        
        if(!(is.null(dev.list()))) 
            for(i in 1:length(dev.list())) dev.off()
        
        win <- ls(envir=.TkRoot$env, all=TRUE)
        num <- sort(suppressWarnings(as.integer(substr(win, 2, nchar(win)))), decreasing=TRUE)
        
        tmp <- paste(".", num, sep="")
        win <- tmp[.Tk.ID(tt0) != tmp]
        
        if(length(win) > 0) 
            for(i in 1:length(win)) tcl("destroy", win[i])
        
        tclvalue(tt0.done.var) <- 1
        tkdestroy(tt0)
        rm(tt0, pos=".GlobalEnv")
    } 
    
  # open binary data file
    
    proj.new <- function() {
        ans <- clear.objects("project")
        if(ans == "cancel") return()
    }
    
  # open binary data file
    
    proj.open <- function() {
        f <- getFile(cmd="Open", exts="rda", caption="Open Project File")
        if(is.null(f)) return(tkfocus(tt0))
        
        ans <- clear.objects("project")
        if(ans != "cancel") {
            load(file=f$path, envir=.GlobalEnv)
            srvy.dat("proj.file", f$path)
            gui.restore()
            gui.update()
        }
        tkfocus(tt0)
    }
    
  # save binary data file
    
    proj.save <- function() {
        if(!is.null(srvy.dat("proj.file"))) 
            if(file.access(srvy.dat("proj.file"), mode = 0) != 0) 
                srvy.dat("proj.file", NULL)
        if(is.null(srvy.dat("proj.file"))) {
            f <- getFile(cmd="Save As", exts="rda", caption="Save Project As", defaultextension="rda")
            if(!is.null(f)) {
                srvy.dat("proj.file", f$path)
                
                pth <- paste(head(unlist(strsplit(f$path, "/")), -1), collapse="/")
                srvy.dat("default.dir", pth)
            }
        }
        if(!is.null(srvy.dat("proj.file"))) {
            gui.update()
            save(list="srvy.dat", file=srvy.dat("proj.file"), compress=TRUE)
        }
        tkfocus(tt0)
    }
    
  # save a new binary data file
    
    proj.saveas <- function() {
        srvy.dat("proj.file", NULL)
        proj.save()
        tkfocus(tt0)
    }
    
  # clear objects
    
    clear.objects <- function(type) {
        
        save.vars <- c("ver", "font.gui", "default.dir", "win.loc", "csi",
                     "nlevels", "width", "cex.pts", "asp.yx", "asp.zx", "vmax", "vxby", "vyby", 
                     "rkey", "show.poly", "img.contour", "show.lines", "show.points", "show.arrows", "vuni", 
                     "date.fmt")
        
        if(type == "data") {
            ans <- ifelse(is.null(srvy.dat("data.file")), "ok", 
                   as.character(tkmessageBox(icon="question", 
                   message="This action will delete existing data?", 
                   title="Warning", type="okcancel", parent=tt0)))
            if(ans == "cancel") return(ans)
            save.vars <- append(save.vars, c("poly", "proj.file"))
            srvy.dat("data.raw", NULL)
            srvy.dat("data.pts", NULL)
            srvy.dat("data.grd", NULL)
        }
        
        if(type == "project") {
            ans <- ifelse(is.null(srvy.dat("proj.file")), "ok", 
                   as.character(tkmessageBox(icon="question", message="Save the existing project?", 
                   title="Warning", type="yesnocancel", parent=tt0)))
            if(ans == "cancel") 
                return(ans)
            if(ans == "yes") 
                proj.save()
        }
        
        hold.par <- srvy.dat()
        srvy.dat(clearAll=TRUE)
        
        for(i in save.vars) 
            srvy.dat(i, hold.par[[i]])
        
        tclvalue(sd.var) <- ""
        tclvalue(sh.var) <- ""
        tclvalue(sm.var) <- ""
        tclvalue(ss.var) <- ""
        tclvalue(ed.var) <- ""
        tclvalue(eh.var) <- ""
        tclvalue(em.var) <- ""
        tclvalue(es.var) <- ""
        tclvalue(x1.var) <- ""
        tclvalue(x2.var) <- ""
        tclvalue(y1.var) <- ""
        tclvalue(y2.var) <- ""
        tclvalue(z1.var) <- ""
        tclvalue(z2.var) <- ""
        
        ans
    }
    
  # import survey data
    
    data.import <- function() {
        f <- getFile(cmd="Open", exts=c("dat", "grd"), caption="Import Data")
        if(is.null(f) || clear.objects("data") != "ok") return(tkfocus(tt0))
        
        d <- readFile(f$path, sep=srvy.dat("sep"), encoding=srvy.dat("encoding"))
        
        srvy.dat("data.raw", d$data)
        srvy.dat("cols",     d$cols)
        srvy.dat("vars",     d$vars)
        
        srvy.dat("data.file", f$name)
        
        gui.update(refresh=TRUE)
        
        setvars()
    }
    
  # close graphic devices
    
    device.close <- function() {
        graphics.off()
        while(rgl.cur() != 0) 
            rgl.close()
    }
    
  # save R graphic devices
    
    device.save.r <- function() {
        if(is.null(dev.list())) return()
        
        f <- getFile(cmd="Save As", exts=c("eps", "png", "jpg", "jpeg", "pdf", "bmp", "tif", "tiff"), 
             caption="Save R Graphic As", defaultextension="eps")
        if(is.null(f)) return(tkfocus(tt0))
        
        savePlot(filename=f$path, type=f$ext)
    }
    
  # save RGL graphic devices
    
    device.save.rgl <- function() {
        if(rgl.cur() == 0) return()
        
        f <- getFile(cmd="Save As", exts=c("png", "eps", "pdf"), caption="Save RGL Graphic As",
             defaultextension="png")
        if(is.null(f)) return(tkfocus(tt0))
        
        
        if(f$ext == "png") 
            rgl.snapshot(filename=f$path, fmt=f$ext)
        else 
            rgl.postscript(filename=f$path, fmt=f$ext)
    }
    
  # polygon management
    
    poly.manage <- function() {
        
        old.ran <- srvy.dat("polyRange")
        old.lim <- srvy.dat("polyLimit")
        if(is.null(old.ran)) old.ran <- ""
        if(is.null(old.lim)) old.lim <- ""
        
        ans <- polyManage(tt0, srvy.dat("poly"), srvy.dat("polyRange"), srvy.dat("polyLimit"))
        srvy.dat("poly", ans$ply)
        srvy.dat("polyRange", ans$ply.ran)
        srvy.dat("polyLimit", ans$ply.lim)
        
        new.ran <- if(is.null(ans$ply.ran)) "" else ans$ply.ran
        new.lim <- if(is.null(ans$ply.lim)) "" else ans$ply.lim
        
        if(new.ran != old.ran || new.lim != old.lim) {
            srvy.dat("data.pts", NULL)
            srvy.dat("data.grd", NULL)
        }
    }
    
  # about package
    
    about <- function() {
        con <- ifelse(file.access(paste(getwd(), "/DESCRIPTION", sep=""), mode=0) == 0, "DESCRIPTION", 
               paste(.path.package(package="RSurvey", quiet=FALSE), "/DESCRIPTION", sep=""))
        txt <- paste(readLines(con, n=-1), collapse="\n")
        tkmessageBox(icon="info", message=txt, title="About RSurvey", parent=tt0)
    }
    
  # set data fields
    
    setvars <- function() {
        if(is.null(srvy.dat("vars"))) 
            stop(call.=FALSE, "No data exists.")
        
        cols <- srvy.dat("cols")
        n <- length(cols)
        cols.name  <- unlist(lapply(1:n, function(i) cols[[i]]$name))
        cols.class <- unlist(lapply(1:n, function(i) cols[[i]]$class))
        
        vars.old <- srvy.dat("vars")
        vars <- srvy.vars(cols.name, cols.class, vars.old, tt0)
        srvy.dat("vars", vars)
        
        if(!identical(vars$t, vars.old$t)) {
            tclvalue(sd.var) <- ""
            tclvalue(ed.var) <- ""
            tclvalue(sh.var) <- ""
            tclvalue(eh.var) <- ""
            tclvalue(sm.var) <- ""
            tclvalue(em.var) <- ""
            tclvalue(ss.var) <- ""
            tclvalue(es.var) <- ""
        }
        if(!identical(vars$x, vars.old$x)) {
            tclvalue(x1.var) <- ""
            tclvalue(x2.var) <- ""
        }
        if(!identical(vars$y, vars.old$y)) {
            tclvalue(y1.var) <- ""
            tclvalue(y2.var) <- ""
        }
        if(!identical(vars$z, vars.old$z)) {
            tclvalue(z1.var) <- ""
            tclvalue(z2.var) <- ""
        }
        if(!identical(vars, vars.old)) {
            srvy.dat("data.pts", NULL)
            srvy.dat("data.grd", NULL)
            gui.update()
        }
    }
    
  # axis label
    
    axis.label <- function(cols, variable) {
        if(is.null(variable) || is.null(cols[[variable]])) return(NULL)
        paste(cols[[variable]]$name, " (", cols[[variable]]$unit, ")", sep="")
    }
    
  # construct polygon
    
    poly.construct <- function(type) {
        if(is.null(srvy.dat("data.file"))) 
            stop(call.=FALSE, "No data is available.")
        
        txt <- paste("After the plot has been created, use the mouse to identify the",
               "vertices of the polygon. The identification process can be terminated",
               "by clicking the second button and selecting [Stop] from the menu,",
               "or from the [Stop] menu on the graphics window.", sep="\n")
        tkmessageBox(icon="info", message=txt, title="Polygon Construction", parent=tt0)
        
        tkconfigure(tt0, cursor="watch")
        
        plot2d(type=type, buildpoly=TRUE)
        
        tkconfigure(tt0, cursor="arrow")
        tkfocus(tt0)
    }
    
  # autocrop polygon
    
    poly.autocrop <- function(type) {
        
        if(is.null(srvy.dat("data.file"))) 
            stop(call.=FALSE, "No data is available.")
        gui.update()
        srvy.process()
        
        dat  <- srvy.dat("data.pts")
        vars <- srvy.dat("vars")
        cols <- srvy.dat("cols")
        
        dat$vx <- NULL
        dat$vy <- NULL
        xlab <- axis.label(cols, vars$x)
        ylab <- axis.label(cols, vars$y)
        zlab <- axis.label(cols, vars$z)
        
        plyNew <- polyAutocrop(dat, xlab, ylab, zlab, tt0)
        
        if(class(plyNew) == "gpc.poly") {
            
            ply <- if(is.null(srvy.dat("poly"))) list() else srvy.dat("poly")
            plyName <- namePolygon(old=names(ply))
            ply[[plyName]] <- plyNew
            srvy.dat("poly", ply)
            srvy.dat("polyLimit", plyName)
            
            srvy.dat("data.pts", NULL)
            srvy.dat("data.grd", NULL)
            
            srvy.process(const.3d=TRUE)
        }
        
        tkfocus(tt0)
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
    
  # plot temporal data
    
    plott <- function() {
        
        vars <- srvy.dat("vars")
        if(is.null(vars$t)) 
            stop("No temporal data is available")
        
        tkconfigure(tt0, cursor="watch")
        
        gui.update()
        srvy.process()
        
        dat <- srvy.dat("data.pts")
        lim <- srvy.dat("limits")
        
        cols <- srvy.dat("cols")
        vars <- srvy.dat("vars")
        
        ylab <- axis.label(cols, vars$z)
        
        gap   <- srvy.dat("time.gap")
        width <- srvy.dat("width")
        cex   <- srvy.dat("cex.pts")
        
        plotTime(x=dat$t, y=dat$z, xlim=lim$tlim, ylim=lim$zlim, ylab=ylab, gap=gap, width=width)
        
        tkconfigure(tt0, cursor="arrow")
        tkfocus(tt0)
    }
    
  # plot point or 2d surface data
    
    plot2d <- function(type, buildpoly=FALSE) {
        
        vars <- srvy.dat("vars")
        if(is.null(vars$x) | is.null(vars$y)) 
            stop("spatial data is not available")
        if(is.null(vars$z) & type %in% c("l", "g"))
            stop("state variable has not been specified")
        
        tkconfigure(tt0, cursor="watch")
        
        gui.update()
        srvy.process(const.3d=TRUE)
        
        polyName <- if(type == "p") srvy.dat("polyRange") else srvy.dat("polyLimit")
        
        ply <- if(is.null(polyName)) NULL else srvy.dat("poly")[[polyName]]
        showpoly <- srvy.dat("show.poly") && class(ply) == "gpc.poly"
        
        showarrows <- srvy.dat("show.arrows")
        
        showlines  <- type %in% c("l", "g") && srvy.dat("show.lines")
        showpoints <- type %in% c("l", "g") && srvy.dat("show.points")
        
        cols <- srvy.dat("cols")
        
        xlab <- axis.label(cols, vars$x)
        ylab <- axis.label(cols, vars$y)
        zlab <- axis.label(cols, vars$z)
        
        if(type == "p") 
            dat <- srvy.dat("data.pts")
        if(type %in% c("l", "g")) {
            dat <- srvy.dat("data.grd")
            
            if(type == "g") {
                elemAve <- function(z) {
                    m <- nrow(z)
                    n <- ncol(z)
                    (z[1:(m-1), 1:(n-1)] + z[1:(m-1), 2:n] + z[2:m, 1:(n-1)] + z[2:m, 2:n]) / 4
                }
                dat$z <- elemAve(dat$z)
                if(showarrows) {
                    if(!is.null(dat$vx)) dat$vx <- elemAve(dat$vx)
                    if(!is.null(dat$vy)) dat$vy <- elemAve(dat$vy)
                }
            }
        }
        
        if(!showarrows) {
            dat$vx <- NULL
            dat$vy <- NULL
        }
        
        xran <- range(dat$x, finite=TRUE)
        yran <- range(dat$y, finite=TRUE)
        
      # adjust plot axes limits for polygon
        
        lim <- srvy.dat("limits")
        
        xlim <- if(is.null(lim$xlim)) c(NA, NA) else lim$xlim
        ylim <- if(is.null(lim$ylim)) c(NA, NA) else lim$ylim
        
        if(showpoly) {
            bbx <- bby <- NULL
            
            if(showpoly) {
                
                bb <- get.bbox(ply)
                
                if(!is.na(xlim[1])) bb$x[1] <- xlim[1]
                if(!is.na(xlim[2])) bb$x[2] <- xlim[2]
                if(!is.na(ylim[1])) bb$y[1] <- ylim[1]
                if(!is.na(ylim[2])) bb$y[2] <- ylim[2]
                
                xy <- cbind(x=c(bb$x, rev(bb$x)), y=c(bb$y[c(1,1)], bb$y[c(2,2)]))
                
                bb <- get.bbox(intersect(ply, as(xy, "gpc.poly")))
                bbx <- range(bb$x, f=0.02)
                bby <- range(bb$y, f=0.02)
            }
            
            bbx <- extendrange(bbx, f=0.02)
            bby <- extendrange(bby, f=0.02)
            if(is.na(xlim[1]) && bbx[1] < xran[1]) lim$xlim[1] <- bbx[1]
            if(is.na(xlim[2]) && bbx[2] > xran[2]) lim$xlim[2] <- bbx[2]
            if(is.na(ylim[1]) && bby[1] < yran[1]) lim$ylim[1] <- bby[1]
            if(is.na(ylim[2]) && bby[2] > yran[2]) lim$ylim[2] <- bby[2]
        }
        
        
        plotSurvey2d(dat, type=type, xlim=lim$xlim, ylim=lim$ylim, zlim=lim$zlim, 
            xlab=xlab, ylab=ylab, zlab=zlab, asp=srvy.dat("asp.yx"), csi=srvy.dat("csi"), 
            width=srvy.dat("width"), nlevels=srvy.dat("nlevels"), cex.pts=srvy.dat("cex.pts"), 
            rkey=srvy.dat("rkey"), vuni=srvy.dat("vuni"), vmax=srvy.dat("vmax"), 
            vxby=srvy.dat("vxby"), vyby=srvy.dat("vyby"))
        
        
        if(showlines) {
            contourLines(dat, nlevels=srvy.dat("nlevels"))
            contour(dat, col="black", lty="solid", add=TRUE, nlevels=srvy.dat("nlevels"), 
                vfont = c("sans serif", "plain"))
        }
        
        if(showpoly) 
            plot(ply, add=TRUE, poly.args=list(border="black", lty=3))
        
        if(showpoints) {
            points(x=srvy.dat("data.pts")$x, y=srvy.dat("data.pts")$y, pch=19, 
                cex=srvy.dat("cex.pts") / 2, col="black")
        }
        
        if(buildpoly) {
            v <- locator(type="o", col="black", cex=0.5, pch=15)
            lines(cbind(c(v$x, v$x[1]), c(v$y, v$y[1])), col="black")
            
            plyNew <- as(as.data.frame(v), "gpc.poly")
            
            ply <- if(is.null(srvy.dat("poly"))) list() else srvy.dat("poly")
            plyName <- namePolygon(old=names(ply))
            ply[[plyName]] <- plyNew
            srvy.dat("poly", ply)
            
            if(class(plyNew) == "gpc.poly") {
                if(type == "p") {
                    pol <- get.pts(plyNew)[[1]]
                    pts <- point.in.polygon(point.x=dat$x, point.y=dat$y, pol.x=pol$x, pol.y=pol$y) > 0
                    points(dat$x[pts], dat$y[pts], col="red", cex=srvy.dat("cex.pts"), pch=20)
                    srvy.dat("polyRange", plyName)
                }
                if(type == "l") {
                    cutout <- polyCutout(dat, plyNew)
                    x <- cutout$x
                    y <- cutout$y
                    z <- cutout$z
                    levels <- pretty(range(z, na.rm=TRUE), srvy.dat("nlevels"))
                    col <- colorRampPalette(c("blue", "white", "red"))(length(levels) - 1)
                    if(!is.double(z)) storage.mode(z) <- "double"
                    .Internal(filledcontour(as.double(x), as.double(y), z, as.double(levels), col=col))
                    srvy.dat("polyLimit", plyName)
                }
                
                srvy.dat("data.pts", NULL)
                srvy.dat("data.grd", NULL)
                
                srvy.process(const.3d=TRUE)
            }
        }
        tkconfigure(tt0, cursor="arrow")
        tkfocus(tt0)
    }
    
  # plot 3d surface data
    
    plot3d <- function() {
        
        vars <- srvy.dat("vars")
        if(is.null(vars$x) | is.null(vars$y) | is.null(vars$z)) 
            stop("No data is available")
        
        tkconfigure(tt0, cursor="watch")
        
        gui.update()
        srvy.process(const.3d=TRUE)
        
        dat <- srvy.dat("data.grd")
        
        pts <- NULL
        if(srvy.dat("show.points")) 
            pts <- srvy.dat("data.pts")
        
      # scale z
        
        if(!is.null(srvy.dat("depth")) && as.logical(srvy.dat("depth"))) {
            zmax <- max(dat$z, na.rm=TRUE)
            dat$z <- zmax - dat$z
            if(!is.null(pts$z)) 
                pts$z <- zmax - pts$z
        }
        
        plotSurvey3d(x=dat, px=pts, vasp=srvy.dat("asp.zx"), hasp=srvy.dat("asp.yx"), 
            width=srvy.dat("width"), cex.pts=srvy.dat("cex.pts"))
        
        tkconfigure(tt0, cursor="arrow")
        tkfocus(tt0)
    }
    
  # open html help for R functions
    
    funHelp <- function() {
        if(!("RSurvey" %in% .packages(all.available=TRUE))) 
            stop("requires installed RSurvey package", call. = FALSE)
        if(tools:::httpdPort == 0L) 
            tools::startDynamicHelp()
        if(tools:::httpdPort > 0L) {
            url <- paste("http://127.0.0.1:", tools:::httpdPort, "/library/RSurvey/html/00Index.html", sep = "")
            cat(gettextf("If nothing happens, you should open\n'%s' yourself\n", url))
            browseURL(url, browser=getOption("browser"))
        }
        else stop("requires the HTTP server to be running", call.=FALSE)
        invisible()
    }
    
    
    
# main program
    
  # load required R packages
    
    loadPackages()
    
  # warn if using Windows and running in MDI mode
    
    if(.Platform$OS.type == "windows" && getIdentification() == "RGui") 
        message("\n\n    You are running R in MDI mode which *may* interfere\n", 
                    "    with the functionality of the graphical user interface.\n", 
                    "    It is recommended to use R in SDI mode which can be\n", 
                    "    set in the command line or by clicking in the Menu:\n", 
                    "    Edit - GUI Preferences: SDI, then Save and restart R.\n\n")
    
  # establish working directory
    
    path <- ifelse("package:RSurvey" %in% search(), system.file("RSurvey-ex", package="RSurvey"), getwd())
    
    if(is.null(srvy.dat("default.dir"))) 
        srvy.dat("default.dir", path)
    
    if("package:RSurvey" %in% search()) 
        imgPath <- system.file("images", package="RSurvey")
    else 
        imgPath <- paste(path, "/inst/images/", sep="")
    
  # exit if GUI object is present
    
    if(exists("tt0", where=".GlobalEnv")) 
        stop("An older instance of RSurvey is active and will be brought forward.")
    
  # determine graphics parameters and set options
    
    x11(pointsize=12)
    srvy.dat("csi", par("csi"))
    dev.off()
    
    options(digits.secs=3)
    
  # assign variables linked to Tk entry widgets
    
    sd.var <- tclVar()
    sh.var <- tclVar()
    sm.var <- tclVar()
    ss.var <- tclVar()
    ed.var <- tclVar()
    eh.var <- tclVar()
    em.var <- tclVar()
    es.var <- tclVar()
    x1.var <- tclVar()
    x2.var <- tclVar()
    y1.var <- tclVar()
    y2.var <- tclVar()
    z1.var <- tclVar()
    z2.var <- tclVar()
    
    tt0.done.var <- tclVar(0)
    
  # package version number
    
    f <- ifelse("package:RSurvey" %in% search(), 
         system.file("DESCRIPTION", package="RSurvey"), "DESCRIPTION")
    ver <- scan(f, what="character", skip=3, nlines=1, quiet=TRUE)[2]
    srvy.dat("ver", paste("RSurvey", ver))
    
  # open gui
    
    tt0 <- tktoplevel()
    tkwm.geometry(tt0, srvy.dat("win.loc"))
    tktitle(tt0) <- srvy.dat("ver")
    tkwm.resizable(tt0, 0, 0)
    
  # top menu
    
    top.menu <- tkmenu(tt0, tearoff=0)
    
  # project menu
    
    menu.proj <- tkmenu(tt0, tearoff=0)
    tkadd(top.menu, "cascade", label="Project", menu=menu.proj, underline=0)
    
    tkadd(menu.proj, "command", label="New", accelerator="Ctrl+N", command=proj.new)
    tkadd(menu.proj, "separator")
    tkadd(menu.proj, "command", label="Open", accelerator="Ctrl+O", command=proj.open)
    tkadd(menu.proj, "command", label="Save", accelerator="Ctrl+S", command=proj.save)
    tkadd(menu.proj, "command", label="Save As", accelerator="Shift+Ctrl+S", command=proj.saveas)
    tkadd(menu.proj, "separator")
    tkadd(menu.proj, "command", label="Close All Plots", accelerator="Ctrl+F4", command=device.close)
    
    menu.proj.save <- tkmenu(tt0, tearoff=0)
    tkadd(menu.proj.save, "command", label="R Graphic", accelerator="Ctrl+R", command=device.save.r)
    tkadd(menu.proj.save, "command", label="RGL Graphic", command=device.save.rgl)
    tkadd(menu.proj, "cascade", label="Save Plot", menu=menu.proj.save)
    
    tkadd(menu.proj, "separator")
    tkadd(menu.proj, "command", label="Exit", command=gui.close)
    
    tkbind(tt0, "<Control-n>", proj.new)
    tkbind(tt0, "<Control-o>", proj.open)
    tkbind(tt0, "<Control-s>", proj.save)
    tkbind(tt0, "<Shift-Control-S>", proj.saveas)
    tkbind(tt0, "<Control-F4>", device.close)
    tkbind(tt0, "<Control-r>", device.save.r)
    
  # data menu
    
    menu.data <- tkmenu(tt0, tearoff=0)
    tkadd(top.menu, "cascade", label="Data", menu=menu.data, underline=0)
    
    tkadd(menu.data, "command", label="Import", command=data.import)
    
    tkadd(menu.data, "command", label="Export", 
        command=function() {
            gui.update()
            if(is.null(srvy.dat("data.raw"))) 
                stop(call.=FALSE, "No data exists.")
            srvy.process(const.3d=TRUE)
            if(!is.null(srvy.dat("data.grd"))) 
                writeFile()
            tkfocus(tt0)
        }
    )
    tkadd(menu.data, "separator")
    tkadd(menu.data, "command", label="Variables", command=setvars)
    tkadd(menu.data, "command", label="Preferences", 
        command=function() {
            gui.update()
            srvy.pref(tt0)
        }
    )
    tkadd(menu.data, "separator")
    tkadd(menu.data, "command", label="Axes Limits", 
        command=function() {
            srvy.dat("limits", axesLimits(tt0, srvy.dat("limits"), addt=TRUE))
        }
    )
    
  # polygon menu
    
    menu.poly <- tkmenu(tt0, tearoff=0)
    
    tkadd(top.menu, "cascade", label="Polygon", menu=menu.poly, underline=0)
    
    tkadd(menu.poly, "command", label="Manage", command=poly.manage)
    
    tkadd(menu.poly, "separator")
    
    menu.poly.con <- tkmenu(tt0, tearoff=0)
    tkadd(menu.poly.con, "command", label="Range", 
        command=function() {
            poly.construct(type="p")
        }
    )
    tkadd(menu.poly.con, "command", label="Limits", 
        command=function() {
            poly.construct(type="l")
        }
    )
    tkadd(menu.poly, "cascade", label="Construct ", menu=menu.poly.con)
    
    tkadd(menu.poly, "command", label="Autocrop Limits", command=poly.autocrop)
    
    tkadd(menu.poly, "separator")
    tkadd(menu.poly, "command", label="Clear All", 
        command=function() {
            
            srvy.dat("polyRange", NULL)
            srvy.dat("polyLimit", NULL)
            
            srvy.dat("data.pts", NULL)
            srvy.dat("data.grd", NULL)
        }
    )
    
  # advanced menu
    
    menu.advanced <- tkmenu(tt0, tearoff=0)
    tkadd(top.menu, "cascade", label="Advanced", menu=menu.advanced, underline=0)
    
    tkadd(menu.advanced, "command", label="Configuration", 
        command=function() {
            srvy.config(tt0)
        }
    )
    tkadd(menu.advanced, "command", label="Geographical Projection", 
        command=function() {
            ans <- geoproj(tt0, old=srvy.dat("projection"))
            srvy.dat("projection", ans)
        }
    )
    if(!("RSurvey" %in% .packages())) {
        tkadd(menu.advanced, "separator")
        tkadd(menu.advanced, "command", label="Restore R Image", 
            command=function() {
                gui.close()
                restoreSession(paste(getwd(), "R", sep="/"), save.objs="srvy.dat", fun.call="srvy")
            }
        )
    }
    
  # help menu
    
    menu.help <- tkmenu(tt0, tearoff=0)
    tkadd(top.menu, "cascade", label="Help", menu=menu.help, underline=0)
    tkadd(menu.help, "command", label="R Functions", command=funHelp)
    tkadd(menu.help, "separator")
    tkadd(menu.help, "command", label="About", command=about)
    
  # finalize top menu
    
    tkconfigure(tt0, menu=top.menu)
    assign("tt0", tt0, pos=1)
    
  #  frame 0 contains a toolbar with command buttons
    
    new.var      <- tclVar()
    open.var     <- tclVar()
    save.var     <- tclVar()
    import.var   <- tclVar()
    export.var   <- tclVar()
    axes.var     <- tclVar()
    polygon.var  <- tclVar()
    config.var   <- tclVar()
    globe.var    <- tclVar()
    help.var     <- tclVar()
    refresh.var  <- tclVar()
    close.var    <- tclVar()
    
    frame0 <- ttkframe(tt0, relief="flat", borderwidth=2)
    tkpack(frame0, side="top", fill="x")
    
    tkimage.create("photo", new.var,      format="GIF", file=paste(imgPath, "new.gif",      sep="/"))
    tkimage.create("photo", open.var,     format="GIF", file=paste(imgPath, "open.gif",     sep="/"))
    tkimage.create("photo", save.var,     format="GIF", file=paste(imgPath, "save.gif",     sep="/"))
    tkimage.create("photo", globe.var,    format="GIF", file=paste(imgPath, "globe.gif",    sep="/"))
    tkimage.create("photo", config.var,   format="GIF", file=paste(imgPath, "config.gif",   sep="/"))
    tkimage.create("photo", axes.var,     format="GIF", file=paste(imgPath, "axes.gif",     sep="/"))
    tkimage.create("photo", polygon.var,  format="GIF", file=paste(imgPath, "polygon.gif",  sep="/"))
    tkimage.create("photo", help.var,     format="GIF", file=paste(imgPath, "help.gif",     sep="/"))
    tkimage.create("photo", refresh.var,  format="GIF", file=paste(imgPath, "refresh.gif",  sep="/"))
    tkimage.create("photo", close.var,    format="GIF", file=paste(imgPath, "close.gif",    sep="/"))
    
    frame0.but.1  <- tkbutton(frame0, relief="flat", overrelief="raised", borderwidth=1, image=new.var,    command=proj.new)
    frame0.but.2  <- tkbutton(frame0, relief="flat", overrelief="raised", borderwidth=1, image=open.var,   command=proj.open)
    frame0.but.3  <- tkbutton(frame0, relief="flat", overrelief="raised", borderwidth=1, image=save.var,   command=proj.save)
    frame0.but.4  <- tkbutton(frame0, relief="flat", overrelief="raised", borderwidth=1, image=axes.var, 
                         command=function() {
                             srvy.dat("limits", axesLimits(tt0, srvy.dat("limits"), addt=TRUE))
                         }
                     )
    frame0.but.5  <- tkbutton(frame0, relief="flat", overrelief="raised", borderwidth=1, image=polygon.var, command=poly.manage)
    frame0.but.6  <- tkbutton(frame0, relief="flat", overrelief="raised", borderwidth=1, image=globe.var, 
                         command=function() {
                             ans <- geoproj(tt0, old=srvy.dat("projection"))
                             srvy.dat("projection", ans)
                         }
                     )
    frame0.but.7  <- tkbutton(frame0, relief="flat", overrelief="raised", borderwidth=1, image=config.var, 
                         command=function() {
                             srvy.config(tt0)
                         }
                     )
    frame0.but.8  <- tkbutton(frame0, relief="flat", overrelief="raised", borderwidth=1, image=help.var, command=funHelp)
    frame0.but.9  <- tkbutton(frame0, relief="flat", overrelief="raised", borderwidth=1, image=refresh.var, 
                         command=function() {
                             gui.update(refresh=TRUE)
                             tkconfigure(tt0, cursor="arrow")
                             tkfocus(tt0)
                         }
                     )
    frame0.but.10 <- tkbutton(frame0, relief="flat", overrelief="raised", borderwidth=1, image=close.var, command=device.close)
    
    tkpack(frame0.but.1, frame0.but.2, frame0.but.3, frame0.but.4, frame0.but.5, frame0.but.6, frame0.but.7, 
        frame0.but.8, frame0.but.9, frame0.but.10, side="left")
    
    separator <- ttkseparator(tt0, orient="horizontal")
    tkpack(separator, fill="x")
    
  # frame 1 contains temporal information
    
    frame1 <- ttkframe(tt0, relief="flat", borderwidth=2)
    
    frame1.lab.1.1 <- ttklabel(frame1, text=" ")
    frame1.lab.1.2 <- ttklabel(frame1, text="Date (Y-m-d)")
    frame1.lab.1.3 <- ttklabel(frame1, text="Hr. (0-23)")
    frame1.lab.1.4 <- ttklabel(frame1, text="Min. (0-59)")
    frame1.lab.1.5 <- ttklabel(frame1, text="Sec. (0-59.999)")
    frame1.lab.2.1 <- ttklabel(frame1, text="Min")
    frame1.lab.3.1 <- ttklabel(frame1, text="Max")
    
    frame1.lab.4.1 <- ttklabel(frame1, text=" ")
    frame1.lab.4.2 <- ttklabel(frame1, text="Minimum Range [L]")
    frame1.lab.4.4 <- ttklabel(frame1, text="Maximum Range [L]")
    
    frame1.lab.5.1 <- ttklabel(frame1, text="x")
    frame1.lab.6.1 <- ttklabel(frame1, text="y")
    frame1.lab.7.1 <- ttklabel(frame1, text="z")
    
    frame1.ent.2.2 <- ttkentry(frame1, width=14, textvariable=sd.var)
    frame1.ent.2.3 <- ttkentry(frame1, width= 9, textvariable=sh.var)
    frame1.ent.2.4 <- ttkentry(frame1, width= 9, textvariable=sm.var)
    frame1.ent.2.5 <- ttkentry(frame1, width=14, textvariable=ss.var)
    frame1.ent.3.2 <- ttkentry(frame1, width=14, textvariable=ed.var)
    frame1.ent.3.3 <- ttkentry(frame1, width= 9, textvariable=eh.var)
    frame1.ent.3.4 <- ttkentry(frame1, width= 9, textvariable=em.var)
    frame1.ent.3.5 <- ttkentry(frame1, width=14, textvariable=es.var)
    
    frame1.ent.5.2 <- ttkentry(frame1, textvariable=x1.var)
    frame1.ent.5.4 <- ttkentry(frame1, textvariable=x2.var)
    frame1.ent.6.2 <- ttkentry(frame1, textvariable=y1.var)
    frame1.ent.6.4 <- ttkentry(frame1, textvariable=y2.var)
    frame1.ent.7.2 <- ttkentry(frame1, textvariable=z1.var)
    frame1.ent.7.4 <- ttkentry(frame1, textvariable=z2.var)
    
    tkbind(frame1.ent.2.2, "<KeyRelease>", function(){tclvalue(sd.var) <- keyEvent("date",   tclvalue(sd.var), rm.data=TRUE)})
    tkbind(frame1.ent.2.3, "<KeyRelease>", function(){tclvalue(sh.var) <- keyEvent("hour",   tclvalue(sh.var), rm.data=TRUE)})
    tkbind(frame1.ent.2.4, "<KeyRelease>", function(){tclvalue(sm.var) <- keyEvent("minute", tclvalue(sm.var), rm.data=TRUE)})
    tkbind(frame1.ent.2.5, "<KeyRelease>", function(){tclvalue(ss.var) <- keyEvent("second", tclvalue(ss.var), rm.data=TRUE)})
    tkbind(frame1.ent.3.2, "<KeyRelease>", function(){tclvalue(ed.var) <- keyEvent("date",   tclvalue(ed.var), rm.data=TRUE)})
    tkbind(frame1.ent.3.3, "<KeyRelease>", function(){tclvalue(eh.var) <- keyEvent("hour",   tclvalue(eh.var), rm.data=TRUE)})
    tkbind(frame1.ent.3.4, "<KeyRelease>", function(){tclvalue(em.var) <- keyEvent("minute", tclvalue(em.var), rm.data=TRUE)})
    tkbind(frame1.ent.3.5, "<KeyRelease>", function(){tclvalue(es.var) <- keyEvent("second", tclvalue(es.var), rm.data=TRUE)})
    tkbind(frame1.ent.5.2, "<KeyRelease>", function(){tclvalue(x1.var) <- keyEvent("real",   tclvalue(x1.var), rm.data=TRUE)})
    tkbind(frame1.ent.5.4, "<KeyRelease>", function(){tclvalue(x2.var) <- keyEvent("real",   tclvalue(x2.var), rm.data=TRUE)})
    tkbind(frame1.ent.6.2, "<KeyRelease>", function(){tclvalue(y1.var) <- keyEvent("real",   tclvalue(y1.var), rm.data=TRUE)})
    tkbind(frame1.ent.6.4, "<KeyRelease>", function(){tclvalue(y2.var) <- keyEvent("real",   tclvalue(y2.var), rm.data=TRUE)})
    tkbind(frame1.ent.7.2, "<KeyRelease>", function(){tclvalue(z1.var) <- keyEvent("real",   tclvalue(z1.var), rm.data=TRUE)})
    tkbind(frame1.ent.7.4, "<KeyRelease>", function(){tclvalue(z2.var) <- keyEvent("real",   tclvalue(z2.var), rm.data=TRUE)})
    
    tkgrid(frame1.lab.1.1, frame1.lab.1.2, frame1.lab.1.3, frame1.lab.1.4, frame1.lab.1.5, pady=0, padx=1)
    tkgrid(frame1.lab.2.1, frame1.ent.2.2, frame1.ent.2.3, frame1.ent.2.4, frame1.ent.2.5, pady=1, padx=1)
    tkgrid(frame1.lab.3.1, frame1.ent.3.2, frame1.ent.3.3, frame1.ent.3.4, frame1.ent.3.5, pady=1, padx=1)
    tkgrid(frame1.lab.4.1, frame1.lab.4.2, frame1.lab.4.4, pady=1, padx=1)
    tkgrid(frame1.lab.5.1, frame1.ent.5.2, frame1.ent.5.4, pady=1, padx=1)
    tkgrid(frame1.lab.6.1, frame1.ent.6.2, frame1.ent.6.4, pady=1, padx=1)
    tkgrid(frame1.lab.7.1, frame1.ent.7.2, frame1.ent.7.4, pady=1, padx=1)
    
    tkgrid.configure(frame1.lab.2.1, sticky="e")
    tkgrid.configure(frame1.lab.3.1, sticky="e")
    tkgrid.configure(frame1.lab.4.2, column=1, columnspan=2, row=3)
    tkgrid.configure(frame1.lab.4.4, column=3, columnspan=2, row=3)
    tkgrid.configure(frame1.lab.5.1, sticky="e")
    tkgrid.configure(frame1.ent.5.2, column=1, columnspan=2, row=4, sticky="w e")
    tkgrid.configure(frame1.ent.5.4, column=3, columnspan=2, row=4, sticky="w e")
    tkgrid.configure(frame1.lab.6.1, sticky="e")
    tkgrid.configure(frame1.ent.6.2, column=1, columnspan=2, row=5, sticky="w e")
    tkgrid.configure(frame1.ent.6.4, column=3, columnspan=2, row=5, sticky="w e")
    tkgrid.configure(frame1.lab.7.1, sticky="e")
    tkgrid.configure(frame1.ent.7.2, column=1, columnspan=2, row=6, sticky="w e")
    tkgrid.configure(frame1.ent.7.4, column=3, columnspan=2, row=6, sticky="w e")
    
    tkpack(frame1, fill="both", padx=2)
    
  # frame 2 contains plotting buttons
    
    frame2 <- ttkframe(tt0, relief="flat", borderwidth=2)
    
    frame2.but.1 <- ttkbutton(frame2, width=12, text="TEMPORAL", command=plott)
    frame2.but.2 <- ttkbutton(frame2, width=12, text="POINTS",
                        command=function() {
                            plot2d(type="p")
                        }
                    )
    frame2.but.3 <- ttkbutton(frame2, width=12, text="2D SURFACE", 
                        command=function() {
                            type <- if(srvy.dat("img.contour")) "g" else "l"
                            plot2d(type=type)
                        }
                    )
    frame2.but.4 <- ttkbutton(frame2, width=12, text="3D SURFACE", command=plot3d)
    
    tkgrid(frame2.but.1, frame2.but.2, frame2.but.3, frame2.but.4, padx=2, pady=2)
    
    tkpack(frame2, pady=2, padx=2)
    
  # gui closure
    
    gui.restore()
    gui.update()
    tkfocus(tt0)
    tkbind(tt0, "<Destroy>", gui.close)
}

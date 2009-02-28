"plotTime" <- function() {
    
  # window setup
    
    win.width <- win.height <- srvy.dat("win.width")
    x11(width=win.width, height=win.height, pointsize=10)
    
    op <- par(mfrow=c(2, 1), bg="white", mar=c(3, 4, 1, 1) + 0.1)
    
  # axis labels
    
    vars <- srvy.dat("vars")
    state.id <- vars[4]
    
  # state variable and time offsets
    
    off.z <- ifelse(is.null(srvy.dat("off.z")), 0, srvy.dat("off.z"))
    off.t <- ifelse(is.null(srvy.dat("off.t")), 0, srvy.dat("off.t"))
    
  # simplify data
    
    data.raw <- srvy.dat("data.raw")
    data.mod <- srvy.dat("data.mod")
    
  # upper plot
    
    datetime <- strptime(data.raw[[make.names(vars[1])]], "%Y-%m-%d %H:%M:%OS")
    
    x <- as.numeric(difftime(datetime, min(datetime, na.rm=TRUE), units="secs"))
    y <- data.raw[[make.names(vars[4])]]
    
    plot(NA, xlim=range(x, na.rm=TRUE), ylim=range(y, na.rm=TRUE), type="n", ann=FALSE, cex.axis=0.7, xaxt="n")
    
    lines(x, y, col="red")
    
    x.tic <- datetime[1] + axTicks(1)
    xlab <- format(x.tic, format="%H:%M:%OS\n%Y-%m-%d")
    axis(side=1, las=1, cex.axis=0.7, labels=xlab, at=axTicks(1))
    
    y.lab <- ifelse(is.null(state.id), "", state.id)
    
    title(ylab=y.lab, line=2.5)
    minorTics(1:2)
    
    legend(x="topright", legend=c("raw", "processed"), 
        col=c("red", "dark green"), lty=c(1, 1), pch=c(-1, -1), cex=0.7)
    
  # lower plot
    
    datetime <- strptime(data.mod$datetime, "%Y-%m-%d %H:%M:%OS")
    x <- as.numeric(difftime(datetime, min(datetime), units="secs"))
    y <- data.mod$z
    
    plot(NA, xlim=range(x), ylim=range(y), type="n", ann=FALSE, cex.axis=0.7, xaxt="n")
    
    if(is.null(srvy.dat("time.gap"))) 
        lines(x, y, col="dark green")
    else {
        seg <- c(0, x[c(diff(x) > srvy.dat("time.gap"), TRUE)])
        for(i in 2:length(seg)) {
            x.seg <- x[x > seg[i - 1] & x <= seg[i]]
            y.seg <- y[x > seg[i - 1] & x <= seg[i]]
            lines(x.seg, y.seg, col="dark green")
        }
    }
    
    x.tic <- datetime[1] + axTicks(1)
    xlab <- format(x.tic, format="%H:%M:%OS\n%Y-%m-%d")
    axis(side=1, las=1, cex.axis=0.7, labels=xlab, at=axTicks(1))
    
    title(ylab=y.lab, line=2.5)
    minorTics(1:2)
    
  # add file name, correction info, and signature to plot
    
    txt <- NULL
    if(!is.null(srvy.dat("grad.tol"))) 
        txt <- append(txt, paste("Gradient Tolerance:", srvy.dat("grad.tol")))
    if(!is.null(srvy.dat("time.gap"))) 
        txt <- append(txt, paste("Time Gap:", srvy.dat("time.gap")))
    txt <- append(txt, paste("Num. Raw Points:", nrow(data.raw)))
    txt <- append(txt, paste("Num. Discarded Points:", nrow(data.raw) - nrow(data.mod)))
    title(main=paste(txt, collapse="; "), cex.main=0.7, font.main=1)
    
    txt <- NULL
    if(!is.null(srvy.dat("off.z"))) 
        txt <- append(txt, paste("State Offset: ", srvy.dat("off.z"), "m", sep=""))
    if(!is.null(srvy.dat("off.t"))) 
        txt <- append(txt, paste("Time Offset: ", srvy.dat("off.t"), "s", sep=""))
    mtext(paste(na.omit(txt), collapse="; "), side=1, line=-1.1, cex=0.7, outer=TRUE, adj=0)
    
    data.file <- srvy.dat("data.file")
    if(nchar(data.file) > 40) 
        data.file <- paste(substr(data.file, 1, 40), "...", sep="")
    txt <- paste("Raw File(s):", data.file, sep=" ")
    mtext(txt, side=3, line=-0.6, cex=0.7, outer=TRUE, adj=0)
    
    txt <- paste(srvy.dat("ver"), date(), sep=", ")
    mtext(txt, side=1, line=-1.1, cex=0.7, outer=TRUE, adj=1)
    
    par(op)
}

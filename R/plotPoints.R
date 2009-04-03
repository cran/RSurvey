"plotPoints" <- function() {
    
  # simplify data notation
    
    data.mod <- srvy.dat("data.mod")
    x <- data.mod$x
    y <- data.mod$y
    z <- data.mod$z
    
  # plot labels
    
    vars <- srvy.dat("vars")
    x.lab <- vars[2]
    y.lab <- vars[3]
    z.lab <- vars[4]
    
  # limits
    
    x.min <- min(x)
    x.max <- max(x)
    y.min <- min(y)
    y.max <- max(y)
    x.ext <- diff(range(x)) * 0.04
    x.lim <- c(x.min - x.ext, x.max + x.ext)
    y.lim <- c(y.min - x.ext, y.max + x.ext)
    z.lim <- range(z)

  # define the approximate number of state variable intervals, levels and colors
    
    levels <- pretty(z.lim, srvy.dat("n.levels"))
    col <- rainbow(length(levels) - 1, start=.35, end=.75)
    
  # window setup 
    
    win.width <- srvy.dat("win.width")
    asp.ratio <- srvy.dat("yx.ratio")
    
    if(is.null(asp.ratio)) {
        win.height <- win.width
        asp.ratio <- NA
    }
    else {
        mar <- c(5.1, 4.1 / asp.ratio, 4.1, 8.1 / asp.ratio)
        mar.width <- (mar[2] + mar[4]) * srvy.dat("csi")
        mar.height <- mar.width * ((mar[1] + mar[3]) / (mar[2] + mar[4]))
        ratio <- diff(y.lim) / diff(x.lim) * asp.ratio
        win.height <- ((win.width - mar.width) * ratio) + mar.height
    }
    x11(width=win.width, height=win.height, pointsize=10)
    
  # graphics layout
    
    mar <- par("mar")
    w <- (3 + mar[2]) * srvy.dat("csi") * 2.54
    layout(matrix(c(2, 1), nc=2), widths=c(1, lcm(w)))
    
  # legend
    
    par(las=1)
    par(mar=c(mar[1], 1, mar[3], mar[2]))
    
    plot.new()
    
    y.limit <- range(levels)
    
    if(!is.null(srvy.dat("depth")) && is.null(srvy.dat("wtr.elev"))) 
        if(as.logical(srvy.dat("depth"))) y.limit <- rev(y.limit)
    
    plot.window(xlim=c(0, 1), ylim=y.limit, xaxs="i", yaxs="i")
    
    rect(0, levels[-length(levels)], 1, levels[-1], col=col)
    axis(4, las=1, cex.axis=0.7)
    box()
    
  # plot
    
    par(mar=c(mar[1:3], 1))
    
    plot.new()
    plot.window(xlim=x.lim, ylim=y.lim, log="", xaxs="i", yaxs="i", asp=asp.ratio)
    
    axis(1, las=1, cex.axis=0.7)
    axis(2, las=0, cex.axis=0.7)
    
    minorTics(1:2)
    
    m.lab <- paste(c("Point Locations", z.lab), collapse=" and Corresponding ")
    
    title(main=m.lab, xlab=x.lab, ylab=y.lab, cex.main=0.9)
    
    for(i in 1:length(col)) {
      z.logic <- z >= levels[i] & z <= levels[i + 1]
      x.hld <- x[z.logic]
      y.hld <- y[z.logic]
      points(x.hld, y.hld, col=col[i], cex=0.5, pch=20)
    }
    
    ids <- names(tran.dat())
    if(length(ids) > 0) {
        for(id in ids) {
            vertices <- tran.dat(id, "vertices")
            lines(vertices)
            points(vertices, pch=19)
            text(vertices, pos=1, cex=0.7, labels=paste(id, c("L", "R"), sep="-"))
        }
    }
    
    box()
    
  # margin text
    
    data.file <- srvy.dat("data.file")
    if(!is.null(data.file) && nchar(data.file) > 40) 
        data.file <- paste(substr(data.file, 1, 40), "...", sep="")
    
    txt <- paste("Raw File:", data.file, sep=" ")
    mtext(txt, side=3, line=-0.6, cex=0.7, outer=TRUE, adj=0) 
    
    txt <- srvy.dat("projection")
    if(!is.null(srvy.dat("off.z"))) 
        txt <- append(txt, paste("State Variable Offset:", srvy.dat("off.z"), sep=" "))
    mtext(paste(na.omit(txt), collapse="; "), side=1, line=-1.1, cex=0.7, outer=TRUE, adj=0)
    
    txt <- paste(srvy.dat("ver"), date(), sep=", ")
    mtext(txt, side=1, line=-1.1, cex=0.7, outer=TRUE, adj=1)
}

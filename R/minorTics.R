"minorTics" <- function(side, loc.tics=NULL) {
    for(i in side) {
        
        if(is.null(loc.tics)) {
            if(i == 1 || i == 3) axp <- par()$xaxp
            if(i == 2 || i == 4) axp <- par()$yaxp
        }
        else 
            axp <- c(range(loc.tics), length(loc.tics) - 1)
        
        y1 <- axp[1]
        y2 <- axp[2]
        n  <- axp[3]
        
        delta <- (y2 - y1) / n
        
        y1 <- y1 - delta
        y2 <- y2 + delta
        
        n <- n + 2
        
        typ <- round(delta / 10^floor(log10(delta) + .Machine$double.eps))
        
        low.tics <- seq(y1, y2, length=10 * n + 1)
        
        if(typ == 1 || typ == 2) {
            mid.tics <- seq(y1, y2, length=2 * n + 1)
        }
        else if(typ == 5) {
            mid.tics <- seq(y1, y2, length=5 * n + 1)
        }
        else 
            stop(call.=FALSE, "Unexpected tick interval. No minor ticks plotted.")
        
        if(!(is.null(loc.tics))) {
            ran <- range(loc.tics)
            delta <- (ran[2] - ran[1]) / (length(loc.tics) - 1)
            lim <- c(ran[1] - delta, ran[2] + delta)
            low.tics  <- diff(lim) * ((low.tics - y1) / diff(c(y1, y2))) + lim[1]
            mid.tics  <- diff(lim) * ((mid.tics - y1) / diff(c(y1, y2))) + lim[1]
        }
        
        axis(i, at=low.tics, label=FALSE, tck=-5e-3)
        axis(i, at=mid.tics, label=FALSE, tck=-1e-2)
    }
}

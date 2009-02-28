"tran.profile" <- function(vertices) {
    
  # simplify data notation for spatial information
    
    data.tin <- srvy.dat("data.tin")
    x <- data.tin$x
    y <- data.tin$y
    z <- data.tin$z
    
    lin.int <- sqrt(mean(diff(x))^2 + mean(diff(y))^2)
    
    v.x <- as.vector(vertices[,1])
    v.y <- as.vector(vertices[,2])
    n <- nrow(vertices)
    m <- n - 1
    
  # determine points along profile
    
    p.x <- v.x[1]
    p.y <- v.y[1]
    p.l <- 0
    
    lin.ang <- abs(atan(diff(v.y) / diff(v.x)))
    lin.len <- sqrt((v.x[2:n] - v.x[1:m])^2 + (v.y[2:n] - v.y[1:m])^2) 
    lin.sum <- c(0, cumsum(lin.len))
    
    for(i in 1:m) {
        lin.seq <- seq(lin.int, lin.len[i], by=lin.int)
        
        if(max(lin.seq) != max(lin.len[i])) 
            lin.seq <- c(lin.seq, lin.len[i])
        
        x.seq <- lin.seq * cos(lin.ang[i])
        y.seq <- lin.seq * sin(lin.ang[i])
        
        lin.x <- if(v.x[i] < v.x[i + 1]) v.x[i] + x.seq else v.x[i] - x.seq
        lin.y <- if(v.y[i] < v.y[i + 1]) v.y[i] + y.seq else v.y[i] - y.seq
        
        p.x <- c(p.x, lin.x)
        p.y <- c(p.y, lin.y)
        p.l <- c(p.l, lin.seq + lin.sum[i])
    }
    
    p.z <- rep(NA, length(p.l))
    
  # determine state variable along profile
    
    x.range <- if(length(unique(p.x)) > 1) range(p.x) else c(p.x[1] - 1, p.x[1] + 1)
    y.range <- if(length(unique(p.y)) > 1) range(p.y) else c(p.y[1] - 1, p.y[1] + 1)
    
    id.x <- (1:length(x))[x >= x.range[1] & x <= x.range[2]]
    id.y <- (1:length(y))[y >= y.range[1] & y <= y.range[2]]
    
    id.x <- (min(id.x) - 10):(max(id.x) + 10)
    id.y <- (min(id.y) - 10):(max(id.y) + 10)
    
    id.x <- id.x[id.x > 0 & id.x < length(x)]
    id.y <- id.y[id.y > 0 & id.y < length(y)]
    
    x <- x[id.x]
    y <- y[id.y]
    z <- z[id.x, id.y]
    
    for(i in 1:(length(x) - 1)) {
        for(j in 1:(length(y) - 1)) {
            hld <- point.in.polygon(p.x, p.y, 
                   c(x[i], x[i + 1], x[i + 1], x[i]), c(y[j], y[j], y[j + 1], y[j + 1]))
            p.z[hld != 0] <- mean(c(z[i, j], z[i + 1, j], z[i + 1, j + 1], z[i, j + 1]), na.rm=TRUE)
        }
    }
    
    d.profile <- cbind(p.x[!is.na(p.z)], p.y[!is.na(p.z)], p.l[!is.na(p.z)], p.z[!is.na(p.z)])
    colnames(d.profile) <- c("x", "y", "h", "z")
    
    d.profile
}

"plotSurface3d" <- function() {
    
  # simplify data
    
    data.tin <- srvy.dat("data.tin")
    
  # incorporate spatial domain using polygon
    
    ply <- srvy.dat("poly")
    if(!is.null(ply)) 
        data.tin <- polyCutout(data.tin, ply)
    
  # simplify data
    
    x <- data.tin$x
    y <- data.tin$y
    z <- data.tin$z
    
  # scale y
    
    if(!is.null(srvy.dat("yx.ratio"))) 
        y <- y * srvy.dat("yx.ratio")
    
  # scale z
    
    if(!is.null(srvy.dat("depth"))) {
        if(as.logical(srvy.dat("depth"))) 
            z <- max(z, na.rm=TRUE) - z
    }
    
    if(!is.null(srvy.dat("zx.ratio"))) 
        z <- z * srvy.dat("zx.ratio")
    
  # define colors for terrain
    
    z.lim <- range(z, na.rm=TRUE)
    z.len <- z.lim[2] - z.lim[1] + 1
    
    colorlut <- rev(terrain.colors(z.len))
    z.col <- colorlut[z - z.lim[1] + 1]
    
  # open new device
    
    rgl.open()
    
  # size of plot window
    
    ppi <- 96
    winDim <- ppi + srvy.dat("win.width") * ppi
    par3d(windowRect=c(ppi, ppi, winDim, winDim * 0.75), mouseMode=c("trackball", "zAxis", "zoom"))
    
  # add terrain surface shape (i.e. population density)
    
    rgl.bg(color="white")
    
    surface3d(x, y, z, color=z.col, back="fill")
    
    rgl.viewpoint(theta=0, phi=-35, fov=60, zoom=0.5)
}

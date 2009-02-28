"tran.vector" <- function(vertices, vel.vect, arrow.max) {
    
    m <- ((vertices[2,2] - vertices[1,2]) / (vertices[2,1] - vertices[1,1]))
    theta <- atan(m)
    delta <- pi/2 - abs(theta)
    delta <- ifelse(m > 0, -delta, delta)
    
    v1 <- vel.vect[,"x"]
    v2 <- vel.vect[,"y"]
    vz <- vel.vect[,"z"]
    
    magn <- sqrt(v1^2 + v2^2 + vz^2)
    
    v1.i.x <- (v1 * cos(theta)) * cos(theta)
    v1.i.y <- (v1 * cos(theta)) * sin(theta)
    v1.j.x <- (v1 * cos(delta)) * cos(delta)
    v1.j.y <- (v1 * cos(delta)) * sin(delta)
    
    v2.i.x <- (v2 * sin(theta)) * cos(theta)
    v2.i.y <- (v2 * sin(theta)) * sin(theta)
    v2.j.x <- (v2 * sin(delta)) * cos(delta)
    v2.j.y <- (v2 * sin(delta)) * sin(delta)
    
    v3.i.x <- v1.i.x + v2.i.x
    v3.i.y <- v1.i.y + v2.i.y
    norm <- sqrt(v3.i.x^2 + v3.i.y^2)
    v.tran <- sign((v3.i.x / norm) + (v3.i.y / norm)) * norm
    
    v3.j.x <- v1.j.x + v2.j.x
    v3.j.y <- v1.j.y + v2.j.y
    norm <- sqrt(v3.j.x^2 + v3.j.y^2)
    v.long <- sign((v3.j.x / norm) + (v3.j.y / norm)) * norm
    if(mean(v.long) < 0) 
        v.long <- -v.long  # adjust for flow direction (TEMPORARY FIX)
    
    long  <- v.long
    tran  <- sqrt(v.tran^2 + vz^2)
    gimma <- atan(vz / v.tran)
    
    per.err <- (arrow.max - max(tran)) / max(tran)
    arrow <- (1 + per.err) * tran
    
    arrow.x <- sign(v.tran) * arrow * cos(gimma)
    arrow.y <- sign(vz) * sqrt(arrow^2 - arrow.x^2)
    
    vel.vect <- cbind(magn, long, arrow.x, arrow.y)
    
    vel.vect
}

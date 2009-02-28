"autocrop" <- function(mesh, maxLength, maxItr) {

# additional functions (subroutines)
    
  # eliminate elements with arc lengths greater than maxLength
    
    modTri <- function(tri) {
        
        n <- nrow(tri)
        
        unArcId <- rep(NA, n)
        unArcId[tri[,"tr1"] == 0] <- tri[tri[,"tr1"] == 0, "arc1"]
        unArcId[tri[,"tr2"] == 0] <- tri[tri[,"tr2"] == 0, "arc2"]
        unArcId[tri[,"tr3"] == 0] <- tri[tri[,"tr3"] == 0, "arc3"]
        
        elemArc <- rep(NA, n)
        elemArc[unArcId == tri[,"arc1"]] <- 1
        elemArc[unArcId == tri[,"arc2"]] <- 2
        elemArc[unArcId == tri[,"arc3"]] <- 3
        
        pt1 <- array(elemBuild[elemArc, 1])
        pt2 <- array(elemBuild[elemArc, 2])
        
        pt1Id <- pt2Id <- rep(NA, n)
        pt1Id[pt1 == 1 & !is.na(pt1)] <- tri[pt1 == 1 & !is.na(pt1), "node1"]
        pt1Id[pt1 == 2 & !is.na(pt1)] <- tri[pt1 == 2 & !is.na(pt1), "node2"]
        pt1Id[pt1 == 3 & !is.na(pt1)] <- tri[pt1 == 3 & !is.na(pt1), "node3"]
        pt2Id[pt2 == 1 & !is.na(pt2)] <- tri[pt2 == 1 & !is.na(pt2), "node1"]
        pt2Id[pt2 == 2 & !is.na(pt2)] <- tri[pt2 == 2 & !is.na(pt2), "node2"]
        pt2Id[pt2 == 3 & !is.na(pt2)] <- tri[pt2 == 3 & !is.na(pt2), "node3"]
        
        pt1xy <- pt2xy <- list(x=rep(NA, n), y=rep(NA, n))
        pt1xy$x[!is.na(pt1Id)] <- mesh$x[na.omit(pt1Id)]
        pt1xy$y[!is.na(pt1Id)] <- mesh$y[na.omit(pt1Id)]
        pt2xy$x[!is.na(pt2Id)] <- mesh$x[na.omit(pt2Id)]
        pt2xy$y[!is.na(pt2Id)] <- mesh$y[na.omit(pt2Id)]
        
        arcLength <- sqrt((pt2xy$x - pt1xy$x)^2 + (pt2xy$y - pt1xy$y)^2)
        
        omitElems <- tri[!is.na(arcLength) & arcLength >  maxLength, "elem"]
        
        newTri <- NA
        if(length(omitElems) > 0) {
            tri[tri[,"tr1"] %in% omitElems, c("tr1", "arc1")] <- 0
            tri[tri[,"tr2"] %in% omitElems, c("tr2", "arc2")] <- 0
            tri[tri[,"tr3"] %in% omitElems, c("tr3", "arc3")] <- 0
            newTri <- tri[!(tri[,"elem"] %in% omitElems),]
        }
        newTri
    }
    
    
    
# main program
    
    elemBuild <- matrix(c(2, 3, 3, 1, 1, 2), nrow=3, ncol=2, byrow=TRUE, 
                 dimnames=list(c("arc1", "arc2", "arc3"), c("pt1", "pt2")))
    
    tri <- triangles(mesh)
    tri <- cbind(elem=1:nrow(tri), tri)
    
    itr <- 0
    oldTri <- newTri <- tri
    while(any(!is.na(newTri)) & itr < maxItr) {
        itr <- itr + 1
        oldTri <- newTri
        newTri <- modTri(oldTri)
        if(class(newTri) == "numeric") {
            oldTri <- t(as.matrix(newTri))
            newTri <- NA
        }
    }
    
    tri <- oldTri
    n <- nrow(tri)
    
    tri <- tri[tri[,"tr1"] == 0 | tri[,"tr2"] == 0 | tri[,"tr3"] == 0,]
    elems <- tri[,"elem"]
    
    tri[tri[,"tr1"] == 0 & (tri[,"tr2"] != 0 & tri[,"tr3"] != 0), "node1"] <- 0
    tri[tri[,"tr2"] == 0 & (tri[,"tr1"] != 0 & tri[,"tr3"] != 0), "node2"] <- 0
    tri[tri[,"tr3"] == 0 & (tri[,"tr1"] != 0 & tri[,"tr2"] != 0), "node3"] <- 0
    
    nodes <- c()
    for(i in 1:nrow(tri)) {
        hld <- array(tri[i, c("node1", "node2", "node3")])
        if(0 %in% hld) 
            nodes <- rbind(nodes, hld[hld != 0])
        else {
            if(tri[i, "tr1"] == 0) 
                nodes <- rbind(nodes, hld[c(2, 3)])
            if(tri[i, "tr2"] == 0) 
                nodes <- rbind(nodes, hld[c(1, 3)])
            if(tri[i, "tr3"] == 0) 
                nodes <- rbind(nodes, hld[c(1, 2)])
        }
    }
    
    sortNodes <- nodes[1,]
    nodes <- nodes[-1,]
    
    for(i in 1:nrow(nodes)) {
        hld <- sortNodes[length(sortNodes)]
        logic <- nodes[,1] %in% hld | nodes[,2] %in% hld
        tmp <- nodes[logic,]
        if(hld != tmp[1]) 
            tmp <- rev(tmp)
        sortNodes <- c(sortNodes, tmp)
        nodes <- nodes[!logic,]
        if(class(nodes) == "numeric") 
            nodes <- t(as.matrix(nodes))
    }
    
    sortNodes <- unique(sortNodes)
    
    x <- mesh$x[sortNodes]
    y <- mesh$y[sortNodes]
    
    ply <- as(structure(c(x, y), .Dim=c(length(x), 2)), "gpc.poly")
    
    ply
}


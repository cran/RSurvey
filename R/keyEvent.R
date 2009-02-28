"keyEvent" <- function (ent.typ, ent.str="", rm.data=FALSE) {
    
    if(ent.str == "") return("")
    
    chk <- unlist(strsplit(ent.str, split=""))
    
    if(ent.typ == "real") accept.vals <- c(as.character(0:9), ".", "-")
    if(ent.typ == "second") accept.vals <- c(as.character(0:9), ".")
    if(ent.typ %in% c("integer", "hour", "minute")) accept.vals <- c(as.character(0:9))
    if(ent.typ == "date") {
        accept.vals <- c("a", "A", "b", "c", "C", "d", "D", "e", "E", "F", "g", "G", "h", "H", 
                       "I", "j", "k", "l", "m", "M", "n", "O", "p", "r", "R", "S", "t", "T", 
                       "u", "U", "V", "w", "W", "x", "X", "y", "Y", "z", "Z",
                       "/", "-", ":", "%", "#", " ", as.character(0:9))
    }
    
    if(all(chk %in% accept.vals)) 
        ans <- ent.str
    else 
        ans <- paste(chk[chk %in% accept.vals], collapse="")
    
    if(ent.typ == "hour" & ans != "") 
        if(as.integer(ans) > 23) ans <- "23"
    
    if(ent.typ == "minute" & ans != "") 
        if(as.integer(ans) > 59) ans <- "59"
    
    if(ent.typ == "second" & ans != "") {
        if(as.real(ans) < 0) ans <- "0"
        if(as.real(ans) > 59.999) ans <- "59.999"
    }
    
    if(rm.data) {
        srvy.dat("data.mod", NULL)
        srvy.dat("data.tin", NULL)
    }
    
    ans
}

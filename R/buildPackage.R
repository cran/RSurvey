"buildPackage" <- function() {
    
    if(.Platform$OS.type != "windows") 
        stop(call.=FALSE, "This function requires a Windows platform.")
    
# Software requirements for building R packages in Windows XP Pro.

# Download and install the Rtools installer (choose to update Search Path)
#   http://www.murdoch-sutherland.com/Rtools/installer.html
# Download and install the HTML Help Workshop
#   http://msdn2.microsoft.com/en-us/library/ms669985.aspx
# Download and install the MiKTeX installation program
#   http://www.miktex.org/setup.html
    
    pkg <- rev(unlist(strsplit(getwd(), "/")))[1]
    
    pkg.path <- shQuote(getwd())
    tmp.path <- shQuote(paste("C:/", pkg, sep=""))
    chk.path <- shQuote(paste("C:/", pkg, "/", pkg, ".Rcheck", sep=""))
    zip.path <- shQuote(paste(pkg, "_*", sep=""))
    cmd.path <- paste(R.home(component="bin"), "/Rcmd", sep="")
    
    cmd <- NULL
    cmd <- append(cmd, paste("RM -f ", getwd(), "/", pkg, "*", sep=""))
    cmd <- append(cmd, paste(cmd.path, "REMOVE", pkg, sep=" "))
    cmd <- append(cmd, paste("CP -r", pkg.path, shQuote("C:/"), sep=" "))
    cmd <- append(cmd, paste("RMDIR /S /Q", chk.path, sep=" "))
    cmd <- append(cmd, paste(cmd.path, "build", tmp.path, sep=" "))
    cmd <- append(cmd, paste(cmd.path, "install --build", tmp.path, sep=" "))
    cmd <- append(cmd, paste(cmd.path, " check --outdir=", pkg.path, " ", tmp.path, sep=""))
    cmd <- append(cmd, paste("RMDIR /S /Q", tmp.path, sep=" "))
    cmd <- append(cmd, paste("MOVE /Y", zip.path, pkg.path, sep=" "))
    
    cmd <- paste(Sys.getenv("COMSPEC"), "/c", cmd, sep=" ")
    
    for(i in cmd) 
        cat(paste(i, "\n", sep=""))
}


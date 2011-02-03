EditFunction <- function(cols, index=NULL, parent=NULL) {
  # A GUI for defining a function in the R language with a focus on table data.
  
  # Additional functions (subroutines)
  
  # Add field to text box
  
  AddVar <- function() {
    tkfocus(frame1.txt)
    idx <- as.integer(tkcurselection(frame0.lst))
    if (length(idx) == 0 || idx == (index - 1)) 
      return()
    tkselection.clear(frame0.lst, idx, idx)
    
    tcl(frame1.txt, "edit", "separator")
    seltxt <- as.character(tktag.ranges(frame1.txt, 'sel'))
    if (length(seltxt) > 1) 
      tcl(frame1.txt, "delete", seltxt[1], seltxt[2])
    
    txt <- paste("DATA[[\"", ids[idx + 1], "\"]]", sep="")
    tkinsert(frame1.txt, "insert", txt)
  }
  
  # Save function
  
  SaveFunction <- function() {
    txt <- as.character(tclvalue(tkget(frame1.txt, '1.0', 'end-1c')))
    if (txt == "") {
      new.fun <<- "NA"
    } else {
      fun <- try(parse(text=paste("function(DATA) {", txt, "}", sep="")), 
                 silent=TRUE)
      if (inherits(fun, "try-error")) {
        msg <- "There's a problem with the function syntax, try revising."
        tkmessageBox(icon="error", message=msg, detail=fun, title="Error", 
                     type="ok", parent=tt)
        return()
      }
      val <- EvalFunction(txt, cols)
      if (inherits(val, "try-error")) {
        msg <- "Function results in an error during evaluation, try revising."
        tkmessageBox(icon="error", message=msg, detail=val, title="Error", 
                     type="ok", parent=tt)
        return()
      }
      if (identical(val, "length-error")) {
        msg <- paste("Evaluation of the function results in an array length",
                     "that is\nunequal to the number of rows in DATA, try",
                     "revising.")
        tkmessageBox(icon="error", message=msg, title="Error", type="ok", 
                     parent=tt)
        return()
      }
      new.fun <<- txt
    }
    tclvalue(tt.done.var) <- 1
  }
  
  # Text edit functions
  
  EditUndo <- function() {
    tcl(frame1.txt, "edit", "undo")
  }
  EditRedo <- function() {
    tcl(frame1.txt, "edit", "redo")
  }
  EditCut <- function() {
    tcl("tk_textCut", frame1.txt)
  }
  EditCopy <- function() {
    tcl("tk_textCopy", frame1.txt)
  }
  EditPaste <- function() {
    tcl("tk_textPaste", frame1.txt)
  }
  EditSelectAll <- function() {
    tktag.add(frame1.txt, 'sel', '1.0', 'end')
  }
  
  
  # Main program
  
  old.fun <- if (is.null(index)) NULL else cols[[index]]$fun
  new.fun <- NULL
  
  ids <- sapply(cols, function(i) i$id)
  
  # Assign variables linked to Tk widgets
  
  list.var <- tclVar()
  
  for (i in seq(along=ids)) {
    tcl("lappend", list.var, ids[i]) # must be unique
  }
  
  tt.done.var <- tclVar(0)
  
  # Open GUI
  
  tclServiceMode(FALSE)
  
  tt <- tktoplevel(padx=0, pady=0)
  
  if (!is.null(parent)) {
    tkwm.transient(tt, parent)
    tmp <- unlist(strsplit(as.character(tkwm.geometry(parent)), "\\+"))
    tkwm.geometry(tt, paste("+", as.integer(tmp[2]) + 25, 
                            "+", as.integer(tmp[3]) + 25, sep=""))
  }
  
  tktitle(tt) <- "Edit Function"
  
  # Top menu
  
  top.menu <- tkmenu(tt, tearoff=0)
  
  # Project menu
  
  menu.edit <- tkmenu(tt, tearoff=0)
  tkadd(top.menu, "cascade", label="Edit", menu=menu.edit, underline=0)
  
  tkadd(menu.edit, "command", label="Undo", accelerator="Ctrl+Z", 
        command=EditUndo)
  tkadd(menu.edit, "command", label="Redo", accelerator="Ctrl+Y", 
        command=EditRedo)
  tkadd(menu.edit, "separator")
  tkadd(menu.edit, "command", label="Cut", accelerator="Ctrl+X", 
        command=EditCut)
  tkadd(menu.edit, "command", label="Copy", accelerator="Ctrl+C", 
        command=EditCopy)
  tkadd(menu.edit, "command", label="Paste", accelerator="Ctrl+V", 
        command=EditPaste)
  tkadd(menu.edit, "separator")
  tkadd(menu.edit, "command", label="Select all", accelerator="Ctrl+A", 
        command=EditSelectAll)
  
  # Finalize top menu
  
  tkconfigure(tt, menu=top.menu)
  
  # Paned window
  
  pw <- ttkpanedwindow(tt, orient="horizontal")
  
  # Frame 0
  
  frame0 <- tkframe(pw, relief="flat", padx=0, pady=0)
  
  frame0.lab <- ttklabel(frame0, text="Click to insert", foreground="#414042")
  tkpack(frame0.lab, side="top", anchor="w", padx=c(5, 0), pady=c(5, 0))
  
  frame0.lst <- tklistbox(frame0, selectmode="browse", activestyle="none", 
                          relief="flat", borderwidth=5, width=20, height=8, 
                          exportselection=FALSE, listvariable=list.var, 
                          highlightthickness=0)
  frame0.ysc <- ttkscrollbar(frame0, orient="vertical")
  
  tkconfigure(frame0.lst, background="white", 
              yscrollcommand=paste(.Tk.ID(frame0.ysc), "set"))
  tkconfigure(frame0.ysc, command=paste(.Tk.ID(frame0.lst), "yview"))
  
  tkpack(frame0.lst, side="left",  fill="both", expand=TRUE, 
         padx=c(5, 0), pady=c(2, 0))
  tkpack(frame0.ysc, side="right", fill="y", anchor="w", 
         padx=c(0, 2), pady=c(2, 0))
  
  if (is.null(index)) {
    index <- 1
  } else {
    tkitemconfigure(frame0.lst, index - 1, foreground="gray", 
                    selectforeground="gray", background="white", 
                    selectbackground="white")
  }
  
  tkbind(frame0.lst, "<ButtonRelease-1>", AddVar)
  
  # Frame 1
  
  frame1 <- tkframe(pw, relief="flat", padx=0, pady=0)
  
  txt <- paste("DATA[[\"", ids[index], 
               "\"]] = function(DATA) {<Define function below>}", sep="")
  frame1.lab <- ttklabel(frame1, text=txt, foreground="#414042")
  tkpack(frame1.lab, side="top", anchor="w", pady=c(5, 0))
  
  fnt <- tkfont.create(family="Courier New", size=9)
  
  frame1.txt <- tktext(frame1, bg="white", font=fnt, padx=2, pady=2, width=75, 
                       height=12, undo=1, wrap="none", foreground="black", 
                       relief="flat", 
                       xscrollcommand=function(...) tkset(frame1.xsc,...),
                       yscrollcommand=function(...) tkset(frame1.ysc,...))
  
  frame1.xsc <- ttkscrollbar(frame1, orient="horizontal")
  tkconfigure(frame1.xsc, command=paste(.Tk.ID(frame1.txt), "xview"))
  
  frame1.ysc <- ttkscrollbar(frame1, orient="vertical")
  tkconfigure(frame1.ysc, command=paste(.Tk.ID(frame1.txt), "yview"))
  
  tkpack(frame1.xsc, side="bottom", fill="x", anchor="w", 
         padx=c(0, 20), pady=0)
  tkpack(frame1.ysc, side="right",  fill="y", anchor="w", 
         padx=c(0, 5), pady=c(2, 0))
  tkpack(frame1.txt, side="left",  fill="both", expand=TRUE, 
         pady=c(2, 0))
  
  if (!is.null(old.fun) && old.fun != "NA") 
    tkinsert(frame1.txt, "end", old.fun)
  
  tcl(frame1.txt, "edit", "reset")
  
  tkmark.set(frame1.txt, "insert", "end")
  
  tkadd(pw, frame0, weight=0)
  tkadd(pw, frame1, weight=1)
  
  tkpack(pw, fill="both", expand="yes")
  
  # Frame 2 and size grip
  
  frame2 <- tkframe(tt, relief="flat", padx=0, pady=0)
  
  frame2.but.1 <- ttkbutton(frame2, width=12, text="OK", 
                            command=SaveFunction)
  frame2.but.2 <- ttkbutton(frame2, width=12, text="Cancel", 
                            command=function() tclvalue(tt.done.var) <- 1)
  frame2.grp.3 <- ttksizegrip(frame2)
  
  tkgrid(frame2.but.1, frame2.but.2, frame2.grp.3)
  
  tkgrid.configure(frame2.but.1, sticky="e", padx=2, pady=c(10, 8))
  tkgrid.configure(frame2.but.2, sticky="w", padx=2, pady=c(10, 8), rowspan=2)
  tkgrid.configure(frame2.grp.3, sticky="se")
  
  tkpack(frame2, side="bottom", anchor="e")
  
  # Text bindings
  
  tkbind("Text", "<Control-z>", EditUndo)
  tkbind("Text", "<Control-y>", EditRedo)
  tkbind("Text", "<Control-v>", EditPaste)
  tkbind("Text", "<Control-a>", EditSelectAll)
  
  # GUI control
  
  tkfocus(frame1.txt)
  tkgrab(tt)
  tkbind(tt, "<Destroy>", function() tclvalue(tt.done.var) <- 1)
  
  tclServiceMode(TRUE)
  tkwait.variable(tt.done.var)
  
  tclServiceMode(FALSE)
  tkgrab.release(tt)
  tkdestroy(tt)
  tclServiceMode(TRUE)
  
  new.fun
}

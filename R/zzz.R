.onLoad <- function(libname, pkgname){
  
  # inject dtq logging and read.only to data.table:::`[.data.table`
  # based on: data.table:::`.onLoad`
  expr <- expression({
    # dtq logging
    if(isTRUE(getOption("dtq.log"))){
      te <- topenv(parent.frame(1))
      if(!isNamespace(te) || !(getNamespaceName(te) %in% c("data.table", getOption("dtq.log.exclude")))){
        # custom defined pkgs for which exclude log, as character vector
        dtcall <- sys.call() # data.table query
        nrow_in <- nrow(x)
        if(isTRUE(getOption("dtq.log.gc"))) gc(FALSE)
        pt <- if(isTRUE(getOption("dtq.log.nano")) && requireNamespace("microbenchmark", quietly=TRUE)) microbenchmark::get_nanotime()*1e-9 else proc.time()[[3L]]
        eval(bquote( # bquote prevent to overwride vars (pt, te, nrow_in) during `[.data.table` execution
          on.exit({
            dtq::dtq.log$add(
              list(timestamp = Sys.time(),
                   env = .(environmentName(te)),
                   dtcall = .(list(dtcall)),
                   elapsed = (if(isTRUE(getOption("dtq.log.nano"))) microbenchmark::get_nanotime()*1e-9 else proc.time()[[3L]]) - .(pt),
                   nrow_in = .(nrow_in),
                   nrow_out = NA_integer_)
            )
          })
        ))
      }
    }
    # read.only mode
    if(isTRUE(attr(x,"read.only",TRUE))){
      x <- copy(x)
      setattr(x,"read.only",NULL)
    }
  })
  tt = get("[.data.table", envir=asNamespace("data.table"), inherits=FALSE)
  ss = body(tt)
  if (class(ss)!="{") ss = as.call(c(as.name("{"), ss))
  ss = ss[c(1L,NA,2:length(ss))]
  ss[[2L]] = expr[[1L]]
  body(tt) = ss
  (unlockBinding)("[.data.table", asNamespace("data.table"))
  assign("[.data.table", tt, envir=asNamespace("data.table"), inherits=FALSE)
  lockBinding("[.data.table", asNamespace("data.table"))
  rm(expr, tt, ss)
  
  # logging related opts
  
  options("dtq.log" = TRUE) # turn off logging here
  options("dtq.log.size" = 1e5L) # when log reach that num it stop logging and start throwing warnings
  options("dtq.log.gc" = FALSE) # do gc() before each timing
  options("dtq.log.nano" = requireNamespace("microbenchmark", quietly=TRUE)) # if microbenchmark available it will use get_nanotime
  options("dtq.log.exclude" = c("dtq","dwtools","logR")) # packages to exclude
  
  # chain processing opts
  
  options("dtq.apply.depth" = 20L)
  options("dtq.arg.names" = FALSE)
    
}

dtq.log <- dtq.log$new()

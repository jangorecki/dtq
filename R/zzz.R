.onLoad <- function(libname, pkgname){
  
  # logging related opts
  
  options("dtq.log" = TRUE) # turn on logging
  options("dtq.log.size" = 1e5L) # when log reach that num it stop logging and start throwing warnings
  options("dtq.log.gc" = FALSE) # do gc() before each timing
  options("dtq.log.nano" = TRUE) # if microbenchmark available it will use get_nanotime
  options("dtq.log.exclude" = character()) # packages to exclude
  options("dtq.log.include" = character()) # packages to include
  
  # dtq processing opts
  
  options("dtq.apply.depth" = 20L)
    
}

.onAttach <- function(libname, pkgname){
  
  # inject dtq logging and read.only to data.table:::`[.data.table`
  # based on: data.table:::`.onLoad`
  expr <- expression({
    # dtq logging
    if(isTRUE(getOption("dtq.log"))){
      te <- topenv(parent.frame(1))
      if(dtq::do.dtq.log(te)){
        dtq_call <- match.call() # data.table query
        dtq.local.log <- local({
          env <- environmentName(te)
          dtq_call <- list(dtq_call)
          in_rows <- nrow(x)
          if(isTRUE(getOption("dtq.log.gc"))) gc(FALSE)
          start <- if(isTRUE(getOption("dtq.log.nano")) && requireNamespace("microbenchmark", quietly=TRUE)) microbenchmark::get_nanotime()*1e-9 else proc.time()[[3L]]
          function(timestamp, end, out_rows) dtq::dtq.log$add(list(timestamp = timestamp, env = env, dtq_call = dtq_call, elapsed = end - start, in_rows = in_rows, out_rows = out_rows))
        })
        on.exit(
          dtq.local.log(
            timestamp = Sys.time(),
            end = if(isTRUE(getOption("dtq.log.nano")) && requireNamespace("microbenchmark", quietly=TRUE)) microbenchmark::get_nanotime()*1e-9 else proc.time()[[3L]],
            out_rows = as.integer(nrow(returnValue()))[1L]
          )
        )
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
  
}

dtq.log <- dtq.log$new()

#' @title dtq-package
#' @docType package
#' @import data.table R6
#' @author Jan Gorecki
#' @name dtq-package
NULL

#' @title Prepare and call append log to cache
#' @param te environment
#' @param dtcall call
#' @param pt numeric
append.log <- function(te, dtcall, pt){
  # finish timing
  et <- if(isTRUE(getOption("dtq.log.nano")) && requireNamespace("microbenchmark",quietly=TRUE)) microbenchmark::get_nanotime()*1e-9 else proc.time()[[3L]]
  # insert
  dtq.log$add(list(timestamp = Sys.time(), env = environmentName(te), dtcall = list(dtcall), elapsed = et - pt))
}

#' @title Get data.table queries logs
#' @param chain.only logical if \emph{TRUE} then aggregated logs to full chains
#' @param purge logical when true will clear cache and return empty data.table
#' @export
dtt <- function(chain.only = FALSE, purge = FALSE){
  if(isTRUE(purge)) dtq.log$purge()
  dt <- dtq.log$print()
  if(isTRUE(chain.only)){
    dt[, c(.SD, rbindlist(lapply(dtq, function(x) x$process())))
       ][, .(dtq_depth = dtq_seq[.N],
             timestamp = timestamp[.N],
             env = env[.N],
             elapsed = sum(elapsed),
             query = paste(query,collapse="")),
         .(dtq_id)]
  } else dt
}

dtq.log <- R6Class(
  classname = "dtq.log",
  public = list(
    log = list(),
    initialize = function() invisible(self),
    add = function(x){
      if(self$length() > getOption("dtq.log.size")){
        warning(paste0("data.table query log size limit exceeded, query will not be logged, use purge or increase the limit."))
      } else {
        self$log <- c(self$log, list(x))
      }
      invisible(self)
    },
    purge = function(){
      self$log <- list()
      invisible(self)
    },
    length = function(){
      base::length(self$log)
    },
    process = function(){
      if(self$length() < 1L) return(self$empty)
      rbindlist(self$log)[
        ][, seq := seq_len(.N)
          ][, dtq := lapply(dtcall, function(x) dtq$new(x, env)) # add: dtq$new()$print ad return multiple columns in one batch as a result of print/process
            ][, dtq_seq := sapply(dtq, function(x) x$depth())
              ][, dtq_id := cumsum(dtq_seq==1L)
                ][, .(seq, dtq_id, dtq_seq, dtq, timestamp, env, elapsed)
                  ][]
    },
    print = function(){
      self$process()
    }
  ),
  active = list(
    empty = function() data.table(seq=integer(), dtq_id=integer(), dtq_seq=integer(), dtq=list(), timestamp=Sys.time()[-1L], env=character(), elapsed=numeric())
  ))

#' @title data.table query
#' @docType class
#' @format An R6 class object.
#' @name dtq
#' @export
dtq <- R6Class(
  classname = "dtq",
  public = list(
    call = list(),
    env = character(),
    query = list(),
    # recursive trasnform calls to lists
    decall = function(x){
      if(is.call(x)){
        if(x[[1L]]==as.name("[") || x[[1L]]==as.name("[.data.table")) mx <- as.list(match.call(match.fun(data.table:::`[.data.table`), x)) else return(x)
      } else return(x)
      lx <- as.list(x)
      to_decall <- chmatch(c("x","i"), names(mx))
      for(id in to_decall[!is.na(to_decall)]) lx[[id]] <- self$decall(x[[id]])
      lx
    },
    initialize = function(x, env){
      self$call <- x
      self$env <- env
      self$query <- self$decall(self$call)
      invisible(self)
    },
    # recursive transform lists to calls
    recall = function(x){
      browser()
      # TO DO 
      # self$call <- NULL
      if(!is.list(x)) return(x)
      nms <- names(x)
      for(nm in nms[nms %in% c("x","i")]) x[[nm]] <- self$recall(x[[nm]])
      as.call(x)
    },
    length = function() base::length(self$query),
    # chain length, recursive depth
    depth = function(depth = getOption("dtq.apply.depth")){
      for(i.depth in seq_len(depth)){
        if(is.name(self$query[[rep(2L,i.depth)]])) return(i.depth)
      }
      stop(paste0("dtq depth recursive call limit exceeded, current limit ",depth,", use: options('dtq.apply.depth')"))
    },
    # query apply
    qapply = function(FUN, ..., SIMPLIFY = TRUE){
      FUN <- match.fun(FUN)
      q <- list()
      for(i.dtq in seq_len(self$depth())){
        if(isTRUE(SIMPLIFY)){
          if(i.dtq==1L){
            q[[1L]] <- do.call(FUN, list(self$query))
          } else {
            q[[i.dtq]] <- do.call(FUN, list(self$query[[rep(2L,i.dtq-1L)]]))
          }
        } else {
          # q[[rep(2L,i.dtq)]] <- do.call(FUN, list())
          stop("So far only SIMPLIFY=TRUE is supported")
        }
      }
      q
    },
    deparse = function(q, arg.names = getOption("dtq.arg.names")){
      # browser()
      # if(isTRUE(arg.names)) # TO DO
      q <- as.list(match.call(definition = data.table:::`[.data.table`, call = as.call(q)))
      args <- q[-c(1L,2L)] # exclude 'x' argument
      argnames.list <- lapply(names(args), function(x) x[nchar(x)>0L]) # just to easy collapse below
      arg.queries <- vapply(seq_along(args), function(i) paste(c(argnames.list[[i]],paste(deparse(args[[i]], width.cutoff=500L),collapse="\n")),collapse=" = "), "", USE.NAMES=FALSE)
      paste0("[",paste(arg.queries,collapse=", "),"]")
    },
    process = function(chain = FALSE){
      if(isTRUE(chain)){
        # return list(query = [...], chain = [...][...][...])
        x <- unlist(self$qapply(self$deparse))
        data.table(query = x[1L], chain = paste(x,collapse=""))
      } else {
        x <- self$deparse(self$query)
        data.table(query = x)
      }
    },
    print = function(){
      self$process()
    },
    exec = function(){
      eval(self$recall(self$query), envir = asNamespace(env))
    }
  ),
  active = list(
    empty = function() data.table(query = character(), dtq = list())
  )
)

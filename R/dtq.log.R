#' @title Get data.table queries logs
#' @param print logical if \emph{TRUE} then will remove \emph{list} type columns from the result.
#' @param chain logical if \emph{TRUE} then aggregated logs to full chains
#' @param purge logical when \emph{TRUE} it will clear dtq logs and return empty data.table
#' @details \emph{timestamp} represents the time of query (or chain) log after it is evaluated.
#' @export
dtl <- function(print = FALSE, chain = FALSE, purge = FALSE){
  src <- env <- dtq_id <- . <- query <- elapsed <- in_rows <- out_rows <- NULL
  if(isTRUE(purge)){
    dt <- .DTQ$purge()$empty
  } else {
    dt <- .DTQ$process()
  }
  if(!isTRUE(chain)){
    if(isTRUE(print)) dt <- dt[, .SD, .SDcols=-c("dtq","dtq_call")]
  } else if(isTRUE(chain)){
    dt <- dt[,.(dtq_depth=.N, query=paste(query, collapse=""), timestamp=timestamp[.N], elapsed=sum(elapsed), in_rows=in_rows[min(1L,nrow(dt))], out_rows=out_rows[.N]), .(dtq_id, env, src)]
  }
  return(dt)
}

#' @title Check if proceed with dtq logging
#' @param te environment
#' @details Uses \code{dtq.log.exclude} and \code{dtq.log.include} options to define if logging should be done based on the \code{te} environment.
#' @export
do.dtq.log <- function(te){
  exclude <- c("data.table","dtq",getOption("dtq.log.exclude")) # hard exclude data.table and dtq
  include <- getOption("dtq.log.include")[!getOption("dtq.log.include") %chin% exclude] # exclude 'exclude' from 'include' :)
  if(!length(include)){ # no 'dtq.log.include' option set
    if(!isNamespace(te)){
      # called from global env
      !environmentName(te) %chin% exclude
    } else {
      # called from package
      !getNamespaceName(te) %chin% exclude
    }
  } else { # using 'dtq.log.include' option
    if(!isNamespace(te)){
      # called from global env
      environmentName(te) %chin% include
    } else {
      # called from package
      getNamespaceName(te) %chin% include
    }
  }
}

#' @title data.table queries log storage
#' @docType class
#' @format An R6 class object.
#' @name dtq.log
#' @details Environment to store data.table queries, use \link{dtl} to access formatted logs.
#' @seealso \link{dtl}, \link{dtq}
#' @aliases .DTQ
dtq.log <- R6Class(
  classname = "dtq.log",
  public = list(
    log = list(),
    initialize = function(){
      invisible(self)
    },
    add = function(x){
      if(self$length() > getOption("dtq.log.size")){
        warning("data.table query log size limit exceeded, query will not be logged, use purge or increase limit option 'dtq.log.size'")
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
          ][, dtq := list(mapply(dtq$new, dtq_call, env, SIMPLIFY = FALSE)) # using lower granularity class because it recursively match.calls in the chain and allow to get depth
            ][, dtq_seq := sapply(dtq, function(x) x$depth)
              ][, dtq_id := cumsum(dtq_seq==1L)
                ][, dtq_depth := max(dtq_seq), .(dtq_id)
                  ][, dtq := list(mapply(function(x, dtq_seq, dtq_depth) x$apply.DTQ.seq.depth(dtq_seq, dtq_depth), dtq, dtq_seq, dtq_depth, SIMPLIFY = FALSE)) # update child class metadata
                    ][, dtq := list(mapply(function(x, in_rows, out_rows) x$apply.DTQ.rows(in_rows, out_rows), dtq, in_rows, out_rows, SIMPLIFY = FALSE)) # update child class metadata
                      ][, src := sapply(dtq, function(x) x$src)
                        ][, query := unlist(deparse.dtq.call(dtq_call))
                          ][, .(seq, dtq_id, dtq_seq, dtq, dtq_call, src, query, timestamp, env, elapsed, in_rows, out_rows)
                            ][]
    },
    print = function(){
      print(self$process())
      invisible(self)
    }
  ),
  active = list(
    empty = function() data.table(seq=integer(), dtq_id=integer(), dtq_seq=integer(), dtq=list(), dtq_call=list(), src=character(), query=character(), timestamp=Sys.time()[-1L], env=character(), elapsed=numeric(), in_rows=integer(), out_rows=integer()),
    na = function() self$empty[1L]
  )
)

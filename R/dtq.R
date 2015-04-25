#' @title dtq-package
#' @docType package
#' @import data.table R6
#' @author Jan Gorecki
#' @description After loading package by default it start to log data.table queries and their processing details, logs can be accessed by \link{dtl} function.
#' @name dtq-package
NULL

#' @title Get data.table queries logs
#' @param print logical if \emph{TRUE} then will remove \emph{list} type columns from the result.
#' @param chain logical if \emph{TRUE} then aggregated logs to full chains
#' @param purge logical when \emph{TRUE} it will clear dtq logs and return empty data.table
#' @details \emph{timestamp} represents the time of query (or chain) log after it is evaluated.
#' @export
dtl <- function(print = FALSE, chain = FALSE, purge = FALSE){
  src <- env <- dtq_id <- . <- query <- elapsed <- in_rows <- out_rows <- NULL
  if(isTRUE(purge)) return(invisible(.DTQ$purge()))
  dt <- .DTQ$process()
  if(!isTRUE(chain)){
    if(isTRUE(print)) dt <- dt[, .SD, .SDcols=-c("dtq","dtq_call")]
  }
  else if(isTRUE(chain)){
    dt <- dt[,.(dtq_depth=.N, query=paste(query, collapse=""), timestamp=timestamp[.N], elapsed=sum(elapsed), in_rows=in_rows[1L], out_rows=out_rows[.N]), .(dtq_id, env, src)]
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

# dtq.log -------------------------------------------------------------------

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
    empty = function() data.table(seq=integer(), dtq_id=integer(), dtq_seq=integer(), dtq=list(), query=character(), timestamp=Sys.time()[-1L], env=character(), elapsed=numeric(), in_rows=integer(), out_rows=integer()),
    na = function() self$empty[1L]
  ))

# dtq ---------------------------------------------------------------------

#' @title data.table query
#' @docType class
#' @format An R6 class object.
#' @name dtq
#' @details Following fields are created during initialization
#' \itemize{
#' \item{call} call, includes nested preceding calls in chain
#' \item{env} character, top environment of the parent frame from where call was called
#' \item{query} list of language objects, one per query in chain, still recursively nested
#' }
#' Calls are stored in argument non-matched form. They are later matched when transforming into non-language output. So user can access calls as they were called without argument matching applied.
#' @export
dtq <- R6Class(
  classname = "dtq",
  public = list(
    call = NULL,
    env = character(),
    query = list(),
    depth = integer(),
    src = character(),
    DTQ_seq = NA_integer_,
    DTQ_depth = NA_integer_,
    DTQ_in_rows = NA_integer_,
    DTQ_out_rows = NA_integer_,
    decall = function(x){
      if(is.call(x)){
        if(x[[1L]]==as.name("[") || x[[1L]]==as.name("[.data.table")) mx <- as.list(match.call(match.fun(data.table:::`[.data.table`), x)) else return(x)
      } else return(x)
      #lx <- as.list(x)
      to_decall <- chmatch(c("x","i"), names(mx))
      for(id in to_decall[!is.na(to_decall)]) mx[[id]] <- self$decall(x[[id]])
      mx
    }, # recursive trasnform calls to lists of languages
    initialize = function(x, env = NA_character_){
      self$call <- x
      self$env <- env
      self$query <- self$decall(self$call)
      self$depth <- self$get.depth()
      self$src <- self$get.src()
      invisible(self)
    },
    length = function(){
      base::length(self$query)
    },
    get.depth = function(apply.depth = getOption("dtq.apply.depth")){
      for(i.depth in seq_len(apply.depth)){
        if(i.depth > 1L && length(self$query[[rep(2L,i.depth-1L)]]) < 2L) return(i.depth-1L) # handle: data.table()[]
        if(is.name(self$query[[rep(2L,i.depth)]])) return(i.depth)
      }
      stop(paste0("dtq depth recursive call limit exceeded, current limit ",apply.depth,", use: options('dtq.apply.depth')"))
    }, # dtq depth
    get.src = function(){
      if(self$depth > 1L && is.name(self$query[[rep(2L,self$depth-1L)]])){
        paste(deparse(self$query[[rep(2L,self$depth-1L)]], width.cutoff = 500L), collapse="\n")
      }
      else {
        paste(deparse(self$query[[rep(2L,self$depth)]], width.cutoff = 500L), collapse="\n")
      }
    },
    apply.DTQ.seq.depth = function(seq, depth){
      # update btq class object for higher level data from DTQ class
      self$DTQ_seq <- seq
      self$DTQ_depth <- depth
      invisible(self)
    },
    apply.DTQ.rows = function(in_rows, out_rows){
      # update btq class object for higher level data from DTQ class
      self$DTQ_in_rows <- in_rows
      self$DTQ_out_rows <- out_rows
      invisible(self)
    },
    print = function(){
      cat("data.table query:\n")
      cat("  data source name:",self$src,"\n")
      cat("  transformation step:",self$DTQ_seq,"/",self$DTQ_depth,"\n")
      cat("  body:",unlist(deparse.dtq.call(list(self$call))),"\n")
      cat("  in rows:",self$DTQ_in_rows,"| out rows",self$DTQ_out_rows,"\n")
      invisible(self)
    }
  ),
  active = list(
    empty = function() character(),
    na = function() self$empty[1L]
  )
)

# helpers -----------------------------------------------------------------

#' @title is.dtq.call
#' @param x call or list of calls
#' @return logical scalar or list of logical scalars
is.dtq.call <- function(x){
  is.dtq.call.scalar <- function(x){
    if(!is.call(x)) return(FALSE)
    x[[1L]]==as.name("[.data.table")
  }
  if(is.call(x)) is.dtq.call.scalar(x)
  else if(is.list(x)) lapply(x, is.dtq.call.scalar)
  else if(!is.list(x)) FALSE
}

#' @title match.dtq.call
#' @param x call or list of calls, valid data.table query call
#' @seealso \link{is.dtq.call}
#' @note currently not even required during processing as dtq catches already matched call, but can be utilized in manual catching dtq
#' @return the same call(s) but with matched arguments
match.dtq.call <- function(x){
  match.dtq.call.scalar <- function(x){
    match.call(match.fun(get("[.data.table", envir=asNamespace("data.table"), inherits=FALSE)), x, expand.dots = TRUE, envir = parent.frame(1))
  }
  if(is.call(x)) match.dtq.call.scalar(x)
  else if(is.list(x)) lapply(x, match.dtq.call.scalar)
  else if(!is.list(x)) stop("match.dtq.call accepts call or list only")
}

#' @title deparse argument names and their values
#' @param args.names character
#' @param args language objects
#' @return character
deparse_and_paste_arg <- function(args.names, args){
  paste(c(args.names, paste(deparse(args[[args.names]], width.cutoff=500L), collapse="\n")), collapse=" = ")
}

#' @title deparse data.table query call
#' @param x list of calls
#' @return list of character
deparse.dtq.call <- function(x){
  decall.list <- lapply(ifelse(is.dtq.call(x), match.dtq.call(x),x), as.list)
  args.list <- lapply(decall.list, `[`, -c(1L,2L)) # exclude '[' and x' elements
  query.list <- lapply(args.list, function(args) vapply(names(args), deparse_and_paste_arg, "", args, USE.NAMES=FALSE))
  lapply(query.list, function(query) paste0("[",paste(query,collapse=", "),"]"))
}

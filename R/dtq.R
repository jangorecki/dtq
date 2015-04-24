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
  env <- dtq_id <- . <- query <- elapsed <- in_rows <- out_rows <- NULL
  if(isTRUE(purge)) return(invisible(dtq.log$purge()))
  dt <- dtq.log$process()
  if(!isTRUE(chain)){
    if(isTRUE(print)) dt <- dt[, .SD, .SDcols=-c("dtq","dtq_call")]
  }
  else if(isTRUE(chain)){
    dt <- dt[,.(dtq_depth=.N, query=paste(query,collapse=""), timestamp=timestamp[.N], elapsed=sum(elapsed), in_rows=in_rows[1L], out_rows=out_rows[.N]), .(dtq_id,env)]
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
#' @export
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
            ][, dtq_seq := sapply(dtq, function(x) x$depth())
              ][, query := unlist(deparse.dtq.call(dtq_call))
                ][, dtq_id := cumsum(dtq_seq==1L)
                  ][, .(seq, dtq_id, dtq_seq, dtq, dtq_call, query, timestamp, env, elapsed, in_rows, out_rows)
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
    decall = function(x){
      if(is.call(x)){
        if(x[[1L]]==as.name("[") || x[[1L]]==as.name("[.data.table")) mx <- as.list(match.call(match.fun(data.table:::`[.data.table`), x)) else return(x)
      } else return(x)
      lx <- as.list(x)
      to_decall <- chmatch(c("x","i"), names(mx))
      for(id in to_decall[!is.na(to_decall)]) lx[[id]] <- self$decall(x[[id]])
      lx
    }, # recursive trasnform calls to lists of languages
    initialize = function(x, env = NA_character_){
      self$call <- x
      self$env <- env
      self$query <- self$decall(self$call)
      invisible(self)
    },
    length = function(){
      base::length(self$query)
    },
    depth = function(apply.depth = getOption("dtq.apply.depth")){
      for(i.depth in seq_len(apply.depth)){
        if(i.depth > 1L && length(self$query[[rep(2L,i.depth-1L)]]) < 2L) return(i.depth-1L) # handle: data.table()[]
        if(is.name(self$query[[rep(2L,i.depth)]])) return(i.depth)
      }
      stop(paste0("dtq depth recursive call limit exceeded, current limit ",apply.depth,", use: options('dtq.apply.depth')"))
    }, # dtq depth
    print = function(){
      # print(deparse.dtq.call(self$call))
      invisible(self)
    }
  ),
  active = list(
    empty = function() character(),
    na = function() self$empty[1L]
  )
)

# -----------------------------------------------------------------

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
#' @note currently not used during processing as dtq catches already matched call, but can be utilized in case of any other catched dtq
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

#' @title dtq depth
#' @param x list of calls
#' @param apply.depth integer limit of recursive call in chain
#' @return depth of tested queries in chain, points to the `src` object.
#' @note Currently not used, process in using `dtq` class object `depth` method because dtq class store calls casted to recursive lists of calls. Calls are matched to `[.data.table` and that makes `depth` function catch more cases.
depth.dtq.call <- function(x, apply.depth = getOption("dtq.apply.depth",20L)){
  depth.dtq.call.scalar <- function(x){
    for(i.depth in (seq_len(apply.depth)-1L)){ # i.depth <- 0L
      if(i.depth == 0L){
        if(!is.dtq.call(x)) return(i.depth) # 0L
        else if(is.dtq.call(x[[2L]])) next()
        else return(i.depth+1L)
      }
      else if(i.depth > 0L){
        if(!is.dtq.call(x[[rep(2L,i.depth)]])) return(i.depth-1L) # 0L
        if(is.dtq.call(x[[rep(2L,i.depth)]])){
          if(is.call(x[[rep(2L,i.depth)]][2L])) next()
          return(i.depth)
        }
        next()
      }
    }
    stop(paste0("dtq depth recursive call limit exceeded, current limit ",apply.depth,", use: options('dtq.apply.depth')"))
  }
  if(is.call(x)) depth.dtq.call.scalar(x)
  else if(is.list(x)) lapply(x, depth.dtq.call.scalar)
  else if(!is.list(x)) stop("depth.dtq.call accepts call or list only")
}
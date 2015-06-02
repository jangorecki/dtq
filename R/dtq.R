#' @title dtq-package
#' @docType package
#' @import data.table R6
#' @author Jan Gorecki
#' @description After loading package by default it start to log data.table queries and their processing details, logs can be accessed by \link{dtl} function.
#' @name dtq-package
NULL

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
      if(getOption("dtq.debug") && any(is.null(self$depth), "error" %in% class(self$depth), is.na(self$depth), self$depth < 1L)) browser()
      self$src <- self$get.src()
      if(getOption("dtq.debug") && any(is.null(self$src), "error" %in% class(self$src), is.na(self$src))) browser()
      invisible(self)
    },
    length = function(){
      base::length(self$query)
    },
    get.depth = function(apply.depth = getOption("dtq.apply.depth")){
      if(self$query[[1L]] != as.name("[.data.table")) stop(paste0("Query not recognized as valid data.table call: ",deparse_char(self$call)))
      for(i.depth in seq_len(apply.depth)){ # i.depth <- 1L
        if(i.depth > 1L && length(self$query[[rep(2L,i.depth-1L)]]) < 2L) return(i.depth-1L)
        if(is.name(self$query[[rep(2L,i.depth)]]) || (self$query[[rep(2L,i.depth)]][[1L]] != as.name("[.data.table") && self$query[[rep(2L,i.depth)]][[1L]] != as.name("["))) return(i.depth)
      }
      warning(paste0("dtq depth recursive call limit exceeded, current limit ",apply.depth,", use: options('dtq.apply.depth'). Failed to get depth of call: ",deparse_char(self$call)))
      NA_integer_
    }, # dtq depth
    get.src = function(){
      deparse_char(self$query[[rep(2L,self$depth)]])
    },
    apply.DTQ.seq.depth = function(seq, depth){
      # update dtq class object for higher level data from DTQ class
      self$DTQ_seq <- seq
      self$DTQ_depth <- depth
      invisible(self)
    },
    apply.DTQ.rows = function(in_rows, out_rows){
      # update dtq class object for higher level data from DTQ class
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

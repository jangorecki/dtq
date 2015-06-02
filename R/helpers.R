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

#' @title deparse object to character
#' @param x object to deparse
#' @return character scalar
deparse_char <- function(x) paste(deparse(x, width.cutoff = 500L), collapse="\n")

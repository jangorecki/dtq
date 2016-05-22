library(data.table)
library(dtq)

# dtq new
dt <- data.table(a = 1:10, b = 1:5)
q <- quote(dt[, a, b])
q[[1L]] <- as.name("[.data.table")
r <- dtq$new(q)
stopifnot(
    "dtq" %in% class(r), # dtq$new method class
    is.call(r$call) && length(r$query) && as.logical(r$depth) && is.character(r$src) # dtq$new valid object construction
)

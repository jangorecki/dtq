context("dtq")

test_that("dtq new", {
  
  dt <- data.table(a = 1:10, b = 1:5)
  q <- quote(dt[, a, b])
  q[[1L]] <- as.name("[.data.table")
  r <- dtq$new(q)
  expect_true("dtq" %in% class(r), info="dtq$new method class")
  expect_true(is.call(r$call) && length(r$query) && as.logical(r$depth) && is.character(r$src), info="dtq$new valid object construction")
  
})

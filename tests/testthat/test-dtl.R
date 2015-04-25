context("dtl")

test_that("dtl expected data types", {
  
  dtl(purge=TRUE)
  DT <- data.table(a=1:10, b=1:5)
  DT[, .(a = sum(a)), b
     ][a > median(a), .(a, b, z = b + a)]
  expected_r <- structure(c("integer", "integer", "integer", "list", "list", "character", "character", "double", "character", "double", "integer", "integer"),
                          .Names = c("seq", "dtq_id", "dtq_seq", "dtq", "dtq_call", "src", "query", "timestamp", "env", "elapsed", "in_rows", "out_rows"))
  expect_identical(sapply(dtl(),typeof), expected_r, info="output column types")
  
})

test_that("dtl chain", {
  
  # TO DO
  expect_identical(FALSE, FALSE, info="query to chain aggregation")
  
})

test_that("dtl purge arg", {
  
  dtl(purge=TRUE)
  DT <- data.table(a=1:10, b=1:5)
  DT[, .(a = sum(a)), b
     ][a > median(a), .(a, b, z = b + a)]
  dtl(purge=TRUE)
  expect_identical(nrow(dtl()), 0L, info="purge")
  
})

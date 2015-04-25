context("dtl")

test_that("dtl expected data types", {
  
  dtl(purge=TRUE)
  DT <- data.table(a=1:10, b=1:5)
  DT[, .(a = sum(a)), b
     ][a > median(a), .(a, b, z = b + a)]
  expected_r <- structure(c("integer", "integer", "integer", "list", "list", "character", "character", "double", "character", "double", "integer", "integer"),
                          .Names = c("seq", "dtq_id", "dtq_seq", "dtq", "dtq_call", "src", "query", "timestamp", "env", "elapsed", "in_rows", "out_rows"))
  expect_identical(sapply(dtl(), typeof), expected_r, info="output column types")
  dtl(purge=TRUE)
  expect_identical(sapply(dtl(), typeof), expected_r, info="empty output column types")
  
})

test_that("dtl chain expected data types", {
  
  dtl(purge=TRUE)
  DT <- data.table(a=1:10, b=1:5)
  DT[, .(a = sum(a)), b
     ][a > median(a), .(a, b, z = b + a)]
  DT[][][]
  expect_identical(nrow(dtl(chain=FALSE)), 5L, info="valid number of queries")
  expect_identical(nrow(dtl(chain=TRUE)), 2L, info="valid number of chains")
  expected_r <- structure(c("integer", "character", "character", "integer", "character", "double", "double", "integer", "integer"),
                          .Names = c("dtq_id", "env", "src", "dtq_depth", "query", "timestamp", "elapsed", "in_rows", "out_rows"))
  expect_identical(sapply(dtl(chain=TRUE), typeof), expected_r, info="chain aggregation output column types")
  dtl(purge=TRUE)
  expect_identical(sapply(dtl(chain=TRUE), typeof), expected_r, info="empty chain aggregation output column types")
  
})

test_that("dtl purge arg", {
  
  dtl(purge=TRUE)
  DT <- data.table(a=1:10, b=1:5)
  DT[, .(a = sum(a)), b
     ][a > median(a), .(a, b, z = b + a)]
  dtl(purge=TRUE)
  expect_identical(nrow(dtl()), 0L, info="purge")
  
})

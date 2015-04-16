context("dtl")

test_that("dtl expected data types", {
  
  dtl(purge=TRUE)
  DT <- data.table(a=1:10, b=1:5)
  DT[, .(a = sum(a)), b
     ][a > median(a), .(a, b, z = b + a)]
  expected_r <- structure(c("integer", "integer", "integer", "list", "character", "double", "character", "double", "integer", "integer"),
                          .Names = c("seq", "dtq_id", "dtq_seq", "dtq", "query", "timestamp", "env", "elapsed", "nrow_in", "nrow_out"))
  expect_identical(sapply(dtl(),typeof),expected_r)
  
})

test_that("dtl expected nrow within sequence", {
  
  dtl(purge=TRUE)
  DT <- data.table(a=1:10, b=1:5)
  DT[, .(a = sum(a)), b
     ][a > median(a), .(a, b, z = b + a)]
  DT[,.(a,b)]
  DT[,.(a,b)][,.(a,b)][,.(a,b)]
  expect_identical(dtl()[,.N,.(dtq_id)][,N], c(2L,1L,3L))
  
})

test_that("dtl expected purge", {
  
  dtl(purge=TRUE)
  DT <- data.table(a=1:10, b=1:5)
  DT[, .(a = sum(a)), b
     ][a > median(a), .(a, b, z = b + a)]
  dtl(purge=TRUE)
  expect_true(nrow(dtl())==0L)
  
})


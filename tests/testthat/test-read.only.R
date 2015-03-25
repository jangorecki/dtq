context("read.only")

test_that("read.only attribute tests", {
  
  dt <- data.table(a = 1L)
  dt[, a := 2L]
  expect_equal(dt, data.table(a = 2L))
  
  dt <- data.table(a = 1L)
  setattr(dt,"read.only",TRUE)
  dt[, a := 2L]
  expect_equal(dt, data.table(a = 1L), check.attributes = FALSE)
  
})
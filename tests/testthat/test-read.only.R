context("read.only")

test_that("read.only attribute tests", {
  
  dt <- data.table(a = 1L)
  setattr(dt,"read.only",NULL)
  dt[, a := 2L]
  expect_equal(dt, data.table(a = 2L), info="read.only NULL")
  
  dt <- data.table(a = 1L)
  setattr(dt,"read.only",FALSE)
  dt[, a := 2L]
  expect_equal(dt, data.table(a = 2L), check.attributes = FALSE, info="read.only FALSE")
  
  dt <- data.table(a = 1L)
  setattr(dt,"read.only",TRUE)
  dt2 <- dt[, a := 2L]
  expect_equal(dt, data.table(a = 1L), check.attributes = FALSE, info="read.only TRUE prevent write on input")
  expect_equal(dt2, data.table(a = 2L), info="read.only TRUE still writes output")
  
})
context("test.data.table")

test_that("test dtq and test.data.table", {
  
  test.dt <- data.table::test.data.table
  environment(test.dt) <- .BaseNamespaceEnv
  
  dtl(purge = TRUE)
  DT <- data.table(a = 1, b = letters[1])
  f <- function(x) x[]
  environment(f) <- .BaseNamespaceEnv
  f(DT)
  
  expect_identical(dtq.log$length(), 1L, info="test.data.table confirm logging active")
  expect_identical(dtq.log$log[[1L]]$env, "base", info="test.data.table confirm logging active env")
  expect_null(suppressMessages(suppressMessages(test.dt())), info="test.data.table without errors")
  rm(test.dt)
  expect_true(dtq.log$length() > 7000L, info="test.data.table confirm more logs than 7e3")
  
})

context("test.data.table")
# moved to manual as not needed to test it every time, also good to resolve #2 first

test_that("test dtq and test.data.table", {
  
  dtl(purge = TRUE)
  DT <- data.table(a = 1:10, b = letters[1:5])
  DT[]
  
  expect_identical(dtq.log$length(), 1L, info="test.data.table confirm logging active")
  expect_null(suppressMessages(suppressMessages(test.data.table())), info="test.data.table without errors")
  
})

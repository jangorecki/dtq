context("do.dtq.log")
# moved to manual as tests failed in auto check while passing in interactive mode, most likely due to topenv(parent.frame(1)) in zzz.R

test_that("do.dtq.log exclude-include opts", {
  
  DT <- data.table(a = 1L)
  options("dtq.log.exclude" = "R_GlobalEnv",
          "dtq.log.include" = character())
  dtl(purge = TRUE)
  DT[]
  expect_identical(dtq.log$length(), 0L, info="exclude log global env")
  
  options("dtq.log.exclude" = character(),
          "dtq.log.include" = "dwtools")
  dtl(purge = TRUE)
  DT[]
  expect_identical(dtq.log$length(), 0L, info="include log from ext package")
  
  options("dtq.log.exclude" = character(),
          "dtq.log.include" = "R_GlobalEnv")
  dtl(purge = TRUE)
  DT[]
  writeLines(dtq.log$log[[1L]]$env,"env.txt")
  expect_identical(dtq.log$length(), 1L, info="include log global env")
  
  options("dtq.log.exclude" = "R_GlobalEnv",
          "dtq.log.include" = "R_GlobalEnv")
  dtl(purge = TRUE)
  DT[]
  expect_identical(dtq.log$length(), 0L, info="include and exclude log global env")
  
  options("dtq.log.exclude" = character(),
          "dtq.log.include" = character())
  
})

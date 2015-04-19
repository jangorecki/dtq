context("dtq.log")

test_that("dtq.log basics", {
  
  log_fields <- c("timestamp", "env", "dtcall", "elapsed", "in_rows", "out_rows")
  
  dtl(purge = TRUE)
  DT <- data.table(a = 1:10, b = letters[1:5])
  LKP <- data.table(b = letters[1:5], ratio = c(1.2,5,2.5,3.2,0.5), key = "b")
  DT2 <- DT[, .(a = sum(a)), b
            ][a > median(a), .(b, a, adj_a = a * 1.1)]
  LKP[DT2, .(b, a, adj2_a = adj_a * ratio)]
  
  expect_identical(length(dtq.log$log), dtq.log$length(), info="dtq.log$length method")
  expect_identical(dtq.log$length(), 3L, info="number of calls")
  expect_true(all(sapply(dtq.log$log, function(x) identical(names(x), log_fields))), info="all logs contains full set of fields")
  
})

test_that("dtq.log process method: in-out rows", {
  
  DT <- data.table(a = 1:10, b = 1:5)
  dtl(purge = TRUE)
  DT[FALSE][,.(a,b)][,z := a+b]
  DT[,.(sum(a)),b][-.N][]
  expect_identical(sapply(dtq.log$log,`[[`,"in_rows"), c(10L,0L,0L,10L,5L,4L), info="basic in rows")
  
  f <- function() data.table(a = 1:3)
  dtl(purge = TRUE)
  f()[,.(sum(a))]
  data.table()[]
  expect_identical(sapply(dtq.log$log,`[[`,"in_rows"), c(3L,0L), info="advanced in rows")
  
  DT <- data.table(a = 1:10, b = 1:5)
  dtl(purge = TRUE)
  DT[,.(a,b)]
  DT[,.(sum(a))]
  DT[,sum(a),b]
  DT[,z:=sum(a),b]
  expect_identical(sapply(dtq.log$log,`[[`,"out_rows"), c(10L,1L,5L,10L), info="basic out rows")
  
  DT <- data.table(a = 1:10, b = rnorm(10))
  dtl(purge = TRUE)
  DT[,plot(x=a,y=b)]
  DT[,a]
  DT[,mean(a)]
  DT[,.N]
  expect_identical(sapply(dtq.log$log,`[[`,"out_rows"), rep(NA_integer_,4L), info="NA out rows")
  
  DT <- data.table(a = 1:10, b = 1:5, key="b")
  f <- function() data.table(a = 1:3)
  dtl(purge = TRUE)
  data.table()[]
  DT[DT]
  DT[,.SD]
  DT[-.N][-.N][-.N][-.N]
  f()[-2L]
  expect_identical(sapply(dtq.log$log,`[[`,"out_rows"), c(0L,20L,10L,9:6,2L), info="advanced out rows")
  
})

test_that("dtq.log purge method", {
  
  dtl(purge=TRUE)
  DT <- data.table(a=1:10, b=1:5)
  DT[, .(a = sum(a)), b
     ][a > median(a), .(a, b, z = b + a)]
  dtq.log$purge()
  expect_identical(dtq.log$length(), 0L, info="purge")
  
})

test_that("dtq.log process method", {
  
  dtl(purge = TRUE)
  data.table()[]
  dtq.log$process()
  expect_identical(dtq.log$length(), 1L, info="NULL data.table edge case")
  
  dtl(purge=TRUE)
  DT <- data.table(a=1:10, b=1:5)
  DT[, .(a = sum(a)), b
     ][a > median(a), .(a, b, z = b + a)]
  DT[,.(a,b)]
  DT[,.(a,b)][,.(a,b)][,.(a,b)]
  dt <- dtq.log$process()
  expect_identical(dt[,.N,.(dtq_id)][,N], c(2L,1L,3L), info="queries within sequence")
  
})

test_that("dtq.log TO DO", {
  
  expect_identical(FALSE, FALSE, info="timing correct for Sys.sleep case")
  expect_identical(FALSE, FALSE, info="reordered input to `[`")
  expect_identical(FALSE, FALSE, info="dtq.log=FALSE option works")
  expect_identical(FALSE, FALSE, info="dtq.apply.depth option works")
  
})
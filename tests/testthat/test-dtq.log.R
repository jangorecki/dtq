context("dtq.log")

test_that("dtq.log basics", {
  
  options("dtq.log"=FALSE)
  dtl(purge = TRUE)
  DT <- data.table(a = 1:10, b = letters[1:5])
  DT[,.(a,b)]
  expect_identical(.DTQ$length(), 0L, info="dtq.log=FALSE option works")
  options("dtq.log"=TRUE)
  
  dtl(purge = TRUE)
  DT <- data.table(a = 1:10, b = letters[1:5])
  LKP <- data.table(b = letters[1:5], ratio = c(1.2,5,2.5,3.2,0.5), key = "b")
  DT2 <- DT[, .(a = sum(a)), b
            ][a > median(a), .(b, a, adj_a = a * 1.1)]
  LKP[DT2, .(b, a, adj2_a = adj_a * ratio)]
  expect_identical(length(.DTQ$log), .DTQ$length(), info="dtq.log$length method")
  expect_identical(.DTQ$length(), 3L, info="number of calls")
  log_fields <- c("timestamp", "env", "dtq_call", "elapsed", "in_rows", "out_rows")
  expect_true(all(sapply(.DTQ$log, function(x) identical(names(x), log_fields))), info="all logs contains full set of fields")
  
})

test_that("dtq.log process method: in-out rows", {
  
  DT <- data.table(a = 1:10, b = 1:5)
  dtl(purge = TRUE)
  DT[FALSE][,.(a,b)][,z := a+b]
  DT[,.(sum(a)),b][-.N][]
  expect_identical(sapply(.DTQ$log,`[[`,"in_rows"), c(10L,0L,0L,10L,5L,4L), info="basic in rows")
  
  f <- function() data.table(a = 1:3)
  dtl(purge = TRUE)
  f()[,.(sum(a))]
  data.table()[]
  expect_identical(sapply(.DTQ$log,`[[`,"in_rows"), c(3L,0L), info="advanced in rows")
  
  DT <- data.table(a = 1:10, b = 1:5)
  dtl(purge = TRUE)
  DT[,.(a,b)]
  DT[,.(sum(a))]
  DT[,sum(a),b]
  DT[,z:=sum(a),b]
  expect_identical(sapply(.DTQ$log,`[[`,"out_rows"), c(10L,1L,5L,10L), info="basic out rows")
  
  DT <- data.table(a = 1:10, b = rnorm(10))
  dtl(purge = TRUE)
  DT[,plot(x=a,y=b)]
  DT[,a]
  DT[,mean(a)]
  DT[,.N]
  expect_identical(sapply(.DTQ$log,`[[`,"out_rows"), rep(NA_integer_,4L), info="NA out rows")
  
  DT <- data.table(a = 1:10, b = 1:5, key="b")
  f <- function() data.table(a = 1:3)
  dtl(purge = TRUE)
  data.table()[]
  DT[DT]
  DT[,.SD]
  DT[-.N][-.N][-.N][-.N]
  f()[-2L]
  expect_identical(sapply(.DTQ$log,`[[`,"out_rows"), c(0L,20L,10L,9:6,2L), info="advanced out rows")
  
})

test_that("dtq.log purge method", {
  
  dtl(purge=TRUE)
  DT <- data.table(a=1:10, b=1:5)
  DT[, .(a = sum(a)), b
     ][a > median(a), .(a, b, z = b + a)]
  .DTQ$purge()
  expect_identical(.DTQ$length(), 0L, info="purge")
  
})

test_that("dtq.log process method", {
  
  dtl(purge = TRUE)
  data.table()[]
  expect_identical(.DTQ$length(), 1L, info="NULL data.table edge case")
  
  dtl(purge = TRUE)
  data.table()[][][][][][][]
  expect_equal(subset(.DTQ$process(), select = c("seq", "dtq_id", "dtq_seq", "src", "query", "in_rows", "out_rows")), data.table(seq=1:7,dtq_id=1L,dtq_seq=1:7,src=rep("data.table()",7),query=rep("[]",7),in_rows=0L,out_rows=0L), info="NULL data.table edge edge case")
  
  dtl(purge=TRUE)
  DT <- data.table(a=1:10, b=1:5)
  DT[, .(a = sum(a)), b
     ][a > median(a), .(a, b, z = b + a)]
  DT[,.(a,b)]
  DT[,.(a,b)][,.(a,b)][,.(a,b)]
  dt <- .DTQ$process()
  expect_identical(dt[,.N,.(dtq_id)][,N], c(2L,1L,3L), info="queries within sequence")
  
  dtl(purge=TRUE)
  DT1 <- data.table(a=1:3, b=letters[1:3], key="a")
  DT2 <- data.table(a=1:3, z=rnorm(3))
  DT1[DT2[,.(z=sum(z)),,a]]
  expect_identical(.DTQ$length(), 2L, info="join with nested subset")
  expect_identical(.DTQ$process()$src, c("DT2","DT1"), info="chain join select and aggr on DT1, DT2 variables, src test")
  
  dtl(purge=TRUE)
  DT <- data.table(a=1:10, b=1:5)
  DT[,.(.I, .N, GRP = .GRP), b][, head(.SD,1L), GRP]
  expect_identical(.DTQ$process()$query, c("[j = .(.I, .N, GRP = .GRP), by = b]","[j = head(.SD, 1L), by = GRP]"), info="usage of .I, .N, .GRP, .SD")
  expect_identical(.DTQ$process()$src, c("DT","DT"), info="chain select and aggr on single DT variable, src test")
  
  dtl(purge = TRUE)
  DT <- data.table(a = 1:10, b = letters[1:5])
  DT[keyby=b,,.(a=sum(a))]
  expect_identical(.DTQ$process()$query[[1L]], "[j = .(a = sum(a)), keyby = b]", info="reordered input to `[`")
  expect_identical(.DTQ$process()$src, "DT", info="keyby on single DT variable, src test")
  
  dtl(purge = TRUE)
  DT <- data.table(a = 1:10, b = letters[1:5])
  DT[,.(a,b)][,.(a,b)]
  options("dtq.apply.depth"=1L)
  expect_error(.DTQ$process(), info="dtq.apply.depth option")
  options("dtq.apply.depth"=20L)
  
  dtl(purge=TRUE)
  DT1 <- data.table(a=1:3, b=letters[1:3], key="a")
  DT2 <- data.table(a=1:3, z=rnorm(3))
  DT1[DT2[,.(z=sum(z)),,a]]
  expect_identical(.DTQ$process()$query, c("[j = .(z = sum(z)), keyby = a]","[i = DT2[, .(z = sum(z)), , a]]"), info="deparse `i` while join, test for #3")
  
  dtl(purge=TRUE)
  DT <- data.table(a=1:4, b=letters[1:4])
  x <- 1:5
  DT[x[2:3]]
  expect_identical(.DTQ$length(), 1L, info="subset dt on row id from subset of integer vector, length")
  expect_identical(.DTQ$process()$query, "[i = x[2:3]]", info="subset dt on row id from subset of integer vector, query")
  
  dtl(purge=TRUE)
  DT <- data.table(a=1:4, b=letters[1:4])
  X <- data.table(z = 1:5)
  DT[X[2:3,z]]
  expect_identical(.DTQ$length(), 2L, info="subset dt on row id from subset of data.table, length")
  expect_identical(.DTQ$process()$query, c("[i = 2:3, j = z]","[i = X[2:3, z]]"), info="subset dt on row id from subset of data.table, query")
  
  dtl(purge=TRUE)
  DT <- data.table(a=1:4, b=letters[1:4])
  f <- function() 1:5
  DT[f()[2:3]]
  expect_identical(.DTQ$length(), 1L, info="subset dt on row id from function which returns integer vector, length")
  expect_identical(.DTQ$process()$query, "[i = f()[2:3]]", info="subset dt on row id from function which returns integer vector, query")
  
  dtl(purge=TRUE)
  DT <- data.table(a=1:4, b=letters[1:4])
  f <- function() data.table(z = 1:5)
  DT[f()[2:3, z]]
  expect_identical(.DTQ$length(), 2L, info="subset dt on row id from function which returns data.table, length")
  expect_identical(.DTQ$process()$query, c("[i = 2:3, j = z]","[i = f()[2:3, z]]"), info="subset dt on row id from function which returns data.table, query")
  expect_identical(.DTQ$process()$src, c("f()","DT"), info="subset dt on row id from function which returns data.table, src")
  
  dtl(purge=TRUE)
  DT <- function() data.table(a=1:4, b=letters[1:4])
  f <- function() data.table(z = 1:5)
  DT()[f()[2:3, z]]
  expect_identical(.DTQ$length(), 2L, info="subset function-data.table to function-data.table, length")
  expect_identical(.DTQ$process()$query, c("[i = 2:3, j = z]", "[i = f()[2:3, z]]"), info="subset function-data.table to function-data.table, query")
  expect_identical(.DTQ$process()$src, c("f()","DT()"), info="subset function-data.table to function-data.table, src")
  
  # closes #5
  dtl(purge=TRUE)
  f <- function() data.table(z = 1:5)
  data.table(a=1:4, b=letters[1:4])[f()[2:3, z]]
  expect_identical(.DTQ$length(), 2L, info="subset data.table-function to function-data.table, length")
  expect_identical(.DTQ$process()$query, c("[i = 2:3, j = z]", "[i = f()[2:3, z]]"), info="subset data.table-function to function-data.table, query")
  expect_identical(.DTQ$process()$src, c("f()","data.table(a = 1:4, b = letters[1:4])"), info="subset data.table-function to function-data.table, src")
  dtl(purge=TRUE)
  g <- function(seq) data.table(z = seq)
  data.table(a=1:4, b=letters[1:4])[g(1:5)[2:3, z]]
  expect_identical(.DTQ$length(), 2L, info="subset data.table-function to function-data.table, length")
  expect_identical(.DTQ$process()$query, c("[i = 2:3, j = z]", "[i = g(1:5)[2:3, z]]"), info="subset data.table-function to function-data.table, query")
  expect_identical(.DTQ$process()$src, c("g(1:5)","data.table(a = 1:4, b = letters[1:4])"), info="subset data.table-function to function-data.table, src")
  
  # TO DO check NA
  # options("dtq.debug" = TRUE)
  # dtl(purge=TRUE)
  # tables(silent = TRUE)[, NAME]
  # .DTQ$process()

})

test_that("dtq.log timing", {
  
  dtl(purge=TRUE)
  DT <- data.table(a=1:10, b=1:5)
  DT[,{Sys.sleep(0.16); .(a = sum(a))}]
  expect_true(.DTQ$log[[1L]]$elapsed > 0.15, info="timing correct for Sys.sleep case")
  
})

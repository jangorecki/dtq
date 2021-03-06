library(dtq)
library(testthat)

context("test.data.table")

test.dt <- data.table::test.data.table
environment(test.dt) <- .BaseNamespaceEnv

dtl(purge = TRUE)
DT <- data.table(a = 1, b = letters[1])
f <- function(x) x[]
environment(f) <- .BaseNamespaceEnv
f(DT)

expect_identical(.DTQ$length(), 1L, info="test.data.table confirm logging active")
expect_identical(.DTQ$log[[1L]]$env, "base", info="test.data.table confirm logging active env")
expect_null(suppressMessages(test.dt()), info="test.data.table without errors")
rm(test.dt)
expect_true(.DTQ$length() > 7000L, info="test.data.table confirm more logs than 7e3")
expect_true("data.table" %chin% class(.DTQ$process()), info="dtq.log$process passed on all test.data.table queries")

# Running /home/jan/R/x86_64-pc-linux-gnu-library/3.2/data.table/tests/tests.Rraw 
# Running test id 1440     Tests 1441-1444 not run. If required install the 'fr_FR.utf8' locale.
# All 4243 tests (lastID=1525) in inst/tests/tests.Rraw completed ok in 00:01:17 on Mon Jun  1 23:27:32 2015 (endian=little, sizeof(long double)==16)

context("dtq.log")

test_that("dtq.log logs all calls and all fields", {
  
  dtl(purge = TRUE)
  DT <- data.table(a = 1:10, b = letters[1:5])
  LKP <- data.table(b = letters[1:5], ratio = rnorm(5), key = "b")
  DT2 <- DT[, .(a = sum(a)), b
            ][a > median(a), .(b, a, adj_a = a * 1.1)]
  LKP[DT2, .(b, a, adj2_a = adj_a * ratio)]
  expected_names <- c("timestamp", "env", "dtcall", "elapsed", "nrow_in", "nrow_out")
  expect_identical(length(dtq.log$log), 3L)
  expect_true(all(sapply(dtq.log$log, function(x) identical(names(x), expected_names))))
  
})

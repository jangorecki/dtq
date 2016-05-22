library(data.table)
library(dtq)

# read.only attribute tests
dt <- data.table(a = 1L)
setattr(dt,"read.only",NULL)
dt[, a := 2L]
stopifnot(all.equal(dt, data.table(a = 2L))) # read.only NULL

dt <- data.table(a = 1L)
setattr(dt,"read.only",FALSE)
dt[, a := 2L]
stopifnot(all.equal(dt, data.table(a = 2L), check.attributes = FALSE)) # read.only FALSE

dt <- data.table(a = 1L)
setattr(dt,"read.only",TRUE)
dt2 <- dt[, a := 2L]
stopifnot(
    all.equal(dt, data.table(a = 1L), check.attributes = FALSE), # TRUE prevent write on input
    all.equal(dt2, data.table(a = 2L)) # TRUE still writes output
)

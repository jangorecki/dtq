library(data.table)
library(dtq)

# dtl expected data types
dtl(purge=TRUE)
DT <- data.table(a=1:10, b=1:5)
DT[, .(a = sum(a)), b
   ][a > median(a), .(a, b, z = b + a)]
expected_r <- structure(c("integer", "integer", "integer", "list", "list", "character", "character", "double", "character", "double", "integer", "integer"),
                        .Names = c("seq", "dtq_id", "dtq_seq", "dtq", "dtq_call", "src", "query", "timestamp", "env", "elapsed", "in_rows", "out_rows"))
stopifnot(
    identical(sapply(dtl(), typeof), expected_r), # output column types
    {
        dtl(purge=TRUE)
        identical(sapply(dtl(), typeof), expected_r) # empty output column types
    }
)

# dtl chain expected data types
dtl(purge=TRUE)
DT <- data.table(a=1:10, b=1:5)
DT[, .(a = sum(a)), b
   ][a > median(a), .(a, b, z = b + a)]
DT[][][]
expected_r <- structure(c("integer", "character", "character", "integer", "character", "double", "double", "integer", "integer"),
                        .Names = c("dtq_id", "env", "src", "dtq_depth", "query", "timestamp", "elapsed", "in_rows", "out_rows"))
stopifnot(
    identical(nrow(dtl(chain=FALSE)), 5L), # valid number of queries
    identical(nrow(dtl(chain=TRUE)), 2L), # valid number of chains
    identical(sapply(dtl(chain=TRUE), typeof), expected_r), # chain aggregation output column types
    identical(dtl(chain=TRUE)$query, c("[j = .(a = sum(a)), by = b][i = a > median(a), j = .(a, b, z = b + a)]","[][][]")), # chain aggregation output query
    {
        dtl(purge=TRUE)
        identical(sapply(dtl(chain=TRUE), typeof), expected_r) # empty chain aggregation output column types
    }
)

# dtl purge arg
dtl(purge=TRUE)
DT <- data.table(a=1:10, b=1:5)
DT[, .(a = sum(a)), b
   ][a > median(a), .(a, b, z = b + a)]
dtl(purge=TRUE)
stopifnot(identical(nrow(dtl()), 0L)) # purge

library(data.table)
library(dtq)

# dtq.log basics

options("dtq.log"=FALSE)
dtl(purge = TRUE)
DT <- data.table(a = 1:10, b = letters[1:5])
DT[,.(a,b)]
stopifnot(identical(.DTQ$length(), 0L)) # dtq.log=FALSE option works
options("dtq.log"=TRUE)

dtl(purge = TRUE)
DT <- data.table(a = 1:10, b = letters[1:5])
LKP <- data.table(b = letters[1:5], ratio = c(1.2,5,2.5,3.2,0.5), key = "b")
DT2 <- DT[, .(a = sum(a)), b
          ][a > median(a), .(b, a, adj_a = a * 1.1)]
LKP[DT2, .(b, a, adj2_a = adj_a * ratio)]
stopifnot(
    identical(length(.DTQ$log), .DTQ$length()), # dtq.log$length method
    identical(.DTQ$length(), 3L), # number of calls
    {
        log_fields <- c("timestamp", "env", "dtq_call", "elapsed", "in_rows", "out_rows")
        all(sapply(.DTQ$log, function(x) identical(names(x), log_fields))) # all logs contains full set of fields
    }
)

# dtq.log process method: in-out rows
DT <- data.table(a = 1:10, b = 1:5)
dtl(purge = TRUE)
DT[FALSE][,.(a,b)][,z := a+b]
DT[,.(sum(a)),b][-.N][]
stopifnot(identical(sapply(.DTQ$log,`[[`,"in_rows"), c(10L,0L,0L,10L,5L,4L))) # basic in rows

f <- function() data.table(a = 1:3)
dtl(purge = TRUE)
f()[,.(sum(a))]
data.table()[]
stopifnot(identical(sapply(.DTQ$log,`[[`,"in_rows"), c(3L,0L))) # advanced in rows

DT <- data.table(a = 1:10, b = 1:5)
dtl(purge = TRUE)
DT[,.(a,b)]
DT[,.(sum(a))]
DT[,sum(a),b]
DT[,z:=sum(a),b]
stopifnot(identical(sapply(.DTQ$log,`[[`,"out_rows"), c(10L,1L,5L,10L))) # basic out rows

DT <- data.table(a = 1:10, b = rnorm(10))
dtl(purge = TRUE)
DT[,plot(x=a,y=b)]
DT[,a]
DT[,mean(a)]
DT[,.N]
stopifnot(identical(sapply(.DTQ$log,`[[`,"out_rows"), rep(NA_integer_,4L))) # NA out rows

DT <- data.table(a = 1:10, b = 1:5, key="b")
f <- function() data.table(a = 1:3)
dtl(purge = TRUE)
data.table()[]
DT[DT]
DT[,.SD]
DT[-.N][-.N][-.N][-.N]
f()[-2L]
stopifnot(identical(sapply(.DTQ$log,`[[`,"out_rows"), c(0L,20L,10L,9:6,2L))) # advanced out rows

# dtq.log purge method
dtl(purge=TRUE)
DT <- data.table(a=1:10, b=1:5)
DT[, .(a = sum(a)), b
   ][a > median(a), .(a, b, z = b + a)]
.DTQ$purge()
stopifnot(identical(.DTQ$length(), 0L)) # purge

# dtq.log process method - dtq depth basic cases
dtl(purge = TRUE)

data.table(a = numeric())[, .(a)]
r_src <- "data.table(a = numeric())"

data.table(a = 1)[, .(a)]
r_src <- c(r_src, "data.table(a = 1)")

DT <- data.table(a = 1)
DT[, .(a)]
r_src <- c(r_src, "DT")

DT2 <- DT
DT2[, .(a)]
r_src <- c(r_src, "DT2")

make_dt <- function() data.table(a = 1:5)
make_dt()[, .(a)]
r_src <- c(r_src, "make_dt()")

make_dt_seq <- function(seq) data.table(a = seq)
make_dt_seq(1:5)[, .(a)]
r_src <- c(r_src, "make_dt_seq(1:5)")

DT <- data.table(a = 1)
get_dt <- function() DT
get_dt()[, .(a)]
r_src <- c(r_src, "get_dt()")

make_CJ_dt <- function(x, y) CJ(a = x, b = y)
make_CJ_dt(1:3, 1:2)[, .(a)]
x <- 1:3
y <- 1:2
make_CJ_dt(x, y)[, .(a)]
r_src <- c(r_src, "make_CJ_dt(1:3, 1:2)")
r_src <- c(r_src, "make_CJ_dt(x, y)")

x <- 1:10
data.table(a = x[2:5], b = x[2:5])[, .(a)]
r_src <- c(r_src, "data.table(a = x[2:5], b = x[2:5])")

stopifnot(
    identical(.DTQ$process()$seq, seq_len(length(r_src))), # seq for various basic cases
    identical(.DTQ$process()$dtq_seq, rep(1L, length(r_src))), # depth for various basic cases
    identical(.DTQ$process()$src, r_src), # src for various basic cases
    identical(.DTQ$process()$query, rep("[j = .(a)]", length(r_src))), # query for various basic cases
    identical(.DTQ$process()$dtq_id, seq_len(length(r_src))) # dtq_id for various basic cases
)

# dtq.log process method

dtl(purge = TRUE)
data.table()[]
stopifnot(
    identical(.DTQ$length(), 1L), # NULL data.table print edge case length
    identical(.DTQ$process()$dtq_seq, 1L), # NULL data.table print edge case dtq_seq
    identical(.DTQ$process()$src, "data.table()"), # NULL data.table print edge case src
    identical(.DTQ$process()$query, "[]") # NULL data.table print edge case query
)

dtl(purge = TRUE)
data.table()[][]
stopifnot(
    identical(.DTQ$length(), 2L), # NULL data.table print x2 edge case length
    identical(.DTQ$process()$dtq_seq, 1:2), # NULL data.table print x2 edge case dtq_seq
    identical(.DTQ$process()$src, rep("data.table()",2)), # NULL data.table print x2 edge case src
    identical(.DTQ$process()$query, rep("[]",2)) # NULL data.table print x2 edge case query
)

dtl(purge = TRUE)
data.table()[][][][][][][]
stopifnot(all.equal(subset(.DTQ$process(), select = c("seq", "dtq_id", "dtq_seq", "src", "query", "in_rows", "out_rows")), data.table(seq=1:7,dtq_id=1L,dtq_seq=1:7,src=rep("data.table()",7),query=rep("[]",7),in_rows=0L,out_rows=0L))) # NULL data.table edge edge case

dtl(purge=TRUE)
DT <- data.table(a=1:10, b=1:5)
DT[, .(a = sum(a)), b
   ][a > median(a), .(a, b, z = b + a)]
DT[,.(a,b)]
DT[,.(a,b)][,.(a,b)][,.(a,b)]
dt <- .DTQ$process()
stopifnot(identical(dt[,.N,.(dtq_id)][,N], c(2L,1L,3L))) # queries within sequence

dtl(purge=TRUE)
DT1 <- data.table(a=1:3, b=letters[1:3], key="a")
DT2 <- data.table(a=1:3, z=rnorm(3))
DT1[DT2[,.(z=sum(z)),,a]]
stopifnot(
    identical(.DTQ$length(), 2L), # join with nested subset
    identical(.DTQ$process()$src, c("DT2","DT1")) # chain join select and aggr on DT1, DT2 variables, src test
)
    
dtl(purge=TRUE)
DT <- data.table(a=1:10, b=1:5)
DT[,.(.I, .N, GRP = .GRP), b][, head(.SD,1L), GRP]
stopifnot(
    identical(.DTQ$process()$query, c("[j = .(.I, .N, GRP = .GRP), by = b]","[j = head(.SD, 1L), by = GRP]")), # usage of .I, .N, .GRP, .SD
    identical(.DTQ$process()$src, c("DT","DT")) # chain select and aggr on single DT variable, src test
)

dtl(purge = TRUE)
DT <- data.table(a = 1:10, b = letters[1:5])
DT[keyby=b,,.(a=sum(a))]
stopifnot(
    identical(.DTQ$process()$query[[1L]], "[j = .(a = sum(a)), keyby = b]"), # reordered input to `[`
    identical(.DTQ$process()$src, "DT") # keyby on single DT variable, src test
)

dtl(purge = TRUE)
DT <- data.table(a = 1:10, b = letters[1:5])
DT[,.(a,b)][,.(a,b)]
options("dtq.apply.depth"=1L)
stopifnot(
    tryCatch(.DTQ$process(), warning=function(w) w$message) %like% "dtq depth recursive call limit exceeded" # dtq.apply.depth option
)
options("dtq.apply.depth"=20L)

dtl(purge=TRUE)
DT1 <- data.table(a=1:3, b=letters[1:3], key="a")
DT2 <- data.table(a=1:3, z=rnorm(3))
DT1[DT2[,.(z=sum(z)),,a]]
stopifnot(identical(.DTQ$process()$query, c("[j = .(z = sum(z)), keyby = a]","[i = DT2[, .(z = sum(z)), , a]]"))) # deparse `i` while join, test for #3

dtl(purge=TRUE)
DT <- data.table(a=1:4, b=letters[1:4])
x <- 1:5
DT[x[2:3]]
stopifnot(
    identical(.DTQ$length(), 1L), # subset dt on row id from subset of integer vector, length
    identical(.DTQ$process()$query, "[i = x[2:3]]") # subset dt on row id from subset of integer vector, query
)

dtl(purge=TRUE)
DT <- data.table(a=1:4, b=letters[1:4])
X <- data.table(z = 1:5)
DT[X[2:3,z]]
stopifnot(
    identical(.DTQ$length(), 2L), # subset dt on row id from subset of data.table, length
    identical(.DTQ$process()$query, c("[i = 2:3, j = z]","[i = X[2:3, z]]")) # subset dt on row id from subset of data.table, query
)

dtl(purge=TRUE)
DT <- data.table(a=1:4, b=letters[1:4])
f <- function() 1:5
DT[f()[2:3]]
stopifnot(
    identical(.DTQ$length(), 1L), # subset dt on row id from function which returns integer vector, length
    identical(.DTQ$process()$query, "[i = f()[2:3]]") # subset dt on row id from function which returns integer vector, query
)

dtl(purge=TRUE)
DT <- data.table(a=1:4, b=letters[1:4])
f <- function() data.table(z = 1:5)
DT[f()[2:3, z]]
stopifnot(
    identical(.DTQ$length(), 2L), # subset dt on row id from function which returns data.table, length
    identical(.DTQ$process()$query, c("[i = 2:3, j = z]","[i = f()[2:3, z]]")), # subset dt on row id from function which returns data.table, query
    identical(.DTQ$process()$src, c("f()","DT")) # subset dt on row id from function which returns data.table, src
)

dtl(purge=TRUE)
DT <- function() data.table(a=1:4, b=letters[1:4])
f <- function() data.table(z = 1:5)
DT()[f()[2:3, z]]
stopifnot(
    identical(.DTQ$length(), 2L), # subset function-data.table to function-data.table, length
    identical(.DTQ$process()$query, c("[i = 2:3, j = z]", "[i = f()[2:3, z]]")), # subset function-data.table to function-data.table, query
    identical(.DTQ$process()$src, c("f()","DT()")) # subset function-data.table to function-data.table, src
)

# closes #5
dtl(purge=TRUE)
f <- function() data.table(z = 1:5)
data.table(a=1:4, b=letters[1:4])[f()[2:3, z]]
stopifnot(
    identical(.DTQ$length(), 2L), # subset data.table-function to function-data.table, length
    identical(.DTQ$process()$query, c("[i = 2:3, j = z]", "[i = f()[2:3, z]]")), # subset data.table-function to function-data.table, query
    identical(.DTQ$process()$src, c("f()","data.table(a = 1:4, b = letters[1:4])")) # subset data.table-function to function-data.table, src
)
dtl(purge=TRUE)
g <- function(seq) data.table(z = seq)
data.table(a=1:4, b=letters[1:4])[g(1:5)[2:3, z]]
stopifnot(
    identical(.DTQ$length(), 2L), # subset data.table-function to function-data.table, length
    identical(.DTQ$process()$query, c("[i = 2:3, j = z]", "[i = g(1:5)[2:3, z]]")), # subset data.table-function to function-data.table, query
    identical(.DTQ$process()$src, c("g(1:5)","data.table(a = 1:4, b = letters[1:4])")) # subset data.table-function to function-data.table, src    
)

# dtq.log timing
dtl(purge=TRUE)
DT <- data.table(a=1:10, b=1:5)
DT[,{Sys.sleep(0.16); .(a = sum(a))}]
stopifnot(.DTQ$log[[1L]]$elapsed > 0.15) # timing correct for Sys.sleep case

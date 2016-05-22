# dtq: data.table query [![Build Status](https://gitlab.com/jangorecki/dtq/badges/master/build.svg)](https://gitlab.com/jangorecki/dtq/builds)

**Current version:** [0.1.9.5](NEWS.md)  

- [x] log data.table query details
  - [x] call
  - [x] sequence
  - [x] environment
  - [x] timing
  - [x] in rows
  - [x] out rows
  - [x] source name
- [x] control query logging environments
- [x] `read.only` attribute for data.table
- [x] `dtq` class to store dtq call and metadata

## Installation

```r
stopifnot(getRversion() >= "3.2.0")
install.packages("dtq", repos=c("https://jangorecki.gitlab.io/dtq", "https://cran.rstudio.com"))
```

## Usage

See [vignette](https://jangorecki.gitlab.io/dtq/library/dtq/doc/dtq.html).  

```r
library(data.table)
library(dtq)
DT <- data.table(a = 1:10, b = letters[1:5])
LKP <- data.table(b = letters[1:5], ratio = rnorm(5), key = "b")
DT2 <- DT[, .(a = sum(a)), b
          ][a > median(a), .(b, a, adj_a = a * 1.1)]
LKP[DT2, .(b, a, adj2_a = adj_a * ratio)]
dtl()
```

Control logging by global options:

- `dtq.log.exclude` character, exclude queries from provided packages
- `dtq.log.include` character, log queries only from provided packages

Unless `dtq.log.include` option is set the logging is active for all data.table queries excluding calls from `data.table` and `dtq` packages.  
Otherwise only queries from provided packages will be logged, still excluding packages from `dtq.log.exclude` option.  
While using *include* option character `R_GlobalEnv` can be provided to log also calls from global env.  

## License

GPL-3  

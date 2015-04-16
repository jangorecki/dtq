# data.table query [![Build Status](https://travis-ci.org/jangorecki/dtq.svg?branch=master)](https://travis-ci.org/jangorecki/dtq)

**Current version:** [0.1.8](NEWS.md)  

- [x] log data.table query details
  - [x] call
  - [x] sequence
  - [x] environment
  - [x] timing
  - [x] in rows
  - [ ] out rows (experimental, soon)
- [x] `read.only` attribute for data.table

## Installation

```r
library(devtools)
install_github("jangorecki/dtq")
```

## Usage

See [vignette](https://rawgit.com/jangorecki/9153d1e81195d10d7e5c/raw/dcbb5b5713186968eb024175d4a255b653228f9a/dtq.html).  

```r
library(dtq)
DT <- data.table(a = 1:10, b = letters[1:5])
LKP <- data.table(b = letters[1:5], ratio = rnorm(5), key = "b")
DT2 <- DT[, .(a = sum(a)), b
          ][a > median(a), .(b, a, adj_a = a * 1.1)]
LKP[DT2, .(b, a, adj2_a = adj_a * ratio)]
dtl()
dtl(chain=TRUE)
```

## License

GPL-3  

## Contact

`J.Gorecki@wit.edu.pl`

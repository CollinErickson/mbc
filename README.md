
<!-- README.md is generated from README.Rmd. Please edit that file -->
mbc
===

The goal of mbc is to make comparisons between different code expressions. It builds on the R package microbenchmark, adding a comparison of the output provided by each expression in addition to the comparison of run time as given by microbenchmark.

Installation
------------

You can install mbc from github with:

``` r
# install.packages("devtools")
devtools::install_github("CollinErickson/mbc")
```

Example
-------

This is a basic example which shows you how to solve a common problem. It creates random samples of 100 exponential data points and finds the mean or median. The results show that the mean is 4 times faster (since it doesn't have to sort the values) and that the mean is around 1 while the median is around 0.71.

``` r
## basic example code
mbc::mbc(mean(rexp(100)), median(rexp(100)))
#> Unit: microseconds
#>               expr    min     lq     mean median     uq     max neval cld
#>    mean(rexp(100))  9.503  9.883 12.41081 10.263 10.644 211.725   100  a 
#>  median(rexp(100)) 37.251 38.012 40.28096 38.772 39.152 181.315   100   b
#> 
#> Output summary
#>                expr       min        lq      mean    median        uq
#> 1   mean(rexp(100)) 0.7628441 0.9280183 0.9958466 0.9833868 1.0626128
#> 2 median(rexp(100)) 0.4698297 0.6108430 0.6847258 0.6702297 0.7651643
#>         max neval
#> 1 1.2770045   100
#> 2 0.9357437   100
```

This package contains a collection of utility functions.

C++ utility functions (making use of the Rcpp API) are provided, but is done
in a way that the `raim` package does note depend directly on Rcpp or require
compilation. This was done to support use in environments without sufficient
compilers. Function headers and implementations are exported in `inst/include`.
Some examples showing how to use these functions in external code are given in
the `inst/examples` folder. All R functions provided in the `raim` package are
implemented in pure R and do not require compilation.

For more information on exposing C++/Rcpp interfaces from an R package, see the
following [post](https://stackoverflow.com/questions/58999192/is-it-possible-to-share-c-classes-between-r-packages) on Stack Overflow.

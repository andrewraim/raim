This package contains a collection of utility functions.

- Functions for printing log messages and formatted output (`printf`).
- log-sum-exp functions for addition and subtraction on the log-scale.
- Distribution functions for Gumbel, inverse gamma, multivariate normal, and
  others which are not quite standard.
- A function to generate draws from a given discrete distribution using the
  [Gumbel trick](https://francisbach.com/the-gumbel-trick).
- Functions to support maximum likelihood estimation based on `optim`.
- A Metropolis-Hastings sampler and support functions.
- Commonly used transformations and their inverses such as multinomial logit,
  polar, and shift-scale.
- A function to compute
  [randomized quantile residuals](<https://doi.org/10.1080/10618600.1996.10474708>),
  which are useful to assess fit in generalized linear models.
- Functions to facilitate common vector operations, constructing a vector of
  `na` values and a "not in" operator which indicates whether an element is not
  in a given set.

The contents of this package are subject to evolve over time.

Several C++ utility functions are provided using the Rcpp API. However, the
package purposely does not export R interfaces to these functions. This ensures
that the package can be used in environments with a missing or obsolete C++
development environment. C++ functions - both interfaces and implementations -
are exported in `inst/include`. Some examples showing how to use these
functions in external code are given in the `inst/examples` folder.

For more information on exposing C++/Rcpp interfaces from an R package, see the
following [post](https://stackoverflow.com/questions/58999192/is-it-possible-to-share-c-classes-between-r-packages) on Stack Overflow.


# PUMS inflation adjustment to a year other than the survey year

Applies PCE deflator to yield real values in specified dollar-year terms
Helpful when comparing between separate surveys Requires \[St. Louis
Federal Reserve (FRED) API
key\](http://sboysel.github.io/fredr/articles/fredr.html#authentication)

## Usage

``` r
real_dollars(so, refyear)
```

## Arguments

- so:

  srvyr object returned by get_psrc_pums

- refyear:

  dollar year in which value should be returned be returned

## Author

Michael Jensen

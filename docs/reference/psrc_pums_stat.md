# Generic call for PUMS summary statistics

Given specific form by related
[`pums_stat`](https://psrc.github.io/psrccensus/reference/pums_stat.md)
functions.

## Usage

``` r
psrc_pums_stat(so, stat_type, stat_var, group_vars, incl_na = TRUE, rr = FALSE)
```

## Arguments

- so:

  The srvyr object returned by
  [`get_psrc_pums`](https://psrc.github.io/psrccensus/reference/get_psrc_pums.md)

- stat_type:

  Desired survey statistic

- stat_var:

  The numeric variable to summarize

- group_vars:

  Factor variable/s for grouping, as an UPPERCASE string element or list

- incl_na:

  option to remove NA from group_vars (if FALSE, the total may not
  reflect the full dataset)

- rr:

  optional relative reliability column, i.e. coefficient of variation as
  category levels (breakpoints: .15/.3./.5 -\>
  good/fair/weak/unreliable)

## Value

A summary tibble, including variable names, summary statistic and margin
of error

## Author

Michael Jensen

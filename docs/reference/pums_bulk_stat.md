# PUMS bulk summary statistics

Generate a statistic separately for a list of grouping variables List
input items can be multiple, i.e. character vector

## Usage

``` r
pums_bulk_stat(
  so,
  stat_type,
  stat_var = NULL,
  group_var_list,
  incl_na = TRUE,
  rr = FALSE
)
```

## Arguments

- so:

  The srvyr object returned by
  [`get_psrc_pums`](https://psrc.github.io/psrccensus/reference/get_psrc_pums.md)

- stat_type:

  Desired survey statistic

- stat_var:

  The numeric variable to summarize

- group_var_list:

  List with each list item a grouping variable or set of grouping
  variables

- incl_na:

  option to remove NA from group_vars (if FALSE, the total may not
  reflect the full dataset)

- rr:

  optional relative reliability column, i.e. coefficient of variation as
  category levels (breakpoints: .15/.3./.5 -\>
  good/fair/weak/unreliable)

## Value

A table with the variable names and labels, summary statistic and margin
of error

## Author

Michael Jensen

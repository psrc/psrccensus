# PUMS summary statistics

Separate function for total, count, median, mean

## Usage

``` r
psrc_pums_count(
  so,
  stat_var = NULL,
  group_vars = NULL,
  incl_na = TRUE,
  rr = FALSE
)

psrc_pums_sum(so, stat_var, group_vars = NULL, incl_na = TRUE, rr = FALSE)

psrc_pums_median(so, stat_var, group_vars = NULL, incl_na = TRUE, rr = FALSE)

psrc_pums_mean(so, stat_var, group_vars = NULL, incl_na = TRUE, rr = FALSE)

psrc_pums_summary(so, stat_var, group_vars = NULL, incl_na = TRUE, rr = FALSE)
```

## Arguments

- so:

  The srvyr object returned by
  [`get_psrc_pums`](https://psrc.github.io/psrccensus/reference/get_psrc_pums.md)

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

A table with the variable names and labels, summary statistic and margin
of error

## Author

Michael Jensen

## Examples

``` r
if (FALSE) { # \dontrun{
library(magrittr)
so <- get_psrc_pums(1, 2019, "h", c("HINCP", "TEN"))
rs <- psrc_pums_median(so, "HINCP", "TEN")
} # }
```

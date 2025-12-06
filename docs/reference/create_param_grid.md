# Generate parameter list for all combinations

Helper function for
[`get_acs_recs`](https://psrc.github.io/psrccensus/reference/get_acs_recs.md);
creates a list of parameter combinations for all tables and years

## Usage

``` r
create_param_grid(items, years, params)
```

## Arguments

- items:

  Vector of table or data item names

- years:

  Vector of years

- params:

  Base parameters to include with each combination

## Value

List of parameter combinations

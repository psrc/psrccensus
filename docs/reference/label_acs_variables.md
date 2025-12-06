# Label ACS variables

Helper function for
[`get_acs_single`](https://psrc.github.io/psrccensus/reference/get_acs_single.md)
to provide variable labels and concept–i.e. topic–alongside codes

## Usage

``` r
label_acs_variables(dt, table, year, acs.type)
```

## Arguments

- dt:

  data.table with Census API result

- table:

  Census table code

- year:

  the year–or last year–of the ACS survey

- acs.type:

  either acs1 or acs5

## Value

dataframe with labels appended

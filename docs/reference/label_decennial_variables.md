# Label Decennial variables

Helper function for
[`get_decennial_single`](https://psrc.github.io/psrccensus/reference/get_decennial_single.md)
to provide variable labels and concept alongside codes

## Usage

``` r
label_decennial_variables(dt, year, sumfile)
```

## Arguments

- dt:

  data.table with Census API result

- year:

  the year of the Decennial Census survey

- sumfile:

  the summary file type (e.g., 'sf1', 'dp')

## Value

dataframe with labels appended

# PUMS ftp go-fer

Download & filter data from the Census Bureau Microdata ftp server
Helper to the
[`get_psrc_pums`](https://psrc.github.io/psrccensus/reference/get_psrc_pums.md)
function

## Usage

``` r
pums_ftp_gofer(span, dyear, level, vars, dir = NULL)
```

## Arguments

- span:

  Either 1 for acs1 or 5 for acs5

- dyear:

  The data year

- level:

  Either "p" or "h", for "persons" or "households" respectively

- vars:

  PUMS variable/s as an UPPERCASE string element or list

- dir:

  Directory for .gz file, if already downloaded. Default NULL uses the
  Census ftp.

## Value

data.table with all requested variables, sample & replication weights,
and if needed, inflation adjustments

## Author

Michael Jensen

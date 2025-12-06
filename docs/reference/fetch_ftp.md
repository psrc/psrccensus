# Fetch FTP

Helper to the
[`pums_ftp_gofer`](https://psrc.github.io/psrccensus/reference/pums_ftp_gofer.md)
function This is used on both the person and household table to prep
them for join

## Usage

``` r
fetch_ftp(span, dyear, level)
```

## Arguments

- span:

  Either 1 for acs1 or 5 for acs5

- dyear:

  The data year

- level:

  Either "p" or "h", for "persons" or "households" respectively

## Value

data.table

## Author

Michael Jensen

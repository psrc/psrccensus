# Retrieve and assemble PUMS data

The primary PUMS function

## Usage

``` r
get_psrc_pums(span, dyear, level, vars, dir = NULL, labels = TRUE)
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

- labels:

  Default TRUE, recodes varible values to the corresponding label

## Value

A srvyr object with appropriate sampling weight and replication weights

## Author

Michael Jensen

## Examples

``` r
if (FALSE) { # \dontrun{
get_psrc_pums(span=1, dyear=2019, level="p", vars=c("AGEP","SEX"))} # }
```

# Get Data

## Get ACS data

Suppose you wish to tabulate ACS one-year 2019 data for estimates of
total people by race and ethnicity, as provided in table B03002 by PSRC
counties. You would use the following function call:

``` r
x <- get_acs_recs(geography = 'county',
                  table.names = 'B03002',
                  years = 2019,
                  acs.type = 'acs1')

head(x)
```

    ##    GEOID   variable             name      state estimate moe census_geography
    ## 1  53033 B03002_001      King County Washington  2252782   0           County
    ## 2  53035 B03002_001    Kitsap County Washington   271473   0           County
    ## 3  53053 B03002_001    Pierce County Washington   904980   0           County
    ## 4  53061 B03002_001 Snohomish County Washington   822083   0           County
    ## 5 REGION B03002_001           Region Washington  4251318   0           Region
    ## 6  53033 B03002_002      King County Washington  2030140   0           County
    ##                                       label                           concept
    ## 1                          Estimate!!Total: HISPANIC OR LATINO ORIGIN BY RACE
    ## 2                          Estimate!!Total: HISPANIC OR LATINO ORIGIN BY RACE
    ## 3                          Estimate!!Total: HISPANIC OR LATINO ORIGIN BY RACE
    ## 4                          Estimate!!Total: HISPANIC OR LATINO ORIGIN BY RACE
    ## 5                          Estimate!!Total: HISPANIC OR LATINO ORIGIN BY RACE
    ## 6 Estimate!!Total:!!Not Hispanic or Latino: HISPANIC OR LATINO ORIGIN BY RACE
    ##   acs_type year se cv reliability
    ## 1     acs1 2019  0  0        good
    ## 2     acs1 2019  0  0        good
    ## 3     acs1 2019  0  0        good
    ## 4     acs1 2019  0  0        good
    ## 5     acs1 2019  0  0        good
    ## 6     acs1 2019  0  0        good

### Default and custom arguments

By default, without specifying any counties, the jurisdictions returned
will be King, Kitsap, Pierce, and Snohomish Counties. Use
`?get_acs_recs()` for other default values implemented in this function.

To retrieve non-PSRC counties or a subset of the default counties, use
the `counties` argument and provide a vector of counties
(e.g. `counties = c("King", "Thurston")`). Do not use the `fips`
argument as that is reserved for MSA or place geographies.

## Get Census data

The
[`get_decennial_recs()`](https://psrc.github.io/psrccensus/reference/get_decennial_recs.md)
to generate Decennial Census tables operates similarly to the
[`get_acs_recs()`](https://psrc.github.io/psrccensus/reference/get_acs_recs.md).
If you wanted to retrieve housing units and total population by MSA, you
would call the following:

``` r
y <- get_decennial_recs(geography = 'msa',
                        table_codes = c("H001", "P001"),
                        years = c(2010),
                        fips = c('42660', "28420"))

head(y)
```

    ##   GEOID                                    NAME variable   value year label
    ## 1 28420 Kennewick-Pasco-Richland, WA Metro Area  H001001   93041 2010 Total
    ## 2 42660  Seattle-Tacoma-Bellevue, WA Metro Area  H001001 1463295 2010 Total
    ## 3 28420 Kennewick-Pasco-Richland, WA Metro Area  P001001  253340 2010 Total
    ## 4 42660  Seattle-Tacoma-Bellevue, WA Metro Area  P001001 3439809 2010 Total
    ##            concept
    ## 1    HOUSING UNITS
    ## 2    HOUSING UNITS
    ## 3 TOTAL POPULATION
    ## 4 TOTAL POPULATION

Note: the table names are padded with 0s, so you call “H001” as opposed
to “H1” as you would in Elmer. Only SF1 tables are currently
implemented.

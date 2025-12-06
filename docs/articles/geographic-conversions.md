# Census estimates for non-census geographies

Every Census estimate is linked to a geography. However, not every
geography is linked to a Census estimate.

Since the Census Bureau cannot publish data at every conceivable scale,
data users often approximate a custom boundary by choosing a set of
disaggregate Census geographies (e.g. block, block group, or tract) that
closely align with it. This approach is very often adequate to the
purpose.

### Geographic conversion via a granular proxy variable

For more geographic precision, we in effect disaggregate the estimate to
a granular scale–specifically, parcel–by assuming the estimate is in
direct proportion either to a variable reported at that scale (housing
units), or to one of several population measures we can apportion to
that scale (e.g. OFM total population).

With this in mind, PSRC has developed demographic ratios to convert
estimates from Census geographies to its own planning geographies. Each
planning geography has its own
[function](https://psrc.github.io/psrccensus/reference/census_to_psrcgeo.md):

- Regional Growth Strategy classes -
  [`census_to_rgs()`](https://psrc.github.io/psrccensus/reference/census_to_psrcgeo.md)
- Regional Growth Centers -
  [`census_to_rgc()`](https://psrc.github.io/psrccensus/reference/census_to_psrcgeo.md)
- Manufacturing-Industrial Centers -
  [`census_to_mic()`](https://psrc.github.io/psrccensus/reference/census_to_psrcgeo.md)
- Traffic Analysis Zones -
  [`census_to_taz()`](https://psrc.github.io/psrccensus/reference/census_to_psrcgeo.md)
- High Capacity Transit Station areas -
  [`census_to_hct()`](https://psrc.github.io/psrccensus/reference/census_to_psrcgeo.md)
- 2020 Jurisdictional boundaries -
  [`census_to_juris()`](https://psrc.github.io/psrccensus/reference/census_to_psrcgeo.md)

As long as you’re using the default proxy variable (total population),
you only need a single argument: the name of the dataframe (as returned
by
[`get_acs_recs()`](https://psrc.github.io/psrccensus/reference/get_acs_recs.md)
or
[`get_decennial_recs()`](https://psrc.github.io/psrccensus/reference/get_decennial_recs.md))
you wish to convert.

``` r
library(psrccensus)
library(magrittr)

x <- get_acs_recs(geography = 'block group',
                  table.names = 'B03002',
                  years = 2021,
                  acs.type = 'acs5') %>% 
  dplyr::mutate(label=stringr::str_replace_all(label,"(^Estimate!!|Total:!!)",""))

x %>% dplyr::filter(variable=="B03002_012") %>% .[,c(1,5:8)] %>% head()
#>          GEOID      county      state estimate moe
#> 1 530330001011 King County Washington       28  33
#> 2 530330001012 King County Washington      188 176
#> 3 530330001013 King County Washington       48  50
#> 4 530330001021 King County Washington        0  13
#> 5 530330001022 King County Washington      181 206
#> 6 530330001023 King County Washington      110 151

rgc_race <- census_to_rgc(x)

rgc_race %>% dplyr::filter(variable=="B03002_012") %>% .[,c(1,3,7:8)] %>% head()
#>                      planning_geog      state  year estimate
#>                             <char>     <char> <num>    <num>
#> 1:   not in regional growth center Washington  2021   415884
#> 2:               Seattle Northgate Washington  2021      719
#> 3:    Seattle University Community Washington  2021     1921
#> 4:                  Seattle Uptown Washington  2021      649
#> 5: Seattle First Hill/Capitol Hill Washington  2021     3318
#> 6:        Seattle South Lake Union Washington  2021      380
```

### Selecting the correct proxy

The key assumption behind this method is the direct relationship between
the granular proxy variable and the Census variable of interest: the
stronger this relationship, the more defensible the result. Before
splitting a geography, consider which of these metrics is most relevant
to your Census estimate of interest:

- **`total_pop`** –i.e. total population (the default)
- **`household_pop`**
- **`group_quarters_pop`**
- **`housing_units`**
- **`occupied_housing_units`**

You can specify these using the `wgt` argument:

``` r

y <- get_acs_recs(geography = 'tract',
                  table.names = 'B26001',
                  years = 2019,
                  acs.type = 'acs5') %>% 
  dplyr::mutate(label=stringr::str_replace_all(label,"(^Estimate!!|Total:!!)",""))

y %>% .[,c(1,5:8)] %>% head()
#>         GEOID      state estimate moe census_geography
#> 1 53033000100 Washington       38  24            Tract
#> 2 53033000200 Washington       87 150            Tract
#> 3 53033000300 Washington       16   5            Tract
#> 4 53033000401 Washington       89  32            Tract
#> 5 53033000402 Washington      239 151            Tract
#> 6 53033000500 Washington       21  20            Tract

rgc_gq_age <- census_to_rgs(y, wgt="group_quarters_pop")

rgc_gq_age %>% .[,c(1,3,4,7,8)]
#>    planning_geog      state  label  year estimate
#>           <char>     <char> <char> <num>    <num>
#> 1:         Rural Washington Total:  2019     4848
#> 2:         Metro Washington Total:  2019    38944
#> 3:           HCT Washington Total:  2019     8117
#> 4:          Core Washington Total:  2019    11580
#> 5:            UU Washington Total:  2019     7224
#> 6:   CitiesTowns Washington Total:  2019     4945
```

If your dataframe combines tables that would most appropriately apply
two different proxy metrics–for example, one linked to total population,
and another linked to housing units–you might divide the dataframe (or
alter your code to retrieve separate tables originally) before applying
the geographic conversion.

### Further geographies

The set of planning geographies is not strictly limited to the five
listed above; with approval, the set can be expanded (see the
[documentation](http://aws-linux/mediawiki/index.php/Geography_Split_Tables_in_Elmer) -
PSRC VPN required to view).

To convert census estimates to custom geographies that don’t have stored
splits, use the
[`census_to_customgeo()`](https://psrc.github.io/psrccensus/reference/census_to_customgeo.md)
function. This performs an analogous conversion using the same
parcel-level proxy variables, but requires two additional arguments: an
`sf` package object (i.e. your custom geography/geometry), and the
variable name in that file that labels the geography. This function is
much slower than the PSRC planning geography functions listed above
because it must get parcel-level data from Elmer and create the custom
splits rather than just read them. Depending on your download speed and
the complexity of the geography, it’ll take a minute or two to run
(still, not shabby for this level of detail).

### Error margins & limitations

Currently, split-derived calculations are limited to estimates only;
medians cannot be determined via this method. Analysts can calculate
shares by converting estimates of both the numerator and denominator to
the appropriate geography.

Although it may be harder to assume Margins of Error (MOE) are directly
proportional to the split weight, the assumption is applied identically
to both estimate and MOE, which seemed the best choice of the available
options.

# Decennial Census Estimates

Generate decennial estimates for multiple tables by tracts, counties,
MSAs, or places.

## Usage

``` r
get_decennial_recs(
  geography,
  counties = c("King", "Kitsap", "Pierce", "Snohomish"),
  sumfile = "sf1",
  years,
  variables = NULL,
  table_codes = NULL,
  fips = NULL,
  state = "WA"
)
```

## Arguments

- geography:

  A character string as either 'tract', 'county', 'block group', 'msa',
  or 'place'.

- counties:

  A character string or vector of counties. Defaults to PSRC counties.

- sumfile:

  A character string for which summary file to use such as "sf1" or "dp"

- years:

  Numeric or a vector of numeric years. A decennial year or years equal
  or greater than 2000.

- variables:

  A character string or vector of Census variables

- table_codes:

  A character string or vector of Census table codes, the table code
  will be padded with 0s such as "H001", as opposed to "H1"

- fips:

  Character. Single code or vector of either MSA or place fips codes.

- state:

  A character string state abbreviation. Defaults to 'WA'.

## Value

a tibble of decennial estimates by selected geography for selected table
codes. Includes variable names.

## Author

Christy Lam

## Examples

``` r
tbl_names <- paste0('PCT020', LETTERS[1:6])
get_decennial_recs(geography = 'county', table_codes = tbl_names, years = 2010)
#> Getting data from the 2010 decennial Census
#> Loading SF1 variables for 2010 from table PCT020A. To cache this dataset for faster access to Census tables in the future, run this function with `cache_table = TRUE`. You only need to do this once per Census dataset.
#> Using FIPS code '53' for state 'WA'
#> Using FIPS code '033' for 'King County'
#> Using FIPS code '035' for 'Kitsap County'
#> Using FIPS code '053' for 'Pierce County'
#> Using FIPS code '061' for 'Snohomish County'
#> Using Census Summary File 1
#> Getting data from the 2010 decennial Census
#> Loading SF1 variables for 2010 from table PCT020B. To cache this dataset for faster access to Census tables in the future, run this function with `cache_table = TRUE`. You only need to do this once per Census dataset.
#> Using FIPS code '53' for state 'WA'
#> Using FIPS code '033' for 'King County'
#> Using FIPS code '035' for 'Kitsap County'
#> Using FIPS code '053' for 'Pierce County'
#> Using FIPS code '061' for 'Snohomish County'
#> Using Census Summary File 1
#> Getting data from the 2010 decennial Census
#> Loading SF1 variables for 2010 from table PCT020C. To cache this dataset for faster access to Census tables in the future, run this function with `cache_table = TRUE`. You only need to do this once per Census dataset.
#> Using FIPS code '53' for state 'WA'
#> Using FIPS code '033' for 'King County'
#> Using FIPS code '035' for 'Kitsap County'
#> Using FIPS code '053' for 'Pierce County'
#> Using FIPS code '061' for 'Snohomish County'
#> Using Census Summary File 1
#> Getting data from the 2010 decennial Census
#> Loading SF1 variables for 2010 from table PCT020D. To cache this dataset for faster access to Census tables in the future, run this function with `cache_table = TRUE`. You only need to do this once per Census dataset.
#> Using FIPS code '53' for state 'WA'
#> Using FIPS code '033' for 'King County'
#> Using FIPS code '035' for 'Kitsap County'
#> Using FIPS code '053' for 'Pierce County'
#> Using FIPS code '061' for 'Snohomish County'
#> Using Census Summary File 1
#> Getting data from the 2010 decennial Census
#> Loading SF1 variables for 2010 from table PCT020E. To cache this dataset for faster access to Census tables in the future, run this function with `cache_table = TRUE`. You only need to do this once per Census dataset.
#> Using FIPS code '53' for state 'WA'
#> Using FIPS code '033' for 'King County'
#> Using FIPS code '035' for 'Kitsap County'
#> Using FIPS code '053' for 'Pierce County'
#> Using FIPS code '061' for 'Snohomish County'
#> Using Census Summary File 1
#> Getting data from the 2010 decennial Census
#> Loading SF1 variables for 2010 from table PCT020F. To cache this dataset for faster access to Census tables in the future, run this function with `cache_table = TRUE`. You only need to do this once per Census dataset.
#> Using FIPS code '53' for state 'WA'
#> Using FIPS code '033' for 'King County'
#> Using FIPS code '035' for 'Kitsap County'
#> Using FIPS code '053' for 'Pierce County'
#> Using FIPS code '061' for 'Snohomish County'
#> Using Census Summary File 1
#>      GEOID                         NAME   variable value county year
#> 1    53033      King County, Washington PCT020A001 24629  53033 2010
#> 2    53035    Kitsap County, Washington PCT020A001  6784  53035 2010
#> 3    53053    Pierce County, Washington PCT020A001 13393  53053 2010
#> 4    53061 Snohomish County, Washington PCT020A001  8080  53061 2010
#> 5   REGION                       Region PCT020A001 52886   <NA> 2010
#> 6    53033      King County, Washington PCT020A002  7394  53033 2010
#> 7    53035    Kitsap County, Washington PCT020A002  1658  53035 2010
#> 8    53053    Pierce County, Washington PCT020A002  6659  53053 2010
#> 9    53061 Snohomish County, Washington PCT020A002  4636  53061 2010
#> 10  REGION                       Region PCT020A002 20347   <NA> 2010
#> 11   53033      King County, Washington PCT020A003  1977  53033 2010
#> 12   53035    Kitsap County, Washington PCT020A003   303  53035 2010
#> 13   53053    Pierce County, Washington PCT020A003  3667  53053 2010
#> 14   53061 Snohomish County, Washington PCT020A003  2706  53061 2010
#> 15  REGION                       Region PCT020A003  8653   <NA> 2010
#> 16   53033      King County, Washington PCT020A004   392  53033 2010
#> 17   53035    Kitsap County, Washington PCT020A004     0  53035 2010
#> 18   53053    Pierce County, Washington PCT020A004   886  53053 2010
#> 19   53061 Snohomish County, Washington PCT020A004     0  53061 2010
#> 20  REGION                       Region PCT020A004  1278   <NA> 2010
#> 21   53033      King County, Washington PCT020A005     0  53033 2010
#> 22   53035    Kitsap County, Washington PCT020A005     0  53035 2010
#> 23   53053    Pierce County, Washington PCT020A005     0  53053 2010
#> 24   53061 Snohomish County, Washington PCT020A005     0  53061 2010
#> 25  REGION                       Region PCT020A005     0   <NA> 2010
#> 26   53033      King County, Washington PCT020A006     0  53033 2010
#> 27   53035    Kitsap County, Washington PCT020A006     0  53035 2010
#> 28   53053    Pierce County, Washington PCT020A006  1947  53053 2010
#> 29   53061 Snohomish County, Washington PCT020A006  1752  53061 2010
#> 30  REGION                       Region PCT020A006  3699   <NA> 2010
#> 31   53033      King County, Washington PCT020A007  1287  53033 2010
#> 32   53035    Kitsap County, Washington PCT020A007   260  53035 2010
#> 33   53053    Pierce County, Washington PCT020A007   745  53053 2010
#> 34   53061 Snohomish County, Washington PCT020A007   949  53061 2010
#> 35  REGION                       Region PCT020A007  3241   <NA> 2010
#> 36   53033      King County, Washington PCT020A008   298  53033 2010
#> 37   53035    Kitsap County, Washington PCT020A008    43  53035 2010
#> 38   53053    Pierce County, Washington PCT020A008    89  53053 2010
#> 39   53061 Snohomish County, Washington PCT020A008     5  53061 2010
#> 40  REGION                       Region PCT020A008   435   <NA> 2010
#> 41   53033      King County, Washington PCT020A009     0  53033 2010
#> 42   53035    Kitsap County, Washington PCT020A009     0  53035 2010
#> 43   53053    Pierce County, Washington PCT020A009     0  53053 2010
#> 44   53061 Snohomish County, Washington PCT020A009     0  53061 2010
#> 45  REGION                       Region PCT020A009     0   <NA> 2010
#> 46   53033      King County, Washington PCT020A010   251  53033 2010
#> 47   53035    Kitsap County, Washington PCT020A010    28  53035 2010
#> 48   53053    Pierce County, Washington PCT020A010    30  53053 2010
#> 49   53061 Snohomish County, Washington PCT020A010    50  53061 2010
#> 50  REGION                       Region PCT020A010   359   <NA> 2010
#> 51   53033      King County, Washington PCT020A011    29  53033 2010
#> 52   53035    Kitsap County, Washington PCT020A011     0  53035 2010
#> 53   53053    Pierce County, Washington PCT020A011     4  53053 2010
#> 54   53061 Snohomish County, Washington PCT020A011    12  53061 2010
#> 55  REGION                       Region PCT020A011    45   <NA> 2010
#> 56   53033      King County, Washington PCT020A012   104  53033 2010
#> 57   53035    Kitsap County, Washington PCT020A012     0  53035 2010
#> 58   53053    Pierce County, Washington PCT020A012     9  53053 2010
#> 59   53061 Snohomish County, Washington PCT020A012     4  53061 2010
#> 60  REGION                       Region PCT020A012   117   <NA> 2010
#> 61   53033      King County, Washington PCT020A013   118  53033 2010
#> 62   53035    Kitsap County, Washington PCT020A013    28  53035 2010
#> 63   53053    Pierce County, Washington PCT020A013    17  53053 2010
#> 64   53061 Snohomish County, Washington PCT020A013    34  53061 2010
#> 65  REGION                       Region PCT020A013   197   <NA> 2010
#> 66   53033      King County, Washington PCT020A014  4983  53033 2010
#> 67   53035    Kitsap County, Washington PCT020A014  1257  53035 2010
#> 68   53053    Pierce County, Washington PCT020A014  2361  53053 2010
#> 69   53061 Snohomish County, Washington PCT020A014  1800  53061 2010
#> 70  REGION                       Region PCT020A014 10401   <NA> 2010
#> 71   53033      King County, Washington PCT020A015   183  53033 2010
#> 72   53035    Kitsap County, Washington PCT020A015    70  53035 2010
#> 73   53053    Pierce County, Washington PCT020A015   601  53053 2010
#> 74   53061 Snohomish County, Washington PCT020A015    80  53061 2010
#> 75  REGION                       Region PCT020A015   934   <NA> 2010
#> 76   53033      King County, Washington PCT020A016    70  53033 2010
#> 77   53035    Kitsap County, Washington PCT020A016    13  53035 2010
#> 78   53053    Pierce County, Washington PCT020A016   560  53053 2010
#> 79   53061 Snohomish County, Washington PCT020A016    29  53061 2010
#> 80  REGION                       Region PCT020A016   672   <NA> 2010
#> 81   53033      King County, Washington PCT020A017     5  53033 2010
#> 82   53035    Kitsap County, Washington PCT020A017     0  53035 2010
#> 83   53053    Pierce County, Washington PCT020A017    35  53053 2010
#> 84   53061 Snohomish County, Washington PCT020A017    42  53061 2010
#> 85  REGION                       Region PCT020A017    82   <NA> 2010
#> 86   53033      King County, Washington PCT020A018   108  53033 2010
#> 87   53035    Kitsap County, Washington PCT020A018     0  53035 2010
#> 88   53053    Pierce County, Washington PCT020A018     6  53053 2010
#> 89   53061 Snohomish County, Washington PCT020A018     9  53061 2010
#> 90  REGION                       Region PCT020A018   123   <NA> 2010
#> 91   53033      King County, Washington PCT020A019     0  53033 2010
#> 92   53035    Kitsap County, Washington PCT020A019    57  53035 2010
#> 93   53053    Pierce County, Washington PCT020A019     0  53053 2010
#> 94   53061 Snohomish County, Washington PCT020A019     0  53061 2010
#> 95  REGION                       Region PCT020A019    57   <NA> 2010
#> 96   53033      King County, Washington PCT020A020     0  53033 2010
#> 97   53035    Kitsap County, Washington PCT020A020     0  53035 2010
#> 98   53053    Pierce County, Washington PCT020A020     0  53053 2010
#> 99   53061 Snohomish County, Washington PCT020A020     0  53061 2010
#> 100 REGION                       Region PCT020A020     0   <NA> 2010
#> 101  53033      King County, Washington PCT020A021 17235  53033 2010
#> 102  53035    Kitsap County, Washington PCT020A021  5126  53035 2010
#> 103  53053    Pierce County, Washington PCT020A021  6734  53053 2010
#> 104  53061 Snohomish County, Washington PCT020A021  3444  53061 2010
#> 105 REGION                       Region PCT020A021 32539   <NA> 2010
#> 106  53033      King County, Washington PCT020A022  8427  53033 2010
#> 107  53035    Kitsap County, Washington PCT020A022    24  53035 2010
#> 108  53053    Pierce County, Washington PCT020A022  2045  53053 2010
#> 109  53061 Snohomish County, Washington PCT020A022   125  53061 2010
#> 110 REGION                       Region PCT020A022 10621   <NA> 2010
#> 111  53033      King County, Washington PCT020A023   294  53033 2010
#> 112  53035    Kitsap County, Washington PCT020A023  4079  53035 2010
#> 113  53053    Pierce County, Washington PCT020A023  2382  53053 2010
#> 114  53061 Snohomish County, Washington PCT020A023   996  53061 2010
#> 115 REGION                       Region PCT020A023  7751   <NA> 2010
#> 116  53033      King County, Washington PCT020A024     0  53033 2010
#> 117  53035    Kitsap County, Washington PCT020A024  1754  53035 2010
#> 118  53053    Pierce County, Washington PCT020A024  2382  53053 2010
#> 119  53061 Snohomish County, Washington PCT020A024   507  53061 2010
#> 120 REGION                       Region PCT020A024  4643   <NA> 2010
#> 121  53033      King County, Washington PCT020A025   294  53033 2010
#> 122  53035    Kitsap County, Washington PCT020A025  2325  53035 2010
#> 123  53053    Pierce County, Washington PCT020A025     0  53053 2010
#> 124  53061 Snohomish County, Washington PCT020A025   489  53061 2010
#> 125 REGION                       Region PCT020A025  3108   <NA> 2010
#>                                                                                                                                                                                             label
#> 1                                                                                                                                                                                           Total
#> 2                                                                                                                                                                                           Total
#> 3                                                                                                                                                                                           Total
#> 4                                                                                                                                                                                           Total
#> 5                                                                                                                                                                                           Total
#> 6                                                                                                                            Total!!Institutionalized population (101-106, 201-203, 301, 401-405)
#> 7                                                                                                                            Total!!Institutionalized population (101-106, 201-203, 301, 401-405)
#> 8                                                                                                                            Total!!Institutionalized population (101-106, 201-203, 301, 401-405)
#> 9                                                                                                                            Total!!Institutionalized population (101-106, 201-203, 301, 401-405)
#> 10                                                                                                                           Total!!Institutionalized population (101-106, 201-203, 301, 401-405)
#> 11                                                                             Total!!Institutionalized population (101-106, 201-203, 301, 401-405)!!Correctional facilities for adults (101-106)
#> 12                                                                             Total!!Institutionalized population (101-106, 201-203, 301, 401-405)!!Correctional facilities for adults (101-106)
#> 13                                                                             Total!!Institutionalized population (101-106, 201-203, 301, 401-405)!!Correctional facilities for adults (101-106)
#> 14                                                                             Total!!Institutionalized population (101-106, 201-203, 301, 401-405)!!Correctional facilities for adults (101-106)
#> 15                                                                             Total!!Institutionalized population (101-106, 201-203, 301, 401-405)!!Correctional facilities for adults (101-106)
#> 16                                            Total!!Institutionalized population (101-106, 201-203, 301, 401-405)!!Correctional facilities for adults (101-106)!!Federal detention centers (101)
#> 17                                            Total!!Institutionalized population (101-106, 201-203, 301, 401-405)!!Correctional facilities for adults (101-106)!!Federal detention centers (101)
#> 18                                            Total!!Institutionalized population (101-106, 201-203, 301, 401-405)!!Correctional facilities for adults (101-106)!!Federal detention centers (101)
#> 19                                            Total!!Institutionalized population (101-106, 201-203, 301, 401-405)!!Correctional facilities for adults (101-106)!!Federal detention centers (101)
#> 20                                            Total!!Institutionalized population (101-106, 201-203, 301, 401-405)!!Correctional facilities for adults (101-106)!!Federal detention centers (101)
#> 21                                                      Total!!Institutionalized population (101-106, 201-203, 301, 401-405)!!Correctional facilities for adults (101-106)!!Federal prisons (102)
#> 22                                                      Total!!Institutionalized population (101-106, 201-203, 301, 401-405)!!Correctional facilities for adults (101-106)!!Federal prisons (102)
#> 23                                                      Total!!Institutionalized population (101-106, 201-203, 301, 401-405)!!Correctional facilities for adults (101-106)!!Federal prisons (102)
#> 24                                                      Total!!Institutionalized population (101-106, 201-203, 301, 401-405)!!Correctional facilities for adults (101-106)!!Federal prisons (102)
#> 25                                                      Total!!Institutionalized population (101-106, 201-203, 301, 401-405)!!Correctional facilities for adults (101-106)!!Federal prisons (102)
#> 26                                                        Total!!Institutionalized population (101-106, 201-203, 301, 401-405)!!Correctional facilities for adults (101-106)!!State prisons (103)
#> 27                                                        Total!!Institutionalized population (101-106, 201-203, 301, 401-405)!!Correctional facilities for adults (101-106)!!State prisons (103)
#> 28                                                        Total!!Institutionalized population (101-106, 201-203, 301, 401-405)!!Correctional facilities for adults (101-106)!!State prisons (103)
#> 29                                                        Total!!Institutionalized population (101-106, 201-203, 301, 401-405)!!Correctional facilities for adults (101-106)!!State prisons (103)
#> 30                                                        Total!!Institutionalized population (101-106, 201-203, 301, 401-405)!!Correctional facilities for adults (101-106)!!State prisons (103)
#> 31               Total!!Institutionalized population (101-106, 201-203, 301, 401-405)!!Correctional facilities for adults (101-106)!!Local jails and other municipal confinement facilities (104)
#> 32               Total!!Institutionalized population (101-106, 201-203, 301, 401-405)!!Correctional facilities for adults (101-106)!!Local jails and other municipal confinement facilities (104)
#> 33               Total!!Institutionalized population (101-106, 201-203, 301, 401-405)!!Correctional facilities for adults (101-106)!!Local jails and other municipal confinement facilities (104)
#> 34               Total!!Institutionalized population (101-106, 201-203, 301, 401-405)!!Correctional facilities for adults (101-106)!!Local jails and other municipal confinement facilities (104)
#> 35               Total!!Institutionalized population (101-106, 201-203, 301, 401-405)!!Correctional facilities for adults (101-106)!!Local jails and other municipal confinement facilities (104)
#> 36                                  Total!!Institutionalized population (101-106, 201-203, 301, 401-405)!!Correctional facilities for adults (101-106)!!Correctional residential facilities (105)
#> 37                                  Total!!Institutionalized population (101-106, 201-203, 301, 401-405)!!Correctional facilities for adults (101-106)!!Correctional residential facilities (105)
#> 38                                  Total!!Institutionalized population (101-106, 201-203, 301, 401-405)!!Correctional facilities for adults (101-106)!!Correctional residential facilities (105)
#> 39                                  Total!!Institutionalized population (101-106, 201-203, 301, 401-405)!!Correctional facilities for adults (101-106)!!Correctional residential facilities (105)
#> 40                                  Total!!Institutionalized population (101-106, 201-203, 301, 401-405)!!Correctional facilities for adults (101-106)!!Correctional residential facilities (105)
#> 41                             Total!!Institutionalized population (101-106, 201-203, 301, 401-405)!!Correctional facilities for adults (101-106)!!Military disciplinary barracks and jails (106)
#> 42                             Total!!Institutionalized population (101-106, 201-203, 301, 401-405)!!Correctional facilities for adults (101-106)!!Military disciplinary barracks and jails (106)
#> 43                             Total!!Institutionalized population (101-106, 201-203, 301, 401-405)!!Correctional facilities for adults (101-106)!!Military disciplinary barracks and jails (106)
#> 44                             Total!!Institutionalized population (101-106, 201-203, 301, 401-405)!!Correctional facilities for adults (101-106)!!Military disciplinary barracks and jails (106)
#> 45                             Total!!Institutionalized population (101-106, 201-203, 301, 401-405)!!Correctional facilities for adults (101-106)!!Military disciplinary barracks and jails (106)
#> 46                                                                                            Total!!Institutionalized population (101-106, 201-203, 301, 401-405)!!Juvenile facilities (201-203)
#> 47                                                                                            Total!!Institutionalized population (101-106, 201-203, 301, 401-405)!!Juvenile facilities (201-203)
#> 48                                                                                            Total!!Institutionalized population (101-106, 201-203, 301, 401-405)!!Juvenile facilities (201-203)
#> 49                                                                                            Total!!Institutionalized population (101-106, 201-203, 301, 401-405)!!Juvenile facilities (201-203)
#> 50                                                                                            Total!!Institutionalized population (101-106, 201-203, 301, 401-405)!!Juvenile facilities (201-203)
#> 51                                        Total!!Institutionalized population (101-106, 201-203, 301, 401-405)!!Juvenile facilities (201-203)!!Group homes for juveniles (non-correctional) (201)
#> 52                                        Total!!Institutionalized population (101-106, 201-203, 301, 401-405)!!Juvenile facilities (201-203)!!Group homes for juveniles (non-correctional) (201)
#> 53                                        Total!!Institutionalized population (101-106, 201-203, 301, 401-405)!!Juvenile facilities (201-203)!!Group homes for juveniles (non-correctional) (201)
#> 54                                        Total!!Institutionalized population (101-106, 201-203, 301, 401-405)!!Juvenile facilities (201-203)!!Group homes for juveniles (non-correctional) (201)
#> 55                                        Total!!Institutionalized population (101-106, 201-203, 301, 401-405)!!Juvenile facilities (201-203)!!Group homes for juveniles (non-correctional) (201)
#> 56                      Total!!Institutionalized population (101-106, 201-203, 301, 401-405)!!Juvenile facilities (201-203)!!Residential treatment centers for juveniles (non-correctional) (202)
#> 57                      Total!!Institutionalized population (101-106, 201-203, 301, 401-405)!!Juvenile facilities (201-203)!!Residential treatment centers for juveniles (non-correctional) (202)
#> 58                      Total!!Institutionalized population (101-106, 201-203, 301, 401-405)!!Juvenile facilities (201-203)!!Residential treatment centers for juveniles (non-correctional) (202)
#> 59                      Total!!Institutionalized population (101-106, 201-203, 301, 401-405)!!Juvenile facilities (201-203)!!Residential treatment centers for juveniles (non-correctional) (202)
#> 60                      Total!!Institutionalized population (101-106, 201-203, 301, 401-405)!!Juvenile facilities (201-203)!!Residential treatment centers for juveniles (non-correctional) (202)
#> 61                                      Total!!Institutionalized population (101-106, 201-203, 301, 401-405)!!Juvenile facilities (201-203)!!Correctional facilities intended for juveniles (203)
#> 62                                      Total!!Institutionalized population (101-106, 201-203, 301, 401-405)!!Juvenile facilities (201-203)!!Correctional facilities intended for juveniles (203)
#> 63                                      Total!!Institutionalized population (101-106, 201-203, 301, 401-405)!!Juvenile facilities (201-203)!!Correctional facilities intended for juveniles (203)
#> 64                                      Total!!Institutionalized population (101-106, 201-203, 301, 401-405)!!Juvenile facilities (201-203)!!Correctional facilities intended for juveniles (203)
#> 65                                      Total!!Institutionalized population (101-106, 201-203, 301, 401-405)!!Juvenile facilities (201-203)!!Correctional facilities intended for juveniles (203)
#> 66                                                                      Total!!Institutionalized population (101-106, 201-203, 301, 401-405)!!Nursing facilities/Skilled-nursing facilities (301)
#> 67                                                                      Total!!Institutionalized population (101-106, 201-203, 301, 401-405)!!Nursing facilities/Skilled-nursing facilities (301)
#> 68                                                                      Total!!Institutionalized population (101-106, 201-203, 301, 401-405)!!Nursing facilities/Skilled-nursing facilities (301)
#> 69                                                                      Total!!Institutionalized population (101-106, 201-203, 301, 401-405)!!Nursing facilities/Skilled-nursing facilities (301)
#> 70                                                                      Total!!Institutionalized population (101-106, 201-203, 301, 401-405)!!Nursing facilities/Skilled-nursing facilities (301)
#> 71                                                                                 Total!!Institutionalized population (101-106, 201-203, 301, 401-405)!!Other institutional facilities (401-405)
#> 72                                                                                 Total!!Institutionalized population (101-106, 201-203, 301, 401-405)!!Other institutional facilities (401-405)
#> 73                                                                                 Total!!Institutionalized population (101-106, 201-203, 301, 401-405)!!Other institutional facilities (401-405)
#> 74                                                                                 Total!!Institutionalized population (101-106, 201-203, 301, 401-405)!!Other institutional facilities (401-405)
#> 75                                                                                 Total!!Institutionalized population (101-106, 201-203, 301, 401-405)!!Other institutional facilities (401-405)
#> 76  Total!!Institutionalized population (101-106, 201-203, 301, 401-405)!!Other institutional facilities (401-405)!!Mental (Psychiatric) hospitals and psychiatric units in other hospitals (401)
#> 77  Total!!Institutionalized population (101-106, 201-203, 301, 401-405)!!Other institutional facilities (401-405)!!Mental (Psychiatric) hospitals and psychiatric units in other hospitals (401)
#> 78  Total!!Institutionalized population (101-106, 201-203, 301, 401-405)!!Other institutional facilities (401-405)!!Mental (Psychiatric) hospitals and psychiatric units in other hospitals (401)
#> 79  Total!!Institutionalized population (101-106, 201-203, 301, 401-405)!!Other institutional facilities (401-405)!!Mental (Psychiatric) hospitals and psychiatric units in other hospitals (401)
#> 80  Total!!Institutionalized population (101-106, 201-203, 301, 401-405)!!Other institutional facilities (401-405)!!Mental (Psychiatric) hospitals and psychiatric units in other hospitals (401)
#> 81                 Total!!Institutionalized population (101-106, 201-203, 301, 401-405)!!Other institutional facilities (401-405)!!Hospitals with patients who have no usual home elsewhere (402)
#> 82                 Total!!Institutionalized population (101-106, 201-203, 301, 401-405)!!Other institutional facilities (401-405)!!Hospitals with patients who have no usual home elsewhere (402)
#> 83                 Total!!Institutionalized population (101-106, 201-203, 301, 401-405)!!Other institutional facilities (401-405)!!Hospitals with patients who have no usual home elsewhere (402)
#> 84                 Total!!Institutionalized population (101-106, 201-203, 301, 401-405)!!Other institutional facilities (401-405)!!Hospitals with patients who have no usual home elsewhere (402)
#> 85                 Total!!Institutionalized population (101-106, 201-203, 301, 401-405)!!Other institutional facilities (401-405)!!Hospitals with patients who have no usual home elsewhere (402)
#> 86                                            Total!!Institutionalized population (101-106, 201-203, 301, 401-405)!!Other institutional facilities (401-405)!!In-patient hospice facilities (403)
#> 87                                            Total!!Institutionalized population (101-106, 201-203, 301, 401-405)!!Other institutional facilities (401-405)!!In-patient hospice facilities (403)
#> 88                                            Total!!Institutionalized population (101-106, 201-203, 301, 401-405)!!Other institutional facilities (401-405)!!In-patient hospice facilities (403)
#> 89                                            Total!!Institutionalized population (101-106, 201-203, 301, 401-405)!!Other institutional facilities (401-405)!!In-patient hospice facilities (403)
#> 90                                            Total!!Institutionalized population (101-106, 201-203, 301, 401-405)!!Other institutional facilities (401-405)!!In-patient hospice facilities (403)
#> 91                     Total!!Institutionalized population (101-106, 201-203, 301, 401-405)!!Other institutional facilities (401-405)!!Military treatment facilities with assigned patients (404)
#> 92                     Total!!Institutionalized population (101-106, 201-203, 301, 401-405)!!Other institutional facilities (401-405)!!Military treatment facilities with assigned patients (404)
#> 93                     Total!!Institutionalized population (101-106, 201-203, 301, 401-405)!!Other institutional facilities (401-405)!!Military treatment facilities with assigned patients (404)
#> 94                     Total!!Institutionalized population (101-106, 201-203, 301, 401-405)!!Other institutional facilities (401-405)!!Military treatment facilities with assigned patients (404)
#> 95                     Total!!Institutionalized population (101-106, 201-203, 301, 401-405)!!Other institutional facilities (401-405)!!Military treatment facilities with assigned patients (404)
#> 96                         Total!!Institutionalized population (101-106, 201-203, 301, 401-405)!!Other institutional facilities (401-405)!!Residential schools for people with disabilities (405)
#> 97                         Total!!Institutionalized population (101-106, 201-203, 301, 401-405)!!Other institutional facilities (401-405)!!Residential schools for people with disabilities (405)
#> 98                         Total!!Institutionalized population (101-106, 201-203, 301, 401-405)!!Other institutional facilities (401-405)!!Residential schools for people with disabilities (405)
#> 99                         Total!!Institutionalized population (101-106, 201-203, 301, 401-405)!!Other institutional facilities (401-405)!!Residential schools for people with disabilities (405)
#> 100                        Total!!Institutionalized population (101-106, 201-203, 301, 401-405)!!Other institutional facilities (401-405)!!Residential schools for people with disabilities (405)
#> 101                                                                                           Total!!Noninstitutionalized population (501, 601-602, 701-702, 704, 706, 801-802, 900-901, 903-904)
#> 102                                                                                           Total!!Noninstitutionalized population (501, 601-602, 701-702, 704, 706, 801-802, 900-901, 903-904)
#> 103                                                                                           Total!!Noninstitutionalized population (501, 601-602, 701-702, 704, 706, 801-802, 900-901, 903-904)
#> 104                                                                                           Total!!Noninstitutionalized population (501, 601-602, 701-702, 704, 706, 801-802, 900-901, 903-904)
#> 105                                                                                           Total!!Noninstitutionalized population (501, 601-602, 701-702, 704, 706, 801-802, 900-901, 903-904)
#> 106                                                 Total!!Noninstitutionalized population (501, 601-602, 701-702, 704, 706, 801-802, 900-901, 903-904)!!College/University student housing (501)
#> 107                                                 Total!!Noninstitutionalized population (501, 601-602, 701-702, 704, 706, 801-802, 900-901, 903-904)!!College/University student housing (501)
#> 108                                                 Total!!Noninstitutionalized population (501, 601-602, 701-702, 704, 706, 801-802, 900-901, 903-904)!!College/University student housing (501)
#> 109                                                 Total!!Noninstitutionalized population (501, 601-602, 701-702, 704, 706, 801-802, 900-901, 903-904)!!College/University student housing (501)
#> 110                                                 Total!!Noninstitutionalized population (501, 601-602, 701-702, 704, 706, 801-802, 900-901, 903-904)!!College/University student housing (501)
#> 111                                                              Total!!Noninstitutionalized population (501, 601-602, 701-702, 704, 706, 801-802, 900-901, 903-904)!!Military quarters (601-602)
#> 112                                                              Total!!Noninstitutionalized population (501, 601-602, 701-702, 704, 706, 801-802, 900-901, 903-904)!!Military quarters (601-602)
#> 113                                                              Total!!Noninstitutionalized population (501, 601-602, 701-702, 704, 706, 801-802, 900-901, 903-904)!!Military quarters (601-602)
#> 114                                                              Total!!Noninstitutionalized population (501, 601-602, 701-702, 704, 706, 801-802, 900-901, 903-904)!!Military quarters (601-602)
#> 115                                                              Total!!Noninstitutionalized population (501, 601-602, 701-702, 704, 706, 801-802, 900-901, 903-904)!!Military quarters (601-602)
#> 116  Total!!Noninstitutionalized population (501, 601-602, 701-702, 704, 706, 801-802, 900-901, 903-904)!!Military quarters (601-602)!!Military barracks and dormitories (non-disciplinary) (601)
#> 117  Total!!Noninstitutionalized population (501, 601-602, 701-702, 704, 706, 801-802, 900-901, 903-904)!!Military quarters (601-602)!!Military barracks and dormitories (non-disciplinary) (601)
#> 118  Total!!Noninstitutionalized population (501, 601-602, 701-702, 704, 706, 801-802, 900-901, 903-904)!!Military quarters (601-602)!!Military barracks and dormitories (non-disciplinary) (601)
#> 119  Total!!Noninstitutionalized population (501, 601-602, 701-702, 704, 706, 801-802, 900-901, 903-904)!!Military quarters (601-602)!!Military barracks and dormitories (non-disciplinary) (601)
#> 120  Total!!Noninstitutionalized population (501, 601-602, 701-702, 704, 706, 801-802, 900-901, 903-904)!!Military quarters (601-602)!!Military barracks and dormitories (non-disciplinary) (601)
#> 121                                        Total!!Noninstitutionalized population (501, 601-602, 701-702, 704, 706, 801-802, 900-901, 903-904)!!Military quarters (601-602)!!Military ships (602)
#> 122                                        Total!!Noninstitutionalized population (501, 601-602, 701-702, 704, 706, 801-802, 900-901, 903-904)!!Military quarters (601-602)!!Military ships (602)
#> 123                                        Total!!Noninstitutionalized population (501, 601-602, 701-702, 704, 706, 801-802, 900-901, 903-904)!!Military quarters (601-602)!!Military ships (602)
#> 124                                        Total!!Noninstitutionalized population (501, 601-602, 701-702, 704, 706, 801-802, 900-901, 903-904)!!Military quarters (601-602)!!Military ships (602)
#> 125                                        Total!!Noninstitutionalized population (501, 601-602, 701-702, 704, 706, 801-802, 900-901, 903-904)!!Military quarters (601-602)!!Military ships (602)
#>                                                            concept
#> 1   GROUP QUARTERS POPULATION BY GROUP QUARTERS TYPE (WHITE ALONE)
#> 2   GROUP QUARTERS POPULATION BY GROUP QUARTERS TYPE (WHITE ALONE)
#> 3   GROUP QUARTERS POPULATION BY GROUP QUARTERS TYPE (WHITE ALONE)
#> 4   GROUP QUARTERS POPULATION BY GROUP QUARTERS TYPE (WHITE ALONE)
#> 5   GROUP QUARTERS POPULATION BY GROUP QUARTERS TYPE (WHITE ALONE)
#> 6   GROUP QUARTERS POPULATION BY GROUP QUARTERS TYPE (WHITE ALONE)
#> 7   GROUP QUARTERS POPULATION BY GROUP QUARTERS TYPE (WHITE ALONE)
#> 8   GROUP QUARTERS POPULATION BY GROUP QUARTERS TYPE (WHITE ALONE)
#> 9   GROUP QUARTERS POPULATION BY GROUP QUARTERS TYPE (WHITE ALONE)
#> 10  GROUP QUARTERS POPULATION BY GROUP QUARTERS TYPE (WHITE ALONE)
#> 11  GROUP QUARTERS POPULATION BY GROUP QUARTERS TYPE (WHITE ALONE)
#> 12  GROUP QUARTERS POPULATION BY GROUP QUARTERS TYPE (WHITE ALONE)
#> 13  GROUP QUARTERS POPULATION BY GROUP QUARTERS TYPE (WHITE ALONE)
#> 14  GROUP QUARTERS POPULATION BY GROUP QUARTERS TYPE (WHITE ALONE)
#> 15  GROUP QUARTERS POPULATION BY GROUP QUARTERS TYPE (WHITE ALONE)
#> 16  GROUP QUARTERS POPULATION BY GROUP QUARTERS TYPE (WHITE ALONE)
#> 17  GROUP QUARTERS POPULATION BY GROUP QUARTERS TYPE (WHITE ALONE)
#> 18  GROUP QUARTERS POPULATION BY GROUP QUARTERS TYPE (WHITE ALONE)
#> 19  GROUP QUARTERS POPULATION BY GROUP QUARTERS TYPE (WHITE ALONE)
#> 20  GROUP QUARTERS POPULATION BY GROUP QUARTERS TYPE (WHITE ALONE)
#> 21  GROUP QUARTERS POPULATION BY GROUP QUARTERS TYPE (WHITE ALONE)
#> 22  GROUP QUARTERS POPULATION BY GROUP QUARTERS TYPE (WHITE ALONE)
#> 23  GROUP QUARTERS POPULATION BY GROUP QUARTERS TYPE (WHITE ALONE)
#> 24  GROUP QUARTERS POPULATION BY GROUP QUARTERS TYPE (WHITE ALONE)
#> 25  GROUP QUARTERS POPULATION BY GROUP QUARTERS TYPE (WHITE ALONE)
#> 26  GROUP QUARTERS POPULATION BY GROUP QUARTERS TYPE (WHITE ALONE)
#> 27  GROUP QUARTERS POPULATION BY GROUP QUARTERS TYPE (WHITE ALONE)
#> 28  GROUP QUARTERS POPULATION BY GROUP QUARTERS TYPE (WHITE ALONE)
#> 29  GROUP QUARTERS POPULATION BY GROUP QUARTERS TYPE (WHITE ALONE)
#> 30  GROUP QUARTERS POPULATION BY GROUP QUARTERS TYPE (WHITE ALONE)
#> 31  GROUP QUARTERS POPULATION BY GROUP QUARTERS TYPE (WHITE ALONE)
#> 32  GROUP QUARTERS POPULATION BY GROUP QUARTERS TYPE (WHITE ALONE)
#> 33  GROUP QUARTERS POPULATION BY GROUP QUARTERS TYPE (WHITE ALONE)
#> 34  GROUP QUARTERS POPULATION BY GROUP QUARTERS TYPE (WHITE ALONE)
#> 35  GROUP QUARTERS POPULATION BY GROUP QUARTERS TYPE (WHITE ALONE)
#> 36  GROUP QUARTERS POPULATION BY GROUP QUARTERS TYPE (WHITE ALONE)
#> 37  GROUP QUARTERS POPULATION BY GROUP QUARTERS TYPE (WHITE ALONE)
#> 38  GROUP QUARTERS POPULATION BY GROUP QUARTERS TYPE (WHITE ALONE)
#> 39  GROUP QUARTERS POPULATION BY GROUP QUARTERS TYPE (WHITE ALONE)
#> 40  GROUP QUARTERS POPULATION BY GROUP QUARTERS TYPE (WHITE ALONE)
#> 41  GROUP QUARTERS POPULATION BY GROUP QUARTERS TYPE (WHITE ALONE)
#> 42  GROUP QUARTERS POPULATION BY GROUP QUARTERS TYPE (WHITE ALONE)
#> 43  GROUP QUARTERS POPULATION BY GROUP QUARTERS TYPE (WHITE ALONE)
#> 44  GROUP QUARTERS POPULATION BY GROUP QUARTERS TYPE (WHITE ALONE)
#> 45  GROUP QUARTERS POPULATION BY GROUP QUARTERS TYPE (WHITE ALONE)
#> 46  GROUP QUARTERS POPULATION BY GROUP QUARTERS TYPE (WHITE ALONE)
#> 47  GROUP QUARTERS POPULATION BY GROUP QUARTERS TYPE (WHITE ALONE)
#> 48  GROUP QUARTERS POPULATION BY GROUP QUARTERS TYPE (WHITE ALONE)
#> 49  GROUP QUARTERS POPULATION BY GROUP QUARTERS TYPE (WHITE ALONE)
#> 50  GROUP QUARTERS POPULATION BY GROUP QUARTERS TYPE (WHITE ALONE)
#> 51  GROUP QUARTERS POPULATION BY GROUP QUARTERS TYPE (WHITE ALONE)
#> 52  GROUP QUARTERS POPULATION BY GROUP QUARTERS TYPE (WHITE ALONE)
#> 53  GROUP QUARTERS POPULATION BY GROUP QUARTERS TYPE (WHITE ALONE)
#> 54  GROUP QUARTERS POPULATION BY GROUP QUARTERS TYPE (WHITE ALONE)
#> 55  GROUP QUARTERS POPULATION BY GROUP QUARTERS TYPE (WHITE ALONE)
#> 56  GROUP QUARTERS POPULATION BY GROUP QUARTERS TYPE (WHITE ALONE)
#> 57  GROUP QUARTERS POPULATION BY GROUP QUARTERS TYPE (WHITE ALONE)
#> 58  GROUP QUARTERS POPULATION BY GROUP QUARTERS TYPE (WHITE ALONE)
#> 59  GROUP QUARTERS POPULATION BY GROUP QUARTERS TYPE (WHITE ALONE)
#> 60  GROUP QUARTERS POPULATION BY GROUP QUARTERS TYPE (WHITE ALONE)
#> 61  GROUP QUARTERS POPULATION BY GROUP QUARTERS TYPE (WHITE ALONE)
#> 62  GROUP QUARTERS POPULATION BY GROUP QUARTERS TYPE (WHITE ALONE)
#> 63  GROUP QUARTERS POPULATION BY GROUP QUARTERS TYPE (WHITE ALONE)
#> 64  GROUP QUARTERS POPULATION BY GROUP QUARTERS TYPE (WHITE ALONE)
#> 65  GROUP QUARTERS POPULATION BY GROUP QUARTERS TYPE (WHITE ALONE)
#> 66  GROUP QUARTERS POPULATION BY GROUP QUARTERS TYPE (WHITE ALONE)
#> 67  GROUP QUARTERS POPULATION BY GROUP QUARTERS TYPE (WHITE ALONE)
#> 68  GROUP QUARTERS POPULATION BY GROUP QUARTERS TYPE (WHITE ALONE)
#> 69  GROUP QUARTERS POPULATION BY GROUP QUARTERS TYPE (WHITE ALONE)
#> 70  GROUP QUARTERS POPULATION BY GROUP QUARTERS TYPE (WHITE ALONE)
#> 71  GROUP QUARTERS POPULATION BY GROUP QUARTERS TYPE (WHITE ALONE)
#> 72  GROUP QUARTERS POPULATION BY GROUP QUARTERS TYPE (WHITE ALONE)
#> 73  GROUP QUARTERS POPULATION BY GROUP QUARTERS TYPE (WHITE ALONE)
#> 74  GROUP QUARTERS POPULATION BY GROUP QUARTERS TYPE (WHITE ALONE)
#> 75  GROUP QUARTERS POPULATION BY GROUP QUARTERS TYPE (WHITE ALONE)
#> 76  GROUP QUARTERS POPULATION BY GROUP QUARTERS TYPE (WHITE ALONE)
#> 77  GROUP QUARTERS POPULATION BY GROUP QUARTERS TYPE (WHITE ALONE)
#> 78  GROUP QUARTERS POPULATION BY GROUP QUARTERS TYPE (WHITE ALONE)
#> 79  GROUP QUARTERS POPULATION BY GROUP QUARTERS TYPE (WHITE ALONE)
#> 80  GROUP QUARTERS POPULATION BY GROUP QUARTERS TYPE (WHITE ALONE)
#> 81  GROUP QUARTERS POPULATION BY GROUP QUARTERS TYPE (WHITE ALONE)
#> 82  GROUP QUARTERS POPULATION BY GROUP QUARTERS TYPE (WHITE ALONE)
#> 83  GROUP QUARTERS POPULATION BY GROUP QUARTERS TYPE (WHITE ALONE)
#> 84  GROUP QUARTERS POPULATION BY GROUP QUARTERS TYPE (WHITE ALONE)
#> 85  GROUP QUARTERS POPULATION BY GROUP QUARTERS TYPE (WHITE ALONE)
#> 86  GROUP QUARTERS POPULATION BY GROUP QUARTERS TYPE (WHITE ALONE)
#> 87  GROUP QUARTERS POPULATION BY GROUP QUARTERS TYPE (WHITE ALONE)
#> 88  GROUP QUARTERS POPULATION BY GROUP QUARTERS TYPE (WHITE ALONE)
#> 89  GROUP QUARTERS POPULATION BY GROUP QUARTERS TYPE (WHITE ALONE)
#> 90  GROUP QUARTERS POPULATION BY GROUP QUARTERS TYPE (WHITE ALONE)
#> 91  GROUP QUARTERS POPULATION BY GROUP QUARTERS TYPE (WHITE ALONE)
#> 92  GROUP QUARTERS POPULATION BY GROUP QUARTERS TYPE (WHITE ALONE)
#> 93  GROUP QUARTERS POPULATION BY GROUP QUARTERS TYPE (WHITE ALONE)
#> 94  GROUP QUARTERS POPULATION BY GROUP QUARTERS TYPE (WHITE ALONE)
#> 95  GROUP QUARTERS POPULATION BY GROUP QUARTERS TYPE (WHITE ALONE)
#> 96  GROUP QUARTERS POPULATION BY GROUP QUARTERS TYPE (WHITE ALONE)
#> 97  GROUP QUARTERS POPULATION BY GROUP QUARTERS TYPE (WHITE ALONE)
#> 98  GROUP QUARTERS POPULATION BY GROUP QUARTERS TYPE (WHITE ALONE)
#> 99  GROUP QUARTERS POPULATION BY GROUP QUARTERS TYPE (WHITE ALONE)
#> 100 GROUP QUARTERS POPULATION BY GROUP QUARTERS TYPE (WHITE ALONE)
#> 101 GROUP QUARTERS POPULATION BY GROUP QUARTERS TYPE (WHITE ALONE)
#> 102 GROUP QUARTERS POPULATION BY GROUP QUARTERS TYPE (WHITE ALONE)
#> 103 GROUP QUARTERS POPULATION BY GROUP QUARTERS TYPE (WHITE ALONE)
#> 104 GROUP QUARTERS POPULATION BY GROUP QUARTERS TYPE (WHITE ALONE)
#> 105 GROUP QUARTERS POPULATION BY GROUP QUARTERS TYPE (WHITE ALONE)
#> 106 GROUP QUARTERS POPULATION BY GROUP QUARTERS TYPE (WHITE ALONE)
#> 107 GROUP QUARTERS POPULATION BY GROUP QUARTERS TYPE (WHITE ALONE)
#> 108 GROUP QUARTERS POPULATION BY GROUP QUARTERS TYPE (WHITE ALONE)
#> 109 GROUP QUARTERS POPULATION BY GROUP QUARTERS TYPE (WHITE ALONE)
#> 110 GROUP QUARTERS POPULATION BY GROUP QUARTERS TYPE (WHITE ALONE)
#> 111 GROUP QUARTERS POPULATION BY GROUP QUARTERS TYPE (WHITE ALONE)
#> 112 GROUP QUARTERS POPULATION BY GROUP QUARTERS TYPE (WHITE ALONE)
#> 113 GROUP QUARTERS POPULATION BY GROUP QUARTERS TYPE (WHITE ALONE)
#> 114 GROUP QUARTERS POPULATION BY GROUP QUARTERS TYPE (WHITE ALONE)
#> 115 GROUP QUARTERS POPULATION BY GROUP QUARTERS TYPE (WHITE ALONE)
#> 116 GROUP QUARTERS POPULATION BY GROUP QUARTERS TYPE (WHITE ALONE)
#> 117 GROUP QUARTERS POPULATION BY GROUP QUARTERS TYPE (WHITE ALONE)
#> 118 GROUP QUARTERS POPULATION BY GROUP QUARTERS TYPE (WHITE ALONE)
#> 119 GROUP QUARTERS POPULATION BY GROUP QUARTERS TYPE (WHITE ALONE)
#> 120 GROUP QUARTERS POPULATION BY GROUP QUARTERS TYPE (WHITE ALONE)
#> 121 GROUP QUARTERS POPULATION BY GROUP QUARTERS TYPE (WHITE ALONE)
#> 122 GROUP QUARTERS POPULATION BY GROUP QUARTERS TYPE (WHITE ALONE)
#> 123 GROUP QUARTERS POPULATION BY GROUP QUARTERS TYPE (WHITE ALONE)
#> 124 GROUP QUARTERS POPULATION BY GROUP QUARTERS TYPE (WHITE ALONE)
#> 125 GROUP QUARTERS POPULATION BY GROUP QUARTERS TYPE (WHITE ALONE)
#>  [ reached 'max' / getOption("max.print") -- omitted 835 rows ]

get_decennial_recs(geography = 'county', table_codes = 'P001', years = c(2000, 2010))
#> Getting data from the 2000 decennial Census
#> Loading SF1 variables for 2000 from table P001. To cache this dataset for faster access to Census tables in the future, run this function with `cache_table = TRUE`. You only need to do this once per Census dataset.
#> Using FIPS code '53' for state 'WA'
#> Using FIPS code '033' for 'King County'
#> Using FIPS code '035' for 'Kitsap County'
#> Using FIPS code '053' for 'Pierce County'
#> Using FIPS code '061' for 'Snohomish County'
#> Using Census Summary File 1
#> Getting data from the 2010 decennial Census
#> Loading SF1 variables for 2010 from table P001. To cache this dataset for faster access to Census tables in the future, run this function with `cache_table = TRUE`. You only need to do this once per Census dataset.
#> Using FIPS code '53' for state 'WA'
#> Using FIPS code '033' for 'King County'
#> Using FIPS code '035' for 'Kitsap County'
#> Using FIPS code '053' for 'Pierce County'
#> Using FIPS code '061' for 'Snohomish County'
#> Using Census Summary File 1
#> 
#> Concept for table codes may differ across Census years. Please double check with tidycensus::load_variables()
#>     GEOID                         NAME variable   value county year label
#> 1   53033      King County, Washington  P001001 1737034  53033 2000 Total
#> 2   53035    Kitsap County, Washington  P001001  231969  53035 2000 Total
#> 3   53053    Pierce County, Washington  P001001  700820  53053 2000 Total
#> 4   53061 Snohomish County, Washington  P001001  606024  53061 2000 Total
#> 5  REGION                       Region  P001001 3275847   <NA> 2000 Total
#> 6   53033      King County, Washington  P001001 1931249  53033 2010 Total
#> 7   53035    Kitsap County, Washington  P001001  251133  53035 2010 Total
#> 8   53053    Pierce County, Washington  P001001  795225  53053 2010 Total
#> 9   53061 Snohomish County, Washington  P001001  713335  53061 2010 Total
#> 10 REGION                       Region  P001001 3690942   <NA> 2010 Total
#>             concept
#> 1  TOTAL POPULATION
#> 2  TOTAL POPULATION
#> 3  TOTAL POPULATION
#> 4  TOTAL POPULATION
#> 5  TOTAL POPULATION
#> 6  TOTAL POPULATION
#> 7  TOTAL POPULATION
#> 8  TOTAL POPULATION
#> 9  TOTAL POPULATION
#> 10 TOTAL POPULATION

get_decennial_recs(geography = 'tract', table_codes = tbl_names, years = 2010)
#> Getting data from the 2010 decennial Census
#> Loading SF1 variables for 2010 from table PCT020A. To cache this dataset for faster access to Census tables in the future, run this function with `cache_table = TRUE`. You only need to do this once per Census dataset.
#> Using FIPS code '53' for state 'WA'
#> Using FIPS code '033' for 'King County'
#> Using FIPS code '035' for 'Kitsap County'
#> Using FIPS code '053' for 'Pierce County'
#> Using FIPS code '061' for 'Snohomish County'
#> Using Census Summary File 1
#> Getting data from the 2010 decennial Census
#> Loading SF1 variables for 2010 from table PCT020B. To cache this dataset for faster access to Census tables in the future, run this function with `cache_table = TRUE`. You only need to do this once per Census dataset.
#> Using FIPS code '53' for state 'WA'
#> Using FIPS code '033' for 'King County'
#> Using FIPS code '035' for 'Kitsap County'
#> Using FIPS code '053' for 'Pierce County'
#> Using FIPS code '061' for 'Snohomish County'
#> Using Census Summary File 1
#> Getting data from the 2010 decennial Census
#> Loading SF1 variables for 2010 from table PCT020C. To cache this dataset for faster access to Census tables in the future, run this function with `cache_table = TRUE`. You only need to do this once per Census dataset.
#> Using FIPS code '53' for state 'WA'
#> Using FIPS code '033' for 'King County'
#> Using FIPS code '035' for 'Kitsap County'
#> Using FIPS code '053' for 'Pierce County'
#> Using FIPS code '061' for 'Snohomish County'
#> Using Census Summary File 1
#> Getting data from the 2010 decennial Census
#> Loading SF1 variables for 2010 from table PCT020D. To cache this dataset for faster access to Census tables in the future, run this function with `cache_table = TRUE`. You only need to do this once per Census dataset.
#> Using FIPS code '53' for state 'WA'
#> Using FIPS code '033' for 'King County'
#> Using FIPS code '035' for 'Kitsap County'
#> Using FIPS code '053' for 'Pierce County'
#> Using FIPS code '061' for 'Snohomish County'
#> Using Census Summary File 1
#> Getting data from the 2010 decennial Census
#> Loading SF1 variables for 2010 from table PCT020E. To cache this dataset for faster access to Census tables in the future, run this function with `cache_table = TRUE`. You only need to do this once per Census dataset.
#> Using FIPS code '53' for state 'WA'
#> Using FIPS code '033' for 'King County'
#> Using FIPS code '035' for 'Kitsap County'
#> Using FIPS code '053' for 'Pierce County'
#> Using FIPS code '061' for 'Snohomish County'
#> Using Census Summary File 1
#> Getting data from the 2010 decennial Census
#> Loading SF1 variables for 2010 from table PCT020F. To cache this dataset for faster access to Census tables in the future, run this function with `cache_table = TRUE`. You only need to do this once per Census dataset.
#> Using FIPS code '53' for state 'WA'
#> Using FIPS code '033' for 'King County'
#> Using FIPS code '035' for 'Kitsap County'
#> Using FIPS code '053' for 'Pierce County'
#> Using FIPS code '061' for 'Snohomish County'
#> Using Census Summary File 1
#>           GEOID                                         NAME   variable value
#> 1   53033000100      Census Tract 1, King County, Washington PCT020A001    32
#> 2   53033000200      Census Tract 2, King County, Washington PCT020A001    55
#> 3   53033000300      Census Tract 3, King County, Washington PCT020A001     8
#> 4   53033000401   Census Tract 4.01, King County, Washington PCT020A001    97
#> 5   53033000402   Census Tract 4.02, King County, Washington PCT020A001   247
#> 6   53033000500      Census Tract 5, King County, Washington PCT020A001    17
#> 7   53033000600      Census Tract 6, King County, Washington PCT020A001    34
#> 8   53033000700      Census Tract 7, King County, Washington PCT020A001     9
#> 9   53033000800      Census Tract 8, King County, Washington PCT020A001    52
#> 10  53033000900      Census Tract 9, King County, Washington PCT020A001    13
#> 11  53033001000     Census Tract 10, King County, Washington PCT020A001    12
#> 12  53033001100     Census Tract 11, King County, Washington PCT020A001    64
#> 13  53033001200     Census Tract 12, King County, Washington PCT020A001    38
#> 14  53033001300     Census Tract 13, King County, Washington PCT020A001     3
#> 15  53033001400     Census Tract 14, King County, Washington PCT020A001    58
#> 16  53033001500     Census Tract 15, King County, Washington PCT020A001     0
#> 17  53033001600     Census Tract 16, King County, Washington PCT020A001    21
#> 18  53033001701  Census Tract 17.01, King County, Washington PCT020A001     7
#> 19  53033001702  Census Tract 17.02, King County, Washington PCT020A001   115
#> 20  53033001800     Census Tract 18, King County, Washington PCT020A001    14
#> 21  53033001900     Census Tract 19, King County, Washington PCT020A001    59
#> 22  53033002000     Census Tract 20, King County, Washington PCT020A001    15
#> 23  53033002100     Census Tract 21, King County, Washington PCT020A001    32
#> 24  53033002200     Census Tract 22, King County, Washington PCT020A001    27
#> 25  53033002400     Census Tract 24, King County, Washington PCT020A001     0
#> 26  53033002500     Census Tract 25, King County, Washington PCT020A001     0
#> 27  53033002600     Census Tract 26, King County, Washington PCT020A001     1
#> 28  53033002700     Census Tract 27, King County, Washington PCT020A001     2
#> 29  53033002800     Census Tract 28, King County, Washington PCT020A001    10
#> 30  53033002900     Census Tract 29, King County, Washington PCT020A001     0
#> 31  53033003000     Census Tract 30, King County, Washington PCT020A001     3
#> 32  53033003100     Census Tract 31, King County, Washington PCT020A001    60
#> 33  53033003200     Census Tract 32, King County, Washington PCT020A001     0
#> 34  53033003300     Census Tract 33, King County, Washington PCT020A001     5
#> 35  53033003400     Census Tract 34, King County, Washington PCT020A001     0
#> 36  53033003500     Census Tract 35, King County, Washington PCT020A001   161
#> 37  53033003600     Census Tract 36, King County, Washington PCT020A001    37
#> 38  53033003800     Census Tract 38, King County, Washington PCT020A001     8
#> 39  53033003900     Census Tract 39, King County, Washington PCT020A001     0
#> 40  53033004000     Census Tract 40, King County, Washington PCT020A001    57
#> 41  53033004100     Census Tract 41, King County, Washington PCT020A001     5
#> 42  53033004200     Census Tract 42, King County, Washington PCT020A001     8
#> 43  53033004301  Census Tract 43.01, King County, Washington PCT020A001     5
#> 44  53033004302  Census Tract 43.02, King County, Washington PCT020A001   612
#> 45  53033004400     Census Tract 44, King County, Washington PCT020A001   228
#> 46  53033004500     Census Tract 45, King County, Washington PCT020A001    13
#> 47  53033004600     Census Tract 46, King County, Washington PCT020A001     0
#> 48  53033004700     Census Tract 47, King County, Washington PCT020A001    70
#> 49  53033004800     Census Tract 48, King County, Washington PCT020A001    16
#> 50  53033004900     Census Tract 49, King County, Washington PCT020A001   105
#> 51  53033005000     Census Tract 50, King County, Washington PCT020A001     0
#> 52  53033005100     Census Tract 51, King County, Washington PCT020A001     0
#> 53  53033005200     Census Tract 52, King County, Washington PCT020A001    61
#> 54  53033005301  Census Tract 53.01, King County, Washington PCT020A001  1945
#> 55  53033005302  Census Tract 53.02, King County, Washington PCT020A001  3152
#> 56  53033005400     Census Tract 54, King County, Washington PCT020A001    78
#> 57  53033005600     Census Tract 56, King County, Washington PCT020A001     4
#> 58  53033005700     Census Tract 57, King County, Washington PCT020A001    12
#> 59  53033005801  Census Tract 58.01, King County, Washington PCT020A001    21
#> 60  53033005802  Census Tract 58.02, King County, Washington PCT020A001   362
#> 61  53033005900     Census Tract 59, King County, Washington PCT020A001  1081
#> 62  53033006000     Census Tract 60, King County, Washington PCT020A001   150
#> 63  53033006100     Census Tract 61, King County, Washington PCT020A001     3
#> 64  53033006200     Census Tract 62, King County, Washington PCT020A001    15
#> 65  53033006300     Census Tract 63, King County, Washington PCT020A001    22
#> 66  53033006400     Census Tract 64, King County, Washington PCT020A001    10
#> 67  53033006500     Census Tract 65, King County, Washington PCT020A001     7
#> 68  53033006600     Census Tract 66, King County, Washington PCT020A001   199
#> 69  53033006700     Census Tract 67, King County, Washington PCT020A001    53
#> 70  53033006800     Census Tract 68, King County, Washington PCT020A001     0
#> 71  53033006900     Census Tract 69, King County, Washington PCT020A001     9
#> 72  53033007000     Census Tract 70, King County, Washington PCT020A001    46
#> 73  53033007100     Census Tract 71, King County, Washington PCT020A001   142
#> 74  53033007200     Census Tract 72, King County, Washington PCT020A001   322
#> 75  53033007300     Census Tract 73, King County, Washington PCT020A001   261
#> 76  53033007401  Census Tract 74.01, King County, Washington PCT020A001     0
#> 77  53033007402  Census Tract 74.02, King County, Washington PCT020A001    82
#> 78  53033007500     Census Tract 75, King County, Washington PCT020A001    77
#> 79  53033007600     Census Tract 76, King County, Washington PCT020A001    41
#> 80  53033007700     Census Tract 77, King County, Washington PCT020A001    11
#> 81  53033007800     Census Tract 78, King County, Washington PCT020A001    40
#> 82  53033007900     Census Tract 79, King County, Washington PCT020A001   120
#> 83  53033008001  Census Tract 80.01, King County, Washington PCT020A001   112
#> 84  53033008002  Census Tract 80.02, King County, Washington PCT020A001   230
#> 85  53033008100     Census Tract 81, King County, Washington PCT020A001   405
#> 86  53033008200     Census Tract 82, King County, Washington PCT020A001   104
#> 87  53033008300     Census Tract 83, King County, Washington PCT020A001    58
#> 88  53033008400     Census Tract 84, King County, Washington PCT020A001    30
#> 89  53033008500     Census Tract 85, King County, Washington PCT020A001  1010
#> 90  53033008600     Census Tract 86, King County, Washington PCT020A001  1046
#> 91  53033008700     Census Tract 87, King County, Washington PCT020A001    42
#> 92  53033008800     Census Tract 88, King County, Washington PCT020A001     0
#> 93  53033008900     Census Tract 89, King County, Washington PCT020A001    74
#> 94  53033009000     Census Tract 90, King County, Washington PCT020A001    26
#> 95  53033009100     Census Tract 91, King County, Washington PCT020A001    25
#> 96  53033009200     Census Tract 92, King County, Washington PCT020A001   240
#> 97  53033009300     Census Tract 93, King County, Washington PCT020A001   209
#> 98  53033009400     Census Tract 94, King County, Washington PCT020A001    84
#> 99  53033009500     Census Tract 95, King County, Washington PCT020A001    13
#> 100 53033009600     Census Tract 96, King County, Washington PCT020A001    99
#> 101 53033009701  Census Tract 97.01, King County, Washington PCT020A001    15
#> 102 53033009702  Census Tract 97.02, King County, Washington PCT020A001    68
#> 103 53033009800     Census Tract 98, King County, Washington PCT020A001     7
#> 104 53033009900     Census Tract 99, King County, Washington PCT020A001     8
#> 105 53033010001 Census Tract 100.01, King County, Washington PCT020A001    38
#> 106 53033010002 Census Tract 100.02, King County, Washington PCT020A001    21
#> 107 53033010100    Census Tract 101, King County, Washington PCT020A001    61
#> 108 53033010200    Census Tract 102, King County, Washington PCT020A001    16
#> 109 53033010300    Census Tract 103, King County, Washington PCT020A001    17
#> 110 53033010401 Census Tract 104.01, King County, Washington PCT020A001    24
#> 111 53033010402 Census Tract 104.02, King County, Washington PCT020A001    10
#> 112 53033010500    Census Tract 105, King County, Washington PCT020A001   259
#> 113 53033010600    Census Tract 106, King County, Washington PCT020A001     7
#> 114 53033010701 Census Tract 107.01, King County, Washington PCT020A001    36
#> 115 53033010702 Census Tract 107.02, King County, Washington PCT020A001     5
#> 116 53033010800    Census Tract 108, King County, Washington PCT020A001     7
#> 117 53033010900    Census Tract 109, King County, Washington PCT020A001    40
#> 118 53033011001 Census Tract 110.01, King County, Washington PCT020A001    46
#> 119 53033011002 Census Tract 110.02, King County, Washington PCT020A001     1
#> 120 53033011101 Census Tract 111.01, King County, Washington PCT020A001     5
#> 121 53033011102 Census Tract 111.02, King County, Washington PCT020A001   166
#> 122 53033011200    Census Tract 112, King County, Washington PCT020A001    68
#> 123 53033011300    Census Tract 113, King County, Washington PCT020A001     7
#> 124 53033011401 Census Tract 114.01, King County, Washington PCT020A001     0
#> 125 53033011402 Census Tract 114.02, King County, Washington PCT020A001     0
#>     county year label
#> 1    53033 2010 Total
#> 2    53033 2010 Total
#> 3    53033 2010 Total
#> 4    53033 2010 Total
#> 5    53033 2010 Total
#> 6    53033 2010 Total
#> 7    53033 2010 Total
#> 8    53033 2010 Total
#> 9    53033 2010 Total
#> 10   53033 2010 Total
#> 11   53033 2010 Total
#> 12   53033 2010 Total
#> 13   53033 2010 Total
#> 14   53033 2010 Total
#> 15   53033 2010 Total
#> 16   53033 2010 Total
#> 17   53033 2010 Total
#> 18   53033 2010 Total
#> 19   53033 2010 Total
#> 20   53033 2010 Total
#> 21   53033 2010 Total
#> 22   53033 2010 Total
#> 23   53033 2010 Total
#> 24   53033 2010 Total
#> 25   53033 2010 Total
#> 26   53033 2010 Total
#> 27   53033 2010 Total
#> 28   53033 2010 Total
#> 29   53033 2010 Total
#> 30   53033 2010 Total
#> 31   53033 2010 Total
#> 32   53033 2010 Total
#> 33   53033 2010 Total
#> 34   53033 2010 Total
#> 35   53033 2010 Total
#> 36   53033 2010 Total
#> 37   53033 2010 Total
#> 38   53033 2010 Total
#> 39   53033 2010 Total
#> 40   53033 2010 Total
#> 41   53033 2010 Total
#> 42   53033 2010 Total
#> 43   53033 2010 Total
#> 44   53033 2010 Total
#> 45   53033 2010 Total
#> 46   53033 2010 Total
#> 47   53033 2010 Total
#> 48   53033 2010 Total
#> 49   53033 2010 Total
#> 50   53033 2010 Total
#> 51   53033 2010 Total
#> 52   53033 2010 Total
#> 53   53033 2010 Total
#> 54   53033 2010 Total
#> 55   53033 2010 Total
#> 56   53033 2010 Total
#> 57   53033 2010 Total
#> 58   53033 2010 Total
#> 59   53033 2010 Total
#> 60   53033 2010 Total
#> 61   53033 2010 Total
#> 62   53033 2010 Total
#> 63   53033 2010 Total
#> 64   53033 2010 Total
#> 65   53033 2010 Total
#> 66   53033 2010 Total
#> 67   53033 2010 Total
#> 68   53033 2010 Total
#> 69   53033 2010 Total
#> 70   53033 2010 Total
#> 71   53033 2010 Total
#> 72   53033 2010 Total
#> 73   53033 2010 Total
#> 74   53033 2010 Total
#> 75   53033 2010 Total
#> 76   53033 2010 Total
#> 77   53033 2010 Total
#> 78   53033 2010 Total
#> 79   53033 2010 Total
#> 80   53033 2010 Total
#> 81   53033 2010 Total
#> 82   53033 2010 Total
#> 83   53033 2010 Total
#> 84   53033 2010 Total
#> 85   53033 2010 Total
#> 86   53033 2010 Total
#> 87   53033 2010 Total
#> 88   53033 2010 Total
#> 89   53033 2010 Total
#> 90   53033 2010 Total
#> 91   53033 2010 Total
#> 92   53033 2010 Total
#> 93   53033 2010 Total
#> 94   53033 2010 Total
#> 95   53033 2010 Total
#> 96   53033 2010 Total
#> 97   53033 2010 Total
#> 98   53033 2010 Total
#> 99   53033 2010 Total
#> 100  53033 2010 Total
#> 101  53033 2010 Total
#> 102  53033 2010 Total
#> 103  53033 2010 Total
#> 104  53033 2010 Total
#> 105  53033 2010 Total
#> 106  53033 2010 Total
#> 107  53033 2010 Total
#> 108  53033 2010 Total
#> 109  53033 2010 Total
#> 110  53033 2010 Total
#> 111  53033 2010 Total
#> 112  53033 2010 Total
#> 113  53033 2010 Total
#> 114  53033 2010 Total
#> 115  53033 2010 Total
#> 116  53033 2010 Total
#> 117  53033 2010 Total
#> 118  53033 2010 Total
#> 119  53033 2010 Total
#> 120  53033 2010 Total
#> 121  53033 2010 Total
#> 122  53033 2010 Total
#> 123  53033 2010 Total
#> 124  53033 2010 Total
#> 125  53033 2010 Total
#>                                                            concept
#> 1   GROUP QUARTERS POPULATION BY GROUP QUARTERS TYPE (WHITE ALONE)
#> 2   GROUP QUARTERS POPULATION BY GROUP QUARTERS TYPE (WHITE ALONE)
#> 3   GROUP QUARTERS POPULATION BY GROUP QUARTERS TYPE (WHITE ALONE)
#> 4   GROUP QUARTERS POPULATION BY GROUP QUARTERS TYPE (WHITE ALONE)
#> 5   GROUP QUARTERS POPULATION BY GROUP QUARTERS TYPE (WHITE ALONE)
#> 6   GROUP QUARTERS POPULATION BY GROUP QUARTERS TYPE (WHITE ALONE)
#> 7   GROUP QUARTERS POPULATION BY GROUP QUARTERS TYPE (WHITE ALONE)
#> 8   GROUP QUARTERS POPULATION BY GROUP QUARTERS TYPE (WHITE ALONE)
#> 9   GROUP QUARTERS POPULATION BY GROUP QUARTERS TYPE (WHITE ALONE)
#> 10  GROUP QUARTERS POPULATION BY GROUP QUARTERS TYPE (WHITE ALONE)
#> 11  GROUP QUARTERS POPULATION BY GROUP QUARTERS TYPE (WHITE ALONE)
#> 12  GROUP QUARTERS POPULATION BY GROUP QUARTERS TYPE (WHITE ALONE)
#> 13  GROUP QUARTERS POPULATION BY GROUP QUARTERS TYPE (WHITE ALONE)
#> 14  GROUP QUARTERS POPULATION BY GROUP QUARTERS TYPE (WHITE ALONE)
#> 15  GROUP QUARTERS POPULATION BY GROUP QUARTERS TYPE (WHITE ALONE)
#> 16  GROUP QUARTERS POPULATION BY GROUP QUARTERS TYPE (WHITE ALONE)
#> 17  GROUP QUARTERS POPULATION BY GROUP QUARTERS TYPE (WHITE ALONE)
#> 18  GROUP QUARTERS POPULATION BY GROUP QUARTERS TYPE (WHITE ALONE)
#> 19  GROUP QUARTERS POPULATION BY GROUP QUARTERS TYPE (WHITE ALONE)
#> 20  GROUP QUARTERS POPULATION BY GROUP QUARTERS TYPE (WHITE ALONE)
#> 21  GROUP QUARTERS POPULATION BY GROUP QUARTERS TYPE (WHITE ALONE)
#> 22  GROUP QUARTERS POPULATION BY GROUP QUARTERS TYPE (WHITE ALONE)
#> 23  GROUP QUARTERS POPULATION BY GROUP QUARTERS TYPE (WHITE ALONE)
#> 24  GROUP QUARTERS POPULATION BY GROUP QUARTERS TYPE (WHITE ALONE)
#> 25  GROUP QUARTERS POPULATION BY GROUP QUARTERS TYPE (WHITE ALONE)
#> 26  GROUP QUARTERS POPULATION BY GROUP QUARTERS TYPE (WHITE ALONE)
#> 27  GROUP QUARTERS POPULATION BY GROUP QUARTERS TYPE (WHITE ALONE)
#> 28  GROUP QUARTERS POPULATION BY GROUP QUARTERS TYPE (WHITE ALONE)
#> 29  GROUP QUARTERS POPULATION BY GROUP QUARTERS TYPE (WHITE ALONE)
#> 30  GROUP QUARTERS POPULATION BY GROUP QUARTERS TYPE (WHITE ALONE)
#> 31  GROUP QUARTERS POPULATION BY GROUP QUARTERS TYPE (WHITE ALONE)
#> 32  GROUP QUARTERS POPULATION BY GROUP QUARTERS TYPE (WHITE ALONE)
#> 33  GROUP QUARTERS POPULATION BY GROUP QUARTERS TYPE (WHITE ALONE)
#> 34  GROUP QUARTERS POPULATION BY GROUP QUARTERS TYPE (WHITE ALONE)
#> 35  GROUP QUARTERS POPULATION BY GROUP QUARTERS TYPE (WHITE ALONE)
#> 36  GROUP QUARTERS POPULATION BY GROUP QUARTERS TYPE (WHITE ALONE)
#> 37  GROUP QUARTERS POPULATION BY GROUP QUARTERS TYPE (WHITE ALONE)
#> 38  GROUP QUARTERS POPULATION BY GROUP QUARTERS TYPE (WHITE ALONE)
#> 39  GROUP QUARTERS POPULATION BY GROUP QUARTERS TYPE (WHITE ALONE)
#> 40  GROUP QUARTERS POPULATION BY GROUP QUARTERS TYPE (WHITE ALONE)
#> 41  GROUP QUARTERS POPULATION BY GROUP QUARTERS TYPE (WHITE ALONE)
#> 42  GROUP QUARTERS POPULATION BY GROUP QUARTERS TYPE (WHITE ALONE)
#> 43  GROUP QUARTERS POPULATION BY GROUP QUARTERS TYPE (WHITE ALONE)
#> 44  GROUP QUARTERS POPULATION BY GROUP QUARTERS TYPE (WHITE ALONE)
#> 45  GROUP QUARTERS POPULATION BY GROUP QUARTERS TYPE (WHITE ALONE)
#> 46  GROUP QUARTERS POPULATION BY GROUP QUARTERS TYPE (WHITE ALONE)
#> 47  GROUP QUARTERS POPULATION BY GROUP QUARTERS TYPE (WHITE ALONE)
#> 48  GROUP QUARTERS POPULATION BY GROUP QUARTERS TYPE (WHITE ALONE)
#> 49  GROUP QUARTERS POPULATION BY GROUP QUARTERS TYPE (WHITE ALONE)
#> 50  GROUP QUARTERS POPULATION BY GROUP QUARTERS TYPE (WHITE ALONE)
#> 51  GROUP QUARTERS POPULATION BY GROUP QUARTERS TYPE (WHITE ALONE)
#> 52  GROUP QUARTERS POPULATION BY GROUP QUARTERS TYPE (WHITE ALONE)
#> 53  GROUP QUARTERS POPULATION BY GROUP QUARTERS TYPE (WHITE ALONE)
#> 54  GROUP QUARTERS POPULATION BY GROUP QUARTERS TYPE (WHITE ALONE)
#> 55  GROUP QUARTERS POPULATION BY GROUP QUARTERS TYPE (WHITE ALONE)
#> 56  GROUP QUARTERS POPULATION BY GROUP QUARTERS TYPE (WHITE ALONE)
#> 57  GROUP QUARTERS POPULATION BY GROUP QUARTERS TYPE (WHITE ALONE)
#> 58  GROUP QUARTERS POPULATION BY GROUP QUARTERS TYPE (WHITE ALONE)
#> 59  GROUP QUARTERS POPULATION BY GROUP QUARTERS TYPE (WHITE ALONE)
#> 60  GROUP QUARTERS POPULATION BY GROUP QUARTERS TYPE (WHITE ALONE)
#> 61  GROUP QUARTERS POPULATION BY GROUP QUARTERS TYPE (WHITE ALONE)
#> 62  GROUP QUARTERS POPULATION BY GROUP QUARTERS TYPE (WHITE ALONE)
#> 63  GROUP QUARTERS POPULATION BY GROUP QUARTERS TYPE (WHITE ALONE)
#> 64  GROUP QUARTERS POPULATION BY GROUP QUARTERS TYPE (WHITE ALONE)
#> 65  GROUP QUARTERS POPULATION BY GROUP QUARTERS TYPE (WHITE ALONE)
#> 66  GROUP QUARTERS POPULATION BY GROUP QUARTERS TYPE (WHITE ALONE)
#> 67  GROUP QUARTERS POPULATION BY GROUP QUARTERS TYPE (WHITE ALONE)
#> 68  GROUP QUARTERS POPULATION BY GROUP QUARTERS TYPE (WHITE ALONE)
#> 69  GROUP QUARTERS POPULATION BY GROUP QUARTERS TYPE (WHITE ALONE)
#> 70  GROUP QUARTERS POPULATION BY GROUP QUARTERS TYPE (WHITE ALONE)
#> 71  GROUP QUARTERS POPULATION BY GROUP QUARTERS TYPE (WHITE ALONE)
#> 72  GROUP QUARTERS POPULATION BY GROUP QUARTERS TYPE (WHITE ALONE)
#> 73  GROUP QUARTERS POPULATION BY GROUP QUARTERS TYPE (WHITE ALONE)
#> 74  GROUP QUARTERS POPULATION BY GROUP QUARTERS TYPE (WHITE ALONE)
#> 75  GROUP QUARTERS POPULATION BY GROUP QUARTERS TYPE (WHITE ALONE)
#> 76  GROUP QUARTERS POPULATION BY GROUP QUARTERS TYPE (WHITE ALONE)
#> 77  GROUP QUARTERS POPULATION BY GROUP QUARTERS TYPE (WHITE ALONE)
#> 78  GROUP QUARTERS POPULATION BY GROUP QUARTERS TYPE (WHITE ALONE)
#> 79  GROUP QUARTERS POPULATION BY GROUP QUARTERS TYPE (WHITE ALONE)
#> 80  GROUP QUARTERS POPULATION BY GROUP QUARTERS TYPE (WHITE ALONE)
#> 81  GROUP QUARTERS POPULATION BY GROUP QUARTERS TYPE (WHITE ALONE)
#> 82  GROUP QUARTERS POPULATION BY GROUP QUARTERS TYPE (WHITE ALONE)
#> 83  GROUP QUARTERS POPULATION BY GROUP QUARTERS TYPE (WHITE ALONE)
#> 84  GROUP QUARTERS POPULATION BY GROUP QUARTERS TYPE (WHITE ALONE)
#> 85  GROUP QUARTERS POPULATION BY GROUP QUARTERS TYPE (WHITE ALONE)
#> 86  GROUP QUARTERS POPULATION BY GROUP QUARTERS TYPE (WHITE ALONE)
#> 87  GROUP QUARTERS POPULATION BY GROUP QUARTERS TYPE (WHITE ALONE)
#> 88  GROUP QUARTERS POPULATION BY GROUP QUARTERS TYPE (WHITE ALONE)
#> 89  GROUP QUARTERS POPULATION BY GROUP QUARTERS TYPE (WHITE ALONE)
#> 90  GROUP QUARTERS POPULATION BY GROUP QUARTERS TYPE (WHITE ALONE)
#> 91  GROUP QUARTERS POPULATION BY GROUP QUARTERS TYPE (WHITE ALONE)
#> 92  GROUP QUARTERS POPULATION BY GROUP QUARTERS TYPE (WHITE ALONE)
#> 93  GROUP QUARTERS POPULATION BY GROUP QUARTERS TYPE (WHITE ALONE)
#> 94  GROUP QUARTERS POPULATION BY GROUP QUARTERS TYPE (WHITE ALONE)
#> 95  GROUP QUARTERS POPULATION BY GROUP QUARTERS TYPE (WHITE ALONE)
#> 96  GROUP QUARTERS POPULATION BY GROUP QUARTERS TYPE (WHITE ALONE)
#> 97  GROUP QUARTERS POPULATION BY GROUP QUARTERS TYPE (WHITE ALONE)
#> 98  GROUP QUARTERS POPULATION BY GROUP QUARTERS TYPE (WHITE ALONE)
#> 99  GROUP QUARTERS POPULATION BY GROUP QUARTERS TYPE (WHITE ALONE)
#> 100 GROUP QUARTERS POPULATION BY GROUP QUARTERS TYPE (WHITE ALONE)
#> 101 GROUP QUARTERS POPULATION BY GROUP QUARTERS TYPE (WHITE ALONE)
#> 102 GROUP QUARTERS POPULATION BY GROUP QUARTERS TYPE (WHITE ALONE)
#> 103 GROUP QUARTERS POPULATION BY GROUP QUARTERS TYPE (WHITE ALONE)
#> 104 GROUP QUARTERS POPULATION BY GROUP QUARTERS TYPE (WHITE ALONE)
#> 105 GROUP QUARTERS POPULATION BY GROUP QUARTERS TYPE (WHITE ALONE)
#> 106 GROUP QUARTERS POPULATION BY GROUP QUARTERS TYPE (WHITE ALONE)
#> 107 GROUP QUARTERS POPULATION BY GROUP QUARTERS TYPE (WHITE ALONE)
#> 108 GROUP QUARTERS POPULATION BY GROUP QUARTERS TYPE (WHITE ALONE)
#> 109 GROUP QUARTERS POPULATION BY GROUP QUARTERS TYPE (WHITE ALONE)
#> 110 GROUP QUARTERS POPULATION BY GROUP QUARTERS TYPE (WHITE ALONE)
#> 111 GROUP QUARTERS POPULATION BY GROUP QUARTERS TYPE (WHITE ALONE)
#> 112 GROUP QUARTERS POPULATION BY GROUP QUARTERS TYPE (WHITE ALONE)
#> 113 GROUP QUARTERS POPULATION BY GROUP QUARTERS TYPE (WHITE ALONE)
#> 114 GROUP QUARTERS POPULATION BY GROUP QUARTERS TYPE (WHITE ALONE)
#> 115 GROUP QUARTERS POPULATION BY GROUP QUARTERS TYPE (WHITE ALONE)
#> 116 GROUP QUARTERS POPULATION BY GROUP QUARTERS TYPE (WHITE ALONE)
#> 117 GROUP QUARTERS POPULATION BY GROUP QUARTERS TYPE (WHITE ALONE)
#> 118 GROUP QUARTERS POPULATION BY GROUP QUARTERS TYPE (WHITE ALONE)
#> 119 GROUP QUARTERS POPULATION BY GROUP QUARTERS TYPE (WHITE ALONE)
#> 120 GROUP QUARTERS POPULATION BY GROUP QUARTERS TYPE (WHITE ALONE)
#> 121 GROUP QUARTERS POPULATION BY GROUP QUARTERS TYPE (WHITE ALONE)
#> 122 GROUP QUARTERS POPULATION BY GROUP QUARTERS TYPE (WHITE ALONE)
#> 123 GROUP QUARTERS POPULATION BY GROUP QUARTERS TYPE (WHITE ALONE)
#> 124 GROUP QUARTERS POPULATION BY GROUP QUARTERS TYPE (WHITE ALONE)
#> 125 GROUP QUARTERS POPULATION BY GROUP QUARTERS TYPE (WHITE ALONE)
#>  [ reached 'max' / getOption("max.print") -- omitted 148867 rows ]

get_decennial_recs(geography = 'place',
                   table_codes = 'PCT013',
                   years = 2010,
                   fips = c("5363000", "5308850"))
#> Getting data from the 2010 decennial Census
#> Loading SF1 variables for 2010 from table PCT013. To cache this dataset for faster access to Census tables in the future, run this function with `cache_table = TRUE`. You only need to do this once per Census dataset.
#> Using FIPS code '53' for state 'WA'
#> Using Census Summary File 1
#> Using FIPS code '53' for state 'WA'
#> Using Census Summary File 1
#> Warning: Error retrieving data for item: PCT013 year: 2010 geography: place - Error: Object 'name' not found. Perhaps you intended [NAME]
#>      GEOID                     NAME  variable  value year
#> 1  5308850  Burien city, Washington PCT013001  33013 2010
#> 2  5363000 Seattle city, Washington PCT013001 583735 2010
#> 3  5308850  Burien city, Washington PCT013002  16612 2010
#> 4  5363000 Seattle city, Washington PCT013002 290243 2010
#> 5  5308850  Burien city, Washington PCT013003   1140 2010
#> 6  5363000 Seattle city, Washington PCT013003  16424 2010
#> 7  5308850  Burien city, Washington PCT013004   1065 2010
#> 8  5363000 Seattle city, Washington PCT013004  12919 2010
#> 9  5308850  Burien city, Washington PCT013005    947 2010
#> 10 5363000 Seattle city, Washington PCT013005  10989 2010
#> 11 5308850  Burien city, Washington PCT013006    623 2010
#> 12 5363000 Seattle city, Washington PCT013006   6750 2010
#> 13 5308850  Burien city, Washington PCT013007    420 2010
#> 14 5363000 Seattle city, Washington PCT013007   4933 2010
#> 15 5308850  Burien city, Washington PCT013008    182 2010
#> 16 5363000 Seattle city, Washington PCT013008   3559 2010
#> 17 5308850  Burien city, Washington PCT013009    160 2010
#> 18 5363000 Seattle city, Washington PCT013009   4370 2010
#> 19 5308850  Burien city, Washington PCT013010    642 2010
#> 20 5363000 Seattle city, Washington PCT013010  15875 2010
#> 21 5308850  Burien city, Washington PCT013011   1274 2010
#> 22 5363000 Seattle city, Washington PCT013011  33225 2010
#> 23 5308850  Burien city, Washington PCT013012   1216 2010
#> 24 5363000 Seattle city, Washington PCT013012  29861 2010
#> 25 5308850  Burien city, Washington PCT013013   1213 2010
#> 26 5363000 Seattle city, Washington PCT013013  26689 2010
#> 27 5308850  Burien city, Washington PCT013014   1166 2010
#> 28 5363000 Seattle city, Washington PCT013014  24456 2010
#> 29 5308850  Burien city, Washington PCT013015   1297 2010
#> 30 5363000 Seattle city, Washington PCT013015  20731 2010
#> 31 5308850  Burien city, Washington PCT013016   1303 2010
#> 32 5363000 Seattle city, Washington PCT013016  19209 2010
#> 33 5308850  Burien city, Washington PCT013017   1182 2010
#> 34 5363000 Seattle city, Washington PCT013017  18437 2010
#> 35 5308850  Burien city, Washington PCT013018    393 2010
#> 36 5363000 Seattle city, Washington PCT013018   6576 2010
#> 37 5308850  Burien city, Washington PCT013019    542 2010
#> 38 5363000 Seattle city, Washington PCT013019   8558 2010
#> 39 5308850  Burien city, Washington PCT013020    304 2010
#> 40 5363000 Seattle city, Washington PCT013020   4406 2010
#> 41 5308850  Burien city, Washington PCT013021    332 2010
#> 42 5363000 Seattle city, Washington PCT013021   5079 2010
#> 43 5308850  Burien city, Washington PCT013022    429 2010
#> 44 5363000 Seattle city, Washington PCT013022   5774 2010
#> 45 5308850  Burien city, Washington PCT013023    322 2010
#> 46 5363000 Seattle city, Washington PCT013023   4386 2010
#> 47 5308850  Burien city, Washington PCT013024    239 2010
#> 48 5363000 Seattle city, Washington PCT013024   3441 2010
#> 49 5308850  Burien city, Washington PCT013025    221 2010
#> 50 5363000 Seattle city, Washington PCT013025   3596 2010
#> 51 5308850  Burien city, Washington PCT013026  16401 2010
#> 52 5363000 Seattle city, Washington PCT013026 293492 2010
#> 53 5308850  Burien city, Washington PCT013027   1116 2010
#> 54 5363000 Seattle city, Washington PCT013027  15446 2010
#> 55 5308850  Burien city, Washington PCT013028    903 2010
#> 56 5363000 Seattle city, Washington PCT013028  12904 2010
#> 57 5308850  Burien city, Washington PCT013029    945 2010
#> 58 5363000 Seattle city, Washington PCT013029  10948 2010
#> 59 5308850  Burien city, Washington PCT013030    653 2010
#> 60 5363000 Seattle city, Washington PCT013030   6433 2010
#> 61 5308850  Burien city, Washington PCT013031    364 2010
#> 62 5363000 Seattle city, Washington PCT013031   4899 2010
#> 63 5308850  Burien city, Washington PCT013032    172 2010
#> 64 5363000 Seattle city, Washington PCT013032   3912 2010
#> 65 5308850  Burien city, Washington PCT013033    189 2010
#> 66 5363000 Seattle city, Washington PCT013033   4649 2010
#> 67 5308850  Burien city, Washington PCT013034    641 2010
#> 68 5363000 Seattle city, Washington PCT013034  16746 2010
#> 69 5308850  Burien city, Washington PCT013035   1088 2010
#> 70 5363000 Seattle city, Washington PCT013035  33072 2010
#> 71 5308850  Burien city, Washington PCT013036   1115 2010
#> 72 5363000 Seattle city, Washington PCT013036  28397 2010
#> 73 5308850  Burien city, Washington PCT013037   1184 2010
#> 74 5363000 Seattle city, Washington PCT013037  24747 2010
#> 75 5308850  Burien city, Washington PCT013038   1080 2010
#> 76 5363000 Seattle city, Washington PCT013038  21714 2010
#> 77 5308850  Burien city, Washington PCT013039   1243 2010
#> 78 5363000 Seattle city, Washington PCT013039  18871 2010
#> 79 5308850  Burien city, Washington PCT013040   1266 2010
#> 80 5363000 Seattle city, Washington PCT013040  19172 2010
#> 81 5308850  Burien city, Washington PCT013041   1166 2010
#> 82 5363000 Seattle city, Washington PCT013041  19366 2010
#> 83 5308850  Burien city, Washington PCT013042    410 2010
#> 84 5363000 Seattle city, Washington PCT013042   7065 2010
#> 85 5308850  Burien city, Washington PCT013043    549 2010
#> 86 5363000 Seattle city, Washington PCT013043   9234 2010
#> 87 5308850  Burien city, Washington PCT013044    313 2010
#> 88 5363000 Seattle city, Washington PCT013044   4727 2010
#> 89 5308850  Burien city, Washington PCT013045    360 2010
#> 90 5363000 Seattle city, Washington PCT013045   5546 2010
#> 91 5308850  Burien city, Washington PCT013046    520 2010
#> 92 5363000 Seattle city, Washington PCT013046   6912 2010
#> 93 5308850  Burien city, Washington PCT013047    388 2010
#> 94 5363000 Seattle city, Washington PCT013047   5792 2010
#> 95 5308850  Burien city, Washington PCT013048    353 2010
#> 96 5363000 Seattle city, Washington PCT013048   5639 2010
#> 97 5308850  Burien city, Washington PCT013049    383 2010
#> 98 5363000 Seattle city, Washington PCT013049   7301 2010
#>                               label                                     concept
#> 1                             Total SEX BY AGE FOR THE POPULATION IN HOUSEHOLDS
#> 2                             Total SEX BY AGE FOR THE POPULATION IN HOUSEHOLDS
#> 3                       Total!!Male SEX BY AGE FOR THE POPULATION IN HOUSEHOLDS
#> 4                       Total!!Male SEX BY AGE FOR THE POPULATION IN HOUSEHOLDS
#> 5        Total!!Male!!Under 5 years SEX BY AGE FOR THE POPULATION IN HOUSEHOLDS
#> 6        Total!!Male!!Under 5 years SEX BY AGE FOR THE POPULATION IN HOUSEHOLDS
#> 7         Total!!Male!!5 to 9 years SEX BY AGE FOR THE POPULATION IN HOUSEHOLDS
#> 8         Total!!Male!!5 to 9 years SEX BY AGE FOR THE POPULATION IN HOUSEHOLDS
#> 9       Total!!Male!!10 to 14 years SEX BY AGE FOR THE POPULATION IN HOUSEHOLDS
#> 10      Total!!Male!!10 to 14 years SEX BY AGE FOR THE POPULATION IN HOUSEHOLDS
#> 11      Total!!Male!!15 to 17 years SEX BY AGE FOR THE POPULATION IN HOUSEHOLDS
#> 12      Total!!Male!!15 to 17 years SEX BY AGE FOR THE POPULATION IN HOUSEHOLDS
#> 13     Total!!Male!!18 and 19 years SEX BY AGE FOR THE POPULATION IN HOUSEHOLDS
#> 14     Total!!Male!!18 and 19 years SEX BY AGE FOR THE POPULATION IN HOUSEHOLDS
#> 15            Total!!Male!!20 years SEX BY AGE FOR THE POPULATION IN HOUSEHOLDS
#> 16            Total!!Male!!20 years SEX BY AGE FOR THE POPULATION IN HOUSEHOLDS
#> 17            Total!!Male!!21 years SEX BY AGE FOR THE POPULATION IN HOUSEHOLDS
#> 18            Total!!Male!!21 years SEX BY AGE FOR THE POPULATION IN HOUSEHOLDS
#> 19      Total!!Male!!22 to 24 years SEX BY AGE FOR THE POPULATION IN HOUSEHOLDS
#> 20      Total!!Male!!22 to 24 years SEX BY AGE FOR THE POPULATION IN HOUSEHOLDS
#> 21      Total!!Male!!25 to 29 years SEX BY AGE FOR THE POPULATION IN HOUSEHOLDS
#> 22      Total!!Male!!25 to 29 years SEX BY AGE FOR THE POPULATION IN HOUSEHOLDS
#> 23      Total!!Male!!30 to 34 years SEX BY AGE FOR THE POPULATION IN HOUSEHOLDS
#> 24      Total!!Male!!30 to 34 years SEX BY AGE FOR THE POPULATION IN HOUSEHOLDS
#> 25      Total!!Male!!35 to 39 years SEX BY AGE FOR THE POPULATION IN HOUSEHOLDS
#> 26      Total!!Male!!35 to 39 years SEX BY AGE FOR THE POPULATION IN HOUSEHOLDS
#> 27      Total!!Male!!40 to 44 years SEX BY AGE FOR THE POPULATION IN HOUSEHOLDS
#> 28      Total!!Male!!40 to 44 years SEX BY AGE FOR THE POPULATION IN HOUSEHOLDS
#> 29      Total!!Male!!45 to 49 years SEX BY AGE FOR THE POPULATION IN HOUSEHOLDS
#> 30      Total!!Male!!45 to 49 years SEX BY AGE FOR THE POPULATION IN HOUSEHOLDS
#> 31      Total!!Male!!50 to 54 years SEX BY AGE FOR THE POPULATION IN HOUSEHOLDS
#> 32      Total!!Male!!50 to 54 years SEX BY AGE FOR THE POPULATION IN HOUSEHOLDS
#> 33      Total!!Male!!55 to 59 years SEX BY AGE FOR THE POPULATION IN HOUSEHOLDS
#> 34      Total!!Male!!55 to 59 years SEX BY AGE FOR THE POPULATION IN HOUSEHOLDS
#> 35     Total!!Male!!60 and 61 years SEX BY AGE FOR THE POPULATION IN HOUSEHOLDS
#> 36     Total!!Male!!60 and 61 years SEX BY AGE FOR THE POPULATION IN HOUSEHOLDS
#> 37      Total!!Male!!62 to 64 years SEX BY AGE FOR THE POPULATION IN HOUSEHOLDS
#> 38      Total!!Male!!62 to 64 years SEX BY AGE FOR THE POPULATION IN HOUSEHOLDS
#> 39     Total!!Male!!65 and 66 years SEX BY AGE FOR THE POPULATION IN HOUSEHOLDS
#> 40     Total!!Male!!65 and 66 years SEX BY AGE FOR THE POPULATION IN HOUSEHOLDS
#> 41      Total!!Male!!67 to 69 years SEX BY AGE FOR THE POPULATION IN HOUSEHOLDS
#> 42      Total!!Male!!67 to 69 years SEX BY AGE FOR THE POPULATION IN HOUSEHOLDS
#> 43      Total!!Male!!70 to 74 years SEX BY AGE FOR THE POPULATION IN HOUSEHOLDS
#> 44      Total!!Male!!70 to 74 years SEX BY AGE FOR THE POPULATION IN HOUSEHOLDS
#> 45      Total!!Male!!75 to 79 years SEX BY AGE FOR THE POPULATION IN HOUSEHOLDS
#> 46      Total!!Male!!75 to 79 years SEX BY AGE FOR THE POPULATION IN HOUSEHOLDS
#> 47      Total!!Male!!80 to 84 years SEX BY AGE FOR THE POPULATION IN HOUSEHOLDS
#> 48      Total!!Male!!80 to 84 years SEX BY AGE FOR THE POPULATION IN HOUSEHOLDS
#> 49   Total!!Male!!85 years and over SEX BY AGE FOR THE POPULATION IN HOUSEHOLDS
#> 50   Total!!Male!!85 years and over SEX BY AGE FOR THE POPULATION IN HOUSEHOLDS
#> 51                    Total!!Female SEX BY AGE FOR THE POPULATION IN HOUSEHOLDS
#> 52                    Total!!Female SEX BY AGE FOR THE POPULATION IN HOUSEHOLDS
#> 53     Total!!Female!!Under 5 years SEX BY AGE FOR THE POPULATION IN HOUSEHOLDS
#> 54     Total!!Female!!Under 5 years SEX BY AGE FOR THE POPULATION IN HOUSEHOLDS
#> 55      Total!!Female!!5 to 9 years SEX BY AGE FOR THE POPULATION IN HOUSEHOLDS
#> 56      Total!!Female!!5 to 9 years SEX BY AGE FOR THE POPULATION IN HOUSEHOLDS
#> 57    Total!!Female!!10 to 14 years SEX BY AGE FOR THE POPULATION IN HOUSEHOLDS
#> 58    Total!!Female!!10 to 14 years SEX BY AGE FOR THE POPULATION IN HOUSEHOLDS
#> 59    Total!!Female!!15 to 17 years SEX BY AGE FOR THE POPULATION IN HOUSEHOLDS
#> 60    Total!!Female!!15 to 17 years SEX BY AGE FOR THE POPULATION IN HOUSEHOLDS
#> 61   Total!!Female!!18 and 19 years SEX BY AGE FOR THE POPULATION IN HOUSEHOLDS
#> 62   Total!!Female!!18 and 19 years SEX BY AGE FOR THE POPULATION IN HOUSEHOLDS
#> 63          Total!!Female!!20 years SEX BY AGE FOR THE POPULATION IN HOUSEHOLDS
#> 64          Total!!Female!!20 years SEX BY AGE FOR THE POPULATION IN HOUSEHOLDS
#> 65          Total!!Female!!21 years SEX BY AGE FOR THE POPULATION IN HOUSEHOLDS
#> 66          Total!!Female!!21 years SEX BY AGE FOR THE POPULATION IN HOUSEHOLDS
#> 67    Total!!Female!!22 to 24 years SEX BY AGE FOR THE POPULATION IN HOUSEHOLDS
#> 68    Total!!Female!!22 to 24 years SEX BY AGE FOR THE POPULATION IN HOUSEHOLDS
#> 69    Total!!Female!!25 to 29 years SEX BY AGE FOR THE POPULATION IN HOUSEHOLDS
#> 70    Total!!Female!!25 to 29 years SEX BY AGE FOR THE POPULATION IN HOUSEHOLDS
#> 71    Total!!Female!!30 to 34 years SEX BY AGE FOR THE POPULATION IN HOUSEHOLDS
#> 72    Total!!Female!!30 to 34 years SEX BY AGE FOR THE POPULATION IN HOUSEHOLDS
#> 73    Total!!Female!!35 to 39 years SEX BY AGE FOR THE POPULATION IN HOUSEHOLDS
#> 74    Total!!Female!!35 to 39 years SEX BY AGE FOR THE POPULATION IN HOUSEHOLDS
#> 75    Total!!Female!!40 to 44 years SEX BY AGE FOR THE POPULATION IN HOUSEHOLDS
#> 76    Total!!Female!!40 to 44 years SEX BY AGE FOR THE POPULATION IN HOUSEHOLDS
#> 77    Total!!Female!!45 to 49 years SEX BY AGE FOR THE POPULATION IN HOUSEHOLDS
#> 78    Total!!Female!!45 to 49 years SEX BY AGE FOR THE POPULATION IN HOUSEHOLDS
#> 79    Total!!Female!!50 to 54 years SEX BY AGE FOR THE POPULATION IN HOUSEHOLDS
#> 80    Total!!Female!!50 to 54 years SEX BY AGE FOR THE POPULATION IN HOUSEHOLDS
#> 81    Total!!Female!!55 to 59 years SEX BY AGE FOR THE POPULATION IN HOUSEHOLDS
#> 82    Total!!Female!!55 to 59 years SEX BY AGE FOR THE POPULATION IN HOUSEHOLDS
#> 83   Total!!Female!!60 and 61 years SEX BY AGE FOR THE POPULATION IN HOUSEHOLDS
#> 84   Total!!Female!!60 and 61 years SEX BY AGE FOR THE POPULATION IN HOUSEHOLDS
#> 85    Total!!Female!!62 to 64 years SEX BY AGE FOR THE POPULATION IN HOUSEHOLDS
#> 86    Total!!Female!!62 to 64 years SEX BY AGE FOR THE POPULATION IN HOUSEHOLDS
#> 87   Total!!Female!!65 and 66 years SEX BY AGE FOR THE POPULATION IN HOUSEHOLDS
#> 88   Total!!Female!!65 and 66 years SEX BY AGE FOR THE POPULATION IN HOUSEHOLDS
#> 89    Total!!Female!!67 to 69 years SEX BY AGE FOR THE POPULATION IN HOUSEHOLDS
#> 90    Total!!Female!!67 to 69 years SEX BY AGE FOR THE POPULATION IN HOUSEHOLDS
#> 91    Total!!Female!!70 to 74 years SEX BY AGE FOR THE POPULATION IN HOUSEHOLDS
#> 92    Total!!Female!!70 to 74 years SEX BY AGE FOR THE POPULATION IN HOUSEHOLDS
#> 93    Total!!Female!!75 to 79 years SEX BY AGE FOR THE POPULATION IN HOUSEHOLDS
#> 94    Total!!Female!!75 to 79 years SEX BY AGE FOR THE POPULATION IN HOUSEHOLDS
#> 95    Total!!Female!!80 to 84 years SEX BY AGE FOR THE POPULATION IN HOUSEHOLDS
#> 96    Total!!Female!!80 to 84 years SEX BY AGE FOR THE POPULATION IN HOUSEHOLDS
#> 97 Total!!Female!!85 years and over SEX BY AGE FOR THE POPULATION IN HOUSEHOLDS
#> 98 Total!!Female!!85 years and over SEX BY AGE FOR THE POPULATION IN HOUSEHOLDS

get_decennial_recs(geography = 'msa',
                   table_codes = c("H001"),
                   years = 2010,
                   fips = c('42660', "28420"))
#> Getting data from the 2010 decennial Census
#> Loading SF1 variables for 2010 from table H001. To cache this dataset for faster access to Census tables in the future, run this function with `cache_table = TRUE`. You only need to do this once per Census dataset.
#> Using Census Summary File 1
#>   GEOID                                    NAME variable   value year label
#> 1 28420 Kennewick-Pasco-Richland, WA Metro Area  H001001   93041 2010 Total
#> 2 42660  Seattle-Tacoma-Bellevue, WA Metro Area  H001001 1463295 2010 Total
#>         concept
#> 1 HOUSING UNITS
#> 2 HOUSING UNITS

get_decennial_recs(geography = 'block group',
                   table_codes = c('H001', 'H006'),
                   years = 2010)
#> Getting data from the 2010 decennial Census
#> Loading SF1 variables for 2010 from table H001. To cache this dataset for faster access to Census tables in the future, run this function with `cache_table = TRUE`. You only need to do this once per Census dataset.
#> Using FIPS code '53' for state 'WA'
#> Using FIPS code '033' for 'King County'
#> Using FIPS code '035' for 'Kitsap County'
#> Using FIPS code '053' for 'Pierce County'
#> Using FIPS code '061' for 'Snohomish County'
#> Using Census Summary File 1
#> Getting data from the 2010 decennial Census
#> Loading SF1 variables for 2010 from table H006. To cache this dataset for faster access to Census tables in the future, run this function with `cache_table = TRUE`. You only need to do this once per Census dataset.
#> Using FIPS code '53' for state 'WA'
#> Using FIPS code '033' for 'King County'
#> Using FIPS code '035' for 'Kitsap County'
#> Using FIPS code '053' for 'Pierce County'
#> Using FIPS code '061' for 'Snohomish County'
#> Using Census Summary File 1
#>            GEOID                                                       NAME
#> 1   530330001001     Block Group 1, Census Tract 1, King County, Washington
#> 2   530330001002     Block Group 2, Census Tract 1, King County, Washington
#> 3   530330001003     Block Group 3, Census Tract 1, King County, Washington
#> 4   530330001004     Block Group 4, Census Tract 1, King County, Washington
#> 5   530330001005     Block Group 5, Census Tract 1, King County, Washington
#> 6   530330002001     Block Group 1, Census Tract 2, King County, Washington
#> 7   530330002002     Block Group 2, Census Tract 2, King County, Washington
#> 8   530330002003     Block Group 3, Census Tract 2, King County, Washington
#> 9   530330002004     Block Group 4, Census Tract 2, King County, Washington
#> 10  530330002005     Block Group 5, Census Tract 2, King County, Washington
#> 11  530330002006     Block Group 6, Census Tract 2, King County, Washington
#> 12  530330003001     Block Group 1, Census Tract 3, King County, Washington
#> 13  530330003002     Block Group 2, Census Tract 3, King County, Washington
#> 14  530330004011  Block Group 1, Census Tract 4.01, King County, Washington
#> 15  530330004012  Block Group 2, Census Tract 4.01, King County, Washington
#> 16  530330004013  Block Group 3, Census Tract 4.01, King County, Washington
#> 17  530330004021  Block Group 1, Census Tract 4.02, King County, Washington
#> 18  530330004022  Block Group 2, Census Tract 4.02, King County, Washington
#> 19  530330004023  Block Group 3, Census Tract 4.02, King County, Washington
#> 20  530330004024  Block Group 4, Census Tract 4.02, King County, Washington
#> 21  530330005001     Block Group 1, Census Tract 5, King County, Washington
#> 22  530330005002     Block Group 2, Census Tract 5, King County, Washington
#> 23  530330005003     Block Group 3, Census Tract 5, King County, Washington
#> 24  530330006001     Block Group 1, Census Tract 6, King County, Washington
#> 25  530330006002     Block Group 2, Census Tract 6, King County, Washington
#> 26  530330006003     Block Group 3, Census Tract 6, King County, Washington
#> 27  530330006004     Block Group 4, Census Tract 6, King County, Washington
#> 28  530330006005     Block Group 5, Census Tract 6, King County, Washington
#> 29  530330006006     Block Group 6, Census Tract 6, King County, Washington
#> 30  530330007001     Block Group 1, Census Tract 7, King County, Washington
#> 31  530330007002     Block Group 2, Census Tract 7, King County, Washington
#> 32  530330007003     Block Group 3, Census Tract 7, King County, Washington
#> 33  530330007004     Block Group 4, Census Tract 7, King County, Washington
#> 34  530330008001     Block Group 1, Census Tract 8, King County, Washington
#> 35  530330008002     Block Group 2, Census Tract 8, King County, Washington
#> 36  530330009001     Block Group 1, Census Tract 9, King County, Washington
#> 37  530330009002     Block Group 2, Census Tract 9, King County, Washington
#> 38  530330010001    Block Group 1, Census Tract 10, King County, Washington
#> 39  530330010002    Block Group 2, Census Tract 10, King County, Washington
#> 40  530330011001    Block Group 1, Census Tract 11, King County, Washington
#> 41  530330011002    Block Group 2, Census Tract 11, King County, Washington
#> 42  530330012001    Block Group 1, Census Tract 12, King County, Washington
#> 43  530330012002    Block Group 2, Census Tract 12, King County, Washington
#> 44  530330012003    Block Group 3, Census Tract 12, King County, Washington
#> 45  530330012004    Block Group 4, Census Tract 12, King County, Washington
#> 46  530330012005    Block Group 5, Census Tract 12, King County, Washington
#> 47  530330013001    Block Group 1, Census Tract 13, King County, Washington
#> 48  530330013002    Block Group 2, Census Tract 13, King County, Washington
#> 49  530330013003    Block Group 3, Census Tract 13, King County, Washington
#> 50  530330014001    Block Group 1, Census Tract 14, King County, Washington
#> 51  530330014002    Block Group 2, Census Tract 14, King County, Washington
#> 52  530330014003    Block Group 3, Census Tract 14, King County, Washington
#> 53  530330014004    Block Group 4, Census Tract 14, King County, Washington
#> 54  530330015001    Block Group 1, Census Tract 15, King County, Washington
#> 55  530330015002    Block Group 2, Census Tract 15, King County, Washington
#> 56  530330016001    Block Group 1, Census Tract 16, King County, Washington
#> 57  530330016002    Block Group 2, Census Tract 16, King County, Washington
#> 58  530330016003    Block Group 3, Census Tract 16, King County, Washington
#> 59  530330017011 Block Group 1, Census Tract 17.01, King County, Washington
#> 60  530330017012 Block Group 2, Census Tract 17.01, King County, Washington
#> 61  530330017013 Block Group 3, Census Tract 17.01, King County, Washington
#> 62  530330017021 Block Group 1, Census Tract 17.02, King County, Washington
#> 63  530330017022 Block Group 2, Census Tract 17.02, King County, Washington
#> 64  530330017023 Block Group 3, Census Tract 17.02, King County, Washington
#> 65  530330017024 Block Group 4, Census Tract 17.02, King County, Washington
#> 66  530330018001    Block Group 1, Census Tract 18, King County, Washington
#> 67  530330018002    Block Group 2, Census Tract 18, King County, Washington
#> 68  530330018003    Block Group 3, Census Tract 18, King County, Washington
#> 69  530330019001    Block Group 1, Census Tract 19, King County, Washington
#> 70  530330019002    Block Group 2, Census Tract 19, King County, Washington
#> 71  530330019003    Block Group 3, Census Tract 19, King County, Washington
#> 72  530330020001    Block Group 1, Census Tract 20, King County, Washington
#> 73  530330020002    Block Group 2, Census Tract 20, King County, Washington
#> 74  530330020003    Block Group 3, Census Tract 20, King County, Washington
#> 75  530330021001    Block Group 1, Census Tract 21, King County, Washington
#> 76  530330021002    Block Group 2, Census Tract 21, King County, Washington
#> 77  530330021003    Block Group 3, Census Tract 21, King County, Washington
#> 78  530330022001    Block Group 1, Census Tract 22, King County, Washington
#> 79  530330022002    Block Group 2, Census Tract 22, King County, Washington
#> 80  530330022003    Block Group 3, Census Tract 22, King County, Washington
#> 81  530330022004    Block Group 4, Census Tract 22, King County, Washington
#> 82  530330024001    Block Group 1, Census Tract 24, King County, Washington
#> 83  530330024002    Block Group 2, Census Tract 24, King County, Washington
#> 84  530330024003    Block Group 3, Census Tract 24, King County, Washington
#> 85  530330025001    Block Group 1, Census Tract 25, King County, Washington
#> 86  530330025002    Block Group 2, Census Tract 25, King County, Washington
#> 87  530330026001    Block Group 1, Census Tract 26, King County, Washington
#> 88  530330026002    Block Group 2, Census Tract 26, King County, Washington
#> 89  530330026003    Block Group 3, Census Tract 26, King County, Washington
#> 90  530330026004    Block Group 4, Census Tract 26, King County, Washington
#> 91  530330027001    Block Group 1, Census Tract 27, King County, Washington
#> 92  530330027002    Block Group 2, Census Tract 27, King County, Washington
#> 93  530330027003    Block Group 3, Census Tract 27, King County, Washington
#> 94  530330027004    Block Group 4, Census Tract 27, King County, Washington
#> 95  530330028001    Block Group 1, Census Tract 28, King County, Washington
#> 96  530330028002    Block Group 2, Census Tract 28, King County, Washington
#> 97  530330028003    Block Group 3, Census Tract 28, King County, Washington
#> 98  530330028004    Block Group 4, Census Tract 28, King County, Washington
#> 99  530330029001    Block Group 1, Census Tract 29, King County, Washington
#> 100 530330029002    Block Group 2, Census Tract 29, King County, Washington
#> 101 530330029003    Block Group 3, Census Tract 29, King County, Washington
#> 102 530330030001    Block Group 1, Census Tract 30, King County, Washington
#> 103 530330030002    Block Group 2, Census Tract 30, King County, Washington
#> 104 530330030003    Block Group 3, Census Tract 30, King County, Washington
#> 105 530330030004    Block Group 4, Census Tract 30, King County, Washington
#> 106 530330031001    Block Group 1, Census Tract 31, King County, Washington
#> 107 530330031002    Block Group 2, Census Tract 31, King County, Washington
#> 108 530330031003    Block Group 3, Census Tract 31, King County, Washington
#> 109 530330031004    Block Group 4, Census Tract 31, King County, Washington
#> 110 530330031005    Block Group 5, Census Tract 31, King County, Washington
#> 111 530330032001    Block Group 1, Census Tract 32, King County, Washington
#> 112 530330032002    Block Group 2, Census Tract 32, King County, Washington
#> 113 530330032003    Block Group 3, Census Tract 32, King County, Washington
#> 114 530330032004    Block Group 4, Census Tract 32, King County, Washington
#> 115 530330032005    Block Group 5, Census Tract 32, King County, Washington
#> 116 530330032006    Block Group 6, Census Tract 32, King County, Washington
#> 117 530330032007    Block Group 7, Census Tract 32, King County, Washington
#> 118 530330033001    Block Group 1, Census Tract 33, King County, Washington
#> 119 530330033002    Block Group 2, Census Tract 33, King County, Washington
#> 120 530330033003    Block Group 3, Census Tract 33, King County, Washington
#> 121 530330033004    Block Group 4, Census Tract 33, King County, Washington
#> 122 530330033005    Block Group 5, Census Tract 33, King County, Washington
#> 123 530330034001    Block Group 1, Census Tract 34, King County, Washington
#> 124 530330034002    Block Group 2, Census Tract 34, King County, Washington
#> 125 530330034003    Block Group 3, Census Tract 34, King County, Washington
#>     variable value county year label       concept
#> 1    H001001   565  53033 2010 Total HOUSING UNITS
#> 2    H001001   658  53033 2010 Total HOUSING UNITS
#> 3    H001001   744  53033 2010 Total HOUSING UNITS
#> 4    H001001   838  53033 2010 Total HOUSING UNITS
#> 5    H001001   638  53033 2010 Total HOUSING UNITS
#> 6    H001001   455  53033 2010 Total HOUSING UNITS
#> 7    H001001   590  53033 2010 Total HOUSING UNITS
#> 8    H001001   826  53033 2010 Total HOUSING UNITS
#> 9    H001001   576  53033 2010 Total HOUSING UNITS
#> 10   H001001   698  53033 2010 Total HOUSING UNITS
#> 11   H001001   553  53033 2010 Total HOUSING UNITS
#> 12   H001001   507  53033 2010 Total HOUSING UNITS
#> 13   H001001   654  53033 2010 Total HOUSING UNITS
#> 14   H001001  1935  53033 2010 Total HOUSING UNITS
#> 15   H001001   716  53033 2010 Total HOUSING UNITS
#> 16   H001001  1063  53033 2010 Total HOUSING UNITS
#> 17   H001001   569  53033 2010 Total HOUSING UNITS
#> 18   H001001   430  53033 2010 Total HOUSING UNITS
#> 19   H001001   611  53033 2010 Total HOUSING UNITS
#> 20   H001001   843  53033 2010 Total HOUSING UNITS
#> 21   H001001   520  53033 2010 Total HOUSING UNITS
#> 22   H001001   372  53033 2010 Total HOUSING UNITS
#> 23   H001001   455  53033 2010 Total HOUSING UNITS
#> 24   H001001   553  53033 2010 Total HOUSING UNITS
#> 25   H001001   634  53033 2010 Total HOUSING UNITS
#> 26   H001001   668  53033 2010 Total HOUSING UNITS
#> 27   H001001   727  53033 2010 Total HOUSING UNITS
#> 28   H001001   310  53033 2010 Total HOUSING UNITS
#> 29   H001001   646  53033 2010 Total HOUSING UNITS
#> 30   H001001   917  53033 2010 Total HOUSING UNITS
#> 31   H001001   547  53033 2010 Total HOUSING UNITS
#> 32   H001001   422  53033 2010 Total HOUSING UNITS
#> 33   H001001   407  53033 2010 Total HOUSING UNITS
#> 34   H001001   503  53033 2010 Total HOUSING UNITS
#> 35   H001001   583  53033 2010 Total HOUSING UNITS
#> 36   H001001   479  53033 2010 Total HOUSING UNITS
#> 37   H001001   427  53033 2010 Total HOUSING UNITS
#> 38   H001001   382  53033 2010 Total HOUSING UNITS
#> 39   H001001   420  53033 2010 Total HOUSING UNITS
#> 40   H001001   581  53033 2010 Total HOUSING UNITS
#> 41   H001001   554  53033 2010 Total HOUSING UNITS
#> 42   H001001   543  53033 2010 Total HOUSING UNITS
#> 43   H001001   557  53033 2010 Total HOUSING UNITS
#> 44   H001001  1002  53033 2010 Total HOUSING UNITS
#> 45   H001001   676  53033 2010 Total HOUSING UNITS
#> 46   H001001   903  53033 2010 Total HOUSING UNITS
#> 47   H001001   869  53033 2010 Total HOUSING UNITS
#> 48   H001001   680  53033 2010 Total HOUSING UNITS
#> 49   H001001   739  53033 2010 Total HOUSING UNITS
#> 50   H001001   539  53033 2010 Total HOUSING UNITS
#> 51   H001001   516  53033 2010 Total HOUSING UNITS
#> 52   H001001   670  53033 2010 Total HOUSING UNITS
#> 53   H001001   582  53033 2010 Total HOUSING UNITS
#> 54   H001001   539  53033 2010 Total HOUSING UNITS
#> 55   H001001   586  53033 2010 Total HOUSING UNITS
#> 56   H001001   567  53033 2010 Total HOUSING UNITS
#> 57   H001001   567  53033 2010 Total HOUSING UNITS
#> 58   H001001   713  53033 2010 Total HOUSING UNITS
#> 59   H001001   891  53033 2010 Total HOUSING UNITS
#> 60   H001001   624  53033 2010 Total HOUSING UNITS
#> 61   H001001   536  53033 2010 Total HOUSING UNITS
#> 62   H001001   734  53033 2010 Total HOUSING UNITS
#> 63   H001001   482  53033 2010 Total HOUSING UNITS
#> 64   H001001   408  53033 2010 Total HOUSING UNITS
#> 65   H001001   384  53033 2010 Total HOUSING UNITS
#> 66   H001001   666  53033 2010 Total HOUSING UNITS
#> 67   H001001   789  53033 2010 Total HOUSING UNITS
#> 68   H001001   837  53033 2010 Total HOUSING UNITS
#> 69   H001001  1132  53033 2010 Total HOUSING UNITS
#> 70   H001001   578  53033 2010 Total HOUSING UNITS
#> 71   H001001   538  53033 2010 Total HOUSING UNITS
#> 72   H001001   806  53033 2010 Total HOUSING UNITS
#> 73   H001001   457  53033 2010 Total HOUSING UNITS
#> 74   H001001   416  53033 2010 Total HOUSING UNITS
#> 75   H001001   659  53033 2010 Total HOUSING UNITS
#> 76   H001001   427  53033 2010 Total HOUSING UNITS
#> 77   H001001   684  53033 2010 Total HOUSING UNITS
#> 78   H001001   606  53033 2010 Total HOUSING UNITS
#> 79   H001001   591  53033 2010 Total HOUSING UNITS
#> 80   H001001   553  53033 2010 Total HOUSING UNITS
#> 81   H001001   521  53033 2010 Total HOUSING UNITS
#> 82   H001001   398  53033 2010 Total HOUSING UNITS
#> 83   H001001   396  53033 2010 Total HOUSING UNITS
#> 84   H001001   500  53033 2010 Total HOUSING UNITS
#> 85   H001001   626  53033 2010 Total HOUSING UNITS
#> 86   H001001   674  53033 2010 Total HOUSING UNITS
#> 87   H001001   477  53033 2010 Total HOUSING UNITS
#> 88   H001001   538  53033 2010 Total HOUSING UNITS
#> 89   H001001   625  53033 2010 Total HOUSING UNITS
#> 90   H001001   412  53033 2010 Total HOUSING UNITS
#> 91   H001001   635  53033 2010 Total HOUSING UNITS
#> 92   H001001   576  53033 2010 Total HOUSING UNITS
#> 93   H001001   521  53033 2010 Total HOUSING UNITS
#> 94   H001001   676  53033 2010 Total HOUSING UNITS
#> 95   H001001   563  53033 2010 Total HOUSING UNITS
#> 96   H001001   457  53033 2010 Total HOUSING UNITS
#> 97   H001001   551  53033 2010 Total HOUSING UNITS
#> 98   H001001   658  53033 2010 Total HOUSING UNITS
#> 99   H001001   650  53033 2010 Total HOUSING UNITS
#> 100  H001001   645  53033 2010 Total HOUSING UNITS
#> 101  H001001   628  53033 2010 Total HOUSING UNITS
#> 102  H001001   710  53033 2010 Total HOUSING UNITS
#> 103  H001001   594  53033 2010 Total HOUSING UNITS
#> 104  H001001   624  53033 2010 Total HOUSING UNITS
#> 105  H001001   574  53033 2010 Total HOUSING UNITS
#> 106  H001001   492  53033 2010 Total HOUSING UNITS
#> 107  H001001   515  53033 2010 Total HOUSING UNITS
#> 108  H001001   561  53033 2010 Total HOUSING UNITS
#> 109  H001001   582  53033 2010 Total HOUSING UNITS
#> 110  H001001   576  53033 2010 Total HOUSING UNITS
#> 111  H001001   467  53033 2010 Total HOUSING UNITS
#> 112  H001001   674  53033 2010 Total HOUSING UNITS
#> 113  H001001   710  53033 2010 Total HOUSING UNITS
#> 114  H001001   750  53033 2010 Total HOUSING UNITS
#> 115  H001001   326  53033 2010 Total HOUSING UNITS
#> 116  H001001   839  53033 2010 Total HOUSING UNITS
#> 117  H001001   484  53033 2010 Total HOUSING UNITS
#> 118  H001001   553  53033 2010 Total HOUSING UNITS
#> 119  H001001   542  53033 2010 Total HOUSING UNITS
#> 120  H001001   974  53033 2010 Total HOUSING UNITS
#> 121  H001001   671  53033 2010 Total HOUSING UNITS
#> 122  H001001   479  53033 2010 Total HOUSING UNITS
#> 123  H001001   540  53033 2010 Total HOUSING UNITS
#> 124  H001001   525  53033 2010 Total HOUSING UNITS
#> 125  H001001   478  53033 2010 Total HOUSING UNITS
#>  [ reached 'max' / getOption("max.print") -- omitted 23698 rows ]

get_decennial_recs(geography="tract",
                   variables="DP1_0092C",
                   years=2020, sumfile="dp")
#> Getting data from the 2020 decennial Census
#> Using FIPS code '53' for state 'WA'
#> Using FIPS code '033' for 'King County'
#> Using FIPS code '035' for 'Kitsap County'
#> Using FIPS code '053' for 'Pierce County'
#> Using FIPS code '061' for 'Snohomish County'
#> Using the Demographic Profile
#> Note: 2020 decennial Census data use differential privacy, a technique that
#> introduces errors into data to preserve respondent confidentiality.
#> ℹ Small counts should be interpreted with caution.
#> ℹ See https://www.census.gov/library/fact-sheets/2021/protecting-the-confidentiality-of-the-2020-census-redistricting-data.html for additional guidance.
#> This message is displayed once per session.
#>           GEOID                                        NAME  variable value
#> 1   53033000101  Census Tract 1.01; King County; Washington DP1_0092C  3759
#> 2   53033000102  Census Tract 1.02; King County; Washington DP1_0092C  4321
#> 3   53033000201  Census Tract 2.01; King County; Washington DP1_0092C  4416
#> 4   53033000202  Census Tract 2.02; King County; Washington DP1_0092C  4099
#> 5   53033000300     Census Tract 3; King County; Washington DP1_0092C  2820
#> 6   53033000402  Census Tract 4.02; King County; Washington DP1_0092C  5174
#> 7   53033000403  Census Tract 4.03; King County; Washington DP1_0092C  3074
#> 8   53033000404  Census Tract 4.04; King County; Washington DP1_0092C  4067
#> 9   53033000500     Census Tract 5; King County; Washington DP1_0092C  3400
#> 10  53033000601  Census Tract 6.01; King County; Washington DP1_0092C  4074
#> 11  53033000602  Census Tract 6.02; King County; Washington DP1_0092C  4006
#> 12  53033000700     Census Tract 7; King County; Washington DP1_0092C  5204
#> 13  53033000800     Census Tract 8; King County; Washington DP1_0092C  2693
#> 14  53033000900     Census Tract 9; King County; Washington DP1_0092C  2076
#> 15  53033001000    Census Tract 10; King County; Washington DP1_0092C  2012
#> 16  53033001100    Census Tract 11; King County; Washington DP1_0092C  2711
#> 17  53033001201 Census Tract 12.01; King County; Washington DP1_0092C  3781
#> 18  53033001202 Census Tract 12.02; King County; Washington DP1_0092C  3880
#> 19  53033001300    Census Tract 13; King County; Washington DP1_0092C  5085
#> 20  53033001400    Census Tract 14; King County; Washington DP1_0092C  5298
#> 21  53033001500    Census Tract 15; King County; Washington DP1_0092C  2679
#> 22  53033001600    Census Tract 16; King County; Washington DP1_0092C  4504
#> 23  53033001701 Census Tract 17.01; King County; Washington DP1_0092C  4252
#> 24  53033001702 Census Tract 17.02; King County; Washington DP1_0092C  5327
#> 25  53033001800    Census Tract 18; King County; Washington DP1_0092C  5109
#> 26  53033001900    Census Tract 19; King County; Washington DP1_0092C  4959
#> 27  53033002000    Census Tract 20; King County; Washington DP1_0092C  3723
#> 28  53033002100    Census Tract 21; King County; Washington DP1_0092C  4423
#> 29  53033002200    Census Tract 22; King County; Washington DP1_0092C  5948
#> 30  53033002400    Census Tract 24; King County; Washington DP1_0092C  3217
#> 31  53033002500    Census Tract 25; King County; Washington DP1_0092C  3155
#> 32  53033002600    Census Tract 26; King County; Washington DP1_0092C  5355
#> 33  53033002700    Census Tract 27; King County; Washington DP1_0092C  5876
#> 34  53033002800    Census Tract 28; King County; Washington DP1_0092C  4956
#> 35  53033002900    Census Tract 29; King County; Washington DP1_0092C  4690
#> 36  53033003000    Census Tract 30; King County; Washington DP1_0092C  6493
#> 37  53033003100    Census Tract 31; King County; Washington DP1_0092C  6545
#> 38  53033003201 Census Tract 32.01; King County; Washington DP1_0092C  4724
#> 39  53033003202 Census Tract 32.02; King County; Washington DP1_0092C  4781
#> 40  53033003301 Census Tract 33.01; King County; Washington DP1_0092C  4351
#> 41  53033003302 Census Tract 33.02; King County; Washington DP1_0092C  3466
#> 42  53033003400    Census Tract 34; King County; Washington DP1_0092C  3560
#> 43  53033003500    Census Tract 35; King County; Washington DP1_0092C  4210
#> 44  53033003601 Census Tract 36.01; King County; Washington DP1_0092C  3838
#> 45  53033003602 Census Tract 36.02; King County; Washington DP1_0092C  4574
#> 46  53033003800    Census Tract 38; King County; Washington DP1_0092C  2469
#> 47  53033003900    Census Tract 39; King County; Washington DP1_0092C  3041
#> 48  53033004000    Census Tract 40; King County; Washington DP1_0092C  3308
#> 49  53033004101 Census Tract 41.01; King County; Washington DP1_0092C  3914
#> 50  53033004102 Census Tract 41.02; King County; Washington DP1_0092C  4198
#> 51  53033004201 Census Tract 42.01; King County; Washington DP1_0092C  3959
#> 52  53033004202 Census Tract 42.02; King County; Washington DP1_0092C  4441
#> 53  53033004301 Census Tract 43.01; King County; Washington DP1_0092C  3943
#> 54  53033004302 Census Tract 43.02; King County; Washington DP1_0092C  3920
#> 55  53033004401 Census Tract 44.01; King County; Washington DP1_0092C  3810
#> 56  53033004402 Census Tract 44.02; King County; Washington DP1_0092C  3564
#> 57  53033004500    Census Tract 45; King County; Washington DP1_0092C  2972
#> 58  53033004600    Census Tract 46; King County; Washington DP1_0092C  3563
#> 59  53033004701 Census Tract 47.01; King County; Washington DP1_0092C  3639
#> 60  53033004702 Census Tract 47.02; King County; Washington DP1_0092C  3917
#> 61  53033004703 Census Tract 47.03; King County; Washington DP1_0092C  3635
#> 62  53033004800    Census Tract 48; King County; Washington DP1_0092C  5113
#> 63  53033004901 Census Tract 49.01; King County; Washington DP1_0092C  3846
#> 64  53033004902 Census Tract 49.02; King County; Washington DP1_0092C  3143
#> 65  53033005000    Census Tract 50; King County; Washington DP1_0092C  4523
#> 66  53033005100    Census Tract 51; King County; Washington DP1_0092C  4012
#> 67  53033005201 Census Tract 52.01; King County; Washington DP1_0092C  3898
#> 68  53033005202 Census Tract 52.02; King County; Washington DP1_0092C  3595
#> 69  53033005303 Census Tract 53.03; King County; Washington DP1_0092C  5310
#> 70  53033005304 Census Tract 53.04; King County; Washington DP1_0092C  3506
#> 71  53033005305 Census Tract 53.05; King County; Washington DP1_0092C  2885
#> 72  53033005306 Census Tract 53.06; King County; Washington DP1_0092C  3134
#> 73  53033005307 Census Tract 53.07; King County; Washington DP1_0092C  2921
#> 74  53033005401 Census Tract 54.01; King County; Washington DP1_0092C  3853
#> 75  53033005402 Census Tract 54.02; King County; Washington DP1_0092C  3262
#> 76  53033005600    Census Tract 56; King County; Washington DP1_0092C  7130
#> 77  53033005700    Census Tract 57; King County; Washington DP1_0092C  6586
#> 78  53033005801 Census Tract 58.01; King County; Washington DP1_0092C  6116
#> 79  53033005803 Census Tract 58.03; King County; Washington DP1_0092C  3347
#> 80  53033005804 Census Tract 58.04; King County; Washington DP1_0092C  2993
#> 81  53033005901 Census Tract 59.01; King County; Washington DP1_0092C  3570
#> 82  53033005902 Census Tract 59.02; King County; Washington DP1_0092C  4127
#> 83  53033006000    Census Tract 60; King County; Washington DP1_0092C  6061
#> 84  53033006100    Census Tract 61; King County; Washington DP1_0092C  5667
#> 85  53033006200    Census Tract 62; King County; Washington DP1_0092C  4075
#> 86  53033006300    Census Tract 63; King County; Washington DP1_0092C  5530
#> 87  53033006400    Census Tract 64; King County; Washington DP1_0092C  3420
#> 88  53033006500    Census Tract 65; King County; Washington DP1_0092C  4672
#> 89  53033006600    Census Tract 66; King County; Washington DP1_0092C  4061
#> 90  53033006701 Census Tract 67.01; King County; Washington DP1_0092C  4060
#> 91  53033006702 Census Tract 67.02; King County; Washington DP1_0092C  3256
#> 92  53033006703 Census Tract 67.03; King County; Washington DP1_0092C  3259
#> 93  53033006800    Census Tract 68; King County; Washington DP1_0092C  3341
#> 94  53033006900    Census Tract 69; King County; Washington DP1_0092C  4698
#> 95  53033007001 Census Tract 70.01; King County; Washington DP1_0092C  3757
#> 96  53033007002 Census Tract 70.02; King County; Washington DP1_0092C  3981
#> 97  53033007101 Census Tract 71.01; King County; Washington DP1_0092C  3221
#> 98  53033007102 Census Tract 71.02; King County; Washington DP1_0092C  2661
#> 99  53033007201 Census Tract 72.01; King County; Washington DP1_0092C  4646
#> 100 53033007202 Census Tract 72.02; King County; Washington DP1_0092C  4148
#> 101 53033007203 Census Tract 72.03; King County; Washington DP1_0092C  3421
#> 102 53033007301 Census Tract 73.01; King County; Washington DP1_0092C  3736
#> 103 53033007302 Census Tract 73.02; King County; Washington DP1_0092C  5290
#> 104 53033007303 Census Tract 73.03; King County; Washington DP1_0092C  3880
#> 105 53033007403 Census Tract 74.03; King County; Washington DP1_0092C  2545
#> 106 53033007404 Census Tract 74.04; King County; Washington DP1_0092C  2799
#> 107 53033007405 Census Tract 74.05; King County; Washington DP1_0092C  2801
#> 108 53033007406 Census Tract 74.06; King County; Washington DP1_0092C  2589
#> 109 53033007501 Census Tract 75.01; King County; Washington DP1_0092C  3915
#> 110 53033007502 Census Tract 75.02; King County; Washington DP1_0092C  3474
#> 111 53033007503 Census Tract 75.03; King County; Washington DP1_0092C  2460
#> 112 53033007600    Census Tract 76; King County; Washington DP1_0092C  4344
#> 113 53033007700    Census Tract 77; King County; Washington DP1_0092C  5390
#> 114 53033007800    Census Tract 78; King County; Washington DP1_0092C  5285
#> 115 53033007901 Census Tract 79.01; King County; Washington DP1_0092C  2807
#> 116 53033007902 Census Tract 79.02; King County; Washington DP1_0092C  3682
#> 117 53033008002 Census Tract 80.02; King County; Washington DP1_0092C  4419
#> 118 53033008003 Census Tract 80.03; King County; Washington DP1_0092C  4119
#> 119 53033008004 Census Tract 80.04; King County; Washington DP1_0092C  3410
#> 120 53033008101 Census Tract 81.01; King County; Washington DP1_0092C  2808
#> 121 53033008102 Census Tract 81.02; King County; Washington DP1_0092C  3005
#> 122 53033008200    Census Tract 82; King County; Washington DP1_0092C  4802
#> 123 53033008300    Census Tract 83; King County; Washington DP1_0092C  3417
#> 124 53033008401 Census Tract 84.01; King County; Washington DP1_0092C  3469
#> 125 53033008402 Census Tract 84.02; King County; Washington DP1_0092C  3012
#>     county year                                       label
#> 1    53033 2020 Count!!HISPANIC OR LATINO!!Total population
#> 2    53033 2020 Count!!HISPANIC OR LATINO!!Total population
#> 3    53033 2020 Count!!HISPANIC OR LATINO!!Total population
#> 4    53033 2020 Count!!HISPANIC OR LATINO!!Total population
#> 5    53033 2020 Count!!HISPANIC OR LATINO!!Total population
#> 6    53033 2020 Count!!HISPANIC OR LATINO!!Total population
#> 7    53033 2020 Count!!HISPANIC OR LATINO!!Total population
#> 8    53033 2020 Count!!HISPANIC OR LATINO!!Total population
#> 9    53033 2020 Count!!HISPANIC OR LATINO!!Total population
#> 10   53033 2020 Count!!HISPANIC OR LATINO!!Total population
#> 11   53033 2020 Count!!HISPANIC OR LATINO!!Total population
#> 12   53033 2020 Count!!HISPANIC OR LATINO!!Total population
#> 13   53033 2020 Count!!HISPANIC OR LATINO!!Total population
#> 14   53033 2020 Count!!HISPANIC OR LATINO!!Total population
#> 15   53033 2020 Count!!HISPANIC OR LATINO!!Total population
#> 16   53033 2020 Count!!HISPANIC OR LATINO!!Total population
#> 17   53033 2020 Count!!HISPANIC OR LATINO!!Total population
#> 18   53033 2020 Count!!HISPANIC OR LATINO!!Total population
#> 19   53033 2020 Count!!HISPANIC OR LATINO!!Total population
#> 20   53033 2020 Count!!HISPANIC OR LATINO!!Total population
#> 21   53033 2020 Count!!HISPANIC OR LATINO!!Total population
#> 22   53033 2020 Count!!HISPANIC OR LATINO!!Total population
#> 23   53033 2020 Count!!HISPANIC OR LATINO!!Total population
#> 24   53033 2020 Count!!HISPANIC OR LATINO!!Total population
#> 25   53033 2020 Count!!HISPANIC OR LATINO!!Total population
#> 26   53033 2020 Count!!HISPANIC OR LATINO!!Total population
#> 27   53033 2020 Count!!HISPANIC OR LATINO!!Total population
#> 28   53033 2020 Count!!HISPANIC OR LATINO!!Total population
#> 29   53033 2020 Count!!HISPANIC OR LATINO!!Total population
#> 30   53033 2020 Count!!HISPANIC OR LATINO!!Total population
#> 31   53033 2020 Count!!HISPANIC OR LATINO!!Total population
#> 32   53033 2020 Count!!HISPANIC OR LATINO!!Total population
#> 33   53033 2020 Count!!HISPANIC OR LATINO!!Total population
#> 34   53033 2020 Count!!HISPANIC OR LATINO!!Total population
#> 35   53033 2020 Count!!HISPANIC OR LATINO!!Total population
#> 36   53033 2020 Count!!HISPANIC OR LATINO!!Total population
#> 37   53033 2020 Count!!HISPANIC OR LATINO!!Total population
#> 38   53033 2020 Count!!HISPANIC OR LATINO!!Total population
#> 39   53033 2020 Count!!HISPANIC OR LATINO!!Total population
#> 40   53033 2020 Count!!HISPANIC OR LATINO!!Total population
#> 41   53033 2020 Count!!HISPANIC OR LATINO!!Total population
#> 42   53033 2020 Count!!HISPANIC OR LATINO!!Total population
#> 43   53033 2020 Count!!HISPANIC OR LATINO!!Total population
#> 44   53033 2020 Count!!HISPANIC OR LATINO!!Total population
#> 45   53033 2020 Count!!HISPANIC OR LATINO!!Total population
#> 46   53033 2020 Count!!HISPANIC OR LATINO!!Total population
#> 47   53033 2020 Count!!HISPANIC OR LATINO!!Total population
#> 48   53033 2020 Count!!HISPANIC OR LATINO!!Total population
#> 49   53033 2020 Count!!HISPANIC OR LATINO!!Total population
#> 50   53033 2020 Count!!HISPANIC OR LATINO!!Total population
#> 51   53033 2020 Count!!HISPANIC OR LATINO!!Total population
#> 52   53033 2020 Count!!HISPANIC OR LATINO!!Total population
#> 53   53033 2020 Count!!HISPANIC OR LATINO!!Total population
#> 54   53033 2020 Count!!HISPANIC OR LATINO!!Total population
#> 55   53033 2020 Count!!HISPANIC OR LATINO!!Total population
#> 56   53033 2020 Count!!HISPANIC OR LATINO!!Total population
#> 57   53033 2020 Count!!HISPANIC OR LATINO!!Total population
#> 58   53033 2020 Count!!HISPANIC OR LATINO!!Total population
#> 59   53033 2020 Count!!HISPANIC OR LATINO!!Total population
#> 60   53033 2020 Count!!HISPANIC OR LATINO!!Total population
#> 61   53033 2020 Count!!HISPANIC OR LATINO!!Total population
#> 62   53033 2020 Count!!HISPANIC OR LATINO!!Total population
#> 63   53033 2020 Count!!HISPANIC OR LATINO!!Total population
#> 64   53033 2020 Count!!HISPANIC OR LATINO!!Total population
#> 65   53033 2020 Count!!HISPANIC OR LATINO!!Total population
#> 66   53033 2020 Count!!HISPANIC OR LATINO!!Total population
#> 67   53033 2020 Count!!HISPANIC OR LATINO!!Total population
#> 68   53033 2020 Count!!HISPANIC OR LATINO!!Total population
#> 69   53033 2020 Count!!HISPANIC OR LATINO!!Total population
#> 70   53033 2020 Count!!HISPANIC OR LATINO!!Total population
#> 71   53033 2020 Count!!HISPANIC OR LATINO!!Total population
#> 72   53033 2020 Count!!HISPANIC OR LATINO!!Total population
#> 73   53033 2020 Count!!HISPANIC OR LATINO!!Total population
#> 74   53033 2020 Count!!HISPANIC OR LATINO!!Total population
#> 75   53033 2020 Count!!HISPANIC OR LATINO!!Total population
#> 76   53033 2020 Count!!HISPANIC OR LATINO!!Total population
#> 77   53033 2020 Count!!HISPANIC OR LATINO!!Total population
#> 78   53033 2020 Count!!HISPANIC OR LATINO!!Total population
#> 79   53033 2020 Count!!HISPANIC OR LATINO!!Total population
#> 80   53033 2020 Count!!HISPANIC OR LATINO!!Total population
#> 81   53033 2020 Count!!HISPANIC OR LATINO!!Total population
#> 82   53033 2020 Count!!HISPANIC OR LATINO!!Total population
#> 83   53033 2020 Count!!HISPANIC OR LATINO!!Total population
#> 84   53033 2020 Count!!HISPANIC OR LATINO!!Total population
#> 85   53033 2020 Count!!HISPANIC OR LATINO!!Total population
#> 86   53033 2020 Count!!HISPANIC OR LATINO!!Total population
#> 87   53033 2020 Count!!HISPANIC OR LATINO!!Total population
#> 88   53033 2020 Count!!HISPANIC OR LATINO!!Total population
#> 89   53033 2020 Count!!HISPANIC OR LATINO!!Total population
#> 90   53033 2020 Count!!HISPANIC OR LATINO!!Total population
#> 91   53033 2020 Count!!HISPANIC OR LATINO!!Total population
#> 92   53033 2020 Count!!HISPANIC OR LATINO!!Total population
#> 93   53033 2020 Count!!HISPANIC OR LATINO!!Total population
#> 94   53033 2020 Count!!HISPANIC OR LATINO!!Total population
#> 95   53033 2020 Count!!HISPANIC OR LATINO!!Total population
#> 96   53033 2020 Count!!HISPANIC OR LATINO!!Total population
#> 97   53033 2020 Count!!HISPANIC OR LATINO!!Total population
#> 98   53033 2020 Count!!HISPANIC OR LATINO!!Total population
#> 99   53033 2020 Count!!HISPANIC OR LATINO!!Total population
#> 100  53033 2020 Count!!HISPANIC OR LATINO!!Total population
#> 101  53033 2020 Count!!HISPANIC OR LATINO!!Total population
#> 102  53033 2020 Count!!HISPANIC OR LATINO!!Total population
#> 103  53033 2020 Count!!HISPANIC OR LATINO!!Total population
#> 104  53033 2020 Count!!HISPANIC OR LATINO!!Total population
#> 105  53033 2020 Count!!HISPANIC OR LATINO!!Total population
#> 106  53033 2020 Count!!HISPANIC OR LATINO!!Total population
#> 107  53033 2020 Count!!HISPANIC OR LATINO!!Total population
#> 108  53033 2020 Count!!HISPANIC OR LATINO!!Total population
#> 109  53033 2020 Count!!HISPANIC OR LATINO!!Total population
#> 110  53033 2020 Count!!HISPANIC OR LATINO!!Total population
#> 111  53033 2020 Count!!HISPANIC OR LATINO!!Total population
#> 112  53033 2020 Count!!HISPANIC OR LATINO!!Total population
#> 113  53033 2020 Count!!HISPANIC OR LATINO!!Total population
#> 114  53033 2020 Count!!HISPANIC OR LATINO!!Total population
#> 115  53033 2020 Count!!HISPANIC OR LATINO!!Total population
#> 116  53033 2020 Count!!HISPANIC OR LATINO!!Total population
#> 117  53033 2020 Count!!HISPANIC OR LATINO!!Total population
#> 118  53033 2020 Count!!HISPANIC OR LATINO!!Total population
#> 119  53033 2020 Count!!HISPANIC OR LATINO!!Total population
#> 120  53033 2020 Count!!HISPANIC OR LATINO!!Total population
#> 121  53033 2020 Count!!HISPANIC OR LATINO!!Total population
#> 122  53033 2020 Count!!HISPANIC OR LATINO!!Total population
#> 123  53033 2020 Count!!HISPANIC OR LATINO!!Total population
#> 124  53033 2020 Count!!HISPANIC OR LATINO!!Total population
#> 125  53033 2020 Count!!HISPANIC OR LATINO!!Total population
#>                                                       concept
#> 1   PROFILE OF GENERAL POPULATION AND HOUSING CHARACTERISTICS
#> 2   PROFILE OF GENERAL POPULATION AND HOUSING CHARACTERISTICS
#> 3   PROFILE OF GENERAL POPULATION AND HOUSING CHARACTERISTICS
#> 4   PROFILE OF GENERAL POPULATION AND HOUSING CHARACTERISTICS
#> 5   PROFILE OF GENERAL POPULATION AND HOUSING CHARACTERISTICS
#> 6   PROFILE OF GENERAL POPULATION AND HOUSING CHARACTERISTICS
#> 7   PROFILE OF GENERAL POPULATION AND HOUSING CHARACTERISTICS
#> 8   PROFILE OF GENERAL POPULATION AND HOUSING CHARACTERISTICS
#> 9   PROFILE OF GENERAL POPULATION AND HOUSING CHARACTERISTICS
#> 10  PROFILE OF GENERAL POPULATION AND HOUSING CHARACTERISTICS
#> 11  PROFILE OF GENERAL POPULATION AND HOUSING CHARACTERISTICS
#> 12  PROFILE OF GENERAL POPULATION AND HOUSING CHARACTERISTICS
#> 13  PROFILE OF GENERAL POPULATION AND HOUSING CHARACTERISTICS
#> 14  PROFILE OF GENERAL POPULATION AND HOUSING CHARACTERISTICS
#> 15  PROFILE OF GENERAL POPULATION AND HOUSING CHARACTERISTICS
#> 16  PROFILE OF GENERAL POPULATION AND HOUSING CHARACTERISTICS
#> 17  PROFILE OF GENERAL POPULATION AND HOUSING CHARACTERISTICS
#> 18  PROFILE OF GENERAL POPULATION AND HOUSING CHARACTERISTICS
#> 19  PROFILE OF GENERAL POPULATION AND HOUSING CHARACTERISTICS
#> 20  PROFILE OF GENERAL POPULATION AND HOUSING CHARACTERISTICS
#> 21  PROFILE OF GENERAL POPULATION AND HOUSING CHARACTERISTICS
#> 22  PROFILE OF GENERAL POPULATION AND HOUSING CHARACTERISTICS
#> 23  PROFILE OF GENERAL POPULATION AND HOUSING CHARACTERISTICS
#> 24  PROFILE OF GENERAL POPULATION AND HOUSING CHARACTERISTICS
#> 25  PROFILE OF GENERAL POPULATION AND HOUSING CHARACTERISTICS
#> 26  PROFILE OF GENERAL POPULATION AND HOUSING CHARACTERISTICS
#> 27  PROFILE OF GENERAL POPULATION AND HOUSING CHARACTERISTICS
#> 28  PROFILE OF GENERAL POPULATION AND HOUSING CHARACTERISTICS
#> 29  PROFILE OF GENERAL POPULATION AND HOUSING CHARACTERISTICS
#> 30  PROFILE OF GENERAL POPULATION AND HOUSING CHARACTERISTICS
#> 31  PROFILE OF GENERAL POPULATION AND HOUSING CHARACTERISTICS
#> 32  PROFILE OF GENERAL POPULATION AND HOUSING CHARACTERISTICS
#> 33  PROFILE OF GENERAL POPULATION AND HOUSING CHARACTERISTICS
#> 34  PROFILE OF GENERAL POPULATION AND HOUSING CHARACTERISTICS
#> 35  PROFILE OF GENERAL POPULATION AND HOUSING CHARACTERISTICS
#> 36  PROFILE OF GENERAL POPULATION AND HOUSING CHARACTERISTICS
#> 37  PROFILE OF GENERAL POPULATION AND HOUSING CHARACTERISTICS
#> 38  PROFILE OF GENERAL POPULATION AND HOUSING CHARACTERISTICS
#> 39  PROFILE OF GENERAL POPULATION AND HOUSING CHARACTERISTICS
#> 40  PROFILE OF GENERAL POPULATION AND HOUSING CHARACTERISTICS
#> 41  PROFILE OF GENERAL POPULATION AND HOUSING CHARACTERISTICS
#> 42  PROFILE OF GENERAL POPULATION AND HOUSING CHARACTERISTICS
#> 43  PROFILE OF GENERAL POPULATION AND HOUSING CHARACTERISTICS
#> 44  PROFILE OF GENERAL POPULATION AND HOUSING CHARACTERISTICS
#> 45  PROFILE OF GENERAL POPULATION AND HOUSING CHARACTERISTICS
#> 46  PROFILE OF GENERAL POPULATION AND HOUSING CHARACTERISTICS
#> 47  PROFILE OF GENERAL POPULATION AND HOUSING CHARACTERISTICS
#> 48  PROFILE OF GENERAL POPULATION AND HOUSING CHARACTERISTICS
#> 49  PROFILE OF GENERAL POPULATION AND HOUSING CHARACTERISTICS
#> 50  PROFILE OF GENERAL POPULATION AND HOUSING CHARACTERISTICS
#> 51  PROFILE OF GENERAL POPULATION AND HOUSING CHARACTERISTICS
#> 52  PROFILE OF GENERAL POPULATION AND HOUSING CHARACTERISTICS
#> 53  PROFILE OF GENERAL POPULATION AND HOUSING CHARACTERISTICS
#> 54  PROFILE OF GENERAL POPULATION AND HOUSING CHARACTERISTICS
#> 55  PROFILE OF GENERAL POPULATION AND HOUSING CHARACTERISTICS
#> 56  PROFILE OF GENERAL POPULATION AND HOUSING CHARACTERISTICS
#> 57  PROFILE OF GENERAL POPULATION AND HOUSING CHARACTERISTICS
#> 58  PROFILE OF GENERAL POPULATION AND HOUSING CHARACTERISTICS
#> 59  PROFILE OF GENERAL POPULATION AND HOUSING CHARACTERISTICS
#> 60  PROFILE OF GENERAL POPULATION AND HOUSING CHARACTERISTICS
#> 61  PROFILE OF GENERAL POPULATION AND HOUSING CHARACTERISTICS
#> 62  PROFILE OF GENERAL POPULATION AND HOUSING CHARACTERISTICS
#> 63  PROFILE OF GENERAL POPULATION AND HOUSING CHARACTERISTICS
#> 64  PROFILE OF GENERAL POPULATION AND HOUSING CHARACTERISTICS
#> 65  PROFILE OF GENERAL POPULATION AND HOUSING CHARACTERISTICS
#> 66  PROFILE OF GENERAL POPULATION AND HOUSING CHARACTERISTICS
#> 67  PROFILE OF GENERAL POPULATION AND HOUSING CHARACTERISTICS
#> 68  PROFILE OF GENERAL POPULATION AND HOUSING CHARACTERISTICS
#> 69  PROFILE OF GENERAL POPULATION AND HOUSING CHARACTERISTICS
#> 70  PROFILE OF GENERAL POPULATION AND HOUSING CHARACTERISTICS
#> 71  PROFILE OF GENERAL POPULATION AND HOUSING CHARACTERISTICS
#> 72  PROFILE OF GENERAL POPULATION AND HOUSING CHARACTERISTICS
#> 73  PROFILE OF GENERAL POPULATION AND HOUSING CHARACTERISTICS
#> 74  PROFILE OF GENERAL POPULATION AND HOUSING CHARACTERISTICS
#> 75  PROFILE OF GENERAL POPULATION AND HOUSING CHARACTERISTICS
#> 76  PROFILE OF GENERAL POPULATION AND HOUSING CHARACTERISTICS
#> 77  PROFILE OF GENERAL POPULATION AND HOUSING CHARACTERISTICS
#> 78  PROFILE OF GENERAL POPULATION AND HOUSING CHARACTERISTICS
#> 79  PROFILE OF GENERAL POPULATION AND HOUSING CHARACTERISTICS
#> 80  PROFILE OF GENERAL POPULATION AND HOUSING CHARACTERISTICS
#> 81  PROFILE OF GENERAL POPULATION AND HOUSING CHARACTERISTICS
#> 82  PROFILE OF GENERAL POPULATION AND HOUSING CHARACTERISTICS
#> 83  PROFILE OF GENERAL POPULATION AND HOUSING CHARACTERISTICS
#> 84  PROFILE OF GENERAL POPULATION AND HOUSING CHARACTERISTICS
#> 85  PROFILE OF GENERAL POPULATION AND HOUSING CHARACTERISTICS
#> 86  PROFILE OF GENERAL POPULATION AND HOUSING CHARACTERISTICS
#> 87  PROFILE OF GENERAL POPULATION AND HOUSING CHARACTERISTICS
#> 88  PROFILE OF GENERAL POPULATION AND HOUSING CHARACTERISTICS
#> 89  PROFILE OF GENERAL POPULATION AND HOUSING CHARACTERISTICS
#> 90  PROFILE OF GENERAL POPULATION AND HOUSING CHARACTERISTICS
#> 91  PROFILE OF GENERAL POPULATION AND HOUSING CHARACTERISTICS
#> 92  PROFILE OF GENERAL POPULATION AND HOUSING CHARACTERISTICS
#> 93  PROFILE OF GENERAL POPULATION AND HOUSING CHARACTERISTICS
#> 94  PROFILE OF GENERAL POPULATION AND HOUSING CHARACTERISTICS
#> 95  PROFILE OF GENERAL POPULATION AND HOUSING CHARACTERISTICS
#> 96  PROFILE OF GENERAL POPULATION AND HOUSING CHARACTERISTICS
#> 97  PROFILE OF GENERAL POPULATION AND HOUSING CHARACTERISTICS
#> 98  PROFILE OF GENERAL POPULATION AND HOUSING CHARACTERISTICS
#> 99  PROFILE OF GENERAL POPULATION AND HOUSING CHARACTERISTICS
#> 100 PROFILE OF GENERAL POPULATION AND HOUSING CHARACTERISTICS
#> 101 PROFILE OF GENERAL POPULATION AND HOUSING CHARACTERISTICS
#> 102 PROFILE OF GENERAL POPULATION AND HOUSING CHARACTERISTICS
#> 103 PROFILE OF GENERAL POPULATION AND HOUSING CHARACTERISTICS
#> 104 PROFILE OF GENERAL POPULATION AND HOUSING CHARACTERISTICS
#> 105 PROFILE OF GENERAL POPULATION AND HOUSING CHARACTERISTICS
#> 106 PROFILE OF GENERAL POPULATION AND HOUSING CHARACTERISTICS
#> 107 PROFILE OF GENERAL POPULATION AND HOUSING CHARACTERISTICS
#> 108 PROFILE OF GENERAL POPULATION AND HOUSING CHARACTERISTICS
#> 109 PROFILE OF GENERAL POPULATION AND HOUSING CHARACTERISTICS
#> 110 PROFILE OF GENERAL POPULATION AND HOUSING CHARACTERISTICS
#> 111 PROFILE OF GENERAL POPULATION AND HOUSING CHARACTERISTICS
#> 112 PROFILE OF GENERAL POPULATION AND HOUSING CHARACTERISTICS
#> 113 PROFILE OF GENERAL POPULATION AND HOUSING CHARACTERISTICS
#> 114 PROFILE OF GENERAL POPULATION AND HOUSING CHARACTERISTICS
#> 115 PROFILE OF GENERAL POPULATION AND HOUSING CHARACTERISTICS
#> 116 PROFILE OF GENERAL POPULATION AND HOUSING CHARACTERISTICS
#> 117 PROFILE OF GENERAL POPULATION AND HOUSING CHARACTERISTICS
#> 118 PROFILE OF GENERAL POPULATION AND HOUSING CHARACTERISTICS
#> 119 PROFILE OF GENERAL POPULATION AND HOUSING CHARACTERISTICS
#> 120 PROFILE OF GENERAL POPULATION AND HOUSING CHARACTERISTICS
#> 121 PROFILE OF GENERAL POPULATION AND HOUSING CHARACTERISTICS
#> 122 PROFILE OF GENERAL POPULATION AND HOUSING CHARACTERISTICS
#> 123 PROFILE OF GENERAL POPULATION AND HOUSING CHARACTERISTICS
#> 124 PROFILE OF GENERAL POPULATION AND HOUSING CHARACTERISTICS
#> 125 PROFILE OF GENERAL POPULATION AND HOUSING CHARACTERISTICS
#>  [ reached 'max' / getOption("max.print") -- omitted 798 rows ]
```

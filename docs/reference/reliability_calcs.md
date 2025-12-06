# Add estimate reliability metrics

Add coefficient of variation (cv) and relative reliability rating to
estimates Census ACS uses 90 percent confidence interval, i.e. z score
1.645
http://aws-linux/mediawiki/index.php/Understanding_Error_and_Determining_Statistical_Significance

## Usage

``` r
reliability_calcs(df, estimate = "estimate", moe = "moe")
```

## Arguments

- df:

  dataframe or data.table already retrieved from the api, or created by
  a user

- estimate:

  character name of the column that has contains data value estimates

- moe:

  character name of the column that has contains margin of error
  estimates

## Value

the data frames with new columns for se, cv, and reliability

## Author

Suzanne Childress

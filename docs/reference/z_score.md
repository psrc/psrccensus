# Z Score

Stat to determine if two estimates are different

## Usage

``` r
z_score(x, y)
```

## Arguments

- x:

  numeric vector of length 2, or a two-column matrix/data frame,
  containing the first estimate and corresponding MOE to compare

- y:

  numeric vector of length 2, or a two-column matrix/data frame,
  containing the second estimate and corresponding MOE to compare

## Value

Z score; if larger than 1, difference is significant. Returns a numeric
vector when \`x\` and \`y\` contain multiple estimate/MOE pairs.

## Author

Michael Jensen

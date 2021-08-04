testthat::test_that('get_deccenial_recs returns expected results', {
  df <- mycensus::get_decennial_recs(geography = 'county',
                                     table_codes = 'PCT013',
                                     year = 2010)

  # default counties are formatted with 'County, Washington'
  testthat::expect_equal(unique(df$NAME), c('King County, Washington', 'Kitsap County, Washington', 'Pierce County, Washington', 'Snohomish County, Washington'))

  # default columns from tidycensus are the follow six variables
  testthat::expect_identical(colnames(df), c('GEOID', 'NAME', 'variable', 'value', 'label', 'concept'))

})

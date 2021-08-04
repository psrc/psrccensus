testthat::test_that('get_deccenial_recs inputs are functional', {

  # geography arguments are case sensitive
  testthat::expect_error(get_decennial_recs(geography = 'Place',
                                            table_codes = 'PCT013',
                                            year = 2010,
                                            fips = c("5363000", "5308850")))

})


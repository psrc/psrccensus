context("get-acs-recs")

test_that('get-acs-recs returns a df with correctly specified inputs',
          {expect_s3_class(get_acs_recs(geography = 'county',
                                        table.names = c('B03002',"C17002"),
                                        years=c(2017,2019),
                                        acs.type = 'acs1'), 'data.frame')})

show_failure(expect_s3_class(get_acs_recs(geography = 'county',
                                          table.names = c('B03002',"C17002"),
                                          years=c(2017,2019),
                                          acs.type = 'acs1'), 'data.frame'))

test_that('bad function call throws an error',{expect_error(get_acs_recs(geography='msa',
                                                            table.names=('RutabagaBoo'),
                                                            years=2018,
                                                            acs.type = 'acs1'))})

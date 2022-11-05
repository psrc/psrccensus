## ----setup, echo = FALSE, message = FALSE-------------------------------------
knitr::opts_chunk$set(collapse = TRUE, comment = "#>")
library(psrccensus)
library(magrittr)
library(knitr)

## ----median, message=FALSE, eval=FALSE----------------------------------------
#  x <- get_psrc_pums(span = 5,                     # Denoting ACS 5-year estimates; 1-year also available
#                     dyear = 2019,                 # Last data year of span
#                     level = "h",                  # Unit of analysis == household ("p" used for person)
#                     vars = c("HINCP","TEN"))      # PUMS household income & housing tenure variables; using tenure later.
#                                                   #  - You can choose as many variables as you need.
#  psrc_pums_median(x, stat_var = "HINCP")          # Median Household income (without regard to tenure)

## ----median by category, message=FALSE, eval=FALSE----------------------------
#  psrc_pums_median(x,                              # the assigned result of get_psrc_pums()
#                   stat_var = "HINCP",             # Summarizing household income . . .
#                   group_vars = "TEN")             # . . . grouped by housing tenure

## ----count by category, message=FALSE, eval=FALSE-----------------------------
#  psrc_pums_count(x, group_vars=c("COUNTY","TEN"))      # If omitting stat_var argument, group_var must be labeled

## ----summary by category, message=FALSE, eval=FALSE---------------------------
#  psrc_pums_summary(x, "HINCP", "TEN") %>% print()

## ----z score calculation, message=FALSE, eval=FALSE---------------------------
#  rs <- psrc_pums_median(x, "HINCP", "TEN")
#  c1 <- rs[1,(ncol(rs)-1):ncol(rs)]                # First-line estimate and its MOE (as numeric vector)
#  c2 <- rs[3,(ncol(rs)-1):ncol(rs)]                # Third-line estimate and its MOE (as numeric vector)
#  z_score(c1, c2)                                  # A true difference indicated by score > 1
#  


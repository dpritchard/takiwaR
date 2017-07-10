context("se") # Tests for the standard error function...
dat <- c(0,0,0,0,9,18,18,18,18)
dat_na <- c(dat, NA)
expect_equal(se(dat), 3)
expect_true(is.na(se(dat_na)))
expect_equal(se(dat_na, na.rm=TRUE), 3)
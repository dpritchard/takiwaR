context("compare") # Tests for the "compare" function

dat1 <- c(1,1,1,1,1)
dat2 <- c(1,2,1,1,1)

expect_true(compare(dat1))
expect_false(compare(dat2))

context("is_zero") # Tests for is_zero
expect_true(is_zero(0))
expect_true(is_zero(0L))
expect_false(is_zero(1e-7))
expect_true(is_zero(round(1e-7,0)))
expect_true(is_zero(1e-16))
expect_false(is_zero(1e-16, strict_int = TRUE))
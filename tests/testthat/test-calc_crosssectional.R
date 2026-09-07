test_that("calc_crosssectional_binary matches Bhardwaj et al. (2024), raw n", {
  # Worked example: prevalence 30\%, 5\% absolute precision, 95\% CI
  # Paper reports raw n = 323 (before dropout adjustment).
  result <- calc_crosssectional_binary(p = 0.30, precision = 0.05,
                                       dropout_rate = 0.10)
  expect_equal(result$n_raw, 323)
})

test_that("calc_crosssectional_binary dropout-adjusted n uses the correct formula", {
  # NOTE: the paper's own worked example reports 355 here, using the
  # multiplicative approximation n * (1 + d). This package uses the
  # statistically correct n / (1 - d), which gives a slightly larger,
  # more conservative number. See the Validation vignette for details.
  result <- calc_crosssectional_binary(p = 0.30, precision = 0.05,
                                       dropout_rate = 0.10)
  expect_equal(result$n_final, 359)
})

test_that("calc_crosssectional_continuous matches Bhardwaj et al. (2024), raw n", {
  # Worked SBP example: SD = 3 mmHg, precision = 0.5 mmHg, 95\% CI.
  # NOTE: the paper reports 138 here, by truncating 138.29 instead of
  # rounding up. This package rounds up (the conventional, more
  # conservative rule for sample size), giving 139.
  result <- calc_crosssectional_continuous(mean = 120, sd = 3,
                                           precision = 0.5,
                                           dropout_rate = 0.10)
  expect_equal(result$n_raw, 139)
})

test_that("calc_crosssectional_continuous dropout-adjusted n uses the correct formula", {
  # Same n / (1 - d) rationale as above. Paper reports 152.
  result <- calc_crosssectional_continuous(mean = 120, sd = 3,
                                           precision = 0.5,
                                           dropout_rate = 0.10)
  expect_equal(result$n_final, 155)
})

test_that("calc_crosssectional_binary rejects invalid inputs", {
  expect_error(calc_crosssectional_binary(p = 1.5, precision = 0.05))
  expect_error(calc_crosssectional_binary(p = 0.3, precision = 0.05,
                                          error_type = "typo"))
})

test_that("calc_crosssectional_continuous rejects invalid inputs", {
  expect_error(calc_crosssectional_continuous(mean = 120, sd = -1,
                                              precision = 0.5))
  expect_error(calc_crosssectional_continuous(mean = 120, sd = 3,
                                              precision = -0.5))
})

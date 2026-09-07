test_that("calc_diagnostic_accuracy matches epiR's Buderer-method output", {
  result <- calc_diagnostic_accuracy(expected_sensitivity = 0.80,
                                     expected_specificity = 0.90,
                                     prevalence = 0.20,
                                     precision = 0.05)
  expect_equal(result$n_sensitivity, 1230)
  expect_equal(result$n_specificity, 173)

  # NOTE: Bhardwaj et al. (2024) report a total of 1401 here by
  # summing se.n + sp.n, treating them as two independent samples.
  # This package instead takes the larger of the two (1230), following
  # Buderer (1996) / Hajian-Tilaki (2014): the same subjects are used
  # to estimate both sensitivity and specificity at once, so no
  # summing is needed. See the package Validation vignette.
  expect_equal(result$n_raw, 1230)
})

test_that("calc_diagnostic_accuracy rejects invalid inputs", {
  expect_error(calc_diagnostic_accuracy(expected_sensitivity = 1.5,
                                        expected_specificity = 0.90,
                                        prevalence = 0.20, precision = 0.05))
  expect_error(calc_diagnostic_accuracy(expected_sensitivity = 0.80,
                                        expected_specificity = 0.90,
                                        prevalence = 1.5, precision = 0.05))
})

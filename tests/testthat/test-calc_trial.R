test_that("calc_trial_superiority (one-sided) is close to Bhardwaj et al. (2024)", {
  # Paper reports 1704 total, using a one-sided test and a normal
  # approximation formula. epi.sssupb() uses a slightly different
  # (more precise) method -- a difference of a few percent is
  # expected. See package Validation vignette.
  result <- calc_trial_superiority(p_standard = 0.45, p_new = 0.61,
                                   delta = 0.10, sided_test = 1)
  expect_equal(result$n_raw, 1668)
})

test_that("calc_trial_superiority (two-sided, package default) gives a larger n than one-sided", {
  # Two-sided is more conservative and should always require a
  # larger (or equal) sample size than one-sided, for the same
  # parameters.
  one_sided <- calc_trial_superiority(p_standard = 0.45, p_new = 0.61,
                                      delta = 0.10, sided_test = 1)
  two_sided <- calc_trial_superiority(p_standard = 0.45, p_new = 0.61,
                                      delta = 0.10, sided_test = 2)
  expect_gt(two_sided$n_raw, one_sided$n_raw)
})

test_that("calc_trial_noninferiority matches Bhardwaj et al. (2024) closely", {
  # Paper reports 614 total (307 per group). epi.ssninfb() is
  # inherently one-sided (no sided_test argument), matching the
  # paper's own convention exactly, so this should match closely.
  result <- calc_trial_noninferiority(p_standard = 0.45, p_new = 0.45,
                                      delta = 0.10)
  expect_equal(result$n_raw, 614)
})

test_that("calc_trial_equivalence is close to Bhardwaj et al. (2024)", {
  # Paper reports 776 total (388 per group). epi.ssequb() is
  # inherently two-sided, matching the paper's convention.
  result <- calc_trial_equivalence(p_standard = 0.45, p_new = 0.45,
                                   delta = 0.10)
  expect_equal(result$n_raw, 848)
})

test_that("trial functions reject invalid inputs", {
  expect_error(calc_trial_superiority(p_standard = 1.5, p_new = 0.61,
                                      delta = 0.10))
  expect_error(calc_trial_superiority(p_standard = 0.45, p_new = 0.61,
                                      delta = -0.1))
  expect_error(calc_trial_superiority(p_standard = 0.45, p_new = 0.61,
                                      delta = 0.10, sided_test = 3))
  expect_error(calc_trial_noninferiority(p_standard = 0.45, p_new = 1.5,
                                         delta = 0.10))
  expect_error(calc_trial_equivalence(p_standard = 0.45, p_new = 0.45,
                                      delta = -0.1))
})

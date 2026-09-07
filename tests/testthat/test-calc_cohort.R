test_that("calc_cohort raw n is close to Bhardwaj et al. (2024)", {
  # Paper's normal-approximation formula gives 293 per group (586 total).
  # epi.sscohortc() uses the more precise Woodward (2014) method,
  # giving a very close but not identical total. Small differences
  # (a few subjects) are expected -- see package Validation vignette.
  result <- calc_cohort(incidence_unexposed = 0.20,
                        incidence_exposed = 0.30,
                        dropout_rate = 0.10)
  expect_equal(result$n_raw, 588)
})

test_that("calc_cohort dropout-adjusted n uses the correct formula", {
  result <- calc_cohort(incidence_unexposed = 0.20,
                        incidence_exposed = 0.30,
                        dropout_rate = 0.10)
  expect_equal(result$n_final, 654)
})

test_that("calc_cohort rejects invalid inputs", {
  expect_error(calc_cohort(incidence_unexposed = 1.5,
                           incidence_exposed = 0.30))
  expect_error(calc_cohort(incidence_unexposed = 0.20,
                           incidence_exposed = 0.30,
                           exposed_unexposed_ratio = 0.5))
})

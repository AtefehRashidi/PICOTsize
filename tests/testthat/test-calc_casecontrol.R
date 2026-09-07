test_that("calc_casecontrol derives the correct odds ratio", {
  result <- calc_casecontrol(p_exposed_controls = 0.25,
                             p_exposed_cases = 0.40,
                             dropout_rate = 0.10)
  expect_equal(result$or_derived, 2)
})

test_that("calc_casecontrol raw n is close to Bhardwaj et al. (2024)", {
  # Paper's normal-approximation formula gives 153 per group (306 total).
  # epi.sscc() uses the more precise Dupont (1988) method, giving a
  # slightly different (here, slightly smaller) total. A difference of
  # a few subjects is expected and not a bug -- see package Validation
  # vignette for the full comparison.
  result <- calc_casecontrol(p_exposed_controls = 0.25,
                             p_exposed_cases = 0.40,
                             dropout_rate = 0.10)
  expect_equal(result$n_raw, 304)
})

test_that("calc_casecontrol dropout-adjusted n uses the correct formula", {
  result <- calc_casecontrol(p_exposed_controls = 0.25,
                             p_exposed_cases = 0.40,
                             dropout_rate = 0.10)
  expect_equal(result$n_final, 338)
})

test_that("calc_casecontrol rejects invalid inputs", {
  expect_error(calc_casecontrol(p_exposed_controls = 1.5,
                                p_exposed_cases = 0.40))
  expect_error(calc_casecontrol(p_exposed_controls = 0.25,
                                p_exposed_cases = 0.40,
                                control_case_ratio = 0.5))
})

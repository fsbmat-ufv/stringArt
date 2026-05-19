test_that("stcircle retorna uma lista invisível com os pregos", {
  res <- stcircle(n = 10, k = 2, r = 1)

  expect_type(res, "list")
  expect_true("pregos" %in% names(res))
  expect_equal(nrow(res$pregos), 10)
})

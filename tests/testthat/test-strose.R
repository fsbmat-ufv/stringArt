test_that("strose runs without mandatory arguments", {
  expect_error(res <- strose(plot = FALSE), NA)
  expect_type(res, "list")
})

test_that("strose returns standardized output", {
  res <- strose(petals = 8, plot = FALSE)
  expect_true(all(c("pegs", "connections", "total_length", "audit", "meta") %in% names(res)))
})

test_that("strose returns valid tables", {
  res <- strose(n = 120, k = 11, petals = 6, plot = FALSE)
  expect_equal(nrow(res$pegs), 120)
  expect_equal(nrow(res$connections), 120)
  expect_true(res$total_length > 0)
})

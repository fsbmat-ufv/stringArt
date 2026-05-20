test_that("stspiral runs without mandatory arguments", {
  expect_error(res <- stspiral(plot = FALSE), NA)
  expect_type(res, "list")
})

test_that("stspiral returns standardized output", {
  res <- stspiral(turns = 4, plot = FALSE)
  expect_true(all(c("pegs", "connections", "total_length", "audit", "meta") %in% names(res)))
})

test_that("stspiral returns valid tables", {
  res <- stspiral(n = 100, k = 9, plot = FALSE)
  expect_equal(nrow(res$pegs), 100)
  expect_equal(nrow(res$connections), 100)
  expect_true(res$total_length > 0)
})

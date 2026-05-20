test_that("stlissajous runs without mandatory arguments", {
  expect_error(res <- stlissajous(plot = FALSE), NA)
  expect_type(res, "list")
})

test_that("stlissajous returns standardized output", {
  res <- stlissajous(a = 5, b = 4, plot = FALSE)
  expect_true(all(c("pegs", "connections", "total_length", "audit", "meta") %in% names(res)))
})

test_that("stlissajous returns valid tables", {
  res <- stlissajous(n = 120, k = 9, plot = FALSE)
  expect_equal(nrow(res$pegs), 120)
  expect_equal(nrow(res$connections), 120)
  expect_true(res$total_length > 0)
})

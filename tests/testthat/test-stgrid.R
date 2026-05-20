test_that("stgrid runs without mandatory arguments", {
  expect_error(res <- stgrid(plot = FALSE), NA)
  expect_type(res, "list")
})

test_that("stgrid returns standardized output", {
  res <- stgrid(width = 2, height = 1, plot = FALSE)
  expect_true(all(c("pegs", "connections", "total_length", "audit", "meta") %in% names(res)))
})

test_that("stgrid returns valid tables", {
  res <- stgrid(n = 40, k = 5, plot = FALSE)
  expect_equal(nrow(res$pegs), 40)
  expect_equal(nrow(res$connections), 40)
  expect_true(res$total_length > 0)
})

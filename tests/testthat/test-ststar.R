test_that("ststar runs without mandatory arguments", {
  expect_error(res <- ststar(plot = FALSE), NA)
  expect_type(res, "list")
})

test_that("ststar returns standardized output", {
  res <- ststar(n = 7, k = 2, plot = FALSE)
  expect_true(all(c("pegs", "connections", "total_length", "audit", "meta") %in% names(res)))
})

test_that("ststar returns valid tables", {
  res <- ststar(n = 8, k = 3, plot = FALSE)
  expect_equal(nrow(res$pegs), 8)
  expect_equal(nrow(res$connections), 8)
  expect_true(res$meta$parameters$number_of_cycles >= 1)
  expect_true(res$meta$parameters$cycle_length >= 1)
})

test_that("ststar does not return Portuguese aliases", {
  res <- ststar(plot = FALSE)
  expect_false("pregos" %in% names(res))
  expect_false("conexoes" %in% names(res))
  expect_false("comprimento_total" %in% names(res))
})

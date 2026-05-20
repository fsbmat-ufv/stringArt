test_that("stpolygon runs without mandatory arguments", {
  expect_error(res <- stpolygon(plot = FALSE), NA)
  expect_type(res, "list")
})

test_that("stpolygon returns a standardized object", {
  res <- stpolygon(n = 30, k = 4, sides = 5, plot = FALSE)

  expect_true(all(c(
    "pegs", "connections", "total_length", "audit", "meta"
  ) %in% names(res)))
})

test_that("stpolygon returns valid tables", {
  res <- stpolygon(n = 30, k = 4, sides = 6, plot = FALSE)

  expect_true(is.data.frame(res$pegs))
  expect_true(is.data.frame(res$connections))
  expect_equal(nrow(res$pegs), 30)
  expect_equal(nrow(res$connections), 30)
  expect_true(all(c("index", "x", "y") %in% names(res$pegs)))
  expect_true(all(c(
    "connection_index", "from", "to", "x_from", "y_from", "x_to", "y_to", "length"
  ) %in% names(res$connections)))
  expect_true(res$total_length > 0)
})

test_that("stpolygon does not return Portuguese aliases", {
  res <- stpolygon(plot = FALSE)
  expect_false("pregos" %in% names(res))
  expect_false("conexoes" %in% names(res))
  expect_false("comprimento_total" %in% names(res))
})

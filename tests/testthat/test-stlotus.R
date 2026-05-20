test_that("stlotus runs without mandatory arguments", {
  expect_error(
    res <- stlotus(plot = FALSE),
    NA
  )

  expect_type(res, "list")
})

test_that("stlotus returns a standardized object", {
  res <- stlotus(n = 12, k = 5, petals = 5, plot = FALSE)

  expect_true(all(c(
    "pegs",
    "connections",
    "total_length",
    "audit",
    "meta"
  ) %in% names(res)))
})

test_that("stlotus returns valid tables", {
  res <- stlotus(n = 12, k = 5, petals = 5, plot = FALSE)

  expect_true(is.data.frame(res$pegs))
  expect_true(is.data.frame(res$connections))

  expect_equal(nrow(res$pegs), 12 * (5 + 2))
  expect_equal(nrow(res$connections), 12 * (5 + 2))

  expect_true(all(c("index", "x", "y") %in% names(res$pegs)))
  expect_true(all(c(
    "connection_index", "from", "to",
    "x_from", "y_from", "x_to", "y_to", "length"
  ) %in% names(res$connections)))
})

test_that("stlotus does not return Portuguese aliases", {
  res <- stlotus(plot = FALSE)

  expect_false("pregos" %in% names(res))
  expect_false("conexoes" %in% names(res))
  expect_false("comprimento_total" %in% names(res))
})

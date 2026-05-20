test_that("stparabola runs without mandatory arguments", {
  expect_error(
    res <- stparabola(plot = FALSE),
    NA
  )

  expect_type(res, "list")
})

test_that("stparabola returns a standardized object", {
  res <- stparabola(n = 12, k = 1, plot = FALSE)

  expect_type(res, "list")

  expect_true(all(c(
    "pegs",
    "connections",
    "total_length",
    "audit",
    "meta"
  ) %in% names(res)))
})

test_that("stparabola returns a valid pegs table", {
  res <- stparabola(n = 12, k = 1, plot = FALSE)

  expect_true(is.data.frame(res$pegs))
  expect_equal(nrow(res$pegs), 24)

  expect_true(all(c(
    "index",
    "x",
    "y"
  ) %in% names(res$pegs)))

  expect_true(all(c(
    "axis",
    "local_index"
  ) %in% names(res$pegs)))
})

test_that("stparabola returns a valid connections table", {
  res <- stparabola(n = 12, k = 3, plot = FALSE)

  expect_true(is.data.frame(res$connections))
  expect_equal(nrow(res$connections), 36)

  expect_true(all(c(
    "connection_index",
    "from",
    "to",
    "x_from",
    "y_from",
    "x_to",
    "y_to",
    "length"
  ) %in% names(res$connections)))

  expect_true(all(c(
    "sweep",
    "offset",
    "local_from",
    "local_to"
  ) %in% names(res$connections)))

  expect_true(is.numeric(res$total_length))
  expect_equal(length(res$total_length), 1)
  expect_true(res$total_length > 0)
})

test_that("stparabola supports template mode", {
  expect_error(
    res <- stparabola(template = TRUE, plot = FALSE),
    NA
  )

  expect_type(res, "list")
  expect_true(isTRUE(res$meta$parameters$template))
  expect_false(isTRUE(res$meta$parameters$show_strings))
})

test_that("stparabola does not return Portuguese aliases", {
  res <- stparabola(n = 12, k = 1, plot = FALSE)

  expect_false("pregos" %in% names(res))
  expect_false("conexoes" %in% names(res))
  expect_false("comprimento_total" %in% names(res))
})

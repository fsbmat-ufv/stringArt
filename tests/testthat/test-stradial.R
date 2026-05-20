test_that("stradial runs without mandatory arguments", {
  expect_error(
    res <- stradial(plot = FALSE),
    NA
  )

  expect_type(res, "list")
})

test_that("stradial returns a standardized object", {
  res <- stradial(n = 12, k = 4, m = 3, plot = FALSE)

  expect_type(res, "list")

  expect_true(all(c(
    "pegs",
    "connections",
    "total_length",
    "audit",
    "meta"
  ) %in% names(res)))
})

test_that("stradial returns a valid pegs table", {
  res <- stradial(n = 12, k = 4, m = 3, plot = FALSE)

  expect_true(is.data.frame(res$pegs))
  expect_equal(nrow(res$pegs), 36)

  expect_true(all(c(
    "index",
    "x",
    "y",
    "module",
    "local_index"
  ) %in% names(res$pegs)))
})

test_that("stradial returns a valid connections table", {
  res <- stradial(n = 12, k = 4, m = 3, plot = FALSE)

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
    "length",
    "module",
    "local_from",
    "local_to",
    "color"
  ) %in% names(res$connections)))

  expect_true(is.numeric(res$total_length))
  expect_equal(length(res$total_length), 1)
  expect_true(res$total_length > 0)
})

test_that("stradial supports template mode", {
  expect_error(
    res <- stradial(n = 12, k = 4, m = 3, template = TRUE, plot = FALSE),
    NA
  )

  expect_true(is.data.frame(res$pegs))
  expect_true(is.data.frame(res$connections))
})

test_that("stradial does not return Portuguese aliases", {
  res <- stradial(n = 12, k = 4, m = 3, plot = FALSE)

  expect_false("pregos" %in% names(res))
  expect_false("conexoes" %in% names(res))
  expect_false("comprimento_total" %in% names(res))
})

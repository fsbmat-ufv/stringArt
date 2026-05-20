test_that("sthexaflower runs without mandatory arguments", {
  expect_error(
    res <- sthexaflower(plot = FALSE),
    NA
  )

  expect_type(res, "list")
})

test_that("sthexaflower returns a standardized object", {
  res <- sthexaflower(n = 12, k = 2, plot = FALSE)

  expect_type(res, "list")

  expect_true(all(c(
    "pegs",
    "connections",
    "total_length",
    "audit",
    "meta"
  ) %in% names(res)))
})

test_that("sthexaflower returns a valid pegs table", {
  res <- sthexaflower(n = 12, k = 2, plot = FALSE)

  expect_true(is.data.frame(res$pegs))
  expect_equal(nrow(res$pegs), 3 * 12 + 1)

  expect_true(all(c(
    "index",
    "x",
    "y",
    "group",
    "layer",
    "local_index"
  ) %in% names(res$pegs)))
})

test_that("sthexaflower returns a valid connections table", {
  res <- sthexaflower(n = 12, k = 2, plot = FALSE)

  expect_true(is.data.frame(res$connections))
  expect_equal(nrow(res$connections), 3 * 12 + 6)

  expect_true(all(c(
    "connection_index",
    "from",
    "to",
    "x_from",
    "y_from",
    "x_to",
    "y_to",
    "length",
    "block",
    "sector"
  ) %in% names(res$connections)))

  expect_true(is.numeric(res$total_length))
  expect_equal(length(res$total_length), 1)
  expect_true(res$total_length > 0)
})

test_that("sthexaflower does not return Portuguese aliases", {
  res <- sthexaflower(n = 12, k = 2, plot = FALSE)

  expect_false("pregos" %in% names(res))
  expect_false("conexoes" %in% names(res))
  expect_false("comprimento_total" %in% names(res))
})

test_that("sthexaflower validates n as a multiple of 6", {
  expect_error(sthexaflower(n = 10, k = 2, plot = FALSE), "multiple of 6")
})

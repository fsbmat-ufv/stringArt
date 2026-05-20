test_that("stnet runs without mandatory arguments", {
  expect_error(
    res <- stnet(plot = FALSE),
    NA
  )

  expect_type(res, "list")
})

test_that("stnet returns a standardized object", {
  res <- stnet(n = 12, k = 1, plot = FALSE)

  expect_type(res, "list")

  expect_true(all(c(
    "pegs",
    "connections",
    "total_length",
    "audit",
    "meta"
  ) %in% names(res)))
})

test_that("stnet returns a valid pegs table", {
  res <- stnet(n = 12, k = 1, plot = FALSE)

  expect_true(is.data.frame(res$pegs))
  expect_equal(nrow(res$pegs), 24)

  expect_true(all(c(
    "index",
    "x",
    "y"
  ) %in% names(res$pegs)))

  expect_true(all(c(
    "ray",
    "local_index"
  ) %in% names(res$pegs)))
})

test_that("stnet returns a valid connections table", {
  res <- stnet(n = 12, k = 3, plot = FALSE)

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

test_that("stnet supports different angles", {
  expect_error(
    res <- stnet(n = 12, k = 2, angle = 2 * pi / 3, plot = FALSE),
    NA
  )

  expect_type(res, "list")
  expect_equal(res$meta$parameters$angle, 2 * pi / 3)
})

test_that("stnet supports template mode", {
  expect_error(
    res <- stnet(template = TRUE, plot = FALSE),
    NA
  )

  expect_type(res, "list")
  expect_true(isTRUE(res$meta$parameters$template))
  expect_false(isTRUE(res$meta$parameters$show_strings))
})

test_that("stnet does not return Portuguese aliases", {
  res <- stnet(n = 12, k = 1, plot = FALSE)

  expect_false("pregos" %in% names(res))
  expect_false("conexoes" %in% names(res))
  expect_false("comprimento_total" %in% names(res))
})

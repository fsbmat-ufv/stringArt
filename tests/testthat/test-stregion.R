test_that("stregion runs without mandatory arguments", {
  expect_error(
    res <- stregion(plot = FALSE),
    NA
  )

  expect_type(res, "list")
})

test_that("stregion returns a standardized object", {
  res <- stregion(n = 20, k = 3, plot = FALSE)

  expect_type(res, "list")

  expect_true(all(c(
    "pegs",
    "connections",
    "total_length",
    "audit",
    "meta"
  ) %in% names(res)))
})

test_that("stregion returns a valid pegs table", {
  res <- stregion(n = 20, k = 3, plot = FALSE)

  expect_true(is.data.frame(res$pegs))
  expect_equal(nrow(res$pegs), 20)

  expect_true(all(c(
    "index",
    "x",
    "y"
  ) %in% names(res$pegs)))
})

test_that("stregion returns a valid connections table", {
  res <- stregion(n = 20, k = 3, plot = FALSE)

  expect_true(is.data.frame(res$connections))
  expect_equal(nrow(res$connections), 60)

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
    "offset"
  ) %in% names(res$connections)))

  expect_true(is.numeric(res$total_length))
  expect_equal(length(res$total_length), 1)
  expect_true(res$total_length > 0)
})

test_that("stregion accepts a custom contour", {
  custom_contour <- data.frame(
    x = c(0, 1, 0, -1),
    y = c(1, 0, -1, 0)
  )

  res <- stregion(
    contour = custom_contour,
    n = 16,
    k = 2,
    plot = FALSE
  )

  expect_true(is.data.frame(res$pegs))
  expect_equal(nrow(res$pegs), 16)
  expect_equal(nrow(res$connections), 32)
})

test_that("stregion does not return Portuguese aliases", {
  res <- stregion(n = 20, k = 3, plot = FALSE)

  expect_false("pregos" %in% names(res))
  expect_false("conexoes" %in% names(res))
  expect_false("comprimento_total" %in% names(res))
})

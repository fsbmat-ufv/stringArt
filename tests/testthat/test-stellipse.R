test_that("stellipse runs without mandatory arguments", {
  expect_error(
    res <- stellipse(plot = FALSE),
    NA
  )

  expect_type(res, "list")
})

test_that("stellipse returns a standardized object", {
  res <- stellipse(n = 10, k = 2, plot = FALSE)

  expect_type(res, "list")

  expect_true(all(c(
    "pegs",
    "connections",
    "total_length",
    "audit",
    "meta"
  ) %in% names(res)))
})

test_that("stellipse returns a valid pegs table", {
  res <- stellipse(n = 10, k = 2, plot = FALSE)

  expect_true(is.data.frame(res$pegs))
  expect_equal(nrow(res$pegs), 10)

  expect_true(all(c(
    "index",
    "x",
    "y"
  ) %in% names(res$pegs)))
})

test_that("stellipse returns a valid connections table", {
  res <- stellipse(n = 10, k = 2, plot = FALSE)

  expect_true(is.data.frame(res$connections))
  expect_equal(nrow(res$connections), 10)

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

  expect_true(is.numeric(res$total_length))
  expect_equal(length(res$total_length), 1)
  expect_true(res$total_length > 0)
})

test_that("stellipse does not return Portuguese aliases", {
  res <- stellipse(n = 10, k = 2, plot = FALSE)

  expect_false("pregos" %in% names(res))
  expect_false("conexoes" %in% names(res))
  expect_false("comprimento_total" %in% names(res))
})

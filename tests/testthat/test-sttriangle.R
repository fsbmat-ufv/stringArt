test_that("sttriangle runs without mandatory arguments", {
  expect_error(
    res <- sttriangle(plot = FALSE),
    NA
  )

  expect_type(res, "list")
})

test_that("sttriangle returns a standardized object", {
  res <- sttriangle(n = 12, k = 5, plot = FALSE)

  expect_type(res, "list")

  expect_true(all(c(
    "pegs",
    "connections",
    "total_length",
    "audit",
    "meta"
  ) %in% names(res)))
})

test_that("sttriangle returns a valid pegs table", {
  res <- sttriangle(n = 12, k = 5, plot = FALSE)

  expect_true(is.data.frame(res$pegs))
  expect_equal(nrow(res$pegs), 12)

  expect_true(all(c(
    "index",
    "x",
    "y"
  ) %in% names(res$pegs)))
})

test_that("sttriangle returns a valid connections table", {
  res <- sttriangle(n = 12, k = 5, plot = FALSE)

  expect_true(is.data.frame(res$connections))
  expect_equal(nrow(res$connections), 12)

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

test_that("sttriangle supports template mode", {
  expect_error(
    res <- sttriangle(template = TRUE, plot = FALSE),
    NA
  )

  expect_type(res, "list")
  expect_true(isTRUE(res$meta$parameters$template))
  expect_false(isTRUE(res$meta$parameters$show_strings))
})

test_that("sttriangle does not return Portuguese aliases", {
  res <- sttriangle(n = 12, k = 5, plot = FALSE)

  expect_false("pregos" %in% names(res))
  expect_false("conexoes" %in% names(res))
  expect_false("comprimento_total" %in% names(res))
})

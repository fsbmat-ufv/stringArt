test_that("stdecimal runs without mandatory arguments", {
  expect_error(res <- stdecimal(plot = FALSE), NA)
  expect_type(res, "list")
})

test_that("stdecimal returns standardized output", {
  res <- stdecimal(1, 7, plot = FALSE)
  expect_true(all(c("pegs", "connections", "total_length", "audit", "meta") %in% names(res)))
})

test_that("stdecimal uses digit pegs and returns valid tables", {
  res <- stdecimal(1, 13, plot = FALSE)
  expect_equal(nrow(res$pegs), 10)
  expect_true(is.data.frame(res$connections))
  expect_true(all(c("digit", "x", "y") %in% names(res$pegs)))
  expect_true(all(c("digit_from", "digit_to", "position") %in% names(res$connections)))
})

test_that("stdecimal does not return Portuguese aliases", {
  res <- stdecimal(plot = FALSE)
  expect_false("pregos" %in% names(res))
  expect_false("conexoes" %in% names(res))
  expect_false("comprimento_total" %in% names(res))
})

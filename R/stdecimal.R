#' Generate a string art path from a rational decimal expansion
#'
#' `stdecimal()` places digit pegs on a circle and connects consecutive digits
#' in the expansion of a rational number. By default, the function uses base 10,
#' so the pegs are labeled from 0 to 9.
#'
#' @param numerator Integer. Numerator of the rational number.
#' @param denominator Integer. Denominator of the rational number. Must be
#'   nonzero.
#' @param n Integer. Numeral base and number of digit pegs. Default is 10.
#' @param k Integer. Number of repetitions of the repetend shown in the plot when
#'   the expansion is repeating. Must be at least 1.
#' @param col String color passed to [graphics::segments()].
#' @param lwd Positive number. Line width used to draw the digit path.
#' @param plot Logical. If `TRUE`, draws the figure.
#' @param show_points Logical. If `TRUE`, draws the digit pegs.
#' @param show_labels Logical. If `TRUE`, draws digit labels.
#' @param verbose Logical. If `TRUE`, prints a short audit to the console.
#' @param radius Positive number. Circle radius.
#' @param rotate Numeric. Rotation angle in radians applied to the digit circle.
#' @param include_integer_part Logical. If `TRUE`, includes the integer part of
#'   the rational number in the displayed digit sequence.
#' @param show_strings Logical. If `TRUE`, draws the digit connections.
#' @param template Logical. If `TRUE`, draws only the digit template, without
#'   connections. This is equivalent to setting `show_strings = FALSE`
#'   and `show_points = TRUE`.
#' @param point_col Peg color.
#' @param point_cex Positive number. Peg size.
#' @param point_pch Plotting symbol used for pegs.
#' @param point_bg Peg background color when applicable.
#' @param label_cex Positive number. Label size.
#' @param label_col Label color.
#' @param border_col Border color of the digit circle.
#' @param border_lwd Positive number. Border line width.
#' @param bg Plot background color.
#' @param main Optional plot title. If `NULL`, no title is displayed.
#'
#' @details
#' The function computes the base-`n` expansion of `numerator / denominator`
#' using exact long division. When the expansion is repeating, the repetend is
#' displayed `k` times after the preperiod.
#'
#' For the default setting `n = 10`, the function is especially useful for
#' exploring decimal expansions, repeating decimals, periodicity, and patterns
#' in rational numbers.
#'
#' @return Invisibly returns a list of class `stringart_result` with:
#' \describe{
#'   \item{pegs}{A `data.frame` with columns `index`, `x`, `y`, and `digit`.}
#'   \item{connections}{A `data.frame` with columns `connection_index`,
#'   `from`, `to`, `x_from`, `y_from`, `x_to`, `y_to`, `length`,
#'   `digit_from`, `digit_to`, and `position`.}
#'   \item{total_length}{Total string length.}
#'   \item{audit}{A character vector with audit information.}
#'   \item{meta}{A list with construction metadata.}
#' }
#'
#' @examples
#' stdecimal()
#' stdecimal(1, 7)
#' stdecimal(1, 13)
#' stdecimal(22, 7)
#' stdecimal(template = TRUE)
#'
#' @importFrom graphics par plot segments points text lines
#' @export
stdecimal <- function(numerator = 1,
                      denominator = 7,
                      n = 10,
                      k = 2,
                      col = "blue",
                      lwd = 1,
                      plot = TRUE,
                      show_points = TRUE,
                      show_labels = TRUE,
                      verbose = FALSE,
                      radius = 1,
                      rotate = pi / 2,
                      include_integer_part = TRUE,
                      show_strings = TRUE,
                      template = FALSE,
                      point_col = "black",
                      point_cex = 0.9,
                      point_pch = 21,
                      point_bg = "white",
                      label_cex = 0.8,
                      label_col = "black",
                      border_col = "grey50",
                      border_lwd = 1,
                      bg = "white",
                      main = NULL) {

  check_int <- function(x, nm) {
    if (!is.numeric(x) || length(x) != 1L || is.na(x) || x != as.integer(x)) {
      stop(sprintf("`%s` must be a single integer.", nm), call. = FALSE)
    }
  }

  check_int(numerator, "numerator")
  check_int(denominator, "denominator")
  check_int(n, "n")
  check_int(k, "k")

  numerator <- as.integer(numerator)
  denominator <- as.integer(denominator)
  n <- as.integer(n)
  k <- as.integer(k)

  if (denominator == 0L) {
    stop("`denominator` must be nonzero.", call. = FALSE)
  }

  if (n < 2L || n > 10L) {
    stop("`n` must be an integer between 2 and 10.", call. = FALSE)
  }

  if (k < 1L) {
    stop("`k` must be a positive integer.", call. = FALSE)
  }

  positive_args <- list(
    radius = radius,
    lwd = lwd,
    point_cex = point_cex,
    label_cex = label_cex,
    border_lwd = border_lwd
  )

  for (nm in names(positive_args)) {
    value <- positive_args[[nm]]
    if (!is.numeric(value) || length(value) != 1L || is.na(value) || value <= 0) {
      stop(sprintf("`%s` must be a single positive number.", nm), call. = FALSE)
    }
  }

  if (!is.numeric(rotate) || length(rotate) != 1L || is.na(rotate)) {
    stop("`rotate` must be a single numeric value.", call. = FALSE)
  }

  logical_args <- list(
    plot = plot,
    show_points = show_points,
    show_labels = show_labels,
    verbose = verbose,
    include_integer_part = include_integer_part,
    show_strings = show_strings,
    template = template
  )

  for (nm in names(logical_args)) {
    value <- logical_args[[nm]]
    if (!is.logical(value) || length(value) != 1L || is.na(value)) {
      stop(sprintf("`%s` must be TRUE or FALSE.", nm), call. = FALSE)
    }
  }

  if (template) {
    show_strings <- FALSE
    show_points <- TRUE
  }

  int_to_base_digits <- function(value, base) {
    if (value == 0L) {
      return(0L)
    }

    digits <- integer(0)
    x <- abs(value)

    while (x > 0L) {
      digits <- c(x %% base, digits)
      x <- x %/% base
    }

    digits
  }

  rational_base_expansion <- function(num, den, base) {
    sign_value <- if ((num < 0) != (den < 0)) -1L else 1L
    num <- abs(num)
    den <- abs(den)

    integer_part <- num %/% den
    remainder <- num %% den

    int_digits <- int_to_base_digits(integer_part, base)

    remainders_seen <- integer(0)
    remainders_pos <- integer(0)
    frac_digits <- integer(0)
    repeat_start <- NA_integer_

    pos <- 1L
    while (remainder != 0L && !(remainder %in% remainders_seen)) {
      remainders_seen <- c(remainders_seen, remainder)
      remainders_pos <- c(remainders_pos, pos)

      remainder <- remainder * base
      digit <- remainder %/% den
      remainder <- remainder %% den

      frac_digits <- c(frac_digits, digit)
      pos <- pos + 1L
    }

    if (remainder != 0L) {
      repeat_start <- remainders_pos[match(remainder, remainders_seen)]
    }

    list(
      sign = sign_value,
      integer_digits = int_digits,
      fractional_digits = frac_digits,
      repeat_start = repeat_start
    )
  }

  expansion <- rational_base_expansion(numerator, denominator, n)

  frac_digits <- expansion$fractional_digits
  repeat_start <- expansion$repeat_start

  if (length(frac_digits) == 0L) {
    displayed_frac <- integer(0)
    preperiod <- integer(0)
    repetend <- integer(0)
    period_length <- 0L
    preperiod_length <- 0L
    is_repeating <- FALSE
  } else if (is.na(repeat_start)) {
    displayed_frac <- frac_digits
    preperiod <- frac_digits
    repetend <- integer(0)
    period_length <- 0L
    preperiod_length <- length(preperiod)
    is_repeating <- FALSE
  } else {
    preperiod <- if (repeat_start > 1L) frac_digits[seq_len(repeat_start - 1L)] else integer(0)
    repetend <- frac_digits[repeat_start:length(frac_digits)]
    period_length <- length(repetend)
    preperiod_length <- length(preperiod)
    displayed_frac <- c(preperiod, rep(repetend, k))
    is_repeating <- TRUE
  }

  digit_sequence <- if (include_integer_part) {
    c(expansion$integer_digits, displayed_frac)
  } else {
    displayed_frac
  }

  if (length(digit_sequence) < 1L) {
    digit_sequence <- 0L
  }

  theta <- rotate + seq(0, 2 * pi, length.out = n + 1L)[-(n + 1L)]
  digit_labels <- 0:(n - 1L)

  pegs <- data.frame(
    index = seq_len(n),
    x = radius * cos(theta),
    y = radius * sin(theta),
    digit = digit_labels
  )

  if (length(digit_sequence) >= 2L) {
    from_digits <- digit_sequence[-length(digit_sequence)]
    to_digits <- digit_sequence[-1L]
    connection_count <- length(from_digits)

    from <- from_digits + 1L
    to <- to_digits + 1L

    connections <- data.frame(
      connection_index = seq_len(connection_count),
      from = from,
      to = to,
      x_from = pegs$x[from],
      y_from = pegs$y[from],
      x_to = pegs$x[to],
      y_to = pegs$y[to],
      digit_from = from_digits,
      digit_to = to_digits,
      position = seq_len(connection_count)
    )

    connections$length <- sqrt(
      (connections$x_to - connections$x_from)^2 +
        (connections$y_to - connections$y_from)^2
    )
  } else {
    connections <- data.frame(
      connection_index = integer(0),
      from = integer(0),
      to = integer(0),
      x_from = numeric(0),
      y_from = numeric(0),
      x_to = numeric(0),
      y_to = numeric(0),
      digit_from = integer(0),
      digit_to = integer(0),
      position = integer(0),
      length = numeric(0)
    )
  }

  total_length <- sum(connections$length)
  displayed_sequence_text <- paste(digit_sequence, collapse = "")

  audit <- c(
    "String Art audit",
    "Figure: decimal",
    sprintf("Rational number: %d/%d", numerator, denominator),
    sprintf("Base: %d", n),
    sprintf("Displayed repetend repetitions: %d", k),
    sprintf("Repeating expansion: %s", if (is_repeating) "yes" else "no"),
    sprintf("Preperiod length: %d", preperiod_length),
    sprintf("Period length: %d", period_length),
    sprintf("Displayed digit sequence: %s", displayed_sequence_text),
    sprintf("Number of pegs: %d", nrow(pegs)),
    sprintf("Number of connections: %d", nrow(connections)),
    sprintf("Total string length: %.4f", total_length)
  )

  if (plot) {
    old_par <- graphics::par(no.readonly = TRUE)
    on.exit(graphics::par(old_par), add = TRUE)
    graphics::par(bg = bg)

    x_range <- range(pegs$x)
    y_range <- range(pegs$y)
    x_pad <- max(0.1, 0.15 * diff(x_range))
    y_pad <- max(0.1, 0.15 * diff(y_range))

    graphics::plot(
      NA, NA,
      xlim = x_range + c(-x_pad, x_pad),
      ylim = y_range + c(-y_pad, y_pad),
      asp = 1,
      axes = FALSE,
      xlab = "",
      ylab = "",
      main = main
    )

    tt <- seq(0, 2 * pi, length.out = 300L)
    graphics::lines(radius * cos(tt), radius * sin(tt), col = border_col, lwd = border_lwd)

    if (show_strings && nrow(connections) > 0L) {
      graphics::segments(
        x0 = connections$x_from,
        y0 = connections$y_from,
        x1 = connections$x_to,
        y1 = connections$y_to,
        col = col,
        lwd = lwd
      )
    }

    if (show_points) {
      graphics::points(
        pegs$x, pegs$y,
        pch = point_pch, col = point_col, bg = point_bg, cex = point_cex
      )
    }

    if (show_labels) {
      graphics::text(
        1.1 * pegs$x,
        1.1 * pegs$y,
        labels = pegs$digit,
        cex = label_cex,
        col = label_col
      )
    }
  }

  if (verbose) {
    message(paste(audit, collapse = "\n"))
  }

  result <- list(
    pegs = pegs,
    connections = connections,
    total_length = total_length,
    audit = audit,
    meta = list(
      figure = "decimal",
      family = "number_pattern",
      rule = "digit_successor_path",
      formula = "Connect consecutive digits in the base-n expansion of numerator/denominator",
      mathematical_topics = c(
        "fractions",
        "rational numbers",
        "decimal expansions",
        "repeating decimals",
        "periodicity",
        "numeral systems"
      ),
      parameters = list(
        numerator = numerator,
        denominator = denominator,
        n = n,
        k = k,
        radius = radius,
        rotate = rotate,
        include_integer_part = include_integer_part,
        col = col,
        lwd = lwd,
        show_points = show_points,
        show_labels = show_labels,
        show_strings = show_strings,
        template = template,
        preperiod_length = preperiod_length,
        period_length = period_length,
        digit_sequence = digit_sequence
      )
    )
  )

  class(result) <- c("stringart_result", class(result))
  invisible(result)
}

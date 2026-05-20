#' Generate a Lissajous string art pattern
#'
#' `stlissajous()` generates a string art figure from pegs sampled on a
#' Lissajous curve.
#'
#' @param n Integer. Number of pegs sampled on the curve. Must be at least 3.
#' @param k Integer. Additive modular step used in the connection rule. Must
#'   satisfy `1 <= k <= n - 1`.
#' @param col String color passed to [graphics::segments()].
#' @param lwd Positive number. Line width used to draw the strings.
#' @param plot Logical. If `TRUE`, draws the figure.
#' @param show_points Logical. If `TRUE`, draws the pegs.
#' @param show_labels Logical. If `TRUE`, draws peg labels.
#' @param verbose Logical. If `TRUE`, prints a short audit to the console.
#' @param a Positive integer. Frequency in the x-coordinate.
#' @param b Positive integer. Frequency in the y-coordinate.
#' @param phase Numeric. Phase shift in radians.
#' @param amplitude_x Positive number. Horizontal amplitude.
#' @param amplitude_y Positive number. Vertical amplitude.
#' @param rotate Numeric. Rotation angle in radians.
#' @param show_strings Logical. If `TRUE`, draws the string connections.
#' @param template Logical. If `TRUE`, draws only the peg template.
#' @param draw_curve Logical. If `TRUE`, draws the underlying Lissajous curve.
#' @param border_col Curve color.
#' @param border_lwd Positive number. Curve line width.
#' @param point_col Peg color.
#' @param point_cex Positive number. Peg size.
#' @param point_pch Plotting symbol used for pegs.
#' @param point_bg Peg background color when applicable.
#' @param label_cex Positive number. Label size.
#' @param label_col Label color.
#' @param bg Plot background color.
#' @param main Optional plot title.
#'
#' @details
#' The curve is given by
#'
#' `x(t) = amplitude_x * sin(a * t + phase)` and
#' `y(t) = amplitude_y * sin(b * t)`.
#'
#' @return Invisibly returns a list of class `stringart_result`.
#'
#' @examples
#' stlissajous()
#' stlissajous(a = 3, b = 2)
#' stlissajous(a = 5, b = 4, phase = pi / 3)
#' stlissajous(template = TRUE)
#'
#' @importFrom graphics par plot segments points text lines
#' @export
stlissajous <- function(n = 300,
                        k = 19,
                        col = "purple",
                        lwd = 0.7,
                        plot = TRUE,
                        show_points = FALSE,
                        show_labels = FALSE,
                        verbose = FALSE,
                        a = 3,
                        b = 2,
                        phase = pi / 2,
                        amplitude_x = 1,
                        amplitude_y = 1,
                        rotate = 0,
                        show_strings = TRUE,
                        template = FALSE,
                        draw_curve = TRUE,
                        border_col = "grey50",
                        border_lwd = 1,
                        point_col = "black",
                        point_cex = 0.5,
                        point_pch = 19,
                        point_bg = "white",
                        label_cex = 0.6,
                        label_col = "black",
                        bg = "white",
                        main = NULL) {

  check_pos_int <- function(x, nm) {
    if (!is.numeric(x) || length(x) != 1L || is.na(x) || x != as.integer(x) || x < 1L) {
      stop(sprintf("`%s` must be a single positive integer.", nm), call. = FALSE)
    }
  }
  check_pos_int(n, "n")
  check_pos_int(k, "k")
  check_pos_int(a, "a")
  check_pos_int(b, "b")

  n <- as.integer(n)
  k <- as.integer(k)
  a <- as.integer(a)
  b <- as.integer(b)

  if (n < 3L) stop("`n` must be a single integer greater than or equal to 3.", call. = FALSE)
  if (k >= n) stop("`k` must satisfy 1 <= k <= n - 1.", call. = FALSE)

  positive_args <- list(
    lwd = lwd, amplitude_x = amplitude_x, amplitude_y = amplitude_y,
    border_lwd = border_lwd, point_cex = point_cex, label_cex = label_cex
  )
  for (nm in names(positive_args)) {
    value <- positive_args[[nm]]
    if (!is.numeric(value) || length(value) != 1L || is.na(value) || value <= 0) {
      stop(sprintf("`%s` must be a single positive number.", nm), call. = FALSE)
    }
  }
  if (!is.numeric(phase) || length(phase) != 1L || is.na(phase)) {
    stop("`phase` must be a single numeric value.", call. = FALSE)
  }
  if (!is.numeric(rotate) || length(rotate) != 1L || is.na(rotate)) {
    stop("`rotate` must be a single numeric value.", call. = FALSE)
  }

  logical_args <- list(plot = plot, show_points = show_points, show_labels = show_labels,
                       verbose = verbose, show_strings = show_strings,
                       template = template, draw_curve = draw_curve)
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

  gcd_int <- function(x, y) {
    x <- abs(as.integer(x)); y <- abs(as.integer(y))
    while (y != 0L) {
      tmp <- y; y <- x %% y; x <- tmp
    }
    x
  }

  rotate_xy <- function(x, y, angle) {
    list(
      x = x * cos(angle) - y * sin(angle),
      y = x * sin(angle) + y * cos(angle)
    )
  }

  t <- seq(0, 2 * pi, length.out = n + 1L)[-(n + 1L)]
  x0 <- amplitude_x * sin(a * t + phase)
  y0 <- amplitude_y * sin(b * t)
  rot <- rotate_xy(x0, y0, rotate)

  pegs <- data.frame(index = seq_len(n), x = rot$x, y = rot$y)

  from <- seq_len(n)
  to <- ((from + k - 1L) %% n) + 1L
  connections <- data.frame(
    connection_index = seq_len(n),
    from = from, to = to,
    x_from = pegs$x[from], y_from = pegs$y[from],
    x_to = pegs$x[to], y_to = pegs$y[to]
  )
  connections$length <- sqrt((connections$x_to - connections$x_from)^2 +
                               (connections$y_to - connections$y_from)^2)

  total_length <- sum(connections$length)
  g_ab <- gcd_int(a, b)

  audit <- c(
    "String Art audit",
    "Figure: lissajous",
    sprintf("Number of pegs: %d", n),
    sprintf("Modular step: %d", k),
    sprintf("Frequency parameters: a = %d, b = %d", a, b),
    sprintf("Frequency ratio reduced by gcd(a, b) = %d", g_ab),
    sprintf("Phase shift: %.4f radians", phase),
    sprintf("Amplitudes: x = %.4f, y = %.4f", amplitude_x, amplitude_y),
    sprintf("Number of connections: %d", nrow(connections)),
    sprintf("Total string length: %.4f", total_length)
  )

  if (plot) {
    old_par <- graphics::par(no.readonly = TRUE); on.exit(graphics::par(old_par), add = TRUE)
    graphics::par(bg = bg)
    x_range <- range(pegs$x); y_range <- range(pegs$y)
    x_pad <- max(0.1, 0.1 * diff(x_range)); y_pad <- max(0.1, 0.1 * diff(y_range))
    graphics::plot(NA, NA, xlim = x_range + c(-x_pad, x_pad), ylim = y_range + c(-y_pad, y_pad),
                   asp = 1, axes = FALSE, xlab = "", ylab = "", main = main)

    if (draw_curve) {
      graphics::lines(c(pegs$x, pegs$x[1]), c(pegs$y, pegs$y[1]), col = border_col, lwd = border_lwd)
    }
    if (show_strings) {
      graphics::segments(connections$x_from, connections$y_from, connections$x_to, connections$y_to,
                         col = col, lwd = lwd)
    }
    if (show_points) {
      graphics::points(pegs$x, pegs$y, pch = point_pch, col = point_col, bg = point_bg, cex = point_cex)
    }
    if (show_labels) {
      graphics::text(pegs$x, pegs$y, labels = pegs$index, cex = label_cex, col = label_col, pos = 3)
    }
  }

  if (verbose) message(paste(audit, collapse = "\n"))

  result <- list(
    pegs = pegs,
    connections = connections,
    total_length = total_length,
    audit = audit,
    meta = list(
      figure = "lissajous",
      family = "parametric_curve",
      rule = "additive_modular",
      formula = "x(t) = amplitude_x * sin(a * t + phase), y(t) = amplitude_y * sin(b * t)",
      mathematical_topics = c(
        "trigonometric functions",
        "frequency",
        "phase",
        "parametric curves",
        "periodicity",
        "frequency ratios"
      ),
      parameters = list(
        n = n, k = k, a = a, b = b, phase = phase,
        amplitude_x = amplitude_x, amplitude_y = amplitude_y, rotate = rotate,
        col = col, lwd = lwd,
        show_points = show_points, show_labels = show_labels,
        show_strings = show_strings, template = template
      )
    )
  )
  class(result) <- c("stringart_result", class(result))
  invisible(result)
}

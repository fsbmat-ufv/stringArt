#' Generate a spiral string art pattern
#'
#' `stspiral()` generates a string art pattern from pegs placed on an
#' Archimedean spiral.
#'
#' @param n Integer. Number of pegs placed on the spiral. Must be at least 3.
#' @param k Integer. Additive modular step used in the connection rule. Must
#'   satisfy `1 <= k <= n - 1`.
#' @param col String color passed to [graphics::segments()].
#' @param lwd Positive number. Line width used to draw the strings.
#' @param plot Logical. If `TRUE`, draws the figure.
#' @param show_points Logical. If `TRUE`, draws the pegs.
#' @param show_labels Logical. If `TRUE`, draws peg labels.
#' @param verbose Logical. If `TRUE`, prints a short audit to the console.
#' @param turns Positive number. Number of spiral turns.
#' @param spacing Positive number. Radial growth per turn.
#' @param inner_radius Nonnegative number. Initial spiral radius.
#' @param rotate Numeric. Rotation angle in radians.
#' @param show_strings Logical. If `TRUE`, draws the string connections.
#' @param template Logical. If `TRUE`, draws only the peg template.
#' @param draw_curve Logical. If `TRUE`, draws the underlying spiral.
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
#' The spiral uses the polar form
#' `r(theta) = inner_radius + spacing * theta / (2 * pi)`.
#'
#' @return Invisibly returns a list of class `stringart_result`.
#'
#' @examples
#' stspiral()
#' stspiral(turns = 4)
#' stspiral(template = TRUE)
#'
#' @importFrom graphics par plot segments points text lines
#' @export
stspiral <- function(n = 180,
                     k = 13,
                     col = "steelblue",
                     lwd = 0.7,
                     plot = TRUE,
                     show_points = FALSE,
                     show_labels = FALSE,
                     verbose = FALSE,
                     turns = 3,
                     spacing = 0.6,
                     inner_radius = 0,
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

  if (!is.numeric(n) || length(n) != 1L || is.na(n) || n != as.integer(n) || n < 3L) {
    stop("`n` must be a single integer greater than or equal to 3.", call. = FALSE)
  }
  if (!is.numeric(k) || length(k) != 1L || is.na(k) || k != as.integer(k) || k < 1L) {
    stop("`k` must be a single positive integer.", call. = FALSE)
  }

  n <- as.integer(n); k <- as.integer(k)
  if (k >= n) stop("`k` must satisfy 1 <= k <= n - 1.", call. = FALSE)

  positive_args <- list(
    turns = turns, spacing = spacing, lwd = lwd,
    border_lwd = border_lwd, point_cex = point_cex, label_cex = label_cex
  )
  for (nm in names(positive_args)) {
    value <- positive_args[[nm]]
    if (!is.numeric(value) || length(value) != 1L || is.na(value) || value <= 0) {
      stop(sprintf("`%s` must be a single positive number.", nm), call. = FALSE)
    }
  }
  if (!is.numeric(inner_radius) || length(inner_radius) != 1L || is.na(inner_radius) || inner_radius < 0) {
    stop("`inner_radius` must be a single nonnegative number.", call. = FALSE)
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

  gcd_int <- function(a, b) {
    a <- abs(as.integer(a)); b <- abs(as.integer(b))
    while (b != 0L) {
      tmp <- b; b <- a %% b; a <- tmp
    }
    a
  }

  theta <- seq(0, 2 * pi * turns, length.out = n)
  r <- inner_radius + spacing * theta / (2 * pi)
  x <- r * cos(theta + rotate)
  y <- r * sin(theta + rotate)

  pegs <- data.frame(index = seq_len(n), x = x, y = y)

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
  d <- gcd_int(n, k)

  audit <- c(
    "String Art audit",
    "Figure: spiral",
    sprintf("Number of pegs: %d", n),
    sprintf("Modular step: %d", k),
    sprintf("Number of turns: %.4f", turns),
    sprintf("Spacing: %.4f", spacing),
    sprintf("Inner radius: %.4f", inner_radius),
    sprintf("gcd(n, k): %d", d),
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
      graphics::lines(pegs$x, pegs$y, col = border_col, lwd = border_lwd)
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
      figure = "spiral",
      family = "polar_curve",
      rule = "additive_modular",
      formula = "r(theta) = inner_radius + spacing * theta / (2 * pi)",
      mathematical_topics = c(
        "sequences",
        "polar coordinates",
        "Archimedean spiral",
        "growth",
        "parameterization"
      ),
      parameters = list(
        n = n, k = k, turns = turns, spacing = spacing, inner_radius = inner_radius,
        rotate = rotate, col = col, lwd = lwd,
        show_points = show_points, show_labels = show_labels,
        show_strings = show_strings, template = template
      )
    )
  )
  class(result) <- c("stringart_result", class(result))
  invisible(result)
}

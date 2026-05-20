#' Generate a rose-like string art pattern
#'
#' `strose()` generates a rose-like string art figure based on a polar curve of
#' the form `r(theta) = amplitude * (1 + cos(petals * theta)) / 2`.
#'
#' @param n Integer. Number of pegs sampled along the curve. Must be at least 3.
#' @param k Integer. Additive modular step used in the connection rule. Must
#'   satisfy `1 <= k <= n - 1`.
#' @param col String color passed to [graphics::segments()].
#' @param lwd Positive number. Line width used to draw the strings.
#' @param plot Logical. If `TRUE`, draws the figure.
#' @param show_points Logical. If `TRUE`, draws the pegs.
#' @param show_labels Logical. If `TRUE`, draws peg labels.
#' @param verbose Logical. If `TRUE`, prints a short audit to the console.
#' @param petals Integer. Number of petals in the rose-like curve.
#' @param amplitude Positive number. Maximum radial amplitude.
#' @param rotate Numeric. Rotation angle in radians.
#' @param show_strings Logical. If `TRUE`, draws the string connections.
#' @param template Logical. If `TRUE`, draws only the peg template, without
#'   string connections. This is equivalent to setting `show_strings = FALSE`
#'   and `show_points = TRUE`.
#' @param draw_curve Logical. If `TRUE`, draws the underlying rose-like curve.
#' @param border_col Curve color.
#' @param border_lwd Positive number. Curve line width.
#' @param point_col Peg color.
#' @param point_cex Positive number. Peg size.
#' @param point_pch Plotting symbol used for pegs.
#' @param point_bg Peg background color when applicable.
#' @param label_cex Positive number. Label size.
#' @param label_col Label color.
#' @param bg Plot background color.
#' @param main Optional plot title. If `NULL`, no title is displayed.
#'
#' @details
#' The pegs are sampled from the polar curve
#' `r(theta) = amplitude * (1 + cos(petals * theta)) / 2`, which produces a
#' rose-like pattern with `petals` visible petals.
#'
#' The connections follow the additive modular rule
#' `to = ((from + k - 1) %% n) + 1`.
#'
#' @return Invisibly returns a list of class `stringart_result`.
#'
#' @examples
#' strose()
#' strose(petals = 6)
#' strose(petals = 8)
#' strose(template = TRUE)
#'
#' @importFrom graphics par plot segments points text lines
#' @export
strose <- function(n = 240,
                   k = 17,
                   col = "deeppink4",
                   lwd = 0.7,
                   plot = TRUE,
                   show_points = FALSE,
                   show_labels = FALSE,
                   verbose = FALSE,
                   petals = 6,
                   amplitude = 1,
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
  if (!is.numeric(petals) || length(petals) != 1L || is.na(petals) ||
      petals != as.integer(petals) || petals < 1L) {
    stop("`petals` must be a single positive integer.", call. = FALSE)
  }

  n <- as.integer(n)
  k <- as.integer(k)
  petals <- as.integer(petals)

  if (k >= n) {
    stop("`k` must satisfy 1 <= k <= n - 1.", call. = FALSE)
  }

  positive_args <- list(
    amplitude = amplitude,
    lwd = lwd,
    border_lwd = border_lwd,
    point_cex = point_cex,
    label_cex = label_cex
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
    show_strings = show_strings,
    template = template,
    draw_curve = draw_curve
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

  gcd_int <- function(a, b) {
    a <- abs(as.integer(a)); b <- abs(as.integer(b))
    while (b != 0L) {
      tmp <- b; b <- a %% b; a <- tmp
    }
    a
  }

  theta <- seq(0, 2 * pi, length.out = n + 1L)[-(n + 1L)]
  r <- amplitude * (1 + cos(petals * theta)) / 2
  x <- r * cos(theta + rotate)
  y <- r * sin(theta + rotate)

  pegs <- data.frame(
    index = seq_len(n),
    x = x,
    y = y
  )

  from <- seq_len(n)
  to <- ((from + k - 1L) %% n) + 1L

  connections <- data.frame(
    connection_index = seq_len(n),
    from = from,
    to = to,
    x_from = pegs$x[from],
    y_from = pegs$y[from],
    x_to = pegs$x[to],
    y_to = pegs$y[to]
  )
  connections$length <- sqrt((connections$x_to - connections$x_from)^2 +
                               (connections$y_to - connections$y_from)^2)

  total_length <- sum(connections$length)
  d <- gcd_int(n, k)

  audit <- c(
    "String Art audit",
    "Figure: rose",
    sprintf("Number of pegs: %d", n),
    sprintf("Modular step: %d", k),
    sprintf("Number of petals: %d", petals),
    sprintf("Amplitude: %.4f", amplitude),
    sprintf("gcd(n, k): %d", d),
    sprintf("Number of connections: %d", nrow(connections)),
    sprintf("Total string length: %.4f", total_length)
  )

  if (plot) {
    old_par <- graphics::par(no.readonly = TRUE); on.exit(graphics::par(old_par), add = TRUE)
    graphics::par(bg = bg)

    x_range <- range(pegs$x); y_range <- range(pegs$y)
    x_pad <- max(0.1, 0.1 * diff(x_range)); y_pad <- max(0.1, 0.1 * diff(y_range))

    graphics::plot(NA, NA,
      xlim = x_range + c(-x_pad, x_pad),
      ylim = y_range + c(-y_pad, y_pad),
      asp = 1, axes = FALSE, xlab = "", ylab = "", main = main
    )

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

  if (verbose) {
    message(paste(audit, collapse = "\n"))
  }

  result <- list(
    pegs = pegs,
    connections = connections,
    total_length = total_length,
    audit = audit,
    meta = list(
      figure = "rose",
      family = "trigonometric",
      rule = "additive_modular",
      formula = "r(theta) = amplitude * (1 + cos(petals * theta)) / 2",
      mathematical_topics = c(
        "radial symmetry",
        "trigonometry",
        "rotation",
        "periodicity",
        "sine and cosine"
      ),
      parameters = list(
        n = n,
        k = k,
        petals = petals,
        amplitude = amplitude,
        rotate = rotate,
        col = col,
        lwd = lwd,
        show_points = show_points,
        show_labels = show_labels,
        show_strings = show_strings,
        template = template
      )
    )
  )
  class(result) <- c("stringart_result", class(result))
  invisible(result)
}

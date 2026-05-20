#' Generate string art on a rectangular grid boundary
#'
#' `stgrid()` generates string art by placing pegs uniformly along the boundary
#' of a rectangle and connecting them using an additive modular rule.
#'
#' @param n Integer. Number of pegs placed on the rectangle boundary. Must be at
#'   least 4.
#' @param k Integer. Additive modular step used in the connection rule. Must
#'   satisfy `1 <= k <= n - 1`.
#' @param col String color passed to [graphics::segments()].
#' @param lwd Positive number. Line width used to draw the strings.
#' @param plot Logical. If `TRUE`, draws the figure.
#' @param show_points Logical. If `TRUE`, draws the pegs.
#' @param show_labels Logical. If `TRUE`, draws peg labels.
#' @param verbose Logical. If `TRUE`, prints a short audit to the console.
#' @param width Positive number. Rectangle width.
#' @param height Positive number. Rectangle height.
#' @param rotate Numeric. Rotation angle in radians.
#' @param show_strings Logical. If `TRUE`, draws the string connections.
#' @param template Logical. If `TRUE`, draws only the peg template.
#' @param border_col Rectangle border color.
#' @param border_lwd Positive number. Border line width.
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
#' Pegs are distributed uniformly along the rectangle boundary and connected
#' using the additive modular rule
#' `to = ((from + k - 1) %% n) + 1`.
#'
#' @return Invisibly returns a list of class `stringart_result`.
#'
#' @examples
#' stgrid()
#' stgrid(width = 2, height = 1)
#' stgrid(template = TRUE)
#'
#' @importFrom graphics par plot segments points text
#' @export
stgrid <- function(n = 60,
                   k = 7,
                   col = "blue",
                   lwd = 1,
                   plot = TRUE,
                   show_points = TRUE,
                   show_labels = FALSE,
                   verbose = FALSE,
                   width = 2,
                   height = 1,
                   rotate = 0,
                   show_strings = TRUE,
                   template = FALSE,
                   border_col = "grey50",
                   border_lwd = 1,
                   point_col = "black",
                   point_cex = 0.8,
                   point_pch = 19,
                   point_bg = "white",
                   label_cex = 0.7,
                   label_col = "black",
                   bg = "white",
                   main = NULL) {

  if (!is.numeric(n) || length(n) != 1L || is.na(n) || n != as.integer(n) || n < 4L) {
    stop("`n` must be a single integer greater than or equal to 4.", call. = FALSE)
  }
  if (!is.numeric(k) || length(k) != 1L || is.na(k) || k != as.integer(k) || k < 1L) {
    stop("`k` must be a single positive integer.", call. = FALSE)
  }

  n <- as.integer(n); k <- as.integer(k)
  if (k >= n) stop("`k` must satisfy 1 <= k <= n - 1.", call. = FALSE)

  positive_args <- list(width = width, height = height, lwd = lwd,
                        border_lwd = border_lwd, point_cex = point_cex, label_cex = label_cex)
  for (nm in names(positive_args)) {
    value <- positive_args[[nm]]
    if (!is.numeric(value) || length(value) != 1L || is.na(value) || value <= 0) {
      stop(sprintf("`%s` must be a single positive number.", nm), call. = FALSE)
    }
  }

  if (!is.numeric(rotate) || length(rotate) != 1L || is.na(rotate)) {
    stop("`rotate` must be a single numeric value.", call. = FALSE)
  }

  logical_args <- list(plot = plot, show_points = show_points, show_labels = show_labels,
                       verbose = verbose, show_strings = show_strings, template = template)
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

  rotate_xy <- function(x, y, angle) {
    data.frame(
      x = x * cos(angle) - y * sin(angle),
      y = x * sin(angle) + y * cos(angle)
    )
  }

  resample_closed_polyline <- function(vertices, n_points) {
    x <- c(vertices$x, vertices$x[1]); y <- c(vertices$y, vertices$y[1])
    dx <- diff(x); dy <- diff(y)
    seg_len <- sqrt(dx^2 + dy^2)
    cumulative <- c(0, cumsum(seg_len))
    total_len <- sum(seg_len)

    s_target <- seq(0, total_len, length.out = n_points + 1L)[-(n_points + 1L)]
    px <- numeric(n_points); py <- numeric(n_points); side_id <- integer(n_points)

    for (i in seq_len(n_points)) {
      s <- s_target[i]
      j <- min(findInterval(s, cumulative, rightmost.closed = TRUE), length(seg_len))
      t <- (s - cumulative[j]) / seg_len[j]
      px[i] <- (1 - t) * x[j] + t * x[j + 1L]
      py[i] <- (1 - t) * y[j] + t * y[j + 1L]
      side_id[i] <- j
    }
    data.frame(x = px, y = py, side = side_id)
  }

  vertices <- data.frame(
    x = c(-width / 2, width / 2, width / 2, -width / 2),
    y = c(-height / 2, -height / 2, height / 2, height / 2)
  )
  rotated_vertices <- rotate_xy(vertices$x, vertices$y, rotate)
  vertices$x <- rotated_vertices$x; vertices$y <- rotated_vertices$y

  sampled <- resample_closed_polyline(vertices, n)
  pegs <- data.frame(index = seq_len(n), x = sampled$x, y = sampled$y, side = sampled$side)

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
    "Figure: grid",
    sprintf("Number of pegs: %d", n),
    sprintf("Modular step: %d", k),
    sprintf("Rectangle width: %.4f", width),
    sprintf("Rectangle height: %.4f", height),
    sprintf("gcd(n, k): %d", d),
    sprintf("Number of connections: %d", nrow(connections)),
    sprintf("Total string length: %.4f", total_length)
  )

  if (plot) {
    old_par <- graphics::par(no.readonly = TRUE); on.exit(graphics::par(old_par), add = TRUE)
    graphics::par(bg = bg)

    x_range <- range(c(vertices$x, pegs$x)); y_range <- range(c(vertices$y, pegs$y))
    x_pad <- max(0.1, 0.1 * diff(x_range)); y_pad <- max(0.1, 0.1 * diff(y_range))
    graphics::plot(NA, NA, xlim = x_range + c(-x_pad, x_pad), ylim = y_range + c(-y_pad, y_pad),
                   asp = 1, axes = FALSE, xlab = "", ylab = "", main = main)

    graphics::segments(
      x0 = vertices$x, y0 = vertices$y,
      x1 = c(vertices$x[-1], vertices$x[1]),
      y1 = c(vertices$y[-1], vertices$y[1]),
      col = border_col, lwd = border_lwd
    )

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
      figure = "grid",
      family = "rectangular_boundary",
      rule = "additive_modular",
      formula = "to = ((from + k - 1) %% n) + 1",
      mathematical_topics = c(
        "cartesian plane",
        "line segments",
        "slope",
        "symmetry",
        "proportionality",
        "coordinates"
      ),
      parameters = list(
        n = n, k = k, width = width, height = height, rotate = rotate,
        col = col, lwd = lwd, show_points = show_points, show_labels = show_labels,
        show_strings = show_strings, template = template
      )
    )
  )
  class(result) <- c("stringart_result", class(result))
  invisible(result)
}

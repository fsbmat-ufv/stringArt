#' Generate a star polygon string art pattern
#'
#' `ststar()` generates a classical star polygon `{n/k}` by placing `n` pegs on
#' a circle and connecting each peg to the peg `k` positions ahead.
#'
#' @param n Integer. Number of pegs on the circle. Must be at least 3.
#' @param k Integer. Star step. Must satisfy `1 <= k <= n - 1`.
#' @param col String color passed to [graphics::segments()].
#' @param lwd Positive number. Line width used to draw the star.
#' @param plot Logical. If `TRUE`, draws the figure.
#' @param show_points Logical. If `TRUE`, draws the pegs.
#' @param show_labels Logical. If `TRUE`, draws peg labels.
#' @param verbose Logical. If `TRUE`, prints a short audit to the console.
#' @param radius Positive number. Circle radius.
#' @param rotate Numeric. Rotation angle in radians applied to the star.
#' @param show_strings Logical. If `TRUE`, draws the star connections.
#' @param template Logical. If `TRUE`, draws only the peg template, without
#'   star connections. This is equivalent to setting `show_strings = FALSE`
#'   and `show_points = TRUE`.
#' @param draw_polygon Logical. If `TRUE`, draws the underlying regular polygon.
#' @param border_col Polygon border color.
#' @param border_lwd Positive number. Border line width.
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
#' The star polygon uses the additive modular rule
#'
#' `to = ((from + k - 1) %% n) + 1`.
#'
#' The greatest common divisor `gcd(n, k)` determines the number of cycles. If
#' `gcd(n, k) = 1`, the star is a single cycle. Otherwise, the construction
#' decomposes into several independent cycles.
#'
#' @return Invisibly returns a list of class `stringart_result` with:
#' \describe{
#'   \item{pegs}{A `data.frame` with columns `index`, `x`, and `y`.}
#'   \item{connections}{A `data.frame` with columns `connection_index`,
#'   `from`, `to`, `x_from`, `y_from`, `x_to`, `y_to`, and `length`.}
#'   \item{total_length}{Total string length.}
#'   \item{audit}{A character vector with audit information.}
#'   \item{meta}{A list with construction metadata.}
#' }
#'
#' @examples
#' ststar()
#' ststar(n = 5, k = 2)
#' ststar(n = 7, k = 2)
#' ststar(n = 8, k = 3)
#' ststar(template = TRUE)
#'
#' @importFrom graphics par plot segments points text
#' @export
ststar <- function(n = 5,
                   k = 2,
                   col = "blue",
                   lwd = 1,
                   plot = TRUE,
                   show_points = TRUE,
                   show_labels = FALSE,
                   verbose = FALSE,
                   radius = 1,
                   rotate = pi / 2,
                   show_strings = TRUE,
                   template = FALSE,
                   draw_polygon = TRUE,
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

  if (!is.numeric(n) || length(n) != 1L || is.na(n) ||
      n != as.integer(n) || n < 3L) {
    stop("`n` must be a single integer greater than or equal to 3.", call. = FALSE)
  }

  if (!is.numeric(k) || length(k) != 1L || is.na(k) ||
      k != as.integer(k) || k < 1L) {
    stop("`k` must be a single positive integer.", call. = FALSE)
  }

  n <- as.integer(n)
  k <- as.integer(k)

  if (k >= n) {
    stop("`k` must satisfy 1 <= k <= n - 1.", call. = FALSE)
  }

  positive_args <- list(
    radius = radius,
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
    draw_polygon = draw_polygon
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
    a <- abs(as.integer(a))
    b <- abs(as.integer(b))
    while (b != 0L) {
      tmp <- b
      b <- a %% b
      a <- tmp
    }
    a
  }

  theta <- rotate + seq(0, 2 * pi, length.out = n + 1L)[-(n + 1L)]

  pegs <- data.frame(
    index = seq_len(n),
    x = radius * cos(theta),
    y = radius * sin(theta)
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

  connections$length <- sqrt(
    (connections$x_to - connections$x_from)^2 +
      (connections$y_to - connections$y_from)^2
  )

  total_length <- sum(connections$length)

  d <- gcd_int(n, k)
  number_of_cycles <- d
  cycle_length <- n / d

  audit <- c(
    "String Art audit",
    "Figure: star",
    sprintf("Star polygon notation: {%d/%d}", n, k),
    sprintf("Number of pegs: %d", n),
    sprintf("Star step: %d", k),
    sprintf("gcd(n, k): %d", d),
    sprintf("Number of cycles: %d", number_of_cycles),
    sprintf("Cycle length: %d", cycle_length),
    if (d == 1L) {
      "The star polygon is a single cycle."
    } else {
      "The construction decomposes into multiple cycles."
    },
    sprintf("Total string length: %.4f", total_length)
  )

  if (plot) {
    old_par <- graphics::par(no.readonly = TRUE)
    on.exit(graphics::par(old_par), add = TRUE)
    graphics::par(bg = bg)

    x_range <- range(pegs$x)
    y_range <- range(pegs$y)
    x_pad <- max(0.1, 0.1 * diff(x_range))
    y_pad <- max(0.1, 0.1 * diff(y_range))

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

    if (draw_polygon) {
      graphics::segments(
        x0 = pegs$x,
        y0 = pegs$y,
        x1 = c(pegs$x[-1], pegs$x[1]),
        y1 = c(pegs$y[-1], pegs$y[1]),
        col = border_col,
        lwd = border_lwd
      )
    }

    if (show_strings) {
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
        pegs$x,
        pegs$y,
        pch = point_pch,
        col = point_col,
        bg = point_bg,
        cex = point_cex
      )
    }

    if (show_labels) {
      graphics::text(
        pegs$x,
        pegs$y,
        labels = pegs$index,
        pos = 3,
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
      figure = "star",
      family = "star_polygon",
      rule = "additive_modular",
      formula = "to = ((from + k - 1) %% n) + 1",
      mathematical_topics = c(
        "greatest common divisor",
        "modular arithmetic",
        "cycles",
        "rotational symmetry",
        "periodicity",
        "star polygons"
      ),
      parameters = list(
        n = n,
        k = k,
        radius = radius,
        rotate = rotate,
        col = col,
        lwd = lwd,
        show_points = show_points,
        show_labels = show_labels,
        show_strings = show_strings,
        template = template,
        draw_polygon = draw_polygon,
        notation = sprintf("{%d/%d}", n, k),
        number_of_cycles = number_of_cycles,
        cycle_length = cycle_length
      )
    )
  )

  class(result) <- c("stringart_result", class(result))
  invisible(result)
}

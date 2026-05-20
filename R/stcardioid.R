#' Generate a circular string art pattern with a cardioid effect
#'
#' `stcardioid()` generates a circular string art pattern by placing equally
#' spaced pegs on a circle and connecting each peg to another peg according to
#' a multiplicative modular rule.
#'
#' @param n Integer. Number of pegs placed on the circle. Must be at least 3.
#' @param k Integer. Multiplication factor used in the modular connection rule.
#'   Must satisfy `1 <= k <= n - 1`.
#' @param col String color passed to [graphics::segments()].
#' @param lwd Positive number. Line width used to draw the strings.
#' @param plot Logical. If `TRUE`, draws the figure.
#' @param show_points Logical. If `TRUE`, draws the pegs.
#' @param show_labels Logical. If `TRUE`, draws peg labels.
#' @param verbose Logical. If `TRUE`, prints a short audit to the console.
#' @param r Positive number. Radius of the circle.
#' @param rotate Numeric. Rotation angle in radians applied to the whole figure.
#' @param show_strings Logical. If `TRUE`, draws the string connections.
#' @param template Logical. If `TRUE`, draws only the peg template, without
#'   string connections. This is equivalent to setting `show_strings = FALSE`
#'   and `show_points = TRUE`.
#' @param point_col Peg border color.
#' @param point_cex Positive number. Peg size.
#' @param point_pch Plotting symbol used for pegs.
#' @param point_bg Peg background color when applicable.
#' @param label_cex Positive number. Label size.
#' @param label_col Label color.
#' @param border_col Circle border color.
#' @param border_lwd Positive number. Circle border line width.
#' @param bg Plot background color.
#' @param main Optional plot title. If `NULL`, no title is displayed.
#'
#' @details
#' The pegs are placed on a circle of radius `r`, centered at the origin. Pegs
#' are indexed from `1` to `n` in counterclockwise order, starting at `(r, 0)`,
#' after applying the optional rotation angle `rotate`.
#'
#' The multiplicative modular connection rule is:
#'
#' `to = ((k * (from - 1)) %% n) + 1`.
#'
#' For `k = 2`, this rule produces the classical circular multiplication-table
#' pattern commonly associated with a cardioid-like envelope.
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
#' stcardioid()
#'
#' res <- stcardioid(plot = FALSE)
#' head(res$pegs)
#' head(res$connections)
#' res$total_length
#'
#' stcardioid(n = 80, k = 2, col = "steelblue", lwd = 0.8)
#' stcardioid(n = 24, k = 5, show_points = TRUE, show_labels = TRUE)
#' stcardioid(template = TRUE)
#'
#' @importFrom graphics par plot lines segments points text
#' @export
stcardioid <- function(n = 120,
                       k = 2,
                       col = "antiquewhite",
                       lwd = 0.8,
                       plot = TRUE,
                       show_points = FALSE,
                       show_labels = FALSE,
                       verbose = FALSE,
                       r = 1,
                       rotate = 0,
                       show_strings = TRUE,
                       template = FALSE,
                       point_col = "darkorange2",
                       point_cex = 0.8,
                       point_pch = 21,
                       point_bg = "white",
                       label_cex = 0.7,
                       label_col = "black",
                       border_col = "goldenrod3",
                       border_lwd = 1.2,
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

  if (!is.numeric(r) || length(r) != 1L || is.na(r) || r <= 0) {
    stop("`r` must be a single positive number.", call. = FALSE)
  }

  if (!is.numeric(lwd) || length(lwd) != 1L || is.na(lwd) || lwd <= 0) {
    stop("`lwd` must be a single positive number.", call. = FALSE)
  }

  if (!is.numeric(border_lwd) || length(border_lwd) != 1L ||
      is.na(border_lwd) || border_lwd <= 0) {
    stop("`border_lwd` must be a single positive number.", call. = FALSE)
  }

  if (!is.numeric(rotate) || length(rotate) != 1L || is.na(rotate)) {
    stop("`rotate` must be a single numeric value.", call. = FALSE)
  }

  if (!is.logical(plot) || length(plot) != 1L || is.na(plot)) {
    stop("`plot` must be TRUE or FALSE.", call. = FALSE)
  }

  if (!is.logical(show_points) || length(show_points) != 1L || is.na(show_points)) {
    stop("`show_points` must be TRUE or FALSE.", call. = FALSE)
  }

  if (!is.logical(show_labels) || length(show_labels) != 1L || is.na(show_labels)) {
    stop("`show_labels` must be TRUE or FALSE.", call. = FALSE)
  }

  if (!is.logical(verbose) || length(verbose) != 1L || is.na(verbose)) {
    stop("`verbose` must be TRUE or FALSE.", call. = FALSE)
  }

  if (!is.logical(show_strings) || length(show_strings) != 1L || is.na(show_strings)) {
    stop("`show_strings` must be TRUE or FALSE.", call. = FALSE)
  }

  if (!is.logical(template) || length(template) != 1L || is.na(template)) {
    stop("`template` must be TRUE or FALSE.", call. = FALSE)
  }

  if (!is.numeric(point_cex) || length(point_cex) != 1L ||
      is.na(point_cex) || point_cex <= 0) {
    stop("`point_cex` must be a single positive number.", call. = FALSE)
  }

  if (!is.numeric(label_cex) || length(label_cex) != 1L ||
      is.na(label_cex) || label_cex <= 0) {
    stop("`label_cex` must be a single positive number.", call. = FALSE)
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

  theta <- seq(0, 2 * pi, length.out = n + 1L)[-(n + 1L)] + rotate

  pegs <- data.frame(
    index = seq_len(n),
    x = r * cos(theta),
    y = r * sin(theta)
  )

  from <- seq_len(n)
  to <- ((k * (from - 1L)) %% n) + 1L

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
  unique_targets <- length(unique(to))
  fixed_connections <- sum(from == to)

  audit <- c(
    "String Art audit",
    "Figure: cardioid",
    sprintf("Number of pegs: %d", n),
    sprintf("Multiplication factor: %d", k),
    sprintf("Radius: %.4f", r),
    sprintf("Rotation angle: %.4f radians", rotate),
    "Rule: to = ((k * (from - 1)) %% n) + 1",
    sprintf("Number of connections: %d", nrow(connections)),
    sprintf("Distinct target pegs: %d", unique_targets),
    sprintf("Fixed or zero-length connections: %d", fixed_connections),
    sprintf("gcd(n, k): %d", d),
    if (d == 1L) {
      "The multiplicative map is a permutation of the peg indices."
    } else {
      "The multiplicative map is not a permutation; some pegs may receive multiple connections."
    },
    sprintf("Total string length: %.4f", total_length)
  )

  if (plot) {
    old_par <- graphics::par(no.readonly = TRUE)
    on.exit(graphics::par(old_par), add = TRUE)

    graphics::par(bg = bg)

    margin <- 0.15 * r
    limits <- c(-r - margin, r + margin)

    graphics::plot(
      NA, NA,
      xlim = limits,
      ylim = limits,
      asp = 1,
      xlab = "",
      ylab = "",
      axes = FALSE,
      main = main
    )

    tt <- seq(0, 2 * pi, length.out = 500L)

    graphics::lines(
      r * cos(tt),
      r * sin(tt),
      col = border_col,
      lwd = border_lwd
    )

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
      label_radius <- 1.08 * r

      graphics::text(
        label_radius * cos(theta),
        label_radius * sin(theta),
        labels = pegs$index,
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
      figure = "cardioid",
      family = "circular",
      rule = "multiplicative_modular",
      formula = "to = ((k * (from - 1)) %% n) + 1",
      parameters = list(
        n = n,
        k = k,
        r = r,
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

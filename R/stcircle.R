#' Circular String Art
#'
#' `stcircle()` creates a circular String Art figure by placing equally spaced
#' pegs on a circle and connecting each peg to another peg using an additive
#' modular step.
#'
#' @param n Integer. Number of pegs on the circle. Defaults to `30`.
#' @param k Integer. Additive modular step used to define the connections.
#'   Defaults to `5`.
#' @param col String color used to draw the segments. Defaults to `"blue"`.
#' @param lwd Positive number. Line width of the string segments. Defaults to `1`.
#' @param plot Logical. If `TRUE`, the figure is drawn using base R graphics.
#' @param show_points Logical. If `TRUE`, the pegs are shown.
#' @param show_labels Logical. If `TRUE`, peg labels are shown.
#' @param verbose Logical. If `TRUE`, an audit summary is printed to the console.
#' @param r Positive number. Radius of the circle. Defaults to `1`.
#' @param show_strings Logical. If `TRUE`, string segments are drawn.
#' @param template Logical. If `TRUE`, only the peg template is drawn. This sets
#'   `show_strings = FALSE` and `show_points = TRUE` internally.
#' @param point_col Color of the pegs.
#' @param point_cex Positive number. Size of the pegs.
#' @param point_pch Plotting symbol used for the pegs.
#' @param point_bg Background color of the pegs when the plotting symbol allows
#'   filling.
#' @param label_cex Positive number. Size of the peg labels.
#' @param label_col Color of the peg labels.
#' @param border_col Color of the circular border.
#' @param border_lwd Positive number. Line width of the circular border.
#' @param main Character string. Plot title. If `NULL`, a default title is used.
#'
#' @details
#' Pegs are numbered from `1` to `n` counterclockwise, starting at `(r, 0)`.
#' For each peg `i`, the connected peg is defined by
#'
#' \deqn{j = ((i + k - 1) \bmod n) + 1.}
#'
#' When `gcd(n, k) = 1`, the rule creates a single cycle passing through all
#' pegs. When `gcd(n, k) > 1`, the figure is decomposed into independent cycles.
#'
#' @return Invisibly returns a list with the following elements:
#' \describe{
#'   \item{pegs}{A data frame with columns `index`, `x`, and `y`.}
#'   \item{connections}{A data frame with columns `connection_index`, `from`,
#'   `to`, `x_from`, `y_from`, `x_to`, `y_to`, and `length`.}
#'   \item{total_length}{Total string length.}
#'   \item{audit}{Character vector with an audit summary.}
#'   \item{meta}{List with metadata about the construction.}
#' }
#'
#' @examples
#' stcircle()
#'
#' res <- stcircle(plot = FALSE)
#' res$total_length
#' head(res$connections)
#'
#' stcircle(n = 24, k = 7, col = "firebrick", lwd = 1.2,
#'          show_points = TRUE, show_labels = TRUE)
#'
#' stcircle(n = 24, k = 7, template = TRUE)
#'
#' @importFrom graphics lines plot points segments text
#' @export
stcircle <- function(n = 30,
                     k = 5,
                     col = "blue",
                     lwd = 1,
                     plot = TRUE,
                     show_points = TRUE,
                     show_labels = FALSE,
                     verbose = FALSE,
                     r = 1,
                     show_strings = TRUE,
                     template = FALSE,
                     point_col = "black",
                     point_cex = 0.8,
                     point_pch = 19,
                     point_bg = "white",
                     label_cex = 0.7,
                     label_col = "black",
                     border_col = "grey50",
                     border_lwd = 1,
                     main = NULL) {

  # ---------------------------------------------------------------------------
  # Validation
  # ---------------------------------------------------------------------------
  if (!is.numeric(n) || length(n) != 1L || is.na(n) ||
      n != as.integer(n) || n < 3L) {
    stop("`n` must be a single integer greater than or equal to 3.",
         call. = FALSE)
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

  if (!is.numeric(point_cex) || length(point_cex) != 1L ||
      is.na(point_cex) || point_cex <= 0) {
    stop("`point_cex` must be a single positive number.", call. = FALSE)
  }

  if (!is.numeric(label_cex) || length(label_cex) != 1L ||
      is.na(label_cex) || label_cex <= 0) {
    stop("`label_cex` must be a single positive number.", call. = FALSE)
  }

  logical_args <- list(
    plot = plot,
    show_points = show_points,
    show_labels = show_labels,
    verbose = verbose,
    show_strings = show_strings,
    template = template
  )

  for (arg_name in names(logical_args)) {
    value <- logical_args[[arg_name]]
    if (!is.logical(value) || length(value) != 1L || is.na(value)) {
      stop("`", arg_name, "` must be TRUE or FALSE.", call. = FALSE)
    }
  }

  if (template) {
    show_strings <- FALSE
    show_points <- TRUE
  }

  # ---------------------------------------------------------------------------
  # Geometry
  # ---------------------------------------------------------------------------
  theta <- seq(0, 2 * pi, length.out = n + 1L)[-(n + 1L)]

  pegs <- data.frame(
    index = seq_len(n),
    x = r * cos(theta),
    y = r * sin(theta)
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

  gcd_value <- local({
    a <- n
    b <- k
    while (b != 0L) {
      tmp <- b
      b <- a %% b
      a <- tmp
    }
    a
  })

  cycle_count <- gcd_value
  cycle_length <- n / gcd_value

  audit <- c(
    "String Art audit",
    "Figure: circle",
    sprintf("Number of pegs: %d", n),
    sprintf("Step k: %d", k),
    sprintf("Radius: %.6f", r),
    "Rule: to = ((from + k - 1) %% n) + 1",
    sprintf("Number of connections: %d", nrow(connections)),
    sprintf("Cycle count: %d", cycle_count),
    sprintf("Cycle length: %d", cycle_length),
    sprintf("Total string length: %.6f", total_length),
    if (show_strings) {
      "Plot mode: complete figure"
    } else {
      "Plot mode: peg template without strings"
    }
  )

  meta <- list(
    figure = "circle",
    rule = "additive modular step",
    n = n,
    k = k,
    r = r,
    cycle_count = cycle_count,
    cycle_length = cycle_length,
    plot = plot,
    show_points = show_points,
    show_labels = show_labels,
    show_strings = show_strings,
    template = template,
    string_color = col,
    string_lwd = lwd
  )

  # ---------------------------------------------------------------------------
  # Plot
  # ---------------------------------------------------------------------------
  if (plot) {
    if (is.null(main)) {
      main <- sprintf("Circular String Art (n = %d, k = %d)", n, k)
    }

    lim <- r * 1.18

    graphics::plot(
      NA,
      xlim = c(-lim, lim),
      ylim = c(-lim, lim),
      asp = 1,
      axes = FALSE,
      xlab = "",
      ylab = "",
      main = main
    )

    border_theta <- seq(0, 2 * pi, length.out = 361L)

    graphics::lines(
      r * cos(border_theta),
      r * sin(border_theta),
      col = border_col,
      lwd = border_lwd
    )

    if (show_strings) {
      graphics::segments(
        connections$x_from,
        connections$y_from,
        connections$x_to,
        connections$y_to,
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
      label_radius <- r * 1.08

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
    cat(paste(audit, collapse = "\n"), "\n")
  }

  result <- list(
    pegs = pegs,
    connections = connections,
    total_length = total_length,
    audit = audit,
    meta = meta
  )

  invisible(result)
}

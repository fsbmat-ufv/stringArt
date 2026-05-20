#' Generate a parabolic string art envelope
#'
#' `stparabola()` generates a classical string art construction on two
#' perpendicular axes. Pegs are placed on a horizontal axis and on a vertical
#' axis, and straight strings are drawn between corresponding peg positions.
#' The resulting family of line segments visually suggests a parabolic envelope.
#'
#' @param n Integer. Number of pegs placed on each axis. Must be at least 3.
#' @param k Integer. Number of shifted sweeps used in the construction. Must
#'   satisfy `1 <= k <= n - 1`. The classical construction is obtained with
#'   `k = 1`.
#' @param col String color passed to [graphics::segments()].
#' @param lwd Positive number. Line width used to draw the strings.
#' @param plot Logical. If `TRUE`, draws the figure.
#' @param show_points Logical. If `TRUE`, draws the pegs.
#' @param show_labels Logical. If `TRUE`, draws peg labels.
#' @param verbose Logical. If `TRUE`, prints a short audit to the console.
#' @param width Positive number. Length of the horizontal axis.
#' @param height Positive number. Length of the vertical axis.
#' @param show_strings Logical. If `TRUE`, draws the string connections.
#' @param template Logical. If `TRUE`, draws only the peg template, without
#'   string connections. This is equivalent to setting `show_strings = FALSE`
#'   and `show_points = TRUE`.
#' @param show_envelope Logical. If `TRUE`, draws the theoretical envelope of
#'   the classical construction.
#' @param envelope_col Color used for the theoretical envelope.
#' @param envelope_lwd Positive number. Line width used for the envelope.
#' @param envelope_lty Line type used for the envelope.
#' @param point_col Peg color.
#' @param point_cex Positive number. Peg size.
#' @param point_pch Plotting symbol used for pegs.
#' @param point_bg Peg background color when applicable.
#' @param label_cex Positive number. Label size.
#' @param label_col Label color.
#' @param border_col Axis color.
#' @param border_lwd Positive number. Axis line width.
#' @param bg Plot background color.
#' @param main Optional plot title. If `NULL`, no title is displayed.
#'
#' @details
#' This is one of the most classical string art constructions. Pegs are placed
#' on two perpendicular axes. In the basic case, the peg at position `i` on the
#' horizontal axis is connected to the peg at position `i` on the vertical axis,
#' where the vertical axis is indexed from top to bottom.
#'
#' For `k = 1`, the construction corresponds to the standard family of segments
#' joining points `(t, 0)` and `(0, 1 - t)`, after scaling by `width` and
#' `height`. Its ideal envelope satisfies
#' `sqrt(x / width) + sqrt(y / height) = 1`.
#'
#' For `k > 1`, the function adds shifted sweeps of the same construction,
#' producing denser string art patterns while preserving the same pedagogical
#' idea of a family of straight lines generating a curved envelope.
#'
#' @return Invisibly returns a list of class `stringart_result` with:
#' \describe{
#'   \item{pegs}{A `data.frame` with columns `index`, `x`, `y`, `axis`, and
#'   `local_index`.}
#'   \item{connections}{A `data.frame` with columns `connection_index`,
#'   `from`, `to`, `x_from`, `y_from`, `x_to`, `y_to`, `length`, `sweep`,
#'   `offset`, `local_from`, and `local_to`.}
#'   \item{total_length}{Total string length.}
#'   \item{audit}{A character vector with audit information.}
#'   \item{meta}{A list with construction metadata.}
#' }
#'
#' @examples
#' stparabola()
#'
#' res <- stparabola(plot = FALSE)
#' head(res$pegs)
#' head(res$connections)
#' res$total_length
#'
#' stparabola(n = 40, k = 1, col = "steelblue", lwd = 1)
#' stparabola(n = 40, k = 4, col = "firebrick", lwd = 0.6)
#' stparabola(show_points = TRUE, show_labels = TRUE)
#' stparabola(template = TRUE)
#'
#' @importFrom graphics par plot segments points text lines
#' @export
stparabola <- function(n = 30,
                       k = 1,
                       col = "blue",
                       lwd = 1,
                       plot = TRUE,
                       show_points = TRUE,
                       show_labels = FALSE,
                       verbose = FALSE,
                       width = 1,
                       height = 1,
                       show_strings = TRUE,
                       template = FALSE,
                       show_envelope = FALSE,
                       envelope_col = "red",
                       envelope_lwd = 1,
                       envelope_lty = 2,
                       point_col = "black",
                       point_cex = 0.8,
                       point_pch = 19,
                       point_bg = "white",
                       label_cex = 0.7,
                       label_col = "black",
                       border_col = "grey50",
                       border_lwd = 1,
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

  positive_numeric <- list(
    lwd = lwd,
    width = width,
    height = height,
    envelope_lwd = envelope_lwd,
    point_cex = point_cex,
    label_cex = label_cex,
    border_lwd = border_lwd
  )

  for (arg_name in names(positive_numeric)) {
    value <- positive_numeric[[arg_name]]
    if (!is.numeric(value) || length(value) != 1L || is.na(value) || value <= 0) {
      stop(sprintf("`%s` must be a single positive number.", arg_name), call. = FALSE)
    }
  }

  logical_args <- list(
    plot = plot,
    show_points = show_points,
    show_labels = show_labels,
    verbose = verbose,
    show_strings = show_strings,
    template = template,
    show_envelope = show_envelope
  )

  for (arg_name in names(logical_args)) {
    value <- logical_args[[arg_name]]
    if (!is.logical(value) || length(value) != 1L || is.na(value)) {
      stop(sprintf("`%s` must be TRUE or FALSE.", arg_name), call. = FALSE)
    }
  }

  if (template) {
    show_strings <- FALSE
    show_points <- TRUE
  }

  horizontal_pegs <- data.frame(
    index = seq_len(n),
    x = seq(0, width, length.out = n),
    y = rep(0, n),
    axis = "horizontal",
    local_index = seq_len(n),
    stringsAsFactors = FALSE
  )

  vertical_pegs <- data.frame(
    index = n + seq_len(n),
    x = rep(0, n),
    y = seq(height, 0, length.out = n),
    axis = "vertical",
    local_index = seq_len(n),
    stringsAsFactors = FALSE
  )

  pegs <- rbind(horizontal_pegs, vertical_pegs)
  rownames(pegs) <- NULL

  connection_list <- vector("list", n * k)
  connection_id <- 0L

  for (sweep in seq_len(k)) {
    offset <- sweep - 1L

    local_from <- seq_len(n)
    local_to <- ((local_from - 1L + offset) %% n) + 1L

    from <- horizontal_pegs$index[local_from]
    to <- vertical_pegs$index[local_to]

    x_from <- pegs$x[from]
    y_from <- pegs$y[from]
    x_to <- pegs$x[to]
    y_to <- pegs$y[to]

    lengths <- sqrt((x_to - x_from)^2 + (y_to - y_from)^2)

    for (i in seq_len(n)) {
      connection_id <- connection_id + 1L

      connection_list[[connection_id]] <- data.frame(
        connection_index = connection_id,
        from = from[i],
        to = to[i],
        x_from = x_from[i],
        y_from = y_from[i],
        x_to = x_to[i],
        y_to = y_to[i],
        length = lengths[i],
        sweep = sweep,
        offset = offset,
        local_from = local_from[i],
        local_to = local_to[i]
      )
    }
  }

  connections <- do.call(rbind, connection_list)
  rownames(connections) <- NULL

  total_length <- sum(connections$length)

  audit <- c(
    "String Art audit",
    "Figure: parabola",
    sprintf("Pegs per axis: %d", n),
    sprintf("Total number of pegs: %d", nrow(pegs)),
    sprintf("Number of sweeps: %d", k),
    sprintf("Number of connections: %d", nrow(connections)),
    sprintf("Width: %.4f", width),
    sprintf("Height: %.4f", height),
    "Classical rule for k = 1: connect horizontal peg i to vertical peg i.",
    "Ideal envelope for k = 1: sqrt(x / width) + sqrt(y / height) = 1.",
    sprintf("Total string length: %.4f", total_length)
  )

  if (plot) {
    old_par <- graphics::par(no.readonly = TRUE)
    on.exit(graphics::par(old_par), add = TRUE)

    graphics::par(bg = bg)

    x_pad <- 0.08 * width
    y_pad <- 0.08 * height

    graphics::plot(
      NA, NA,
      xlim = c(-x_pad, width + x_pad),
      ylim = c(-y_pad, height + y_pad),
      asp = 1,
      axes = FALSE,
      xlab = "",
      ylab = "",
      main = main
    )

    graphics::segments(
      x0 = 0,
      y0 = 0,
      x1 = width,
      y1 = 0,
      col = border_col,
      lwd = border_lwd
    )

    graphics::segments(
      x0 = 0,
      y0 = 0,
      x1 = 0,
      y1 = height,
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

    if (show_envelope) {
      t_values <- seq(0, 1, length.out = 300L)

      graphics::lines(
        x = width * t_values^2,
        y = height * (1 - t_values)^2,
        col = envelope_col,
        lwd = envelope_lwd,
        lty = envelope_lty
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
        labels = pegs$local_index,
        pos = ifelse(pegs$axis == "horizontal", 1, 2),
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
      figure = "parabola",
      family = "envelope",
      rule = "axis_envelope_sweeps",
      formula = "sqrt(x / width) + sqrt(y / height) = 1 for k = 1",
      mathematical_topics = c(
        "analytic geometry",
        "line envelopes",
        "quadratic functions",
        "parametric curves",
        "proportionality"
      ),
      classroom_level = c(
        "middle school",
        "high school",
        "undergraduate calculus"
      ),
      parameters = list(
        n = n,
        k = k,
        width = width,
        height = height,
        col = col,
        lwd = lwd,
        show_points = show_points,
        show_labels = show_labels,
        show_strings = show_strings,
        template = template,
        show_envelope = show_envelope
      )
    )
  )

  class(result) <- c("stringart_result", class(result))

  invisible(result)
}

#' Generate a string art net from two rays
#'
#' `stnet()` generates a string art net by placing pegs on two rays that share
#' a common vertex and connecting corresponding peg positions. The construction
#' generalizes the classical parabolic string art envelope by allowing the angle
#' between the two supporting rays to vary.
#'
#' @param n Integer. Number of pegs placed on each ray. Must be at least 3.
#' @param k Integer. Number of shifted sweeps used in the construction. Must
#'   satisfy `1 <= k <= n - 1`. The classical net is obtained with `k = 1`.
#' @param col String color passed to [graphics::segments()].
#' @param lwd Positive number. Line width used to draw the strings.
#' @param plot Logical. If `TRUE`, draws the figure.
#' @param show_points Logical. If `TRUE`, draws the pegs.
#' @param show_labels Logical. If `TRUE`, draws peg labels.
#' @param verbose Logical. If `TRUE`, prints a short audit to the console.
#' @param length1 Positive number. Length of the first ray.
#' @param length2 Positive number. Length of the second ray.
#' @param angle Numeric. Angle in radians from the first ray to the second ray.
#'   Must not be a multiple of `pi`.
#' @param rotate Numeric. Rotation angle in radians applied to the whole net.
#' @param show_strings Logical. If `TRUE`, draws the string connections.
#' @param template Logical. If `TRUE`, draws only the peg template, without
#'   string connections. This is equivalent to setting `show_strings = FALSE`
#'   and `show_points = TRUE`.
#' @param show_envelope Logical. If `TRUE`, draws the theoretical envelope of
#'   the basic construction.
#' @param envelope_col Color used for the theoretical envelope.
#' @param envelope_lwd Positive number. Line width used for the envelope.
#' @param envelope_lty Line type used for the envelope.
#' @param point_col Peg color.
#' @param point_cex Positive number. Peg size.
#' @param point_pch Plotting symbol used for pegs.
#' @param point_bg Peg background color when applicable.
#' @param label_cex Positive number. Label size.
#' @param label_col Label color.
#' @param border_col Ray color.
#' @param border_lwd Positive number. Ray line width.
#' @param bg Plot background color.
#' @param main Optional plot title. If `NULL`, no title is displayed.
#'
#' @details
#' The construction uses two rays with a common vertex. Pegs are placed uniformly
#' along the first ray from the common vertex to the first endpoint and along the
#' second ray from the second endpoint back to the common vertex.
#'
#' For `k = 1`, the peg at local position `i` on the first ray is connected to
#' the peg at local position `i` on the second ray. In oblique coordinates
#' determined by the two rays, the theoretical envelope is given by
#'
#' `C(t) = t^2 A + (1 - t)^2 B`,
#'
#' where `A` and `B` are the endpoints of the two rays and `0 <= t <= 1`.
#'
#' For `k > 1`, the function adds shifted sweeps of the same construction,
#' producing denser string art nets.
#'
#' @return Invisibly returns a list of class `stringart_result` with:
#' \describe{
#'   \item{pegs}{A `data.frame` with columns `index`, `x`, `y`, `ray`, and
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
#' stnet()
#'
#' res <- stnet(plot = FALSE)
#' head(res$pegs)
#' head(res$connections)
#' res$total_length
#'
#' stnet(n = 40, k = 1, angle = pi / 2, col = "steelblue")
#' stnet(n = 40, k = 3, angle = 2 * pi / 3, col = "firebrick", lwd = 0.7)
#' stnet(show_envelope = TRUE)
#' stnet(template = TRUE)
#'
#' @importFrom graphics par plot segments points text lines
#' @export
stnet <- function(n = 30,
                  k = 1,
                  col = "blue",
                  lwd = 1,
                  plot = TRUE,
                  show_points = TRUE,
                  show_labels = FALSE,
                  verbose = FALSE,
                  length1 = 1,
                  length2 = 1,
                  angle = pi / 2,
                  rotate = 0,
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
    length1 = length1,
    length2 = length2,
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

  if (!is.numeric(angle) || length(angle) != 1L || is.na(angle)) {
    stop("`angle` must be a single numeric value.", call. = FALSE)
  }

  if (abs(sin(angle)) < sqrt(.Machine$double.eps)) {
    stop("`angle` must not be a multiple of pi.", call. = FALSE)
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

  endpoint1 <- c(
    length1 * cos(rotate),
    length1 * sin(rotate)
  )

  endpoint2 <- c(
    length2 * cos(rotate + angle),
    length2 * sin(rotate + angle)
  )

  t_values <- seq(0, 1, length.out = n)

  ray1_pegs <- data.frame(
    index = seq_len(n),
    x = t_values * endpoint1[1],
    y = t_values * endpoint1[2],
    ray = "first",
    local_index = seq_len(n),
    stringsAsFactors = FALSE
  )

  ray2_pegs <- data.frame(
    index = n + seq_len(n),
    x = rev(t_values) * endpoint2[1],
    y = rev(t_values) * endpoint2[2],
    ray = "second",
    local_index = seq_len(n),
    stringsAsFactors = FALSE
  )

  pegs <- rbind(ray1_pegs, ray2_pegs)
  rownames(pegs) <- NULL

  connection_list <- vector("list", n * k)
  connection_id <- 0L

  for (sweep in seq_len(k)) {
    offset <- sweep - 1L

    local_from <- seq_len(n)
    local_to <- ((local_from - 1L + offset) %% n) + 1L

    from <- ray1_pegs$index[local_from]
    to <- ray2_pegs$index[local_to]

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
    "Figure: net",
    sprintf("Pegs per ray: %d", n),
    sprintf("Total number of pegs: %d", nrow(pegs)),
    sprintf("Number of sweeps: %d", k),
    sprintf("Number of connections: %d", nrow(connections)),
    sprintf("First ray length: %.4f", length1),
    sprintf("Second ray length: %.4f", length2),
    sprintf("Angle between rays: %.4f radians", angle),
    sprintf("Rotation angle: %.4f radians", rotate),
    "Classical rule for k = 1: connect local peg i on the first ray to local peg i on the second ray.",
    "Envelope in oblique coordinates for k = 1: C(t) = t^2 A + (1 - t)^2 B.",
    sprintf("Total string length: %.4f", total_length)
  )

  if (plot) {
    old_par <- graphics::par(no.readonly = TRUE)
    on.exit(graphics::par(old_par), add = TRUE)

    graphics::par(bg = bg)

    all_x <- c(0, endpoint1[1], endpoint2[1], pegs$x)
    all_y <- c(0, endpoint1[2], endpoint2[2], pegs$y)

    x_range <- range(all_x)
    y_range <- range(all_y)

    x_pad <- 0.08 * diff(x_range)
    y_pad <- 0.08 * diff(y_range)

    if (x_pad == 0) x_pad <- 0.1
    if (y_pad == 0) y_pad <- 0.1

    graphics::plot(
      NA, NA,
      xlim = c(x_range[1] - x_pad, x_range[2] + x_pad),
      ylim = c(y_range[1] - y_pad, y_range[2] + y_pad),
      asp = 1,
      axes = FALSE,
      xlab = "",
      ylab = "",
      main = main
    )

    graphics::segments(
      x0 = 0,
      y0 = 0,
      x1 = endpoint1[1],
      y1 = endpoint1[2],
      col = border_col,
      lwd = border_lwd
    )

    graphics::segments(
      x0 = 0,
      y0 = 0,
      x1 = endpoint2[1],
      y1 = endpoint2[2],
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
      envelope_t <- seq(0, 1, length.out = 300L)

      graphics::lines(
        x = envelope_t^2 * endpoint1[1] + (1 - envelope_t)^2 * endpoint2[1],
        y = envelope_t^2 * endpoint1[2] + (1 - envelope_t)^2 * endpoint2[2],
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
        pos = ifelse(pegs$ray == "first", 1, 2),
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
      figure = "net",
      family = "envelope",
      rule = "two_ray_envelope_sweeps",
      formula = "C(t) = t^2 A + (1 - t)^2 B for k = 1",
      mathematical_topics = c(
        "analytic geometry",
        "line envelopes",
        "oblique coordinates",
        "proportionality",
        "parametric curves"
      ),
      classroom_level = c(
        "middle school",
        "high school",
        "undergraduate calculus"
      ),
      parameters = list(
        n = n,
        k = k,
        length1 = length1,
        length2 = length2,
        angle = angle,
        rotate = rotate,
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

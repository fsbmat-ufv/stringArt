#' Generate a string art figure from a closed region contour
#'
#' `stregion()` generates a filled string art pattern from a closed contour.
#' Pegs are distributed along the contour and connected to approximately opposite
#' pegs, producing strings that cross the interior of the region.
#'
#' @param n Integer. Number of pegs placed along the contour. Must be at least 4.
#' @param k Integer. Number of sweep offsets used to fill the region. Must be at
#'   least 1.
#' @param col String color passed to [graphics::segments()].
#' @param lwd Positive number. Line width used to draw the strings.
#' @param plot Logical. If `TRUE`, draws the figure.
#' @param show_points Logical. If `TRUE`, draws the pegs.
#' @param show_labels Logical. If `TRUE`, draws peg labels.
#' @param verbose Logical. If `TRUE`, prints a short audit to the console.
#' @param contour Optional `data.frame` with columns `x` and `y` defining a
#'   closed or open polygonal contour. If `NULL`, a default ellipse-like contour
#'   is used.
#' @param show_strings Logical. If `TRUE`, draws the string connections.
#' @param template Logical. If `TRUE`, draws only the peg template, without
#'   string connections. This is equivalent to setting `show_strings = FALSE`
#'   and `show_points = TRUE`.
#' @param draw_border Logical. If `TRUE`, draws the region border.
#' @param border_col Border color.
#' @param border_lwd Positive number. Border line width.
#' @param point_col Peg color.
#' @param point_cex Positive number. Peg size.
#' @param point_pch Plotting symbol used for pegs.
#' @param point_bg Peg background color when applicable.
#' @param label_cex Positive number. Label size.
#' @param label_col Label color.
#' @param bg Plot background color.
#' @param main Optional plot title. If `NULL`, no title is displayed.
#' @param add Logical. If `TRUE`, adds the string art region to the current
#'   graphics device instead of creating a new plot.
#' @param xlim,ylim Optional axis limits used when `plot = TRUE` and
#'   `add = FALSE`.
#'
#' @details
#' Unlike circular, elliptical, or triangular modular string art patterns that
#' usually connect nearby pegs using a fixed additive step, `stregion()` is
#' designed to fill a region. It connects each peg to a peg located approximately
#' on the opposite side of the contour.
#'
#' The main connection rule is:
#'
#' `to = ((from - 1 + floor(n / 2) + offset) %% n) + 1`.
#'
#' The argument `k` controls the number of offsets. Each offset produces one
#' sweep of strings across the interior. Larger values of `k` create denser
#' fillings.
#'
#' @return Invisibly returns a list of class `stringart_result` with:
#' \describe{
#'   \item{pegs}{A `data.frame` with columns `index`, `x`, and `y`.}
#'   \item{connections}{A `data.frame` with columns `connection_index`,
#'   `from`, `to`, `x_from`, `y_from`, `x_to`, `y_to`, `length`, `sweep`,
#'   and `offset`.}
#'   \item{total_length}{Total string length.}
#'   \item{audit}{A character vector with audit information.}
#'   \item{meta}{A list with construction metadata.}
#' }
#'
#' @examples
#' stregion()
#'
#' res <- stregion(plot = FALSE)
#' head(res$pegs)
#' head(res$connections)
#' res$total_length
#'
#' custom_contour <- data.frame(
#'   x = c(0, 1, 0.6, -0.6, -1),
#'   y = c(1, 0.2, -0.9, -0.9, 0.2)
#' )
#' stregion(contour = custom_contour, n = 80, k = 3, col = "steelblue")
#' stregion(template = TRUE)
#'
#' @importFrom graphics par plot polygon segments points text
#' @export
stregion <- function(n = 100,
                     k = 4,
                     col = "red",
                     lwd = 0.6,
                     plot = TRUE,
                     show_points = TRUE,
                     show_labels = FALSE,
                     verbose = FALSE,
                     contour = NULL,
                     show_strings = TRUE,
                     template = FALSE,
                     draw_border = TRUE,
                     border_col = "grey50",
                     border_lwd = 1,
                     point_col = "black",
                     point_cex = 0.5,
                     point_pch = 19,
                     point_bg = "white",
                     label_cex = 0.6,
                     label_col = "black",
                     bg = "white",
                     main = NULL,
                     add = FALSE,
                     xlim = NULL,
                     ylim = NULL) {

  if (!is.numeric(n) || length(n) != 1L || is.na(n) ||
      n != as.integer(n) || n < 4L) {
    stop("`n` must be a single integer greater than or equal to 4.", call. = FALSE)
  }

  if (!is.numeric(k) || length(k) != 1L || is.na(k) ||
      k != as.integer(k) || k < 1L) {
    stop("`k` must be a single positive integer.", call. = FALSE)
  }

  n <- as.integer(n)
  k <- as.integer(k)

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
    template = template,
    draw_border = draw_border,
    add = add
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

  if (is.null(contour)) {
    contour <- .st_default_region_contour()
  }

  if (!is.data.frame(contour) || !all(c("x", "y") %in% names(contour))) {
    stop("`contour` must be a data frame with columns `x` and `y`.", call. = FALSE)
  }

  contour <- contour[, c("x", "y")]
  contour$x <- as.numeric(contour$x)
  contour$y <- as.numeric(contour$y)

  if (nrow(contour) < 3L || anyNA(contour$x) || anyNA(contour$y)) {
    stop("`contour` must contain at least three valid points.", call. = FALSE)
  }

  pegs_xy <- .st_resample_closed_contour(contour, n)

  pegs <- data.frame(
    index = seq_len(n),
    x = pegs_xy$x,
    y = pegs_xy$y
  )

  half_turn <- floor(n / 2L)
  offsets <- seq.int(0L, k - 1L)

  connection_list <- vector("list", n * k)
  id <- 0L

  for (sweep in seq_len(k)) {
    offset <- offsets[sweep]

    from <- seq_len(n)
    to <- ((from - 1L + half_turn + offset) %% n) + 1L

    x_from <- pegs$x[from]
    y_from <- pegs$y[from]
    x_to <- pegs$x[to]
    y_to <- pegs$y[to]

    lengths <- sqrt((x_to - x_from)^2 + (y_to - y_from)^2)

    for (i in seq_len(n)) {
      id <- id + 1L

      connection_list[[id]] <- data.frame(
        connection_index = id,
        from = from[i],
        to = to[i],
        x_from = x_from[i],
        y_from = y_from[i],
        x_to = x_to[i],
        y_to = y_to[i],
        length = lengths[i],
        sweep = sweep,
        offset = offset
      )
    }
  }

  connections <- do.call(rbind, connection_list)
  rownames(connections) <- NULL

  total_length <- sum(connections$length)

  audit <- c(
    "String Art audit",
    "Figure: region",
    sprintf("Number of pegs: %d", n),
    sprintf("Number of sweeps: %d", k),
    sprintf("Half-turn step: %d", half_turn),
    sprintf("Number of connections: %d", nrow(connections)),
    sprintf("Total string length: %.4f", total_length),
    if (n %% 2L == 0L) {
      "The number of pegs is even; opposite peg connections are exact."
    } else {
      "The number of pegs is odd; opposite peg connections are approximate."
    }
  )

  if (plot) {
    old_par <- graphics::par(no.readonly = TRUE)
    on.exit(graphics::par(old_par), add = TRUE)

    graphics::par(bg = bg)

    if (!add) {
      x_range <- range(contour$x)
      y_range <- range(contour$y)

      x_pad <- if (diff(x_range) == 0) 0.1 else 0.06 * diff(x_range)
      y_pad <- if (diff(y_range) == 0) 0.1 else 0.06 * diff(y_range)

      if (is.null(xlim)) {
        xlim <- x_range + c(-x_pad, x_pad)
      }

      if (is.null(ylim)) {
        ylim <- y_range + c(-y_pad, y_pad)
      }

      graphics::plot(
        NA, NA,
        xlim = xlim,
        ylim = ylim,
        asp = 1,
        axes = FALSE,
        xlab = "",
        ylab = "",
        main = main
      )
    }

    if (draw_border) {
      graphics::polygon(
        contour$x,
        contour$y,
        border = border_col,
        lwd = border_lwd,
        col = NA
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
      figure = "region",
      family = "contour",
      rule = "opposite_contour_sweeps",
      formula = "to = ((from - 1 + floor(n / 2) + offset) %% n) + 1",
      parameters = list(
        n = n,
        k = k,
        col = col,
        lwd = lwd,
        show_points = show_points,
        show_labels = show_labels,
        show_strings = show_strings,
        template = template,
        draw_border = draw_border
      )
    )
  )

  class(result) <- c("stringart_result", class(result))

  invisible(result)
}

.st_default_region_contour <- function(n_points = 300L) {
  theta <- seq(0, 2 * pi, length.out = n_points + 1L)[-(n_points + 1L)]

  data.frame(
    x = 1.4 * cos(theta),
    y = 0.9 * sin(theta)
  )
}

.st_resample_closed_contour <- function(contour, n) {
  x <- contour$x
  y <- contour$y

  if (!isTRUE(all.equal(c(x[1], y[1]), c(x[length(x)], y[length(y)])))) {
    x <- c(x, x[1])
    y <- c(y, y[1])
  }

  dx <- diff(x)
  dy <- diff(y)
  segment_lengths <- sqrt(dx^2 + dy^2)

  if (any(segment_lengths <= 0)) {
    keep <- c(TRUE, segment_lengths > 0)
    x <- x[keep]
    y <- y[keep]
    dx <- diff(x)
    dy <- diff(y)
    segment_lengths <- sqrt(dx^2 + dy^2)
  }

  perimeter <- sum(segment_lengths)

  if (!is.finite(perimeter) || perimeter <= 0) {
    stop("`contour` must define a closed contour with positive perimeter.", call. = FALSE)
  }

  target_s <- seq(0, perimeter, length.out = n + 1L)[-(n + 1L)]
  cumulative <- c(0, cumsum(segment_lengths))

  px <- numeric(n)
  py <- numeric(n)

  for (i in seq_len(n)) {
    s <- target_s[i]
    segment_id <- min(findInterval(s, cumulative, rightmost.closed = TRUE),
                      length(segment_lengths))

    local_t <- (s - cumulative[segment_id]) / segment_lengths[segment_id]

    px[i] <- (1 - local_t) * x[segment_id] + local_t * x[segment_id + 1L]
    py[i] <- (1 - local_t) * y[segment_id] + local_t * y[segment_id + 1L]
  }

  data.frame(x = px, y = py)
}

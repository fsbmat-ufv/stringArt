#' Generate a radial string art pattern with triangular modules
#'
#' `stradial()` generates a radial string art pattern composed of triangular
#' modules rotated around the origin. In each module, pegs are placed along the
#' triangular boundary and connected according to an additive modular rule.
#'
#' @param n Integer. Number of pegs in each triangular module. Must be at least 3.
#' @param k Integer. Additive modular step used inside each module. Must satisfy
#'   `1 <= k <= n - 1`.
#' @param col String color, or a vector of colors with length 1 or `m`, used to
#'   draw the string connections.
#' @param lwd Positive number. Line width used to draw the strings.
#' @param plot Logical. If `TRUE`, draws the figure.
#' @param show_points Logical. If `TRUE`, draws the pegs.
#' @param show_labels Logical. If `TRUE`, draws peg labels.
#' @param verbose Logical. If `TRUE`, prints a short audit to the console.
#' @param m Integer. Number of triangular modules.
#' @param r Positive number. Distance from the origin to the two outer vertices
#'   of each triangular module.
#' @param spread Positive number. Angular opening, in radians, of each module.
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
#' @param border_col Module border color.
#' @param border_lwd Positive number. Module border line width.
#' @param bg Plot background color.
#' @param main Optional plot title. If `NULL`, no title is displayed.
#' @param show_center Logical. If `TRUE`, highlights the origin.
#' @param center_col Color used to highlight the origin.
#' @param center_cex Positive number. Size of the highlighted origin.
#'
#' @details
#' Each module is a triangle with vertices at the origin and at two outer points
#' determined by `r` and `spread`. The base module is rotated `m` times around
#' the origin.
#'
#' Within each module, the local connection rule is:
#'
#' `to = ((from + k - 1) %% n) + 1`.
#'
#' This means that each local peg is connected to the peg `k` positions ahead,
#' using modular indexing. The same local rule is applied independently to all
#' modules.
#'
#' @return Invisibly returns a list of class `stringart_result` with:
#' \describe{
#'   \item{pegs}{A `data.frame` with columns `index`, `x`, `y`, `module`, and
#'   `local_index`.}
#'   \item{connections}{A `data.frame` with columns `connection_index`, `from`,
#'   `to`, `x_from`, `y_from`, `x_to`, `y_to`, `length`, `module`,
#'   `local_from`, `local_to`, and `color`.}
#'   \item{total_length}{Total string length.}
#'   \item{audit}{A character vector with audit information.}
#'   \item{meta}{A list with construction metadata.}
#' }
#'
#' @examples
#' stradial()
#'
#' res <- stradial(plot = FALSE)
#' head(res$pegs)
#' head(res$connections)
#' res$total_length
#'
#' stradial(n = 18, k = 5, m = 6, col = "steelblue", lwd = 0.8)
#' stradial(n = 12, k = 4, m = 5, show_points = TRUE, show_labels = TRUE)
#' stradial(template = TRUE)
#'
#' @importFrom graphics par plot segments points text
#' @export
stradial <- function(n = 18,
                     k = 5,
                     col = "blue",
                     lwd = 1,
                     plot = TRUE,
                     show_points = TRUE,
                     show_labels = FALSE,
                     verbose = FALSE,
                     m = 6,
                     r = 1.2,
                     spread = pi / 5,
                     rotate = 0,
                     show_strings = TRUE,
                     template = FALSE,
                     point_col = "black",
                     point_cex = 0.8,
                     point_pch = 21,
                     point_bg = "white",
                     label_cex = 0.7,
                     label_col = "black",
                     border_col = "grey50",
                     border_lwd = 1,
                     bg = "white",
                     main = NULL,
                     show_center = TRUE,
                     center_col = "black",
                     center_cex = 0.9) {

  if (!is.numeric(n) || length(n) != 1L || is.na(n) ||
      n != as.integer(n) || n < 3L) {
    stop("`n` must be a single integer greater than or equal to 3.", call. = FALSE)
  }

  if (!is.numeric(k) || length(k) != 1L || is.na(k) ||
      k != as.integer(k) || k < 1L) {
    stop("`k` must be a single positive integer.", call. = FALSE)
  }

  if (!is.numeric(m) || length(m) != 1L || is.na(m) ||
      m != as.integer(m) || m < 1L) {
    stop("`m` must be a single integer greater than or equal to 1.", call. = FALSE)
  }

  n <- as.integer(n)
  k <- as.integer(k)
  m <- as.integer(m)

  if (k >= n) {
    stop("`k` must satisfy 1 <= k <= n - 1.", call. = FALSE)
  }

  if (!is.numeric(r) || length(r) != 1L || is.na(r) || r <= 0) {
    stop("`r` must be a single positive number.", call. = FALSE)
  }

  if (!is.numeric(spread) || length(spread) != 1L || is.na(spread) || spread <= 0) {
    stop("`spread` must be a single positive number.", call. = FALSE)
  }

  if (!is.numeric(rotate) || length(rotate) != 1L || is.na(rotate)) {
    stop("`rotate` must be a single numeric value.", call. = FALSE)
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

  if (!is.numeric(center_cex) || length(center_cex) != 1L ||
      is.na(center_cex) || center_cex <= 0) {
    stop("`center_cex` must be a single positive number.", call. = FALSE)
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

  if (!is.logical(show_center) || length(show_center) != 1L || is.na(show_center)) {
    stop("`show_center` must be TRUE or FALSE.", call. = FALSE)
  }

  if (template) {
    show_strings <- FALSE
    show_points <- TRUE
  }

  if (length(col) == 1L) {
    col <- rep(col, m)
  }

  if (length(col) != m) {
    stop("`col` must have length 1 or `m`.", call. = FALSE)
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

  rotate_points <- function(x, y, angle) {
    cbind(
      x = x * cos(angle) - y * sin(angle),
      y = x * sin(angle) + y * cos(angle)
    )
  }

  interpolate_segment <- function(p, q, t) {
    c(
      (1 - t) * p[1] + t * q[1],
      (1 - t) * p[2] + t * q[2]
    )
  }

  triangular_boundary_points <- function(a, b, c, n_points) {
    len_ab <- sqrt(sum((b - a)^2))
    len_bc <- sqrt(sum((c - b)^2))
    len_ca <- sqrt(sum((a - c)^2))
    perimeter <- len_ab + len_bc + len_ca

    s <- seq(0, perimeter, length.out = n_points + 1L)[-(n_points + 1L)]
    pts <- matrix(0, nrow = n_points, ncol = 2)

    for (idx in seq_along(s)) {
      si <- s[idx]

      if (si < len_ab) {
        t <- si / len_ab
        pts[idx, ] <- interpolate_segment(a, b, t)
      } else if (si < len_ab + len_bc) {
        t <- (si - len_ab) / len_bc
        pts[idx, ] <- interpolate_segment(b, c, t)
      } else {
        t <- (si - len_ab - len_bc) / len_ca
        pts[idx, ] <- interpolate_segment(c, a, t)
      }
    }

    colnames(pts) <- c("x", "y")
    as.data.frame(pts)
  }

  center <- c(0, 0)
  left_vertex <- c(r * cos(-spread / 2), r * sin(-spread / 2))
  right_vertex <- c(r * cos(spread / 2), r * sin(spread / 2))

  pegs_list <- vector("list", m)
  connections_list <- vector("list", m)
  module_vertices <- vector("list", m)

  for (module_id in seq_len(m)) {
    angle <- rotate + 2 * pi * (module_id - 1L) / m

    center_rot <- as.numeric(rotate_points(center[1], center[2], angle))
    left_rot <- as.numeric(rotate_points(left_vertex[1], left_vertex[2], angle))
    right_rot <- as.numeric(rotate_points(right_vertex[1], right_vertex[2], angle))

    module_vertices[[module_id]] <- data.frame(
      module = module_id,
      vertex = c("center", "left", "right"),
      x = c(center_rot[1], left_rot[1], right_rot[1]),
      y = c(center_rot[2], left_rot[2], right_rot[2]),
      stringsAsFactors = FALSE
    )

    module_pegs <- triangular_boundary_points(center_rot, left_rot, right_rot, n)
    global_index <- (module_id - 1L) * n + seq_len(n)

    pegs_list[[module_id]] <- data.frame(
      index = global_index,
      x = module_pegs$x,
      y = module_pegs$y,
      module = module_id,
      local_index = seq_len(n)
    )

    local_from <- seq_len(n)
    local_to <- ((local_from + k - 1L) %% n) + 1L

    from <- global_index[local_from]
    to <- global_index[local_to]

    con <- data.frame(
      connection_index = (module_id - 1L) * n + seq_len(n),
      from = from,
      to = to,
      x_from = module_pegs$x[local_from],
      y_from = module_pegs$y[local_from],
      x_to = module_pegs$x[local_to],
      y_to = module_pegs$y[local_to],
      module = module_id,
      local_from = local_from,
      local_to = local_to,
      color = col[module_id],
      stringsAsFactors = FALSE
    )

    con$length <- sqrt((con$x_to - con$x_from)^2 + (con$y_to - con$y_from)^2)

    con <- con[, c(
      "connection_index", "from", "to", "x_from", "y_from",
      "x_to", "y_to", "length", "module", "local_from",
      "local_to", "color"
    )]

    connections_list[[module_id]] <- con
  }

  pegs <- do.call(rbind, pegs_list)
  connections <- do.call(rbind, connections_list)
  vertices <- do.call(rbind, module_vertices)

  rownames(pegs) <- NULL
  rownames(connections) <- NULL
  rownames(vertices) <- NULL

  total_length <- sum(connections$length)

  d <- gcd_int(n, k)

  audit <- c(
    "String Art audit",
    "Figure: radial",
    sprintf("Number of modules: %d", m),
    sprintf("Pegs per module: %d", n),
    sprintf("Total pegs: %d", nrow(pegs)),
    sprintf("Step: %d", k),
    sprintf("Radius: %.4f", r),
    sprintf("Spread: %.4f radians", spread),
    sprintf("Rotation angle: %.4f radians", rotate),
    "Rule in each module: to = ((from + k - 1) %% n) + 1",
    sprintf("Connections per module: %d", n),
    sprintf("Total connections: %d", nrow(connections)),
    sprintf("gcd(n, k): %d", d),
    if (d == 1L) {
      "The local modular rule forms one cycle in each module."
    } else {
      sprintf("The local modular rule forms %d independent cycles in each module.", d)
    },
    sprintf("Total string length: %.4f", total_length)
  )

  if (plot) {
    old_par <- graphics::par(no.readonly = TRUE)
    on.exit(graphics::par(old_par), add = TRUE)

    graphics::par(bg = bg)

    x_range <- range(c(pegs$x, vertices$x))
    y_range <- range(c(pegs$y, vertices$y))
    x_margin <- max(0.08 * diff(x_range), 0.1 * r)
    y_margin <- max(0.08 * diff(y_range), 0.1 * r)

    graphics::plot(
      NA, NA,
      xlim = c(x_range[1] - x_margin, x_range[2] + x_margin),
      ylim = c(y_range[1] - y_margin, y_range[2] + y_margin),
      asp = 1,
      axes = FALSE,
      xlab = "",
      ylab = "",
      main = main
    )

    for (module_id in seq_len(m)) {
      v <- vertices[vertices$module == module_id, , drop = FALSE]
      vx <- v$x[match(c("center", "left", "right"), v$vertex)]
      vy <- v$y[match(c("center", "left", "right"), v$vertex)]

      graphics::segments(vx[1], vy[1], vx[2], vy[2], col = border_col, lwd = border_lwd)
      graphics::segments(vx[2], vy[2], vx[3], vy[3], col = border_col, lwd = border_lwd)
      graphics::segments(vx[3], vy[3], vx[1], vy[1], col = border_col, lwd = border_lwd)

      idx_connections <- connections$module == module_id

      if (show_strings) {
        graphics::segments(
          x0 = connections$x_from[idx_connections],
          y0 = connections$y_from[idx_connections],
          x1 = connections$x_to[idx_connections],
          y1 = connections$y_to[idx_connections],
          col = connections$color[idx_connections],
          lwd = lwd
        )
      }
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
        pos = 3,
        cex = label_cex,
        col = label_col
      )
    }

    if (show_center) {
      graphics::points(0, 0, pch = 19, col = center_col, cex = center_cex)
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
      figure = "radial",
      family = "triangular_modules",
      rule = "additive_modular_by_module",
      formula = "to = ((from + k - 1) %% n) + 1",
      parameters = list(
        n = n,
        k = k,
        m = m,
        r = r,
        spread = spread,
        rotate = rotate,
        col = col,
        lwd = lwd,
        show_points = show_points,
        show_labels = show_labels,
        show_strings = show_strings,
        template = template,
        show_center = show_center
      )
    )
  )

  class(result) <- c("stringart_result", class(result))

  invisible(result)
}

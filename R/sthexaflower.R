#' Generate a hexagonal flower string art pattern
#'
#' `sthexaflower()` generates a string art pattern based on three concentric
#' hexagonal peg circuits and one central peg. The construction is fully
#' reproducible and returns both the peg coordinates and the connection table.
#'
#' @param n Integer. Number of pegs in each hexagonal circuit. Must be a
#'   multiple of 6 and at least 6.
#' @param k Integer. Step used in the local modular connection rule. Must
#'   satisfy `1 <= k <= n - 1`.
#' @param col String color passed to [graphics::segments()]. It may have length
#'   1 or 6. If length 6, colors are used by sector.
#' @param lwd Positive number. Line width used to draw the strings.
#' @param plot Logical. If `TRUE`, draws the figure.
#' @param show_points Logical. If `TRUE`, draws the pegs.
#' @param show_labels Logical. If `TRUE`, draws peg labels.
#' @param verbose Logical. If `TRUE`, prints a short audit to the console.
#' @param r Positive number. Radius of the outer hexagonal circuit.
#' @param scale_mid Positive number in `(0, 1)`. Scale of the middle hexagonal
#'   circuit relative to the outer circuit.
#' @param scale_inner Positive number in `(0, scale_mid)`. Scale of the inner
#'   hexagonal circuit relative to the outer circuit.
#' @param offset_mid Numeric value in `[0, 1)`. Discrete relative offset applied
#'   to the middle circuit along the peg sequence.
#' @param offset_inner Numeric value in `[0, 1)`. Discrete relative offset
#'   applied to the inner circuit along the peg sequence.
#' @param rotate Numeric. Rotation angle in radians applied to the whole figure.
#' @param show_strings Logical. If `TRUE`, draws the string connections.
#' @param template Logical. If `TRUE`, draws only the peg template, without
#'   string connections. This is equivalent to setting `show_strings = FALSE`
#'   and `show_points = TRUE`.
#' @param point_col Peg color.
#' @param point_cex Positive number. Peg size.
#' @param point_pch Plotting symbol used for pegs.
#' @param point_bg Peg background color when applicable.
#' @param label_cex Positive number. Label size.
#' @param label_col Label color.
#' @param border_col Hexagonal border color.
#' @param border_lwd Positive number. Hexagonal border line width.
#' @param bg Plot background color.
#' @param main Optional plot title. If `NULL`, no title is displayed.
#'
#' @details
#' The function builds three concentric hexagonal circuits with `n` pegs each
#' and one central peg.
#'
#' The peg table contains the columns `index`, `x`, `y`, `group`, `layer`, and
#' `local_index`.
#'
#' The construction uses four connection blocks:
#'
#' - `outer_border`: consecutive connections on the outer hexagon.
#' - `outer_to_middle`: connections from the outer circuit to the middle circuit.
#' - `middle_to_inner`: connections from the middle circuit to the inner circuit.
#' - `vertices_to_center`: connections from the outer vertices to the central peg.
#'
#' The local additive modular rule used in the two radial blocks is
#'
#' `to_local = ((from_local + k - 1) %% n) + 1`.
#'
#' @return Invisibly returns a list of class `stringart_result` with:
#' \describe{
#'   \item{pegs}{A `data.frame` with peg coordinates and metadata.}
#'   \item{connections}{A `data.frame` with columns `connection_index`,
#'   `from`, `to`, `x_from`, `y_from`, `x_to`, `y_to`, `length`, `block`, and
#'   `sector`.}
#'   \item{total_length}{Total string length.}
#'   \item{audit}{A character vector with audit information.}
#'   \item{meta}{A list with construction metadata.}
#' }
#'
#' @examples
#' sthexaflower()
#'
#' res <- sthexaflower(plot = FALSE)
#' head(res$pegs)
#' head(res$connections)
#' res$total_length
#'
#' sthexaflower(n = 30, k = 7, col = "steelblue", lwd = 0.8)
#' sthexaflower(n = 24, k = 5, show_points = TRUE, show_labels = TRUE)
#' sthexaflower(template = TRUE)
#'
#' @importFrom graphics par plot lines segments points text
#' @export
sthexaflower <- function(n = 24,
                         k = 5,
                         col = c("black", "forestgreen", "darkorange",
                                 "deepskyblue4", "firebrick", "purple"),
                         lwd = 1,
                         plot = TRUE,
                         show_points = TRUE,
                         show_labels = FALSE,
                         verbose = FALSE,
                         r = 1,
                         scale_mid = 0.72,
                         scale_inner = 0.42,
                         offset_mid = 0,
                         offset_inner = 0,
                         rotate = 0,
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
                         bg = "white",
                         main = NULL) {

  if (!is.numeric(n) || length(n) != 1L || is.na(n) ||
      n != as.integer(n) || n < 6L) {
    stop("`n` must be a single integer greater than or equal to 6.", call. = FALSE)
  }

  n <- as.integer(n)

  if (n %% 6L != 0L) {
    stop("`n` must be a multiple of 6.", call. = FALSE)
  }

  if (!is.numeric(k) || length(k) != 1L || is.na(k) ||
      k != as.integer(k) || k < 1L) {
    stop("`k` must be a single positive integer.", call. = FALSE)
  }

  k <- as.integer(k)

  if (k >= n) {
    stop("`k` must satisfy 1 <= k <= n - 1.", call. = FALSE)
  }

  if (!is.numeric(r) || length(r) != 1L || is.na(r) || r <= 0) {
    stop("`r` must be a single positive number.", call. = FALSE)
  }

  if (!is.numeric(scale_mid) || length(scale_mid) != 1L || is.na(scale_mid) ||
      scale_mid <= 0 || scale_mid >= 1) {
    stop("`scale_mid` must be a single number in (0, 1).", call. = FALSE)
  }

  if (!is.numeric(scale_inner) || length(scale_inner) != 1L || is.na(scale_inner) ||
      scale_inner <= 0 || scale_inner >= scale_mid) {
    stop("`scale_inner` must be a single number in (0, scale_mid).", call. = FALSE)
  }

  if (!is.numeric(offset_mid) || length(offset_mid) != 1L || is.na(offset_mid) ||
      offset_mid < 0 || offset_mid >= 1) {
    stop("`offset_mid` must be a single number in [0, 1).", call. = FALSE)
  }

  if (!is.numeric(offset_inner) || length(offset_inner) != 1L ||
      is.na(offset_inner) || offset_inner < 0 || offset_inner >= 1) {
    stop("`offset_inner` must be a single number in [0, 1).", call. = FALSE)
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

  if (length(col) == 1L) {
    col <- rep(col, 6L)
  }

  if (length(col) != 6L) {
    stop("`col` must have length 1 or 6.", call. = FALSE)
  }

  if (template) {
    show_strings <- FALSE
    show_points <- TRUE
  }

  gcd_int <- function(x, y) {
    x <- abs(as.integer(x))
    y <- abs(as.integer(y))

    while (y != 0L) {
      tmp <- y
      y <- x %% y
      x <- tmp
    }

    x
  }

  rotate_points <- function(x, y, angle) {
    data.frame(
      x = x * cos(angle) - y * sin(angle),
      y = x * sin(angle) + y * cos(angle)
    )
  }

  hex_vertices <- function(radius) {
    angles <- seq(pi / 2, pi / 2 + 2 * pi, length.out = 7L)[1:6]

    xy <- rotate_points(
      x = radius * cos(angles),
      y = radius * sin(angles),
      angle = rotate
    )

    data.frame(x = xy$x, y = xy$y)
  }

  hex_perimeter_points <- function(radius, n_points, offset = 0) {
    vertices <- hex_vertices(radius)
    vertices_next <- rbind(vertices[2:6, ], vertices[1, ])

    points_per_side <- as.integer(n_points / 6L)

    pts <- data.frame(
      x = numeric(n_points),
      y = numeric(n_points)
    )

    idx <- 1L

    for (side in seq_len(6L)) {
      x1 <- vertices$x[side]
      y1 <- vertices$y[side]
      x2 <- vertices_next$x[side]
      y2 <- vertices_next$y[side]

      alpha_values <- seq(0, 1, length.out = points_per_side + 1L)[1:points_per_side]

      for (alpha in alpha_values) {
        pts$x[idx] <- (1 - alpha) * x1 + alpha * x2
        pts$y[idx] <- (1 - alpha) * y1 + alpha * y2
        idx <- idx + 1L
      }
    }

    if (offset != 0) {
      shift <- as.integer(round(offset * n_points)) %% n_points

      if (shift > 0L) {
        pts <- pts[c((shift + 1L):n_points, seq_len(shift)), , drop = FALSE]
        rownames(pts) <- NULL
      }
    }

    pts
  }

  sector_from_local_index <- function(local_index, n_local) {
    points_per_side <- as.integer(n_local / 6L)
    sector <- as.integer(((local_index - 1L) %/% points_per_side) + 1L)
    min(sector, 6L)
  }

  outer_pts <- hex_perimeter_points(r, n, offset = 0)
  middle_pts <- hex_perimeter_points(r * scale_mid, n, offset = offset_mid)
  inner_pts <- hex_perimeter_points(r * scale_inner, n, offset = offset_inner)

  pegs <- data.frame(
    index = seq_len(3L * n + 1L),
    x = c(outer_pts$x, middle_pts$x, inner_pts$x, 0),
    y = c(outer_pts$y, middle_pts$y, inner_pts$y, 0),
    group = c(
      rep("outer", n),
      rep("middle", n),
      rep("inner", n),
      "center"
    ),
    layer = c(rep(1L, n), rep(2L, n), rep(3L, n), 0L),
    local_index = c(seq_len(n), seq_len(n), seq_len(n), NA_integer_),
    stringsAsFactors = FALSE
  )

  outer_ids <- seq_len(n)
  middle_ids <- n + seq_len(n)
  inner_ids <- 2L * n + seq_len(n)
  center_id <- 3L * n + 1L

  connections_list <- list()

  add_connection <- function(from, to, block, sector = NA_integer_) {
    p_from <- pegs[from, ]
    p_to <- pegs[to, ]

    connection_index <- length(connections_list) + 1L

    connections_list[[connection_index]] <<- data.frame(
      connection_index = connection_index,
      from = from,
      to = to,
      x_from = p_from$x,
      y_from = p_from$y,
      x_to = p_to$x,
      y_to = p_to$y,
      length = sqrt((p_to$x - p_from$x)^2 + (p_to$y - p_from$y)^2),
      block = block,
      sector = sector,
      stringsAsFactors = FALSE
    )
  }

  for (local_index in seq_len(n)) {
    next_local <- (local_index %% n) + 1L

    add_connection(
      from = outer_ids[local_index],
      to = outer_ids[next_local],
      block = "outer_border",
      sector = sector_from_local_index(local_index, n)
    )
  }

  for (local_index in seq_len(n)) {
    target_local <- ((local_index + k - 1L) %% n) + 1L

    add_connection(
      from = outer_ids[local_index],
      to = middle_ids[target_local],
      block = "outer_to_middle",
      sector = sector_from_local_index(local_index, n)
    )
  }

  for (local_index in seq_len(n)) {
    target_local <- ((local_index + k - 1L) %% n) + 1L

    add_connection(
      from = middle_ids[local_index],
      to = inner_ids[target_local],
      block = "middle_to_inner",
      sector = sector_from_local_index(local_index, n)
    )
  }

  vertex_local_indices <- seq(1L, n, by = n / 6L)

  for (local_index in vertex_local_indices) {
    add_connection(
      from = outer_ids[local_index],
      to = center_id,
      block = "vertices_to_center",
      sector = sector_from_local_index(local_index, n)
    )
  }

  connections <- do.call(rbind, connections_list)
  rownames(connections) <- NULL

  total_length <- sum(connections$length)

  d <- gcd_int(n, k)

  audit <- c(
    "String Art audit",
    "Figure: hexaflower",
    sprintf("Pegs per hexagonal circuit: %d", n),
    sprintf("Total number of pegs: %d", nrow(pegs)),
    sprintf("Step: %d", k),
    sprintf("Outer radius: %.4f", r),
    sprintf("Middle scale: %.4f", scale_mid),
    sprintf("Inner scale: %.4f", scale_inner),
    sprintf("Middle offset: %.4f", offset_mid),
    sprintf("Inner offset: %.4f", offset_inner),
    sprintf("Rotation angle: %.4f radians", rotate),
    "Rule: to_local = ((from_local + k - 1) %% n) + 1",
    sprintf("Number of connections: %d", nrow(connections)),
    sprintf("Number of connection blocks: %d", length(unique(connections$block))),
    sprintf("gcd(n, k): %d", d),
    if (d == 1L) {
      "The local additive modular rule generates one local cycle in each circuit."
    } else {
      sprintf("The local additive modular rule generates %d local cycles in each circuit.", d)
    },
    sprintf("Total string length: %.4f", total_length)
  )

  if (plot) {
    old_par <- graphics::par(no.readonly = TRUE)
    on.exit(graphics::par(old_par), add = TRUE)

    graphics::par(bg = bg)

    x_range <- range(pegs$x)
    y_range <- range(pegs$y)
    x_margin <- 0.10 * diff(x_range)
    y_margin <- 0.10 * diff(y_range)

    if (x_margin == 0) x_margin <- 0.10
    if (y_margin == 0) y_margin <- 0.10

    graphics::plot(
      NA, NA,
      xlim = c(x_range[1] - x_margin, x_range[2] + x_margin),
      ylim = c(y_range[1] - y_margin, y_range[2] + y_margin),
      asp = 1,
      xlab = "",
      ylab = "",
      axes = FALSE,
      main = main
    )

    draw_hex_border <- function(radius) {
      vertices <- hex_vertices(radius)
      closed <- rbind(vertices, vertices[1, ])

      graphics::lines(
        closed$x,
        closed$y,
        col = border_col,
        lwd = border_lwd
      )
    }

    draw_hex_border(r)
    draw_hex_border(r * scale_mid)
    draw_hex_border(r * scale_inner)

    if (show_strings) {
      border_idx <- which(connections$block %in% c("outer_border", "vertices_to_center"))

      if (length(border_idx) > 0L) {
        graphics::segments(
          x0 = connections$x_from[border_idx],
          y0 = connections$y_from[border_idx],
          x1 = connections$x_to[border_idx],
          y1 = connections$y_to[border_idx],
          col = border_col,
          lwd = lwd
        )
      }

      for (sector in seq_len(6L)) {
        sector_idx <- which(
          connections$sector == sector &
            connections$block %in% c("outer_to_middle", "middle_to_inner")
        )

        if (length(sector_idx) > 0L) {
          graphics::segments(
            x0 = connections$x_from[sector_idx],
            y0 = connections$y_from[sector_idx],
            x1 = connections$x_to[sector_idx],
            y1 = connections$y_to[sector_idx],
            col = col[sector],
            lwd = lwd
          )
        }
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
      label_offset_x <- 0.012 * diff(x_range)
      label_offset_y <- 0.012 * diff(y_range)

      graphics::text(
        x = pegs$x + label_offset_x,
        y = pegs$y + label_offset_y,
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
      figure = "hexaflower",
      family = "hexagonal",
      rule = "layered_additive_modular",
      formula = "to_local = ((from_local + k - 1) %% n) + 1",
      parameters = list(
        n = n,
        k = k,
        r = r,
        scale_mid = scale_mid,
        scale_inner = scale_inner,
        offset_mid = offset_mid,
        offset_inner = offset_inner,
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

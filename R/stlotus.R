#' Generate a lotus-like string art pattern
#'
#' `stlotus()` generates a stylized lotus-like string art figure by combining
#' one outer circular module, one central circular module, and several petal
#' modules arranged around the center.
#'
#' @param n Integer. Number of pegs in each circular module. Must be at least 3.
#' @param k Integer. Additive modular step used in each module. Must satisfy
#'   `1 <= k <= n - 1`.
#' @param col String color passed to [graphics::segments()]. It may have length
#'   1 or `petals + 2`. If length is 1, the same color is used for all modules.
#' @param lwd Positive number. Line width used to draw the strings.
#' @param plot Logical. If `TRUE`, draws the figure.
#' @param show_points Logical. If `TRUE`, draws the pegs.
#' @param show_labels Logical. If `TRUE`, draws peg labels.
#' @param verbose Logical. If `TRUE`, prints a short audit to the console.
#' @param petals Integer. Number of petal modules around the center.
#' @param outer_radius Positive number. Radius of the outer circular module.
#' @param petal_radius Positive number. Radius of each petal module.
#' @param petal_center_radius Positive number. Distance from the origin to each
#'   petal center.
#' @param inner_radius Positive number. Radius of the central circular module.
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
#' @param border_col Border color used for the module outlines.
#' @param border_lwd Positive number. Border line width.
#' @param bg Plot background color.
#' @param main Optional plot title. If `NULL`, no title is displayed.
#'
#' @details
#' The figure is built from `petals + 2` circular modules:
#'
#' - one outer circular module;
#' - `petals` petal modules;
#' - one central circular module.
#'
#' Each module contains `n` equally spaced pegs and uses the same additive
#' modular rule:
#'
#' `to_local = ((from_local + k - 1) %% n) + 1`.
#'
#' The final figure is obtained by superimposing all module connections.
#'
#' @return Invisibly returns a list of class `stringart_result` with:
#' \describe{
#'   \item{pegs}{A `data.frame` with peg coordinates and metadata.}
#'   \item{connections}{A `data.frame` with columns `connection_index`,
#'   `from`, `to`, `x_from`, `y_from`, `x_to`, `y_to`, `length`, `module`,
#'   `local_from`, and `local_to`.}
#'   \item{total_length}{Total string length.}
#'   \item{audit}{A character vector with audit information.}
#'   \item{meta}{A list with construction metadata.}
#' }
#'
#' @examples
#' stlotus()
#'
#' res <- stlotus(plot = FALSE)
#' head(res$pegs)
#' head(res$connections)
#' res$total_length
#'
#' stlotus(n = 50, k = 13, col = "deeppink4", lwd = 0.8)
#' stlotus(show_points = TRUE, show_labels = TRUE)
#' stlotus(template = TRUE)
#'
#' @importFrom graphics par plot lines segments points text
#' @export
stlotus <- function(n = 40,
                    k = 11,
                    col = "deeppink4",
                    lwd = 0.8,
                    plot = TRUE,
                    show_points = FALSE,
                    show_labels = FALSE,
                    verbose = FALSE,
                    petals = 5,
                    outer_radius = 1,
                    petal_radius = 0.34,
                    petal_center_radius = 0.34,
                    inner_radius = 0.18,
                    rotate = 0,
                    show_strings = TRUE,
                    template = FALSE,
                    point_col = "black",
                    point_cex = 0.6,
                    point_pch = 19,
                    point_bg = "white",
                    label_cex = 0.6,
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

  if (!is.numeric(petals) || length(petals) != 1L || is.na(petals) ||
      petals != as.integer(petals) || petals < 3L) {
    stop("`petals` must be a single integer greater than or equal to 3.", call. = FALSE)
  }

  petals <- as.integer(petals)

  numeric_positive <- list(
    lwd = lwd,
    outer_radius = outer_radius,
    petal_radius = petal_radius,
    petal_center_radius = petal_center_radius,
    inner_radius = inner_radius,
    point_cex = point_cex,
    label_cex = label_cex,
    border_lwd = border_lwd
  )

  for (arg_name in names(numeric_positive)) {
    value <- numeric_positive[[arg_name]]
    if (!is.numeric(value) || length(value) != 1L || is.na(value) || value <= 0) {
      stop(sprintf("`%s` must be a single positive number.", arg_name), call. = FALSE)
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
    template = template
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

  make_circle_module <- function(cx, cy, radius, n_points, angle_shift = 0) {
    theta <- seq(0, 2 * pi, length.out = n_points + 1L)[-(n_points + 1L)] + angle_shift
    data.frame(
      x = cx + radius * cos(theta),
      y = cy + radius * sin(theta)
    )
  }

  module_names <- c("outer", paste0("petal_", seq_len(petals)), "inner")
  module_count <- length(module_names)

  if (length(col) == 1L) {
    col <- rep(col, module_count)
  }

  if (length(col) != module_count) {
    stop("`col` must have length 1 or `petals + 2`.", call. = FALSE)
  }

  module_specs <- vector("list", module_count)

  module_specs[[1L]] <- list(
    name = "outer",
    cx = 0,
    cy = 0,
    radius = outer_radius,
    angle_shift = rotate,
    layer = 0L,
    color = col[1L]
  )

  for (p in seq_len(petals)) {
    angle_p <- rotate + 2 * pi * (p - 1L) / petals

    module_specs[[p + 1L]] <- list(
      name = paste0("petal_", p),
      cx = petal_center_radius * cos(angle_p),
      cy = petal_center_radius * sin(angle_p),
      radius = petal_radius,
      angle_shift = angle_p,
      layer = 1L,
      color = col[p + 1L]
    )
  }

  module_specs[[module_count]] <- list(
    name = "inner",
    cx = 0,
    cy = 0,
    radius = inner_radius,
    angle_shift = rotate,
    layer = 2L,
    color = col[module_count]
  )

  pegs_list <- list()
  connections_list <- list()

  peg_id <- 0L
  connection_id <- 0L

  for (m in seq_len(module_count)) {
    spec <- module_specs[[m]]

    module_pegs <- make_circle_module(
      cx = spec$cx,
      cy = spec$cy,
      radius = spec$radius,
      n_points = n,
      angle_shift = spec$angle_shift
    )

    global_ids <- integer(n)

    for (i in seq_len(n)) {
      peg_id <- peg_id + 1L
      global_ids[i] <- peg_id

      pegs_list[[peg_id]] <- data.frame(
        index = peg_id,
        x = module_pegs$x[i],
        y = module_pegs$y[i],
        module = spec$name,
        layer = spec$layer,
        local_index = i,
        stringsAsFactors = FALSE
      )
    }

    local_from <- seq_len(n)
    local_to <- ((local_from + k - 1L) %% n) + 1L

    for (i in seq_len(n)) {
      connection_id <- connection_id + 1L

      from_id <- global_ids[local_from[i]]
      to_id <- global_ids[local_to[i]]

      x_from <- module_pegs$x[local_from[i]]
      y_from <- module_pegs$y[local_from[i]]
      x_to <- module_pegs$x[local_to[i]]
      y_to <- module_pegs$y[local_to[i]]

      connections_list[[connection_id]] <- data.frame(
        connection_index = connection_id,
        from = from_id,
        to = to_id,
        x_from = x_from,
        y_from = y_from,
        x_to = x_to,
        y_to = y_to,
        length = sqrt((x_to - x_from)^2 + (y_to - y_from)^2),
        module = spec$name,
        local_from = local_from[i],
        local_to = local_to[i],
        stringsAsFactors = FALSE
      )
    }
  }

  pegs <- do.call(rbind, pegs_list)
  connections <- do.call(rbind, connections_list)

  rownames(pegs) <- NULL
  rownames(connections) <- NULL

  total_length <- sum(connections$length)

  d <- gcd_int(n, k)

  audit <- c(
    "String Art audit",
    "Figure: lotus",
    sprintf("Pegs per module: %d", n),
    sprintf("Number of petals: %d", petals),
    sprintf("Total number of modules: %d", module_count),
    sprintf("Total number of pegs: %d", nrow(pegs)),
    sprintf("Total number of connections: %d", nrow(connections)),
    sprintf("Modular step: %d", k),
    sprintf("gcd(n, k): %d", d),
    sprintf("Outer radius: %.4f", outer_radius),
    sprintf("Petal radius: %.4f", petal_radius),
    sprintf("Petal center radius: %.4f", petal_center_radius),
    sprintf("Inner radius: %.4f", inner_radius),
    sprintf("Rotation angle: %.4f radians", rotate),
    if (d == 1L) {
      "Each module produces a single modular cycle."
    } else {
      sprintf("Each module produces %d independent modular cycles.", d)
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

    if (x_margin == 0) x_margin <- 0.1
    if (y_margin == 0) y_margin <- 0.1

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

    draw_circle_outline <- function(cx, cy, radius) {
      theta <- seq(0, 2 * pi, length.out = 300L)
      graphics::lines(
        cx + radius * cos(theta),
        cy + radius * sin(theta),
        col = border_col,
        lwd = border_lwd
      )
    }

    for (m in seq_len(module_count)) {
      spec <- module_specs[[m]]
      draw_circle_outline(spec$cx, spec$cy, spec$radius)
    }

    if (show_strings) {
      for (m in seq_len(module_count)) {
        idx <- which(connections$module == module_specs[[m]]$name)

        if (length(idx) > 0L) {
          graphics::segments(
            x0 = connections$x_from[idx],
            y0 = connections$y_from[idx],
            x1 = connections$x_to[idx],
            y1 = connections$y_to[idx],
            col = module_specs[[m]]$color,
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
      graphics::text(
        pegs$x,
        pegs$y,
        labels = pegs$local_index,
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
      figure = "lotus",
      family = "composite_circular",
      rule = "layered_additive_modular",
      formula = "to_local = ((from_local + k - 1) %% n) + 1",
      parameters = list(
        n = n,
        k = k,
        petals = petals,
        outer_radius = outer_radius,
        petal_radius = petal_radius,
        petal_center_radius = petal_center_radius,
        inner_radius = inner_radius,
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

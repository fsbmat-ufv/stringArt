#' Gera uma figura hexagonal de String Art com três circuitos de pregos
#'
#' A função `sthexaflower()` constrói uma figura de *String Art* baseada em
#' três circuitos hexagonais concêntricos de pregos e um prego central.
#' Toda a figura é criada a partir de pregos numerados e conexões auditáveis.
#'
#' O usuário define:
#' \itemize{
#'   \item `n`: número de pregos em cada circuito hexagonal;
#'   \item `k`: salto entre os pregos;
#'   \item os parâmetros geométricos dos três circuitos.
#' }
#'
#' Para que os pregos sejam distribuídos uniformemente ao longo dos 6 lados do
#' hexágono, exige-se que `n` seja múltiplo de 6.
#'
#' @param n Inteiro maior ou igual a 6 e múltiplo de 6. Número de pregos em
#'   cada circuito hexagonal.
#' @param k Inteiro maior ou igual a 1. Salto entre os pregos dos circuitos.
#' @param r Número positivo. Raio do circuito hexagonal externo.
#' @param scale_mid Número em `(0, 1)`. Escala do circuito intermediário em
#'   relação ao externo.
#' @param scale_inner Número em `(0, scale_mid)`. Escala do circuito interno em
#'   relação ao externo.
#' @param offset_mid Número em `[0, 1)`. Deslocamento relativo dos pregos do
#'   circuito intermediário ao longo do perímetro.
#' @param offset_inner Número em `[0, 1)`. Deslocamento relativo dos pregos do
#'   circuito interno ao longo do perímetro.
#' @param col Vetor de cores. Pode ter comprimento 1 ou 6.
#' @param lwd Número positivo. Espessura das linhas.
#' @param plot Lógico. Se `TRUE`, desenha a figura.
#' @param show_points Lógico. Se `TRUE`, mostra os pregos.
#' @param cex_pregos Número positivo. Tamanho dos pregos.
#' @param col_pregos Cor dos pregos.
#' @param show_labels Lógico. Se `TRUE`, mostra os índices dos pregos.
#' @param cex_labels Número positivo. Tamanho dos rótulos.
#' @param label_col Cor dos rótulos.
#' @param verbose Lógico. Se `TRUE`, imprime no console as conexões no formato
#'   `Prego i -> Prego j`.
#'
#' @details
#' Os pregos são posicionados em três circuitos hexagonais concêntricos:
#'
#' \enumerate{
#'   \item circuito externo: `1, ..., n`;
#'   \item circuito intermediário: `n + 1, ..., 2n`;
#'   \item circuito interno: `2n + 1, ..., 3n`;
#'   \item centro: `3n + 1`.
#' }
#'
#' Como `n` é múltiplo de 6, cada lado do hexágono recebe exatamente `n / 6`
#' pregos. Assim, o contorno externo também é construído pelos próprios pregos.
#'
#' A regra principal de conexão é:
#'
#' \deqn{j <- (i + k - 1) \%\% n + 1}
#'
#' onde `i` e `j` são índices locais em `1, ..., n`.
#'
#' As conexões são feitas em quatro blocos:
#' \enumerate{
#'   \item contorno do hexágono externo: prego `i` do circuito externo
#'         \eqn{\to} prego `i+1`;
#'   \item conexões do circuito externo para o intermediário com salto `k`;
#'   \item conexões do circuito intermediário para o interno com salto `k`;
#'   \item conexões dos 6 pregos-vértice do circuito externo para o centro.
#' }
#'
#' Os 6 pregos-vértice do circuito externo são os índices locais:
#'
#' `1, n/6 + 1, 2n/6 + 1, ..., 5n/6 + 1`.
#'
#' @return Retorna invisivelmente uma lista com:
#' \describe{
#'   \item{pregos}{`data.frame` com colunas `indice`, `x`, `y` e `grupo`.}
#'   \item{conexoes}{`data.frame` com colunas `prego_inicial`, `prego_final`,
#'   `x_inicial`, `y_inicial`, `x_final`, `y_final`, `comprimento` e `bloco`.}
#'   \item{comprimento_total}{Comprimento total do barbante.}
#' }
#'
#' @examples
#' # Exemplo básico
#' res <- sthexaflower(n = 24, k = 5)
#'
#' # Mostrando pregos e rótulos
#' res <- sthexaflower(
#'   n = 24,
#'   k = 7,
#'   show_points = TRUE,
#'   show_labels = TRUE
#' )
#'
#' # Imprimindo as conexões
#' res <- sthexaflower(
#'   n = 18,
#'   k = 4,
#'   verbose = TRUE,
#'   show_points = TRUE,
#'   show_labels = TRUE
#' )
#'
#' @seealso
#' `stcircle()`, `stellipse()`, `sttriangle()`, `stcardioid()`, `stradial()`
#'
#' @importFrom graphics plot segments points text
#' @export
sthexaflower <- function(
    n = 24,
    k = 5,
    r = 1,
    scale_mid = 0.72,
    scale_inner = 0.42,
    offset_mid = 0,
    offset_inner = 0,
    col = c("black", "forestgreen", "darkorange",
            "deepskyblue4", "firebrick", "purple"),
    lwd = 1,
    plot = TRUE,
    show_points = TRUE,
    cex_pregos = 0.45,
    col_pregos = "black",
    show_labels = FALSE,
    cex_labels = 0.6,
    label_col = "black",
    verbose = FALSE
) {

  if (!is.numeric(n) || length(n) != 1 || is.na(n) || n < 6 || n != as.integer(n)) {
    stop("'n' deve ser um inteiro maior ou igual a 6.")
  }

  if (n %% 6 != 0) {
    stop("'n' deve ser múltiplo de 6 para distribuir os pregos uniformemente pelos 6 lados do hexágono.")
  }

  if (!is.numeric(k) || length(k) != 1 || is.na(k) || k < 1 || k != as.integer(k)) {
    stop("'k' deve ser um inteiro maior ou igual a 1.")
  }

  if (!is.numeric(r) || length(r) != 1 || is.na(r) || r <= 0) {
    stop("'r' deve ser um número positivo.")
  }

  if (!is.numeric(scale_mid) || length(scale_mid) != 1 || is.na(scale_mid) ||
      scale_mid <= 0 || scale_mid >= 1) {
    stop("'scale_mid' deve estar em (0, 1).")
  }

  if (!is.numeric(scale_inner) || length(scale_inner) != 1 || is.na(scale_inner) ||
      scale_inner <= 0 || scale_inner >= scale_mid) {
    stop("'scale_inner' deve estar em (0, scale_mid).")
  }

  if (!is.numeric(offset_mid) || length(offset_mid) != 1 || is.na(offset_mid) ||
      offset_mid < 0 || offset_mid >= 1) {
    stop("'offset_mid' deve estar em [0, 1).")
  }

  if (!is.numeric(offset_inner) || length(offset_inner) != 1 || is.na(offset_inner) ||
      offset_inner < 0 || offset_inner >= 1) {
    stop("'offset_inner' deve estar em [0, 1).")
  }

  if (length(col) == 1) {
    col <- rep(col, 6)
  }

  if (length(col) != 6) {
    stop("'col' deve ter comprimento 1 ou 6.")
  }

  dist_pts <- function(x1, y1, x2, y2) {
    sqrt((x2 - x1)^2 + (y2 - y1)^2)
  }

  hex_vertices <- function(radius) {
    ang <- seq(pi / 2, pi / 2 + 2 * pi, length.out = 7)[1:6]
    data.frame(
      x = radius * cos(ang),
      y = radius * sin(ang)
    )
  }

  hex_perimeter_points <- function(radius, n_pts, offset = 0) {
    vertices <- hex_vertices(radius)
    vertices_next <- rbind(vertices[2:6, ], vertices[1, ])

    m <- n_pts / 6
    pts <- data.frame(x = numeric(n_pts), y = numeric(n_pts))
    idx <- 1

    for (side in 1:6) {
      x1 <- vertices$x[side]
      y1 <- vertices$y[side]
      x2 <- vertices_next$x[side]
      y2 <- vertices_next$y[side]

      alpha <- seq(0, 1, length.out = m + 1)[1:m]

      for (a in alpha) {
        pts$x[idx] <- (1 - a) * x1 + a * x2
        pts$y[idx] <- (1 - a) * y1 + a * y2
        idx <- idx + 1
      }
    }

    if (offset != 0) {
      shift <- round(offset * n_pts) %% n_pts
      if (shift > 0) {
        pts <- pts[c((shift + 1):n_pts, 1:shift), ]
        rownames(pts) <- NULL
      }
    }

    pts
  }

  sector_from_index <- function(i, n) {
    m <- as.integer(n / 6)
    s <- as.integer(((i - 1L) %/% m) + 1L)
    if (s > 6L) s <- 6L
    s
  }

  outer_pts <- hex_perimeter_points(r, n, 0)
  mid_pts   <- hex_perimeter_points(r * scale_mid, n, offset_mid)
  inner_pts <- hex_perimeter_points(r * scale_inner, n, offset_inner)

  pregos <- data.frame(
    indice = integer(0),
    x = numeric(0),
    y = numeric(0),
    grupo = character(0),
    stringsAsFactors = FALSE
  )

  add_nail <- function(x, y, grupo) {
    id <- nrow(pregos) + 1L
    pregos <<- rbind(
      pregos,
      data.frame(
        indice = id,
        x = x,
        y = y,
        grupo = grupo,
        stringsAsFactors = FALSE
      )
    )
    id
  }

  outer_ids <- integer(n)
  for (i in 1:n) {
    outer_ids[i] <- add_nail(outer_pts$x[i], outer_pts$y[i], "externo")
  }

  mid_ids <- integer(n)
  for (i in 1:n) {
    mid_ids[i] <- add_nail(mid_pts$x[i], mid_pts$y[i], "intermediario")
  }

  inner_ids <- integer(n)
  for (i in 1:n) {
    inner_ids[i] <- add_nail(inner_pts$x[i], inner_pts$y[i], "interno")
  }

  center_id <- add_nail(0, 0, "centro")

  conexoes <- data.frame(
    prego_inicial = integer(0),
    prego_final = integer(0),
    x_inicial = numeric(0),
    y_inicial = numeric(0),
    x_final = numeric(0),
    y_final = numeric(0),
    comprimento = numeric(0),
    bloco = character(0),
    stringsAsFactors = FALSE
  )

  comprimento_total <- 0

  add_connection <- function(i, j, bloco) {
    p1 <- pregos[pregos$indice == i, ]
    p2 <- pregos[pregos$indice == j, ]

    comp <- dist_pts(p1$x, p1$y, p2$x, p2$y)

    conexoes <<- rbind(
      conexoes,
      data.frame(
        prego_inicial = i,
        prego_final = j,
        x_inicial = p1$x,
        y_inicial = p1$y,
        x_final = p2$x,
        y_final = p2$y,
        comprimento = comp,
        bloco = bloco,
        stringsAsFactors = FALSE
      )
    )

    comprimento_total <<- comprimento_total + comp

    if (verbose) {
      cat(sprintf("Prego %d -> Prego %d\n", i, j))
    }
  }

  # 1) Contorno externo construído pelos próprios pregos
  for (i in 1:n) {
    j <- i %% n + 1
    add_connection(outer_ids[i], outer_ids[j], "contorno_externo")
  }

  # 2) Externo -> intermediário com salto k
  for (i in 1:n) {
    j <- (i + k - 1) %% n + 1
    add_connection(outer_ids[i], mid_ids[j], "externo_para_intermediario")
  }

  # 3) Intermediário -> interno com salto k
  for (i in 1:n) {
    j <- (i + k - 1) %% n + 1
    add_connection(mid_ids[i], inner_ids[j], "intermediario_para_interno")
  }

  # 4) Seis vértices do circuito externo -> centro
  vertex_local_idx <- seq(1, n, by = n / 6)
  for (i in vertex_local_idx) {
    add_connection(outer_ids[i], center_id, "vertices_para_centro")
  }

  if (plot) {
    xr <- range(pregos$x)
    yr <- range(pregos$y)
    mx <- 0.08 * diff(xr)
    my <- 0.08 * diff(yr)

    graphics::plot(
      NA,
      xlim = c(xr[1] - mx, xr[2] + mx),
      ylim = c(yr[1] - my, yr[2] + my),
      asp = 1,
      axes = FALSE,
      xlab = "",
      ylab = ""
    )

    idx_border <- which(conexoes$bloco %in% c("contorno_externo", "vertices_para_centro"))
    if (length(idx_border) > 0) {
      graphics::segments(
        x0 = conexoes$x_inicial[idx_border],
        y0 = conexoes$y_inicial[idx_border],
        x1 = conexoes$x_final[idx_border],
        y1 = conexoes$y_final[idx_border],
        col = "grey30",
        lwd = lwd
      )
    }

    idx_em <- which(conexoes$bloco == "externo_para_intermediario")
    idx_mi <- which(conexoes$bloco == "intermediario_para_interno")

    sec_em <- sapply(1:n, sector_from_index, n = n)
    sec_mi <- sapply(1:n, sector_from_index, n = n)

    for (s in 1:6) {
      id1 <- idx_em[sec_em == s]
      id2 <- idx_mi[sec_mi == s]

      if (length(id1) > 0) {
        graphics::segments(
          x0 = conexoes$x_inicial[id1],
          y0 = conexoes$y_inicial[id1],
          x1 = conexoes$x_final[id1],
          y1 = conexoes$y_final[id1],
          col = col[s],
          lwd = lwd
        )
      }

      if (length(id2) > 0) {
        graphics::segments(
          x0 = conexoes$x_inicial[id2],
          y0 = conexoes$y_inicial[id2],
          x1 = conexoes$x_final[id2],
          y1 = conexoes$y_final[id2],
          col = col[s],
          lwd = lwd
        )
      }
    }

    if (show_points) {
      graphics::points(
        pregos$x,
        pregos$y,
        pch = 16,
        cex = cex_pregos,
        col = col_pregos
      )
    }

    if (show_labels) {
      rx <- diff(range(pregos$x))
      ry <- diff(range(pregos$y))

      graphics::text(
        x = pregos$x + 0.010 * rx,
        y = pregos$y + 0.010 * ry,
        labels = pregos$indice,
        cex = cex_labels,
        col = label_col
      )
    }
  }

  invisible(list(
    pregos = pregos,
    conexoes = conexoes,
    comprimento_total = comprimento_total
  ))
}

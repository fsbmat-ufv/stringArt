#' Gera uma figura hexagonal de String Art com três circuitos de pregos
#'
#' A função `sthexaflower()` constrói uma figura de *String Art* baseada em
#' três circuitos hexagonais concêntricos de pregos e um prego central.
#' Toda a figura é criada a partir de pregos numerados e conexões auditáveis,
#' mantendo compatibilidade com o contrato padronizado do pacote `stringArt`.
#'
#' @param n Inteiro maior ou igual a 6 e múltiplo de 6. Número de pregos em
#'   cada circuito hexagonal.
#' @param k Inteiro entre 1 e `n - 1`. Salto entre os pregos dos circuitos.
#' @param ... Argumentos adicionais não utilizados diretamente, mantidos para
#'   compatibilidade com o contrato padronizado do pacote.
#' @param r Número positivo. Raio do circuito hexagonal externo.
#' @param scale_mid Número em `(0, 1)`. Escala do circuito intermediário em
#'   relação ao externo.
#' @param scale_inner Número em `(0, scale_mid)`. Escala do circuito interno em
#'   relação ao externo.
#' @param offset_mid Número em `[0, 1)`. Deslocamento relativo dos pregos do
#'   circuito intermediário ao longo do perímetro.
#' @param offset_inner Número em `[0, 1)`. Deslocamento relativo dos pregos do
#'   circuito interno ao longo do perímetro.
#' @param col Vetor de cores de comprimento 1 ou 6.
#' @param lwd Número positivo. Espessura das linhas.
#' @param plot Lógico. Se `TRUE`, desenha a figura.
#' @param show_points Lógico. Se `TRUE`, mostra os pregos.
#' @param cex_pregos Número positivo. Tamanho dos pregos.
#' @param col_pregos Cor dos pregos.
#' @param show_labels Lógico. Se `TRUE`, mostra os índices dos pregos.
#' @param cex_labels Número positivo. Tamanho dos rótulos.
#' @param label_col Cor dos rótulos.
#' @param verbose Lógico. Se `TRUE`, exibe mensagens informativas e imprime as
#'   conexões no formato `Prego i -> Prego j`.
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
#' pregos, garantindo distribuição uniforme e auditável ao longo do contorno.
#'
#' A regra local de salto nos circuitos é:
#' `j <- (i + k - 1) %% n + 1`.
#'
#' As conexões são feitas em quatro blocos:
#'
#' \enumerate{
#'   \item contorno do hexágono externo;
#'   \item externo para intermediário com salto `k`;
#'   \item intermediário para interno com salto `k`;
#'   \item vértices do circuito externo para o centro.
#' }
#'
#' @return Invisivelmente, uma lista com:
#' \describe{
#'   \item{pregos}{`data.frame` com colunas `indice`, `x`, `y` e `grupo`.}
#'   \item{conexoes}{`data.frame` com colunas canônicas
#'   `indice_conexao`, `prego_inicial`, `prego_final`, `x_inicial`,
#'   `y_inicial`, `x_final`, `y_final`, `comprimento`, além dos aliases
#'   `i`, `j`, `x1`, `y1`, `x2`, `y2`, e das colunas extras `bloco` e `setor`.}
#'   \item{comprimento_total}{Comprimento total do barbante.}
#'   \item{meta}{Metadados da construção.}
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
#' # Impressão textual das conexões
#' res <- sthexaflower(
#'   n = 18,
#'   k = 4,
#'   verbose = TRUE,
#'   show_points = TRUE,
#'   show_labels = TRUE
#' )
#'
#' @importFrom graphics plot segments points text
#' @export
sthexaflower <- function(
    n = 24,
    k = 5,
    ...,
    r = 1,
    scale_mid = 0.72,
    scale_inner = 0.42,
    offset_mid = 0,
    offset_inner = 0,
    col = c("black", "forestgreen", "darkorange",
            "deepskyblue4", "firebrick", "purple"),
    lwd = 1,
    plot = TRUE,
    show_points = FALSE,
    cex_pregos = 0.8,
    col_pregos = "black",
    show_labels = FALSE,
    cex_labels = 0.7,
    label_col = "black",
    verbose = TRUE
) {

  # ---------------------------------------------------------------------------
  # Validações
  # ---------------------------------------------------------------------------
  if (!is.numeric(n) || length(n) != 1L || is.na(n) ||
      n != as.integer(n) || n < 6L) {
    stop("`n` must be a single integer greater than or equal to 6.")
  }

  n <- as.integer(n)

  if (n %% 6L != 0L) {
    stop("`n` must be a multiple of 6.")
  }

  if (!is.numeric(k) || length(k) != 1L || is.na(k) ||
      k != as.integer(k) || k < 1L) {
    stop("`k` must be a single positive integer.")
  }

  k <- as.integer(k)

  if (k >= n) {
    stop("`k` must satisfy 1 <= k <= n - 1.")
  }

  if (!is.numeric(r) || length(r) != 1L || is.na(r) || r <= 0) {
    stop("`r` must be a single positive number.")
  }

  if (!is.numeric(scale_mid) || length(scale_mid) != 1L || is.na(scale_mid) ||
      scale_mid <= 0 || scale_mid >= 1) {
    stop("`scale_mid` must be in (0, 1).")
  }

  if (!is.numeric(scale_inner) || length(scale_inner) != 1L || is.na(scale_inner) ||
      scale_inner <= 0 || scale_inner >= scale_mid) {
    stop("`scale_inner` must be in (0, scale_mid).")
  }

  if (!is.numeric(offset_mid) || length(offset_mid) != 1L || is.na(offset_mid) ||
      offset_mid < 0 || offset_mid >= 1) {
    stop("`offset_mid` must be in [0, 1).")
  }

  if (!is.numeric(offset_inner) || length(offset_inner) != 1L || is.na(offset_inner) ||
      offset_inner < 0 || offset_inner >= 1) {
    stop("`offset_inner` must be in [0, 1).")
  }

  if (!is.numeric(lwd) || length(lwd) != 1L || is.na(lwd) || lwd <= 0) {
    stop("`lwd` must be a single positive number.")
  }

  if (!is.logical(plot) || length(plot) != 1L || is.na(plot)) {
    stop("`plot` must be TRUE or FALSE.")
  }

  if (!is.logical(show_points) || length(show_points) != 1L || is.na(show_points)) {
    stop("`show_points` must be TRUE or FALSE.")
  }

  if (!is.logical(show_labels) || length(show_labels) != 1L || is.na(show_labels)) {
    stop("`show_labels` must be TRUE or FALSE.")
  }

  if (!is.logical(verbose) || length(verbose) != 1L || is.na(verbose)) {
    stop("`verbose` must be TRUE or FALSE.")
  }

  if (!is.numeric(cex_pregos) || length(cex_pregos) != 1L ||
      is.na(cex_pregos) || cex_pregos <= 0) {
    stop("`cex_pregos` must be a single positive number.")
  }

  if (!is.numeric(cex_labels) || length(cex_labels) != 1L ||
      is.na(cex_labels) || cex_labels <= 0) {
    stop("`cex_labels` must be a single positive number.")
  }

  if (length(col) == 1L) {
    col <- rep(col, 6L)
  }

  if (length(col) != 6L) {
    stop("`col` must have length 1 or 6.")
  }

  # ---------------------------------------------------------------------------
  # Funções auxiliares
  # ---------------------------------------------------------------------------
  dist_pts <- function(x1, y1, x2, y2) {
    sqrt((x2 - x1)^2 + (y2 - y1)^2)
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

  hex_vertices <- function(radius) {
    ang <- seq(pi / 2, pi / 2 + 2 * pi, length.out = 7L)[1:6]
    data.frame(
      x = radius * cos(ang),
      y = radius * sin(ang)
    )
  }

  hex_perimeter_points <- function(radius, n_pts, offset = 0) {
    vertices <- hex_vertices(radius)
    vertices_next <- rbind(vertices[2:6, ], vertices[1, ])

    m <- as.integer(n_pts / 6L)
    pts <- data.frame(
      x = numeric(n_pts),
      y = numeric(n_pts)
    )

    idx <- 1L
    for (side in 1:6) {
      x1 <- vertices$x[side]
      y1 <- vertices$y[side]
      x2 <- vertices_next$x[side]
      y2 <- vertices_next$y[side]

      alpha <- seq(0, 1, length.out = m + 1L)[1:m]

      for (a in alpha) {
        pts$x[idx] <- (1 - a) * x1 + a * x2
        pts$y[idx] <- (1 - a) * y1 + a * y2
        idx <- idx + 1L
      }
    }

    if (offset != 0) {
      shift <- as.integer(round(offset * n_pts)) %% n_pts
      if (shift > 0L) {
        pts <- pts[c((shift + 1L):n_pts, 1:shift), , drop = FALSE]
        rownames(pts) <- NULL
      }
    }

    pts
  }

  sector_from_local_index <- function(i, n_local) {
    m <- as.integer(n_local / 6L)
    s <- as.integer(((i - 1L) %/% m) + 1L)
    if (s > 6L) s <- 6L
    s
  }

  # ---------------------------------------------------------------------------
  # Pregos
  # ---------------------------------------------------------------------------
  outer_pts <- hex_perimeter_points(r, n, 0)
  mid_pts   <- hex_perimeter_points(r * scale_mid, n, offset_mid)
  inner_pts <- hex_perimeter_points(r * scale_inner, n, offset_inner)

  pregos <- data.frame(
    indice = seq_len(3L * n + 1L),
    x = c(outer_pts$x, mid_pts$x, inner_pts$x, 0),
    y = c(outer_pts$y, mid_pts$y, inner_pts$y, 0),
    grupo = c(
      rep("externo", n),
      rep("intermediario", n),
      rep("interno", n),
      "centro"
    ),
    stringsAsFactors = FALSE
  )

  outer_ids <- seq_len(n)
  mid_ids   <- n + seq_len(n)
  inner_ids <- 2L * n + seq_len(n)
  center_id <- 3L * n + 1L

  # ---------------------------------------------------------------------------
  # Conexões
  # ---------------------------------------------------------------------------
  conexoes_list <- list()

  add_connection <- function(i, j, bloco, setor = NA_integer_) {
    p1 <- pregos[i, ]
    p2 <- pregos[j, ]

    comp <- dist_pts(p1$x, p1$y, p2$x, p2$y)

    idx_conn <- length(conexoes_list) + 1L
    conexoes_list[[idx_conn]] <<- data.frame(
      indice_conexao = idx_conn,
      prego_inicial = i,
      prego_final = j,
      x_inicial = p1$x,
      y_inicial = p1$y,
      x_final = p2$x,
      y_final = p2$y,
      comprimento = comp,
      i = i,
      j = j,
      x1 = p1$x,
      y1 = p1$y,
      x2 = p2$x,
      y2 = p2$y,
      bloco = bloco,
      setor = setor,
      stringsAsFactors = FALSE
    )
  }

  # 1) Contorno externo
  for (i_local in 1:n) {
    j_local <- (i_local %% n) + 1L
    add_connection(
      outer_ids[i_local],
      outer_ids[j_local],
      bloco = "contorno_externo",
      setor = sector_from_local_index(i_local, n)
    )
  }

  # 2) Externo -> intermediário com salto k
  for (i_local in 1:n) {
    j_local <- (i_local + k - 1L) %% n + 1L
    add_connection(
      outer_ids[i_local],
      mid_ids[j_local],
      bloco = "externo_para_intermediario",
      setor = sector_from_local_index(i_local, n)
    )
  }

  # 3) Intermediário -> interno com salto k
  for (i_local in 1:n) {
    j_local <- (i_local + k - 1L) %% n + 1L
    add_connection(
      mid_ids[i_local],
      inner_ids[j_local],
      bloco = "intermediario_para_interno",
      setor = sector_from_local_index(i_local, n)
    )
  }

  # 4) Vértices do circuito externo -> centro
  vertex_local_idx <- seq(1L, n, by = n / 6L)
  for (i_local in vertex_local_idx) {
    add_connection(
      outer_ids[i_local],
      center_id,
      bloco = "vertices_para_centro",
      setor = sector_from_local_index(i_local, n)
    )
  }

  conexoes <- do.call(rbind, conexoes_list)
  rownames(conexoes) <- NULL

  comprimento_total <- sum(conexoes$comprimento)

  # ---------------------------------------------------------------------------
  # Gráfico
  # ---------------------------------------------------------------------------
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
    if (length(idx_border) > 0L) {
      graphics::segments(
        x0 = conexoes$x_inicial[idx_border],
        y0 = conexoes$y_inicial[idx_border],
        x1 = conexoes$x_final[idx_border],
        y1 = conexoes$y_final[idx_border],
        col = "grey30",
        lwd = lwd
      )
    }

    for (s in 1:6) {
      idx_em <- which(
        conexoes$bloco == "externo_para_intermediario" &
          conexoes$setor == s
      )
      idx_mi <- which(
        conexoes$bloco == "intermediario_para_interno" &
          conexoes$setor == s
      )

      if (length(idx_em) > 0L) {
        graphics::segments(
          x0 = conexoes$x_inicial[idx_em],
          y0 = conexoes$y_inicial[idx_em],
          x1 = conexoes$x_final[idx_em],
          y1 = conexoes$y_final[idx_em],
          col = col[s],
          lwd = lwd
        )
      }

      if (length(idx_mi) > 0L) {
        graphics::segments(
          x0 = conexoes$x_inicial[idx_mi],
          y0 = conexoes$y_inicial[idx_mi],
          x1 = conexoes$x_final[idx_mi],
          y1 = conexoes$y_final[idx_mi],
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

  # ---------------------------------------------------------------------------
  # Mensagens
  # ---------------------------------------------------------------------------
  if (verbose) {
    message(sprintf(
      "Comprimento total do barbante: %.4f unidades.",
      comprimento_total
    ))

    d <- gcd_int(n, k)
    message(sprintf(
      "Nos blocos com salto, a regra local j = (i + k - 1) %% n + 1 gera %d ciclo(s) local(is) em cada circuito (gcd(n, k) = %d).",
      d, d
    ))

    conexoes_txt <- paste0(
      "Prego ", conexoes$prego_inicial,
      " -> Prego ", conexoes$prego_final
    )
    cat(paste(conexoes_txt, collapse = "\n"), "\n")
  }

  # ---------------------------------------------------------------------------
  # Retorno padronizado
  # ---------------------------------------------------------------------------
  res <- list(
    pregos = pregos,
    conexoes = conexoes,
    comprimento_total = comprimento_total,
    meta = list(
      figura = "hexaflower",
      regra = paste(
        "Blocos: contorno externo;",
        "j = (i + k - 1) %% n + 1 no externo->intermediario;",
        "j = (i + k - 1) %% n + 1 no intermediario->interno;",
        "vertices externos -> centro."
      ),
      parametros = list(
        n = n,
        k = k,
        r = r,
        scale_mid = scale_mid,
        scale_inner = scale_inner,
        offset_mid = offset_mid,
        offset_inner = offset_inner,
        col = col,
        lwd = lwd
      ),
      pacote = "stringArt"
    )
  )

  class(res) <- c("stringart_result", class(res))

  invisible(res)
}

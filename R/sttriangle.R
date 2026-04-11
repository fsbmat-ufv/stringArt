#' Gera uma figura de String Art triangular
#'
#' A função `sttriangle()` constrói uma figura de *String Art* sobre o contorno
#' de um triângulo equilátero, posicionando `n` pregos ao longo do perímetro e
#' conectando cada prego ao prego `k` posições à frente, segundo uma regra
#' modular fixa.
#'
#' O triângulo é construído com um lado horizontal na base, centrado de forma
#' simples no plano, e os pregos são numerados de `1` a `n` ao longo do
#' perímetro no sentido anti-horário, começando no vértice inferior esquerdo.
#'
#' @param n Inteiro maior ou igual a 3. Número de pregos.
#' @param k Inteiro entre 1 e `n - 1`. Salto modular da conexão.
#' @param side Número positivo. Comprimento do lado do triângulo equilátero.
#' @param ... Argumentos adicionais não utilizados diretamente, mantidos para
#'   compatibilidade com o contrato padronizado do pacote.
#' @param col Cor das conexões.
#' @param lwd Número positivo. Espessura das conexões.
#' @param plot Lógico. Se `TRUE`, desenha a figura.
#' @param show_points Lógico. Se `TRUE`, mostra os pregos.
#' @param cex_pregos Número positivo. Tamanho dos pregos.
#' @param col_pregos Cor dos pregos.
#' @param show_labels Lógico. Se `TRUE`, mostra os rótulos dos pregos.
#' @param cex_labels Número positivo. Tamanho dos rótulos.
#' @param label_col Cor dos rótulos.
#' @param verbose Lógico. Se `TRUE`, exibe mensagens informativas.
#' @param border_col Cor da borda do triângulo.
#' @param border_lwd Número positivo. Espessura da borda do triângulo.
#'
#' @details
#' Os pregos são distribuídos uniformemente ao longo do perímetro do triângulo
#' equilátero, o que garante uma ordenação geométrica, auditável e reproduzível.
#'
#' A regra de ligação utilizada é:
#' `j <- (i + k - 1) %% n + 1`.
#'
#' Quando `gcd(n, k) = 1`, a construção percorre todos os pregos em um único
#' ciclo. Quando `gcd(n, k) > 1`, a figura se decompõe em ciclos independentes.
#'
#' @return Invisivelmente, uma lista com:
#' \describe{
#'   \item{pregos}{`data.frame` com colunas `indice`, `x` e `y`.}
#'   \item{conexoes}{`data.frame` com colunas canônicas
#'   `indice_conexao`, `prego_inicial`, `prego_final`, `x_inicial`,
#'   `y_inicial`, `x_final`, `y_final`, `comprimento`, além dos aliases
#'   `i`, `j`, `x1`, `y1`, `x2`, `y2`.}
#'   \item{comprimento_total}{Comprimento total do barbante.}
#'   \item{vertices}{`data.frame` com os vértices do triângulo.}
#'   \item{meta}{Metadados da construção.}
#' }
#'
#' @examples
#' # Exemplo básico
#' sttriangle(n = 30, k = 7, side = 2, col = "blue", lwd = 1)
#'
#' # Exemplo com pregos e rótulos
#' sttriangle(
#'   n = 18, k = 5, side = 2,
#'   col = "red", lwd = 1,
#'   show_points = TRUE,
#'   show_labels = TRUE
#' )
#'
#' # Exemplo sem gráfico
#' res <- sttriangle(
#'   n = 20, k = 4, side = 2,
#'   plot = FALSE, verbose = FALSE
#' )
#' res$comprimento_total
#' head(res$pregos)
#' head(res$conexoes)
#'
#' @importFrom graphics plot points segments text
#' @export
sttriangle <- function(
    n,
    k,
    side,
    ...,
    col = "blue",
    lwd = 1,
    plot = TRUE,
    show_points = FALSE,
    cex_pregos = 0.8,
    col_pregos = "black",
    show_labels = FALSE,
    cex_labels = 0.7,
    label_col = "black",
    verbose = TRUE,
    border_col = "grey50",
    border_lwd = 1
) {

  # ---------------------------------------------------------------------------
  # Validações
  # ---------------------------------------------------------------------------
  if (!is.numeric(n) || length(n) != 1L || is.na(n) ||
      n != as.integer(n) || n < 3L) {
    stop("`n` must be a single integer greater than or equal to 3.")
  }

  if (!is.numeric(k) || length(k) != 1L || is.na(k) ||
      k != as.integer(k) || k < 1L) {
    stop("`k` must be a single positive integer.")
  }

  if (!is.numeric(side) || length(side) != 1L || is.na(side) || side <= 0) {
    stop("`side` must be a single positive number.")
  }

  if (!is.numeric(lwd) || length(lwd) != 1L || is.na(lwd) || lwd <= 0) {
    stop("`lwd` must be a single positive number.")
  }

  if (!is.numeric(border_lwd) || length(border_lwd) != 1L ||
      is.na(border_lwd) || border_lwd <= 0) {
    stop("`border_lwd` must be a single positive number.")
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

  n <- as.integer(n)
  k <- as.integer(k)

  if (k >= n) {
    stop("`k` must satisfy 1 <= k <= n - 1.")
  }

  # ---------------------------------------------------------------------------
  # Função auxiliar
  # ---------------------------------------------------------------------------
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

  interp_segmento <- function(P, Q, t) {
    c(
      (1 - t) * P[1] + t * Q[1],
      (1 - t) * P[2] + t * Q[2]
    )
  }

  # ---------------------------------------------------------------------------
  # Geometria do triângulo
  # ---------------------------------------------------------------------------
  h <- sqrt(3) / 2 * side

  A <- c(0, 0)
  B <- c(side, 0)
  C <- c(side / 2, h)

  vertices <- data.frame(
    vertice = c("A", "B", "C"),
    x = c(A[1], B[1], C[1]),
    y = c(A[2], B[2], C[2])
  )

  # ---------------------------------------------------------------------------
  # Distribuição uniforme dos pregos no perímetro
  # ---------------------------------------------------------------------------
  L1 <- sqrt(sum((B - A)^2))
  L2 <- sqrt(sum((C - B)^2))
  L3 <- sqrt(sum((A - C)^2))
  perimetro <- L1 + L2 + L3

  s <- seq(0, perimetro, length.out = n + 1L)[-(n + 1L)]

  pts <- matrix(0, nrow = n, ncol = 2)

  for (idx in seq_along(s)) {
    si <- s[idx]

    if (si < L1) {
      t <- si / L1
      pts[idx, ] <- interp_segmento(A, B, t)
    } else if (si < L1 + L2) {
      t <- (si - L1) / L2
      pts[idx, ] <- interp_segmento(B, C, t)
    } else {
      t <- (si - L1 - L2) / L3
      pts[idx, ] <- interp_segmento(C, A, t)
    }
  }

  pregos <- data.frame(
    indice = seq_len(n),
    x = pts[, 1],
    y = pts[, 2]
  )

  # ---------------------------------------------------------------------------
  # Conexões
  # ---------------------------------------------------------------------------
  indice_conexao <- seq_len(n)
  prego_inicial <- seq_len(n)
  prego_final <- ((prego_inicial + k - 1L) %% n) + 1L

  x_inicial <- pregos$x[prego_inicial]
  y_inicial <- pregos$y[prego_inicial]
  x_final   <- pregos$x[prego_final]
  y_final   <- pregos$y[prego_final]

  comprimento <- sqrt((x_final - x_inicial)^2 + (y_final - y_inicial)^2)

  conexoes <- data.frame(
    indice_conexao = indice_conexao,
    prego_inicial = prego_inicial,
    prego_final = prego_final,
    x_inicial = x_inicial,
    y_inicial = y_inicial,
    x_final = x_final,
    y_final = y_final,
    comprimento = comprimento,
    i = prego_inicial,
    j = prego_final,
    x1 = x_inicial,
    y1 = y_inicial,
    x2 = x_final,
    y2 = y_final
  )

  comprimento_total <- sum(conexoes$comprimento)

  # ---------------------------------------------------------------------------
  # Gráfico
  # ---------------------------------------------------------------------------
  if (plot) {
    margem_x <- 0.08 * side
    margem_y <- 0.08 * h

    graphics::plot(
      NA, NA,
      xlim = c(-margem_x, side + margem_x),
      ylim = c(-margem_y, h + margem_y),
      asp = 1,
      xlab = "",
      ylab = "",
      axes = FALSE
    )

    graphics::segments(A[1], A[2], B[1], B[2], col = border_col, lwd = border_lwd)
    graphics::segments(B[1], B[2], C[1], C[2], col = border_col, lwd = border_lwd)
    graphics::segments(C[1], C[2], A[1], A[2], col = border_col, lwd = border_lwd)

    graphics::segments(
      x0 = conexoes$x_inicial,
      y0 = conexoes$y_inicial,
      x1 = conexoes$x_final,
      y1 = conexoes$y_final,
      col = col,
      lwd = lwd
    )

    if (show_points) {
      graphics::points(
        pregos$x, pregos$y,
        pch = 19,
        cex = cex_pregos,
        col = col_pregos
      )
    }

    if (show_labels) {
      graphics::text(
        pregos$x,
        pregos$y,
        labels = pregos$indice,
        pos = 3,
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
    if (d == 1L) {
      message("A regra modular gera um único ciclo passando por todos os pregos.")
    } else {
      message(sprintf(
        "A regra modular gera %d ciclos independentes (gcd(n, k) = %d).",
        d, d
      ))
    }
  }

  # ---------------------------------------------------------------------------
  # Retorno padronizado
  # ---------------------------------------------------------------------------
  res <- list(
    pregos = pregos,
    conexoes = conexoes,
    vertices = vertices,
    comprimento_total = comprimento_total,
    meta = list(
      figura = "triangulo",
      regra = "j = (i + k - 1) %% n + 1",
      parametros = list(
        n = n,
        k = k,
        side = side,
        col = col,
        lwd = lwd
      ),
      pacote = "stringArt"
    )
  )

  class(res) <- c("stringart_result", class(res))

  invisible(res)
}

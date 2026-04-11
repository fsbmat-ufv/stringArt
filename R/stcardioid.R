#' Gera uma figura de String Art circular com efeito de cardioide
#'
#' A função `stcardioid()` constrói uma figura de *String Art* sobre uma
#' circunferência, posicionando `n` pregos igualmente espaçados e conectando
#' cada prego ao prego determinado por uma regra multiplicativa modular baseada
#' em `k`.
#'
#' A circunferência é centrada na origem, com raio `r`, e os pregos são
#' numerados de `1` a `n` no sentido anti-horário, a partir do ponto
#' `(r, 0)`, podendo ser rotacionados por `rotate`.
#'
#' @param n Inteiro maior ou igual a 3. Número de pregos.
#' @param k Inteiro entre 1 e `n - 1`. Fator multiplicativo modular da conexão.
#' @param r Número positivo. Raio da circunferência.
#' @param ... Argumentos adicionais não utilizados diretamente, mantidos para
#'   compatibilidade com o contrato padronizado do pacote.
#' @param col Cor das conexões.
#' @param lwd Número positivo. Espessura das conexões.
#' @param plot Lógico. Se `TRUE`, desenha a figura.
#' @param show_points Lógico. Se `TRUE`, mostra os pregos.
#' @param cex_pregos Número positivo. Tamanho dos pregos.
#' @param col_pregos Cor da borda dos pregos.
#' @param show_labels Lógico. Se `TRUE`, mostra os rótulos dos pregos.
#' @param cex_labels Número positivo. Tamanho dos rótulos.
#' @param label_col Cor dos rótulos.
#' @param verbose Lógico. Se `TRUE`, exibe mensagens informativas.
#' @param pch_pregos Símbolo gráfico dos pregos.
#' @param bg_pregos Cor de preenchimento dos pregos, quando aplicável.
#' @param border_col Cor da circunferência externa.
#' @param border_lwd Número positivo. Espessura da borda externa.
#' @param bg Cor de fundo do gráfico.
#' @param rotate Número real. Ângulo de rotação, em radianos.
#'
#' @details
#' Os pregos são distribuídos uniformemente sobre uma circunferência de raio `r`.
#' A ordem dos pregos é explícita, geométrica, auditável e reproduzível.
#'
#' A regra de ligação utilizada é:
#' `j <- ((k * (i - 1)) %% n) + 1`.
#'
#' Essa regra gera padrões clássicos de *String Art* baseados em tabelas
#' multiplicativas modulares. Quando `k = 2`, obtém-se a figura clássica
#' associada ao efeito de cardioide.
#'
#' @return Invisivelmente, uma lista com:
#' \describe{
#'   \item{pregos}{`data.frame` com colunas `indice`, `x` e `y`.}
#'   \item{conexoes}{`data.frame` com colunas canônicas
#'   `indice_conexao`, `prego_inicial`, `prego_final`, `x_inicial`,
#'   `y_inicial`, `x_final`, `y_final`, `comprimento`, além dos aliases
#'   `i`, `j`, `x1`, `y1`, `x2`, `y2`.}
#'   \item{comprimento_total}{Comprimento total do barbante.}
#'   \item{meta}{Metadados da construção.}
#' }
#'
#' @examples
#' # Exemplo básico
#' stcardioid(n = 120, k = 2, r = 1, col = "wheat", lwd = 0.8)
#'
#' # Exemplo com auditoria visual
#' stcardioid(
#'   n = 12, k = 2, r = 1,
#'   col = "steelblue", lwd = 1,
#'   show_points = TRUE,
#'   show_labels = TRUE,
#'   verbose = TRUE
#' )
#'
#' # Exemplo sem gráfico
#' res <- stcardioid(
#'   n = 80, k = 2, r = 1,
#'   plot = FALSE, verbose = FALSE
#' )
#' res$comprimento_total
#' head(res$pregos)
#' head(res$conexoes)
#'
#' @importFrom graphics plot points segments symbols par text
#' @export
stcardioid <- function(
    n,
    k,
    r = 1,
    ...,
    col = "antiquewhite",
    lwd = 0.8,
    plot = TRUE,
    show_points = FALSE,
    cex_pregos = 0.8,
    col_pregos = "darkorange2",
    show_labels = FALSE,
    cex_labels = 0.7,
    label_col = "black",
    verbose = TRUE,
    pch_pregos = 21,
    bg_pregos = "white",
    border_col = "goldenrod3",
    border_lwd = 1.2,
    bg = "white",
    rotate = 0
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

  if (!is.numeric(r) || length(r) != 1L || is.na(r) || r <= 0) {
    stop("`r` must be a single positive number.")
  }

  if (!is.numeric(lwd) || length(lwd) != 1L || is.na(lwd) || lwd <= 0) {
    stop("`lwd` must be a single positive number.")
  }

  if (!is.numeric(border_lwd) || length(border_lwd) != 1L ||
      is.na(border_lwd) || border_lwd <= 0) {
    stop("`border_lwd` must be a single positive number.")
  }

  if (!is.numeric(rotate) || length(rotate) != 1L || is.na(rotate)) {
    stop("`rotate` must be a single numeric value.")
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

  # ---------------------------------------------------------------------------
  # Pregos: circunferência
  # ---------------------------------------------------------------------------
  theta <- seq(0, 2 * pi, length.out = n + 1L)[-(n + 1L)] + rotate

  pregos <- data.frame(
    indice = seq_len(n),
    x = r * cos(theta),
    y = r * sin(theta)
  )

  # ---------------------------------------------------------------------------
  # Conexões: regra multiplicativa modular
  # ---------------------------------------------------------------------------
  indice_conexao <- seq_len(n)
  prego_inicial <- seq_len(n)
  prego_final <- ((k * (prego_inicial - 1L)) %% n) + 1L

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
    old_par <- graphics::par(no.readonly = TRUE)
    on.exit(graphics::par(old_par), add = TRUE)

    graphics::par(bg = bg)

    margem <- 0.12 * r
    lims <- c(-r - margem, r + margem)

    graphics::plot(
      NA, NA,
      xlim = lims,
      ylim = lims,
      asp = 1,
      axes = FALSE,
      xlab = "",
      ylab = ""
    )

    graphics::symbols(
      0, 0,
      circles = r,
      inches = FALSE,
      add = TRUE,
      fg = border_col,
      bg = NA,
      lwd = border_lwd
    )

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
        pch = pch_pregos,
        col = col_pregos,
        bg = bg_pregos,
        cex = cex_pregos
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
      message("A regra multiplicativa modular gera um único ciclo.")
    } else {
      message(sprintf(
        "A regra multiplicativa modular gera %d ciclos independentes (gcd(n, k) = %d).",
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
    comprimento_total = comprimento_total,
    meta = list(
      figura = "cardioide",
      regra = "j = ((k * (i - 1)) %% n) + 1",
      parametros = list(
        n = n,
        k = k,
        r = r,
        col = col,
        lwd = lwd,
        rotate = rotate
      ),
      pacote = "stringArt"
    )
  )

  class(res) <- c("stringart_result", class(res))

  invisible(res)
}

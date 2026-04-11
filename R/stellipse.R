#' Gera uma figura de String Art em formato elíptico
#'
#' A função `stellipse()` constrói uma figura de *String Art* sobre uma elipse,
#' posicionando `n` pregos ao longo do contorno elíptico e conectando cada prego
#' ao prego `k` posições à frente, segundo uma regra modular fixa.
#'
#' A elipse é centrada na origem e descrita parametricamente por
#' `x = a*cos(theta)` e `y = b*sin(theta)`, com os pregos numerados de `1` a `n`
#' no sentido anti-horário, a partir do ponto `(a, 0)`.
#'
#' @param n Inteiro maior ou igual a 3. Número de pregos.
#' @param k Inteiro entre 1 e `n - 1`. Salto modular da conexão.
#' @param a Número positivo. Semi-eixo horizontal da elipse.
#' @param b Número positivo. Semi-eixo vertical da elipse.
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
#'
#' @details
#' Os pregos são distribuídos segundo uma parametrização angular da elipse,
#' com ângulos igualmente espaçados em `[0, 2*pi)`. A ordem dos pregos é
#' explícita, geométrica, auditável e reproduzível.
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
#'   \item{meta}{Metadados da construção.}
#' }
#'
#' @examples
#' # Exemplo básico
#' stellipse(n = 30, k = 3, a = 4, b = 2, col = "purple", lwd = 1.2)
#'
#' # Exemplo com pregos e rótulos
#' stellipse(
#'   n = 20, k = 4, a = 3, b = 1.8,
#'   col = "steelblue", lwd = 1,
#'   show_points = TRUE,
#'   show_labels = TRUE
#' )
#'
#' # Exemplo sem gráfico
#' res <- stellipse(
#'   n = 24, k = 5, a = 3, b = 1.5,
#'   plot = FALSE, verbose = FALSE
#' )
#' res$comprimento_total
#' head(res$pregos)
#' head(res$conexoes)
#'
#' @importFrom graphics plot lines points segments text
#' @export
stellipse <- function(
    n,
    k,
    a,
    b,
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
    verbose = TRUE
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

  if (!is.numeric(a) || length(a) != 1L || is.na(a) || a <= 0) {
    stop("`a` must be a single positive number.")
  }

  if (!is.numeric(b) || length(b) != 1L || is.na(b) || b <= 0) {
    stop("`b` must be a single positive number.")
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
  # Pregos: parametrização da elipse
  # ---------------------------------------------------------------------------
  theta <- seq(0, 2 * pi, length.out = n + 1L)[-(n + 1L)]

  pregos <- data.frame(
    indice = seq_len(n),
    x = a * cos(theta),
    y = b * sin(theta)
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
    margem_x <- 0.15 * a
    margem_y <- 0.15 * b

    graphics::plot(
      NA, NA,
      xlim = c(-a - margem_x, a + margem_x),
      ylim = c(-b - margem_y, b + margem_y),
      asp = 1,
      xlab = "",
      ylab = "",
      axes = FALSE
    )

    tt <- seq(0, 2 * pi, length.out = 500L)
    graphics::lines(a * cos(tt), b * sin(tt), col = "grey80", lty = 3)

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
      fator_x <- 1.08
      fator_y <- 1.08
      graphics::text(
        x = fator_x * pregos$x,
        y = fator_y * pregos$y,
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
    comprimento_total = comprimento_total,
    meta = list(
      figura = "elipse",
      regra = "j = (i + k - 1) %% n + 1",
      parametros = list(
        n = n,
        k = k,
        a = a,
        b = b,
        col = col,
        lwd = lwd
      ),
      pacote = "stringArt"
    )
  )

  class(res) <- c("stringart_result", class(res))

  invisible(res)
}

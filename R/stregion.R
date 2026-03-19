#' Gera uma figura de String Art a partir de uma regiao em uma malha
#'
#' A funcao `stregion()` cria uma figura do tipo *String Art* a partir de uma
#' malha regular de pregos no plano. Uma funcao logica define quais pregos
#' pertencem a regiao de interesse, e segmentos sao tracados entre pregos dessa
#' regiao, produzindo um preenchimento visual.
#'
#' @param x_seq Vetor numerico com as coordenadas x da malha.
#' @param y_seq Vetor numerico com as coordenadas y da malha.
#' @param inside_func Funcao que recebe vetores `x` e `y` e retorna um vetor
#' logico indicando quais pontos pertencem a regiao.
#' @param n_segments Inteiro. Numero de segmentos a serem desenhados.
#' @param col Cor do barbante.
#' @param lwd Espessura das linhas.
#' @param pch_pregos Simbolo dos pregos.
#' @param cex_pregos Tamanho dos pregos.
#' @param col_pregos Cor dos pregos.
#' @param plot Logico. Se `TRUE`, desenha a figura.
#'
#' @return Invisivelmente, uma lista com os pregos da regiao e as conexoes.
#'
#' @importFrom graphics plot points segments
#' @export
stregion <- function(x_seq, y_seq, inside_func,
                     n_segments = 500,
                     col = "red",
                     lwd = 1,
                     pch_pregos = 19,
                     cex_pregos = 0.4,
                     col_pregos = "gray20",
                     plot = TRUE) {

  grid <- expand.grid(x = x_seq, y = y_seq)
  inside <- inside_func(grid$x, grid$y)
  pregos <- grid[inside, , drop = FALSE]

  if (nrow(pregos) < 2) {
    stop("A regiao possui menos de 2 pregos.")
  }

  conexoes <- data.frame(
    x1 = numeric(n_segments),
    y1 = numeric(n_segments),
    x2 = numeric(n_segments),
    y2 = numeric(n_segments),
    comprimento = numeric(n_segments)
  )

  if (plot) {
    plot(grid$x, grid$y, type = "n", asp = 1, axes = FALSE, xlab = "", ylab = "")
    points(grid$x, grid$y, pch = ".", col = "gray85")
    points(pregos$x, pregos$y, pch = pch_pregos, cex = cex_pregos, col = col_pregos)
  }

  total_length <- 0

  for (i in seq_len(n_segments)) {
    idx <- sample(seq_len(nrow(pregos)), size = 2, replace = FALSE)
    p1 <- pregos[idx[1], ]
    p2 <- pregos[idx[2], ]

    if (plot) {
      segments(p1$x, p1$y, p2$x, p2$y, col = col, lwd = lwd)
    }

    len <- sqrt((p2$x - p1$x)^2 + (p2$y - p1$y)^2)
    total_length <- total_length + len

    conexoes[i, ] <- c(p1$x, p1$y, p2$x, p2$y, len)
  }

  invisible(list(
    pregos = pregos,
    conexoes = conexoes,
    comprimento_total = total_length
  ))
}

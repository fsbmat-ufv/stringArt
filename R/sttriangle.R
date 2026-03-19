#' Gera uma figura de String Art triangular
#'
#' A funcao `sttriangle()` cria uma figura do tipo *String Art* em formato
#' triangular, conectando pontos (pregos) igualmente espacados ao longo do
#' contorno de um triangulo equilatero. Cada prego e conectado ao prego `k`
#' posicoes a frente, formando padroes geometricos visualmente interessantes.
#' Alem disso, a funcao calcula o comprimento total do barbante necessario e
#' retorna as coordenadas e conexoes utilizadas.
#'
#' @param n Inteiro. Numero de pregos distribuidos ao longo do contorno do
#' triangulo. Deve ser pelo menos 3.
#' @param k Inteiro. Passo de ligacao entre os pregos. Por exemplo, se `k = 2`,
#' cada prego e ligado ao segundo prego adiante.
#' @param side Numerico. Comprimento do lado do triangulo equilatero.
#' @param col Cor das linhas do barbante. Pode ser nome de cor (ex: `"blue"`)
#' ou codigo hexadecimal.
#' @param lwd Espessura das linhas do barbante.
#' @param border_col Cor da borda do triangulo.
#' @param border_lwd Espessura da borda do triangulo.
#' @param show_points Logico. Se `TRUE` (padrao), os pregos sao desenhados.
#' @param cex_pregos Numerico. Tamanho dos pregos no grafico.
#' @param col_pregos Cor dos pregos.
#' @param show_labels Logico. Se `TRUE`, escreve o indice de cada prego.
#' @param cex_labels Numerico. Tamanho dos rotulos.
#' @param label_col Cor dos rotulos.
#' @param verbose Logico. Se `TRUE`, imprime no console as conexoes no formato
#' `Prego i -> Prego j`.
#' @param plot Logico. Se `TRUE` (padrao), o grafico e desenhado.
#'
#' @details
#' A funcao constroi um triangulo equilatero de lado `side` e distribui `n`
#' pregos igualmente ao longo de seu contorno. Em seguida, cada prego `i` e
#' conectado ao prego `j`, onde
#'
#' \deqn{j = ((i + k - 2) \bmod n) + 1}
#'
#' produzindo uma figura do tipo *String Art*.
#'
#' @return Invisivelmente, uma lista contendo:
#' \describe{
#'   \item{pregos}{Data frame com indice e coordenadas `(x, y)` dos pregos.}
#'   \item{conexoes}{Data frame com indices inicial e final, coordenadas e comprimento de cada segmento.}
#'   \item{vertices}{Data frame com os vertices do triangulo.}
#'   \item{comprimento_total}{Valor numerico com o comprimento total do barbante.}
#' }
#'
#' @examples
#' # Exemplo basico
#' sttriangle(n = 30, k = 7, side = 2, col = "blue", lwd = 1)
#'
#' # Exemplo com auditoria visual
#' sttriangle(n = 12, k = 3, side = 2,
#'            col = "red", lwd = 1,
#'            show_labels = TRUE, verbose = TRUE)
#'
#' # Exemplo sem exibir o grafico
#' res <- sttriangle(n = 20, k = 4, side = 2, col = "purple", lwd = 1, plot = FALSE)
#' res$comprimento_total
#'
#' @seealso
#' Outras funcoes da serie StringArt, como `stcircle()` e `stellipse()`.
#'
#' @importFrom graphics plot points segments text
#' @export
sttriangle <- function(n, k, side, col, lwd,
                       border_col = "black",
                       border_lwd = 1.5,
                       show_points = TRUE,
                       cex_pregos = 1.1,
                       col_pregos = "black",
                       show_labels = FALSE,
                       cex_labels = 0.7,
                       label_col = "black",
                       verbose = TRUE,
                       plot = TRUE) {

  if (n < 3) stop("E necessario pelo menos 3 pregos.")
  if (k < 1) stop("O salto 'k' deve ser pelo menos 1.")
  if (k >= n) stop("O salto 'k' deve ser menor que 'n'.")
  if (side <= 0) stop("O argumento 'side' deve ser positivo.")

  # Altura do triangulo equilatero
  h <- sqrt(3) / 2 * side

  # Vertices
  A <- c(0, 0)
  B <- c(side, 0)
  C <- c(side / 2, h)

  # Comprimentos dos lados
  L1 <- sqrt(sum((B - A)^2))
  L2 <- sqrt(sum((C - B)^2))
  L3 <- sqrt(sum((A - C)^2))
  per <- L1 + L2 + L3

  # Funcao auxiliar para interpolar ponto no segmento
  interp_segmento <- function(P, Q, t) {
    c((1 - t) * P[1] + t * Q[1],
      (1 - t) * P[2] + t * Q[2])
  }

  # Posicoes ao longo do perimetro
  s <- seq(0, per, length.out = n + 1)[-(n + 1)]

  # Coordenadas dos pregos
  pts <- matrix(0, nrow = n, ncol = 2)

  for (i in seq_along(s)) {
    si <- s[i]

    if (si < L1) {
      t <- si / L1
      pts[i, ] <- interp_segmento(A, B, t)
    } else if (si < L1 + L2) {
      t <- (si - L1) / L2
      pts[i, ] <- interp_segmento(B, C, t)
    } else {
      t <- (si - L1 - L2) / L3
      pts[i, ] <- interp_segmento(C, A, t)
    }
  }

  x <- pts[, 1]
  y <- pts[, 2]

  # Estrutura para armazenar comprimento de barbante
  total_length <- 0
  conexoes <- data.frame(
    i = integer(n),
    j = integer(n),
    x1 = numeric(n),
    y1 = numeric(n),
    x2 = numeric(n),
    y2 = numeric(n),
    comprimento = numeric(n)
  )

  # Plotagem
  if (plot) {
    plot(NA, NA,
         xlim = c(-0.08 * side, 1.08 * side),
         ylim = c(-0.08 * h, 1.08 * h),
         asp = 1,
         xlab = "",
         ylab = "",
         axes = FALSE,
         main = paste("String Art triangular com", n, "pregos"))

    segments(A[1], A[2], B[1], B[2], col = border_col, lwd = border_lwd)
    segments(B[1], B[2], C[1], C[2], col = border_col, lwd = border_lwd)
    segments(C[1], C[2], A[1], A[2], col = border_col, lwd = border_lwd)
  }

  # Conectar os pregos e calcular comprimento do barbante
  for (i in 1:n) {
    j <- (i + k - 1) %% n + 1

    if (plot) {
      segments(x[i], y[i], x[j], y[j], col = col, lwd = lwd)
    }

    len <- sqrt((x[j] - x[i])^2 + (y[j] - y[i])^2)
    total_length <- total_length + len

    conexoes[i, ] <- c(i, j, x[i], y[i], x[j], y[j], len)
  }

  # Desenhar os pregos por cima
  if (plot && show_points) {
    points(x, y, pch = 19, cex = cex_pregos, col = col_pregos)
  }

  # Rotulos
  if (plot && show_labels) {
    text(x, y, labels = 1:n, pos = 3, cex = cex_labels, col = label_col)
  }

  message(sprintf("Comprimento total de barbante: %.2f unidades", total_length))

  # Vetor de conexoes descritivas
  if (verbose) {
    conexoes_texto <- paste0("Prego ", 1:n, " -> Prego ", ((1:n + k - 1) %% n) + 1)
    cat(paste(conexoes_texto, collapse = "\n"), "\n")
  }

  invisible(list(
    pregos = data.frame(indice = 1:n, x = x, y = y),
    conexoes = conexoes,
    vertices = data.frame(
      vertice = c("A", "B", "C"),
      x = c(A[1], B[1], C[1]),
      y = c(A[2], B[2], C[2])
    ),
    comprimento_total = total_length
  ))
}

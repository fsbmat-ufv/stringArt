#' Gera uma figura de String Art em formato eliptico
#'
#' A funcao `stellipse()` cria uma figura do tipo *String Art* baseada em uma
#' elipse definida pelos eixos `a` (horizontal) e `b` (vertical). Os pregos sao
#' igualmente espacado ao longo da elipse, e cada um e conectado ao prego `k`
#' posicoes a frente, formando padroes geometricos interessantes. A funcao tambem
#' calcula o comprimento total do barbante utilizado.
#'
#' @param n Inteiro. Numero de pregos (pontos igualmente espacado sobre a elipse).
#' Deve ser pelo menos 3.
#' @param k Inteiro. Passo de ligacao entre os pregos. Por exemplo, se `k = 2`,
#' cada prego e ligado ao segundo prego adiante.
#' @param a Numerico. Semi-eixo maior (horizontal) da elipse.
#' @param b Numerico. Semi-eixo menor (vertical) da elipse.
#' @param col Cor das linhas do barbante. Pode ser nome de cor (ex: `"blue"`)
#' ou codigo hexadecimal (ex: `"#1E90FF"`).
#' @param lwd Espessura das linhas do barbante.
#' @param plot Logico. Se `TRUE` (padrao), a elipse e as conexoes sao desenhadas.
#'
#' @details
#' A funcao constroi uma elipse com eixos `a` e `b`, distribuindo `n` pregos
#' igualmente ao longo da sua borda. Os pregos sao conectados de acordo com o passo `k`.
#' O resultado e uma estrutura visual similar a um padrao de *String Art*, podendo ser
#' usada para fins artisticos ou didaticos.
#'
#' @return Invisivelmente, uma lista contendo:
#' \describe{
#'   \item{pregos}{Data frame com coordenadas (x, y) dos pregos.}
#'   \item{conexoes}{Data frame com coordenadas iniciais e finais e comprimento de cada segmento.}
#'   \item{comprimento_total}{Valor numerico com o comprimento total do barbante.}
#' }
#'
#' @examples
#' # Exemplo basico com 30 pregos em uma elipse
#' stellipse(n = 30, k = 3, a = 1, b = 0.5, col = "purple", lwd = 1.2)
#'
#' # Exemplo sem exibir o grafico
#' res <- stellipse(n = 20, k = 4, a = 1, b = 0.7, plot = FALSE)
#' res$comprimento_total
#'
#' @seealso
#' Outras funcoes da serie StringArt, como `stcircle()` e `stline()`.
#'
#' @importFrom graphics plot points segments
#' @export
stellipse <- function(n, k, a, b,
                      col, lwd, plot = TRUE) {
  if (n < 3) stop("E necessario pelo menos 3 pregos.")

  # Angulos para distribuicao dos pregos
  theta <- seq(0, 2 * pi, length.out = n + 1)[- (n + 1)]

  # Coordenadas dos pregos ao longo da elipse
  x <- a * cos(theta)
  y <- b * sin(theta)

  # Vetores para armazenar comprimento de barbante
  total_length <- 0
  conexoes <- data.frame(
    x1 = numeric(n),
    y1 = numeric(n),
    x2 = numeric(n),
    y2 = numeric(n),
    comprimento = numeric(n)
  )

  # Plotagem
  if (plot) {
    plot(x, y, type = "n", asp = 1, xlab = "", ylab = "", axes = FALSE,
         main = sprintf("String Art com %d pregos em uma elipse", n))
    points(x, y, pch = 19, col = "black")
  }

  # Conectar os pregos com passo k e calcular comprimento
  for (i in 1:n) {
    j <- (i + k - 1) %% n + 1
    if (plot) {
      segments(x[i], y[i], x[j], y[j], col = col, lwd = lwd)
    }
    len <- sqrt((x[j] - x[i])^2 + (y[j] - y[i])^2)
    total_length <- total_length + len
    conexoes[i, ] <- c(x[i], y[i], x[j], y[j], len)
  }

  # Mensagem com o comprimento total de barbante
  message(sprintf("Comprimento total de barbante: %.2f unidades", total_length))

  # Retornar informacoes
  invisible(list(
    pregos = data.frame(x = x, y = y),
    conexoes = conexoes,
    comprimento_total = total_length
  ))
}

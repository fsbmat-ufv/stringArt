#' Gera uma figura de String Art circular
#'
#' A funcao `stcircle()` cria uma figura do tipo *String Art* em formato circular,
#' conectando pontos (pregos) igualmente espacado sobre um circulo de raio definido.
#' Cada prego e conectado ao prego `k` posicoes a frente, formando padroes geometricos
#' visualmente interessantes. Alem disso, a funcao calcula o comprimento total
#' do barbante necessario e retorna as coordenadas e conexoes utilizadas.
#'
#' @param n Inteiro. Numero de pregos (pontos igualmente espacado sobre o circulo).
#' Deve ser pelo menos 3.
#' @param k Inteiro. Passo de ligacao entre os pregos. Por exemplo, se `k = 2`,
#' cada prego e ligado ao segundo prego adiante.
#' @param r Numerico. Raio do circulo.
#' @param col Cor das linhas do barbante. Pode ser nome de cor (ex: `"blue"`)
#' ou codigo hexadecimal (ex: `"#1E90FF"`).
#' @param lwd Espessura das linhas do barbante.
#' @param plot Logico. Se `TRUE` (padrao), o circulo e as conexoes sao desenhadas.
#'
#' @details
#' A funcao constroi um circulo de raio `r` com `n` pregos igualmente distribuidos,
#' e realiza conexoes seguindo o padrao determinado por `k`. Alem de gerar o grafico
#' (opcionalmente), ela tambem retorna uma lista contendo:
#' \itemize{
#'   \item `pregos`: coordenadas dos pregos (x, y);
#'   \item `conexoes`: data frame com as conexoes e comprimentos individuais;
#'   \item `comprimento_total`: comprimento total do barbante utilizado.
#' }
#'
#' @return Invisivelmente, uma lista contendo:
#' \describe{
#'   \item{pregos}{Data frame com coordenadas (x, y) dos pregos.}
#'   \item{conexoes}{Data frame com coordenadas iniciais e finais e comprimento de cada segmento.}
#'   \item{comprimento_total}{Valor numerico com o comprimento total do barbante.}
#' }
#'
#' @examples
#' # Exemplo basico com 20 pregos, conectando cada prego ao terceiro a frente
#' stcircle(n = 20, k = 3, r = 1, col = "blue", lwd = 1.2)
#'
#' # Exemplo sem exibir o grafico
#' res <- stcircle(n = 10, k = 2, r = 1, col = "red", lwd = 1, plot = FALSE)
#' res$comprimento_total
#'
#' @seealso
#' Outras funcoes da serie StringArt, como `stline()` e `stellipse()` (em desenvolvimento).
#'
#' @importFrom graphics plot points segments lines text
#' @export
stcircle <- function(n, k, r, col, lwd, plot = TRUE) {
  if (n < 3) stop("E necessario pelo menos 3 pregos.")

  # Angulos para os pregos
  theta <- seq(0, 2 * pi, length.out = n + 1)[- (n + 1)]

  # Coordenadas dos pregos
  x <- r * cos(theta)
  y <- r * sin(theta)

  # Vetores para armazenar comprimento de barbante
  total_length <- 0
  conexoes <- data.frame(
    x1 = numeric(n),
    y1 = numeric(n),
    x2 = numeric(n),
    y2 = numeric(n),
    comprimento = numeric(n)
  )

  # Plotar o circulo e os pregos
  if (plot) {
    plot(x, y, type = "n", asp = 1, xlab = "", ylab = "", axes = FALSE,
         main = paste("String Art com", n, "pregos"))
    points(x, y, pch = 19, col = "black")
  }

  # Conectar os pregos e calcular comprimento do barbante
  for (i in 1:n) {
    j <- (i + k - 1) %% n + 1

    if (plot) {
      segments(x[i], y[i], x[j], y[j], col = col, lwd = lwd)
    }

    len <- sqrt((x[j] - x[i])^2 + (y[j] - y[i])^2)
    total_length <- total_length + len

    conexoes[i, ] <- c(x[i], y[i], x[j], y[j], len)
  }

  message(sprintf("Comprimento total de barbante: %.2f unidades", total_length))

  # Vetor de conexoes descritivas
  conexoes_texto <- paste0("Prego ", 1:n, " -> Prego ", ((1:n + k - 1) %% n) + 1)
  cat(paste(conexoes_texto, collapse = "\n"), "\n")

  # Retorna invisivelmente os resultados
  invisible(list(
    pregos = data.frame(x = x, y = y),
    conexoes = conexoes,
    comprimento_total = total_length
  ))
}

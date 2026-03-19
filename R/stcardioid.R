#' Gera uma figura de String Art circular com efeito de cardioide
#'
#' A funcao `stcardioid()` cria uma figura do tipo *String Art* baseada em uma
#' circunferencia. Os pregos sao igualmente espacados ao longo da borda do
#' circulo, e cada prego e conectado a outro conforme uma regra multiplicativa
#' controlada pelo parametro `mult`. Esse tipo de construcao gera padroes
#' geometricos classicos de *String Art*, incluindo figuras semelhantes a
#' cardioides e outras curvas envolventes.
#'
#' @param n Inteiro. Numero de pregos distribuidos ao longo da circunferencia.
#' Deve ser pelo menos 3.
#' @param mult Numerico. Fator multiplicativo usado para definir as conexoes.
#' Por exemplo, se `mult = 2`, o prego de indice `i` e conectado ao prego
#' de indice `2*i` (modulo `n`, com indexacao iniciando em 1).
#' @param r Numerico. Raio da circunferencia.
#' @param col Cor das linhas do barbante. Pode ser nome de cor
#' (ex: `"wheat"`) ou codigo hexadecimal.
#' @param lwd Numerico. Espessura das linhas do barbante.
#' @param col_pregos Cor dos pregos.
#' @param cex_pregos Numerico. Tamanho dos pregos no grafico.
#' @param pch_pregos Simbolo grafico dos pregos.
#' @param bg_pregos Cor de preenchimento dos pregos quando aplicavel.
#' @param border_col Cor da circunferencia externa.
#' @param border_lwd Numerico. Espessura da borda externa.
#' @param bg Cor de fundo do grafico.
#' @param rotate Numerico. Angulo de rotacao, em radianos, aplicado a toda a figura.
#' Permite mudar a orientacao visual do padrao.
#' @param show_labels Logico. Se `TRUE`, escreve o indice de cada prego.
#' @param cex_labels Numerico. Tamanho dos rotulos dos pregos.
#' @param label_col Cor dos rotulos dos pregos.
#' @param verbose Logico. Se `TRUE`, imprime no console as conexoes no formato
#' `Prego i -> Prego j`.
#' @param plot Logico. Se `TRUE` (padrao), o grafico e desenhado.
#'
#' @details
#' A funcao distribui `n` pregos igualmente sobre uma circunferencia de raio `r`.
#' Em seguida, para cada prego `i`, desenha-se um segmento ligando esse ponto ao
#' prego de indice
#'
#' \deqn{j = ((\mathrm{round}(mult * (i - 1))) \bmod n) + 1}
#'
#' Essa regra produz padroes visuais ricos e bastante conhecidos em *String Art*.
#' Quando `mult = 2`, por exemplo, surge uma figura classica semelhante a uma
#' cardioide. Outros valores podem gerar variacoes igualmente interessantes.
#'
#' Os pregos podem ser desenhados com destaque e numerados para facilitar a
#' verificacao da regra de conexao.
#'
#' @return Invisivelmente, uma lista contendo:
#' \describe{
#'   \item{pregos}{Data frame com coordenadas `(x, y)` e indice dos pregos.}
#'   \item{conexoes}{Data frame com indices inicial e final, coordenadas e comprimento de cada segmento.}
#'   \item{comprimento_total}{Valor numerico com o comprimento total do barbante.}
#' }
#'
#' @examples
#' # Exemplo basico
#' stcardioid(n = 120, mult = 2, r = 1,
#'            col = "wheat", lwd = 0.8)
#'
#' # Exemplo com auditoria visual
#' stcardioid(n = 12, mult = 2, r = 1,
#'            col = "steelblue", lwd = 1,
#'            show_labels = TRUE, verbose = TRUE)
#'
#' # Exemplo sem exibir o grafico
#' res <- stcardioid(n = 80, mult = 2, r = 1, plot = FALSE)
#' res$comprimento_total
#'
#' @seealso
#' Outras funcoes da serie StringArt, como `stcircle()`, `stellipse()` e `sttriangle()`.
#'
#' @importFrom graphics plot points segments symbols par text
#' @export
stcardioid <- function(n,
                       mult = 2,
                       r = 1,
                       col = "antiquewhite",
                       lwd = 0.8,
                       col_pregos = "darkorange2",
                       cex_pregos = 0.8,
                       pch_pregos = 21,
                       bg_pregos = "white",
                       border_col = "goldenrod3",
                       border_lwd = 1.2,
                       bg = "white",
                       rotate = 0,
                       show_labels = FALSE,
                       cex_labels = 0.7,
                       label_col = "black",
                       verbose = FALSE,
                       plot = TRUE) {

  if (n < 3) stop("E necessario pelo menos 3 pregos.")
  if (r <= 0) stop("O raio 'r' deve ser positivo.")

  # Angulos dos pregos
  theta <- seq(0, 2 * pi, length.out = n + 1)[-(n + 1)] + rotate

  # Coordenadas dos pregos
  x <- r * cos(theta)
  y <- r * sin(theta)

  pregos <- data.frame(
    indice = 1:n,
    x = x,
    y = y
  )

  # Estrutura para armazenar as conexoes
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
    old_par <- par(no.readonly = TRUE)
    on.exit(par(old_par))

    par(bg = bg)

    plot(x, y,
         type = "n",
         asp = 1,
         axes = FALSE,
         xlab = "",
         ylab = "",
         main = sprintf("String Art circular com %d pregos", n))

    # Borda externa do circulo
    symbols(0, 0, circles = r, inches = FALSE, add = TRUE,
            fg = border_col, bg = NA, lwd = border_lwd)
  }

  # Conexoes usando regra multiplicativa
  for (i in 1:n) {
    j <- (round(mult * (i - 1)) %% n) + 1

    if (plot) {
      segments(x[i], y[i], x[j], y[j], col = col, lwd = lwd)
    }

    len <- sqrt((x[j] - x[i])^2 + (y[j] - y[i])^2)
    total_length <- total_length + len

    conexoes[i, ] <- c(i, j, x[i], y[i], x[j], y[j], len)
  }

  # Desenhar os pregos por cima para maior destaque
  if (plot) {
    points(x, y,
           pch = pch_pregos,
           col = col_pregos,
           bg = bg_pregos,
           cex = cex_pregos)
  }

  # Rotulos dos pregos
  if (plot && show_labels) {
    text(x, y,
         labels = 1:n,
         pos = 3,
         cex = cex_labels,
         col = label_col)
  }

  # Mensagem com comprimento total
  message(sprintf("Comprimento total de barbante: %.2f unidades", total_length))

  # Impressao textual das conexoes
  if (verbose) {
    conexoes_texto <- paste0(
      "Prego ", 1:n,
      " -> Prego ",
      (round(mult * (0:(n - 1))) %% n) + 1
    )
    cat(paste(conexoes_texto, collapse = "\n"), "\n")
  }

  # Retorno
  invisible(list(
    pregos = pregos,
    conexoes = conexoes,
    comprimento_total = total_length
  ))
}

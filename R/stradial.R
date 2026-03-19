#' Gera uma figura radial de String Art com modulos triangulares
#'
#' A funcao `stradial()` cria uma figura do tipo *String Art* composta por
#' modulos triangulares rotacionados em torno de um ponto central. Em cada
#' modulo, `n` pregos sao distribuidos uniformemente ao longo do contorno
#' triangular, e cada prego e ligado ao prego `k` posicoes a frente, formando
#' padroes geometricos caracteristicos.
#'
#' @param n Inteiro. Numero de pregos em cada modulo triangular.
#' @param k Inteiro. Salto entre os pregos.
#' @param m Inteiro. Numero de modulos rotacionados em torno do centro.
#' @param r Numerico. Distancia do centro aos vertices externos do modulo.
#' @param spread Numerico. Abertura angular, em radianos, de cada modulo.
#' @param col Vetor de cores dos modulos.
#' @param lwd Numerico. Espessura das linhas do barbante.
#' @param border_col Cor do contorno dos modulos.
#' @param border_lwd Numerico. Espessura do contorno dos modulos.
#' @param show_points Logico. Se `TRUE`, desenha os pregos.
#' @param point_col Cor da borda dos pregos.
#' @param point_bg Cor de preenchimento dos pregos.
#' @param cex_points Numerico. Tamanho dos pregos.
#' @param pch_points Simbolo dos pregos.
#' @param show_labels Logico. Se `TRUE`, escreve o indice de cada prego.
#' @param cex_labels Numerico. Tamanho dos rotulos.
#' @param label_col Cor dos rotulos.
#' @param verbose Logico. Se `TRUE`, imprime as conexoes no console.
#' @param show_center Logico. Se `TRUE`, destaca o centro.
#' @param center_col Cor do ponto central.
#' @param cex_center Numerico. Tamanho do ponto central.
#' @param plot Logico. Se `TRUE`, desenha a figura.
#'
#' @return Invisivelmente, uma lista contendo:
#' \describe{
#'   \item{pregos}{Data frame com as coordenadas dos pregos.}
#'   \item{conexoes}{Data frame com as conexoes e seus comprimentos.}
#'   \item{comprimento_total}{Comprimento total do barbante.}
#' }
#'
#' @importFrom graphics plot points segments text
#' @export
stradial <- function(n,
                     k,
                     m = 1,
                     r = 1.2,
                     spread = pi / 5,
                     col = rainbow(m),
                     lwd = 1,
                     border_col = "black",
                     border_lwd = 1.1,
                     show_points = TRUE,
                     point_col = "black",
                     point_bg = "white",
                     cex_points = 0.8,
                     pch_points = 21,
                     show_labels = FALSE,
                     cex_labels = 0.7,
                     label_col = "black",
                     verbose = FALSE,
                     show_center = TRUE,
                     center_col = "black",
                     cex_center = 0.9,
                     plot = TRUE) {

  if (n < 3) stop("E necessario pelo menos 3 pregos por modulo.")
  if (k < 1) stop("O salto 'k' deve ser pelo menos 1.")
  if (k >= n) stop("O salto 'k' deve ser menor que 'n'.")
  if (m < 1) stop("E necessario pelo menos 1 modulo.")
  if (length(col) == 1) col <- rep(col, m)
  if (length(col) != m) stop("O argumento 'col' deve ter comprimento 1 ou m.")

  rotaciona <- function(x, y, ang) {
    xr <- x * cos(ang) - y * sin(ang)
    yr <- x * sin(ang) + y * cos(ang)
    cbind(x = xr, y = yr)
  }

  interp_segmento <- function(P, Q, t) {
    c((1 - t) * P[1] + t * Q[1],
      (1 - t) * P[2] + t * Q[2])
  }

  pregos_no_triangulo <- function(A, B, C, n) {
    L1 <- sqrt(sum((B - A)^2))
    L2 <- sqrt(sum((C - B)^2))
    L3 <- sqrt(sum((A - C)^2))
    per <- L1 + L2 + L3

    s <- seq(0, per, length.out = n + 1)[-(n + 1)]
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

    colnames(pts) <- c("x", "y")
    as.data.frame(pts)
  }

  O  <- c(0, 0)
  P1 <- c(r * cos(-spread / 2), r * sin(-spread / 2))
  P2 <- c(r * cos( spread / 2), r * sin( spread / 2))

  total_length <- 0
  conexoes <- data.frame(
    grupo = integer(),
    i = integer(),
    j = integer(),
    x1 = numeric(),
    y1 = numeric(),
    x2 = numeric(),
    y2 = numeric(),
    comprimento = numeric(),
    cor = character(),
    stringsAsFactors = FALSE
  )

  pregos_all <- data.frame(
    grupo = integer(),
    indice = integer(),
    x = numeric(),
    y = numeric(),
    stringsAsFactors = FALSE
  )

  if (plot) {
    lim <- 1.20 * r
    plot(NA, NA,
         xlim = c(-lim, lim),
         ylim = c(-lim, lim),
         asp = 1,
         axes = FALSE,
         xlab = "",
         ylab = "",
         main = sprintf("String Art radial com %d modulo(s)", m))
  }

  for (g in 1:m) {
    ang <- 2 * pi * (g - 1) / m

    Oi  <- as.numeric(rotaciona(O[1],  O[2],  ang))
    P1i <- as.numeric(rotaciona(P1[1], P1[2], ang))
    P2i <- as.numeric(rotaciona(P2[1], P2[2], ang))

    pregos <- pregos_no_triangulo(Oi, P1i, P2i, n)
    pregos$grupo <- g
    pregos$indice <- 1:n

    pregos_all <- rbind(
      pregos_all,
      pregos[, c("grupo", "indice", "x", "y")]
    )

    if (plot) {
      segments(Oi[1], Oi[2], P1i[1], P1i[2], col = border_col, lwd = border_lwd)
      segments(P1i[1], P1i[2], P2i[1], P2i[2], col = border_col, lwd = border_lwd)
      segments(P2i[1], P2i[2], Oi[1], Oi[2], col = border_col, lwd = border_lwd)
    }

    for (i in 1:n) {
      j <- (i + k - 1) %% n + 1

      if (plot) {
        segments(pregos$x[i], pregos$y[i], pregos$x[j], pregos$y[j],
                 col = col[g], lwd = lwd)
      }

      len <- sqrt((pregos$x[j] - pregos$x[i])^2 + (pregos$y[j] - pregos$y[i])^2)
      total_length <- total_length + len

      conexoes <- rbind(conexoes, data.frame(
        grupo = g,
        i = i,
        j = j,
        x1 = pregos$x[i],
        y1 = pregos$y[i],
        x2 = pregos$x[j],
        y2 = pregos$y[j],
        comprimento = len,
        cor = col[g],
        stringsAsFactors = FALSE
      ))
    }

    if (plot && show_points) {
      points(pregos$x, pregos$y,
             pch = pch_points,
             col = point_col,
             bg = point_bg,
             cex = cex_points)
    }

    if (plot && show_labels) {
      text(pregos$x, pregos$y,
           labels = pregos$indice,
           pos = 3,
           cex = cex_labels,
           col = label_col)
    }

    if (verbose) {
      conexoes_texto <- paste0(
        "Modulo ", g, ": Prego ",
        1:n,
        " -> Prego ",
        ((1:n + k - 1) %% n) + 1
      )
      cat(paste(conexoes_texto, collapse = "\n"), "\n")
    }
  }

  if (plot && show_center) {
    points(0, 0, pch = 19, col = center_col, cex = cex_center)
  }

  message(sprintf("Comprimento total de barbante: %.2f unidades", total_length))

  invisible(list(
    pregos = pregos_all,
    conexoes = conexoes,
    comprimento_total = total_length
  ))
}

# =============================================================================
#  stregion.R  –  String Art a partir de um contorno poligonal
# =============================================================================
#
#  COMO O PREENCHIMENTO FUNCIONA
#  ------------------------------
#  A abordagem correta para preencher uma regiao em String Art e:
#
#  1. Distribuir n pregos ao longo do CONTORNO da regiao (como na moldura
#     fisica com pregos nas bordas).
#
#  2. Conectar cada prego i ao prego  j = (i + floor(n/2)) % n
#     Isso garante que o prego destino esteja NO LADO OPOSTO do contorno,
#     de modo que a corda sempre ATRAVESSA o interior.
#
#  3. Para aumentar a densidade, repetir a varredura com diferentes
#     offsets: j = (i + floor(n/2) + offset) % n, para offset em 0..k-1.
#     Cada offset gera um conjunto de cordas ligeiramente inclinadas,
#     e juntas cobrem toda a area como um tecido de linhas cruzadas.
#
#  DIFERENCA CRITICA em relacao as outras funcoes da serie:
#  - stcircle/sttriangle/etc: conecta prego i ao prego i+k (vizinhos
#    proximos => cria padroes geometricos na BORDA)
#  - stregion: conecta prego i ao prego OPOSTO (cruza o interior =>
#    PREENCHE a regiao)
#
# =============================================================================


# -----------------------------------------------------------------------------
# 1. contorno_circulo
# -----------------------------------------------------------------------------

#' Gera um contorno circular (ou arco)
#'
#' @param cx,cy Centro do circulo.
#' @param r Raio.
#' @param ang_ini,ang_fim Angulos inicial e final em radianos.
#' @param n_pts Numero de pontos do poligono aproximador.
#' @return Data frame com colunas `x` e `y`.
#' @export
contorno_circulo <- function(cx = 0, cy = 0, r = 1,
                             ang_ini = 0, ang_fim = 2 * pi,
                             n_pts = 200) {
  theta <- seq(ang_ini, ang_fim, length.out = n_pts)
  data.frame(x = cx + r * cos(theta),
             y = cy + r * sin(theta))
}


# -----------------------------------------------------------------------------
# 2. contorno_elipse
# -----------------------------------------------------------------------------

#' Gera um contorno eliptico (ou arco de elipse)
#'
#' @param cx,cy Centro.
#' @param a Semi-eixo horizontal.
#' @param b Semi-eixo vertical.
#' @param ang_ini,ang_fim Angulos parametricos inicial e final.
#' @param n_pts Numero de pontos.
#' @return Data frame com colunas `x` e `y`.
#' @export
contorno_elipse <- function(cx = 0, cy = 0, a = 1, b = 0.5,
                            ang_ini = 0, ang_fim = 2 * pi,
                            n_pts = 200) {
  theta <- seq(ang_ini, ang_fim, length.out = n_pts)
  data.frame(x = cx + a * cos(theta),
             y = cy + b * sin(theta))
}


# -----------------------------------------------------------------------------
# 3. contorno_poligono
# -----------------------------------------------------------------------------

#' Gera um contorno a partir de vertices arbitrarios
#'
#' @param vx,vy Coordenadas dos vertices.
#' @param n_pts Numero de pontos ao longo do perimetro.
#' @return Data frame com colunas `x` e `y`.
#' @export
contorno_poligono <- function(vx, vy, n_pts = 200) {
  if (length(vx) != length(vy) || length(vx) < 2)
    stop("'vx' e 'vy' devem ter o mesmo comprimento e pelo menos 2 elementos.")
  vx <- c(vx, vx[1]); vy <- c(vy, vy[1])
  dx <- diff(vx); dy <- diff(vy)
  lens <- sqrt(dx^2 + dy^2)
  per  <- sum(lens)
  s_total <- seq(0, per, length.out = n_pts + 1)[-(n_pts + 1)]
  cum_len <- c(0, cumsum(lens))
  pts <- data.frame(x = numeric(n_pts), y = numeric(n_pts))
  for (i in seq_len(n_pts)) {
    s  <- s_total[i]
    sg <- min(findInterval(s, cum_len, rightmost.closed = TRUE), length(lens))
    t  <- (s - cum_len[sg]) / lens[sg]
    pts$x[i] <- (1 - t) * vx[sg] + t * vx[sg + 1]
    pts$y[i] <- (1 - t) * vy[sg] + t * vy[sg + 1]
  }
  pts
}


# -----------------------------------------------------------------------------
# Auxiliar interno: distribui n pregos ao longo de um contorno fechado
# -----------------------------------------------------------------------------
.pregos_no_contorno <- function(contorno, n) {
  cx <- contorno$x; cy <- contorno$y
  dx  <- diff(c(cx, cx[1])); dy  <- diff(c(cy, cy[1]))
  seg <- sqrt(dx^2 + dy^2);  per <- sum(seg)
  s_pts   <- seq(0, per, length.out = n + 1)[-(n + 1)]
  cum_len <- c(0, cumsum(seg))
  cx_c <- c(cx, cx[1]); cy_c <- c(cy, cy[1])
  pts <- data.frame(x = numeric(n), y = numeric(n))
  for (i in seq_len(n)) {
    s  <- s_pts[i]
    sg <- min(findInterval(s, cum_len, rightmost.closed = TRUE), length(seg))
    t  <- (s - cum_len[sg]) / seg[sg]
    pts$x[i] <- (1 - t) * cx_c[sg] + t * cx_c[sg + 1]
    pts$y[i] <- (1 - t) * cy_c[sg] + t * cy_c[sg + 1]
  }
  pts
}


# -----------------------------------------------------------------------------
# 4. stregion
# -----------------------------------------------------------------------------

#' Gera uma figura de String Art preenchida a partir de um contorno
#'
#' Distribui `n` pregos ao longo do perimetro do contorno e conecta cada
#' prego ao seu **oposto** no contorno (prego `i + floor(n/2)`), de forma
#' que cada corda **atravessa o interior** da regiao. Para aumentar a
#' densidade de preenchimento, sao feitas `k` passagens com offsets de
#' 0 a `k-1`, produzindo um feixe de cordas inclinadas que cobrem toda
#' a area como um tecido de linhas entrecruzadas.
#'
#' @param contorno Data frame com colunas `x` e `y` definindo o contorno.
#' @param n Inteiro par >= 4. Numero de pregos no contorno.
#' @param k Inteiro >= 1. Numero de passagens de varredura. Cada passagem
#'   usa um offset diferente, aumentando a densidade de preenchimento.
#'   Use `k = 1` para preenchimento simples (cordas paralelas), `k = 3`
#'   a `k = 6` para preenchimento denso.
#' @param col Cor das linhas.
#' @param lwd Espessura das linhas.
#' @param draw_border Logico. Desenha o contorno.
#' @param border_col Cor do contorno.
#' @param border_lwd Espessura do contorno.
#' @param show_points Logico. Desenha os pregos.
#' @param pch_pregos,cex_pregos,col_pregos Simbolo, tamanho e cor dos pregos.
#' @param show_labels Logico. Escreve indices dos pregos.
#' @param cex_labels,label_col Tamanho e cor dos rotulos.
#' @param verbose Logico. Imprime conexoes no console.
#' @param add Logico. Adiciona ao grafico existente.
#' @param plot Logico. Desenha a figura.
#' @param xlim,ylim Limites dos eixos (so quando `add = FALSE`).
#'
#' @return Invisivelmente, lista com `pregos`, `conexoes` e
#'   `comprimento_total`.
#'
#' @examples
#' # Circulo preenchido
#' ct <- contorno_circulo(r = 1)
#' stregion(ct, n = 100, k = 4, col = "steelblue", lwd = 0.6)
#'
#' # Elipse preenchida
#' ct <- contorno_elipse(a = 1.5, b = 0.8)
#' stregion(ct, n = 120, k = 5, col = "firebrick", lwd = 0.6)
#'
#' @seealso [stmushroom()]
#' @importFrom graphics plot polygon points segments text
#' @export
stregion <- function(contorno,
                     n           = 100,
                     k           = 4,
                     col         = "red",
                     lwd         = 0.6,
                     draw_border = TRUE,
                     border_col  = col,
                     border_lwd  = 1.2,
                     show_points = FALSE,
                     pch_pregos  = 19,
                     cex_pregos  = 0.4,
                     col_pregos  = "gray20",
                     show_labels = FALSE,
                     cex_labels  = 0.6,
                     label_col   = "black",
                     verbose     = FALSE,
                     add         = FALSE,
                     plot        = TRUE,
                     xlim        = NULL,
                     ylim        = NULL) {

  if (!is.data.frame(contorno) || !all(c("x", "y") %in% names(contorno)))
    stop("'contorno' deve ser um data frame com colunas 'x' e 'y'.")
  if (n < 4) stop("'n' deve ser pelo menos 4.")
  if (n %% 2 != 0) { n <- n + 1; message(sprintf("'n' ajustado para %d.", n)) }
  if (k < 1) stop("'k' deve ser pelo menos 1.")

  pregos <- .pregos_no_contorno(contorno, n)
  meio   <- floor(n / 2)

  # Total de conexoes: n * k
  n_conn <- n * k
  conexoes <- data.frame(
    passagem    = integer(n_conn),
    i           = integer(n_conn),
    j           = integer(n_conn),
    x1          = numeric(n_conn),
    y1          = numeric(n_conn),
    x2          = numeric(n_conn),
    y2          = numeric(n_conn),
    comprimento = numeric(n_conn)
  )
  total_length <- 0
  idx <- 1L

  if (plot) {
    if (!add) {
      xl <- if (is.null(xlim)) range(contorno$x) + c(-0.05, 0.05) * diff(range(contorno$x)) else xlim
      yl <- if (is.null(ylim)) range(contorno$y) + c(-0.05, 0.05) * diff(range(contorno$y)) else ylim
      plot(NA, NA, xlim = xl, ylim = yl, asp = 1,
           axes = FALSE, xlab = "", ylab = "")
    }
    if (draw_border)
      polygon(contorno$x, contorno$y,
              border = border_col, lwd = border_lwd, col = NA)
  }

  # k passagens de varredura com offsets de 0 a floor(meio/2).
  # Offsets maiores que meio/2 geram cordas cada vez mais curtas que
  # eventualmente ficam na borda — mantemos so a metade do arco.
  offsets <- round(seq(0, floor(meio / 2), length.out = k))

  for (pass in seq_len(k)) {
    off <- offsets[pass]
    for (i in seq_len(n)) {
      j <- (i - 1 + meio + off) %% n + 1

      if (plot)
        segments(pregos$x[i], pregos$y[i],
                 pregos$x[j], pregos$y[j],
                 col = col, lwd = lwd)

      len <- sqrt((pregos$x[j] - pregos$x[i])^2 +
                    (pregos$y[j] - pregos$y[i])^2)
      total_length      <- total_length + len
      conexoes[idx, ]   <- c(pass, i, j,
                             pregos$x[i], pregos$y[i],
                             pregos$x[j], pregos$y[j],
                             len)
      idx <- idx + 1L
    }
  }

  if (plot && show_points)
    points(pregos$x, pregos$y,
           pch = pch_pregos, cex = cex_pregos, col = col_pregos)

  if (plot && show_labels)
    text(pregos$x, pregos$y, labels = seq_len(n),
         pos = 3, cex = cex_labels, col = label_col)

  if (verbose) {
    for (pass in seq_len(k)) {
      off <- offsets[pass]
      js  <- ((seq_len(n) - 1 + meio + off) %% n) + 1
      txt <- paste0("Passagem ", pass,
                    " – Prego ", seq_len(n), " -> Prego ", js)
      cat(paste(txt, collapse = "\n"), "\n")
    }
  }

  message(sprintf("Comprimento total de barbante: %.2f unidades", total_length))

  invisible(list(
    pregos            = data.frame(indice = seq_len(n),
                                   x = pregos$x, y = pregos$y),
    conexoes          = conexoes,
    comprimento_total = total_length
  ))
}


# -----------------------------------------------------------------------------
# 5. stmushroom
# -----------------------------------------------------------------------------

#' Gera o cogumelo do Mario em String Art
#'
#' Monta o cogumelo usando multiplas chamadas a `stregion()`. As regioes
#' sao desenhadas em camadas (de tras para frente): chapeu vermelho,
#' cabeca bege, bolinhas brancas, olhos pretos.
#'
#' @param n_chapeu  Pregos no chapeu (deve ser par; default 160).
#' @param k_chapeu  Passagens de varredura no chapeu (default 5).
#' @param n_bolinha Pregos em cada bolinha (default 80).
#' @param k_bolinha Passagens de varredura nas bolinhas (default 4).
#' @param n_cabeca  Pregos na cabeca (default 120).
#' @param k_cabeca  Passagens de varredura na cabeca (default 5).
#' @param n_olho    Pregos em cada olho (default 50).
#' @param k_olho    Passagens de varredura nos olhos (default 3).
#' @param lwd       Espessura das linhas (default 0.4).
#' @param bg        Cor de fundo (default `"#1a1a2e"`).
#' @param show_points Logico. Mostra os pregos.
#' @param cex_pregos  Tamanho dos pregos.
#'
#' @return Invisivelmente, lista nomeada com os resultados de cada regiao.
#'
#' @examples
#' stmushroom()
#' stmushroom(n_chapeu = 200, n_bolinha = 100, lwd = 0.3)
#' stmushroom(show_points = TRUE)
#'
#' @seealso [stregion()]
#' @importFrom graphics par plot
#' @export
stmushroom <- function(n_chapeu  = 160, k_chapeu  = 5,
                       n_bolinha = 80,  k_bolinha = 4,
                       n_cabeca  = 120, k_cabeca  = 5,
                       n_olho    = 50,  k_olho    = 3,
                       lwd         = 0.4,
                       bg          = "#1a1a2e",
                       show_points = FALSE,
                       cex_pregos  = 0.3) {

  old_par <- par(no.readonly = TRUE)
  on.exit(par(old_par))
  par(bg = bg, mar = c(0, 0, 0, 0))

  # ------------------------------------------------------------------
  # Geometria
  # ------------------------------------------------------------------
  # O centro da elipse completa da cabeca fica em cy_head.
  # O chapeu desce ate cy_hat = cy_head + sobreposicao, cobrindo
  # a parte superior da cabeca (como na foto fisica).
  #
  # Layout vertical (de cima para baixo):
  #   topo do chapeu    :  cy_head + b_head + r_hat  ~  1.04
  #   base do chapeu    :  cy_hat  = cy_head + overlap
  #   centro da cabeca  :  cy_head = 0.0
  #   base da cabeca    :  cy_head - b_head          ~ -0.44

  b_head  <- 0.44    # semi-eixo vertical da cabeca
  a_head  <- 0.78    # semi-eixo horizontal da cabeca
  cy_head <- 0.0     # centro da elipse da cabeca no meio do canvas

  overlap <- 0.18    # quanto o chapeu desce sobre a cabeca
  cy_hat  <- cy_head + overlap   # base do chapeu (linha de corte)
  r_hat   <- 1.0                 # raio do semicirculo do chapeu

  # Bolinhas brancas (dentro do chapeu, acima de cy_hat)
  cx_bc <- 0.00;  cy_bc <- cy_hat + 0.38; r_bc <- 0.28  # central
  cx_bl <- -0.58; cy_bl <- cy_hat + 0.14; r_bl <- 0.17  # esquerda
  cx_br <-  0.58; cy_br <- cy_hat + 0.14; r_br <- 0.17  # direita

  # Olhos (dentro da cabeca, abaixo de cy_hat)
  cx_ol <- -0.24; cy_ol <- cy_head - 0.08; a_ol <- 0.12; b_ol <- 0.18
  cx_or <-  0.24; cy_or <- cy_head - 0.08; a_or <- 0.12; b_or <- 0.18

  # ------------------------------------------------------------------
  # Plot  –  ylim ajustado a nova geometria
  # ------------------------------------------------------------------
  plot(NA, NA,
       xlim = c(-1.15, 1.15),
       ylim = c(cy_head - b_head - 0.08,
                cy_hat  + r_hat  + 0.08),
       asp = 1, axes = FALSE, xlab = "", ylab = "")

  resultados <- list()

  # ------------------------------------------------------------------
  # 1. Chapeu vermelho
  #    Semicirculo superior com centro em (0, cy_hat) + base reta
  # ------------------------------------------------------------------
  ct_hat <- rbind(
    contorno_circulo(0, cy_hat, r_hat,
                     ang_ini = 0, ang_fim = pi, n_pts = 300),
    data.frame(x = seq(-r_hat, r_hat, length.out = 60),
               y = rep(cy_hat, 60))
  )

  resultados$chapeu <- stregion(
    ct_hat, n = n_chapeu, k = k_chapeu,
    col = "#cc2200", lwd = lwd,
    draw_border = TRUE, border_col = "#881100", border_lwd = 1.2,
    show_points = show_points, cex_pregos = cex_pregos, add = TRUE
  )

  # ------------------------------------------------------------------
  # 2. Cabeca bege  –  SEMI-ELIPSE inferior
  #    Arco de pi a 2*pi (metade de baixo) + base reta em cy_head
  #    fechando pelo topo. Assim a cabeca aparece como uma "barriga"
  #    abaixo do chapeu, com o topo reto encostado na base do chapeu.
  # ------------------------------------------------------------------
  arco_head <- contorno_elipse(0, cy_head, a_head, b_head,
                               ang_ini = pi, ang_fim = 2 * pi,
                               n_pts = 300)
  # Base reta da esquerda para a direita fechando a semi-elipse pelo topo
  base_head <- data.frame(
    x = seq(-a_head, a_head, length.out = 60),
    y = rep(cy_head, 60)
  )
  ct_head <- rbind(arco_head, base_head)

  resultados$cabeca <- stregion(
    ct_head, n = n_cabeca, k = k_cabeca,
    col = "#e8a882", lwd = lwd,
    draw_border = TRUE, border_col = "#c07850", border_lwd = 1.2,
    show_points = show_points, cex_pregos = cex_pregos, add = TRUE
  )

  # ------------------------------------------------------------------
  # 3. Bolinha central branca
  # ------------------------------------------------------------------
  resultados$bolinha_central <- stregion(
    contorno_circulo(cx_bc, cy_bc, r_bc, n_pts = 200),
    n = n_bolinha, k = k_bolinha,
    col = "white", lwd = lwd,
    draw_border = TRUE, border_col = "gray80", border_lwd = 0.8,
    show_points = show_points, cex_pregos = cex_pregos, add = TRUE
  )

  # ------------------------------------------------------------------
  # 4. Bolinha lateral esquerda
  # ------------------------------------------------------------------
  resultados$bolinha_esq <- stregion(
    contorno_circulo(cx_bl, cy_bl, r_bl, n_pts = 160),
    n = n_bolinha, k = k_bolinha,
    col = "white", lwd = lwd,
    draw_border = TRUE, border_col = "gray80", border_lwd = 0.8,
    show_points = show_points, cex_pregos = cex_pregos, add = TRUE
  )

  # ------------------------------------------------------------------
  # 5. Bolinha lateral direita
  # ------------------------------------------------------------------
  resultados$bolinha_dir <- stregion(
    contorno_circulo(cx_br, cy_br, r_br, n_pts = 160),
    n = n_bolinha, k = k_bolinha,
    col = "white", lwd = lwd,
    draw_border = TRUE, border_col = "gray80", border_lwd = 0.8,
    show_points = show_points, cex_pregos = cex_pregos, add = TRUE
  )

  # ------------------------------------------------------------------
  # 6. Olho esquerdo
  # ------------------------------------------------------------------
  resultados$olho_esq <- stregion(
    contorno_elipse(cx_ol, cy_ol, a_ol, b_ol, n_pts = 120),
    n = n_olho, k = k_olho,
    col = "#111111", lwd = lwd,
    draw_border = TRUE, border_col = "#111111", border_lwd = 1.2,
    show_points = show_points, cex_pregos = cex_pregos, add = TRUE
  )

  # ------------------------------------------------------------------
  # 7. Olho direito
  # ------------------------------------------------------------------
  resultados$olho_dir <- stregion(
    contorno_elipse(cx_or, cy_or, a_or, b_or, n_pts = 120),
    n = n_olho, k = k_olho,
    col = "#111111", lwd = lwd,
    draw_border = TRUE, border_col = "#111111", border_lwd = 1.2,
    show_points = show_points, cex_pregos = cex_pregos, add = TRUE
  )

  total <- sum(sapply(resultados, function(r) r$comprimento_total))
  message(sprintf(
    "\nComprimento total (todas as regioes): %.2f unidades", total))

  invisible(resultados)
}

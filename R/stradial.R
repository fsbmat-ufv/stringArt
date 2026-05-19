#' Gera uma figura radial de String Art com módulos triangulares
#'
#' A função `stradial()` constrói uma figura de *String Art* composta por
#' módulos triangulares rotacionados em torno de um ponto central. Em cada
#' módulo, `n` pregos são distribuídos uniformemente ao longo do contorno
#' triangular, e cada prego é ligado ao prego `k` posições à frente,
#' formando padrões geométricos característicos.
#'
#' Cada módulo é tratado de forma auditável, com índices locais e globais
#' dos pregos, permitindo compatibilidade com o contrato padronizado do
#' pacote `stringArt`.
#'
#' @param n Inteiro maior ou igual a 3. Número de pregos em cada módulo triangular.
#' @param k Inteiro entre 1 e `n - 1`. Salto entre os pregos dentro de cada módulo.
#' @param ... Argumentos adicionais não utilizados diretamente, mantidos para
#'   compatibilidade com o contrato padronizado do pacote.
#' @param m Inteiro maior ou igual a 1. Número de módulos rotacionados em torno do centro.
#' @param r Número positivo. Distância do centro aos vértices externos do módulo.
#' @param spread Número positivo em radianos. Abertura angular de cada módulo.
#' @param col Vetor de cores dos módulos. Deve ter comprimento 1 ou `m`.
#' @param lwd Número positivo. Espessura das linhas do barbante.
#' @param plot Lógico. Se `TRUE`, desenha a figura.
#' @param show_points Lógico. Se `TRUE`, desenha os pregos.
#' @param cex_pregos Número positivo. Tamanho dos pregos.
#' @param col_pregos Cor da borda dos pregos.
#' @param show_labels Lógico. Se `TRUE`, escreve o índice local de cada prego.
#' @param cex_labels Número positivo. Tamanho dos rótulos.
#' @param label_col Cor dos rótulos.
#' @param verbose Lógico. Se `TRUE`, imprime informações da construção.
#' @param border_col Cor do contorno dos módulos.
#' @param border_lwd Número positivo. Espessura do contorno dos módulos.
#' @param point_bg Cor de preenchimento dos pregos.
#' @param pch_points Símbolo gráfico dos pregos.
#' @param show_center Lógico. Se `TRUE`, destaca o centro.
#' @param center_col Cor do ponto central.
#' @param cex_center Número positivo. Tamanho do ponto central.
#'
#' @details
#' Cada módulo triangular é formado por três vértices:
#'
#' \enumerate{
#'   \item o centro `(0, 0)`;
#'   \item um vértice externo à esquerda;
#'   \item um vértice externo à direita.
#' }
#'
#' O módulo base é rotacionado de forma uniforme em torno da origem para gerar
#' os `m` módulos. Em cada módulo, os pregos são distribuídos ao longo do
#' contorno triangular, de forma geométrica, coerente, auditável e reproduzível.
#'
#' A regra local de ligação é:
#' `j <- (i + k - 1) %% n + 1`,
#' em que `i` e `j` pertencem a `1, ..., n` dentro de cada módulo.
#'
#' Os índices globais dos pregos são sequenciais ao longo dos módulos:
#'
#' \enumerate{
#'   \item módulo 1: `1, ..., n`;
#'   \item módulo 2: `n + 1, ..., 2n`;
#'   \item ...;
#'   \item módulo `m`: `((m - 1)n + 1), ..., mn`.
#' }
#'
#' @return Invisivelmente, uma lista com:
#' \describe{
#'   \item{pregos}{`data.frame` com colunas `indice`, `x`, `y`, `grupo`
#'   e `indice_local`.}
#'   \item{conexoes}{`data.frame` com colunas canônicas
#'   `indice_conexao`, `prego_inicial`, `prego_final`, `x_inicial`,
#'   `y_inicial`, `x_final`, `y_final`, `comprimento`, além dos aliases
#'   `i`, `j`, `x1`, `y1`, `x2`, `y2`, e das colunas extras
#'   `grupo`, `indice_local_inicial`, `indice_local_final` e `cor`.}
#'   \item{comprimento_total}{Comprimento total do barbante.}
#'   \item{meta}{Metadados da construção.}
#' }
#'
#' @examples
#' # Exemplo básico
#' res <- stradial(n = 20, k = 4, m = 6)
#'
#' # Exemplo com pregos e rótulos
#' res <- stradial(
#'   n = 18, k = 5, m = 5,
#'   show_points = TRUE,
#'   show_labels = TRUE
#' )
#'
#' # Exemplo sem gráfico
#' res <- stradial(
#'   n = 16, k = 3, m = 4,
#'   plot = FALSE, verbose = FALSE
#' )
#' res$comprimento_total
#'
#' @importFrom graphics plot points segments text
#' @export
stradial <- function(
    n,
    k,
    ...,
    m = 1,
    r = 1.2,
    spread = pi / 5,
    col = rainbow(m),
    lwd = 1,
    plot = TRUE,
    show_points = FALSE,
    cex_pregos = 0.8,
    col_pregos = "black",
    show_labels = FALSE,
    cex_labels = 0.7,
    label_col = "black",
    verbose = TRUE,
    border_col = "black",
    border_lwd = 1.1,
    point_bg = "white",
    pch_points = 21,
    show_center = TRUE,
    center_col = "black",
    cex_center = 0.9
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

  if (!is.numeric(m) || length(m) != 1L || is.na(m) ||
      m != as.integer(m) || m < 1L) {
    stop("`m` must be a single integer greater than or equal to 1.")
  }

  if (!is.numeric(r) || length(r) != 1L || is.na(r) || r <= 0) {
    stop("`r` must be a single positive number.")
  }

  if (!is.numeric(spread) || length(spread) != 1L || is.na(spread) || spread <= 0) {
    stop("`spread` must be a single positive number.")
  }

  if (!is.numeric(lwd) || length(lwd) != 1L || is.na(lwd) || lwd <= 0) {
    stop("`lwd` must be a single positive number.")
  }

  if (!is.numeric(border_lwd) || length(border_lwd) != 1L ||
      is.na(border_lwd) || border_lwd <= 0) {
    stop("`border_lwd` must be a single positive number.")
  }

  if (!is.numeric(cex_pregos) || length(cex_pregos) != 1L ||
      is.na(cex_pregos) || cex_pregos <= 0) {
    stop("`cex_pregos` must be a single positive number.")
  }

  if (!is.numeric(cex_labels) || length(cex_labels) != 1L ||
      is.na(cex_labels) || cex_labels <= 0) {
    stop("`cex_labels` must be a single positive number.")
  }

  if (!is.numeric(cex_center) || length(cex_center) != 1L ||
      is.na(cex_center) || cex_center <= 0) {
    stop("`cex_center` must be a single positive number.")
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

  if (!is.logical(show_center) || length(show_center) != 1L || is.na(show_center)) {
    stop("`show_center` must be TRUE or FALSE.")
  }

  n <- as.integer(n)
  k <- as.integer(k)
  m <- as.integer(m)

  if (k >= n) {
    stop("`k` must satisfy 1 <= k <= n - 1.")
  }

  if (length(col) == 1L) {
    col <- rep(col, m)
  }

  if (length(col) != m) {
    stop("`col` must have length 1 or `m`.")
  }

  # ---------------------------------------------------------------------------
  # Funções auxiliares
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

  rotaciona <- function(x, y, ang) {
    xr <- x * cos(ang) - y * sin(ang)
    yr <- x * sin(ang) + y * cos(ang)
    cbind(x = xr, y = yr)
  }

  interp_segmento <- function(P, Q, t) {
    c(
      (1 - t) * P[1] + t * Q[1],
      (1 - t) * P[2] + t * Q[2]
    )
  }

  pregos_no_triangulo <- function(A, B, C, n_local) {
    L1 <- sqrt(sum((B - A)^2))
    L2 <- sqrt(sum((C - B)^2))
    L3 <- sqrt(sum((A - C)^2))
    per <- L1 + L2 + L3

    s <- seq(0, per, length.out = n_local + 1L)[-(n_local + 1L)]
    pts <- matrix(0, nrow = n_local, ncol = 2)

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

    colnames(pts) <- c("x", "y")
    as.data.frame(pts)
  }

  # ---------------------------------------------------------------------------
  # Geometria do módulo base
  # ---------------------------------------------------------------------------
  O  <- c(0, 0)
  P1 <- c(r * cos(-spread / 2), r * sin(-spread / 2))
  P2 <- c(r * cos( spread / 2), r * sin( spread / 2))

  # ---------------------------------------------------------------------------
  # Construção de pregos e conexões
  # ---------------------------------------------------------------------------
  pregos_list <- list()
  conexoes_list <- list()

  global_nail_id <- 0L
  global_conn_id <- 0L

  for (g in 1:m) {
    ang <- 2 * pi * (g - 1L) / m

    Oi  <- as.numeric(rotaciona(O[1],  O[2],  ang))
    P1i <- as.numeric(rotaciona(P1[1], P1[2], ang))
    P2i <- as.numeric(rotaciona(P2[1], P2[2], ang))

    pregos_mod <- pregos_no_triangulo(Oi, P1i, P2i, n)

    ids_mod <- integer(n)
    for (i_local in 1:n) {
      global_nail_id <- global_nail_id + 1L
      ids_mod[i_local] <- global_nail_id

      pregos_list[[global_nail_id]] <- data.frame(
        indice = global_nail_id,
        x = pregos_mod$x[i_local],
        y = pregos_mod$y[i_local],
        grupo = g,
        indice_local = i_local,
        stringsAsFactors = FALSE
      )
    }

    for (i_local in 1:n) {
      j_local <- (i_local + k - 1L) %% n + 1L

      i_global <- ids_mod[i_local]
      j_global <- ids_mod[j_local]

      x1 <- pregos_mod$x[i_local]
      y1 <- pregos_mod$y[i_local]
      x2 <- pregos_mod$x[j_local]
      y2 <- pregos_mod$y[j_local]

      comp <- sqrt((x2 - x1)^2 + (y2 - y1)^2)

      global_conn_id <- global_conn_id + 1L

      conexoes_list[[global_conn_id]] <- data.frame(
        indice_conexao = global_conn_id,
        prego_inicial = i_global,
        prego_final = j_global,
        x_inicial = x1,
        y_inicial = y1,
        x_final = x2,
        y_final = y2,
        comprimento = comp,
        i = i_global,
        j = j_global,
        x1 = x1,
        y1 = y1,
        x2 = x2,
        y2 = y2,
        grupo = g,
        indice_local_inicial = i_local,
        indice_local_final = j_local,
        cor = col[g],
        stringsAsFactors = FALSE
      )
    }
  }

  pregos_all <- do.call(rbind, pregos_list)
  conexoes <- do.call(rbind, conexoes_list)

  rownames(pregos_all) <- NULL
  rownames(conexoes) <- NULL

  comprimento_total <- sum(conexoes$comprimento)

  # ---------------------------------------------------------------------------
  # Gráfico
  # ---------------------------------------------------------------------------
  if (plot) {
    lim <- 1.20 * r

    graphics::plot(
      NA, NA,
      xlim = c(-lim, lim),
      ylim = c(-lim, lim),
      asp = 1,
      axes = FALSE,
      xlab = "",
      ylab = ""
    )

    for (g in 1:m) {
      idx_p <- which(pregos_all$grupo == g)
      pmod <- pregos_all[idx_p, , drop = FALSE]

      Oi  <- c(0, 0)
      ang <- 2 * pi * (g - 1L) / m
      P1i <- as.numeric(rotaciona(P1[1], P1[2], ang))
      P2i <- as.numeric(rotaciona(P2[1], P2[2], ang))

      graphics::segments(Oi[1], Oi[2], P1i[1], P1i[2], col = border_col, lwd = border_lwd)
      graphics::segments(P1i[1], P1i[2], P2i[1], P2i[2], col = border_col, lwd = border_lwd)
      graphics::segments(P2i[1], P2i[2], Oi[1], Oi[2], col = border_col, lwd = border_lwd)

      idx_c <- which(conexoes$grupo == g)
      if (length(idx_c) > 0L) {
        graphics::segments(
          x0 = conexoes$x_inicial[idx_c],
          y0 = conexoes$y_inicial[idx_c],
          x1 = conexoes$x_final[idx_c],
          y1 = conexoes$y_final[idx_c],
          col = col[g],
          lwd = lwd
        )
      }

      if (show_points) {
        graphics::points(
          pmod$x, pmod$y,
          pch = pch_points,
          col = col_pregos,
          bg = point_bg,
          cex = cex_pregos
        )
      }

      if (show_labels) {
        graphics::text(
          pmod$x, pmod$y,
          labels = pmod$indice_local,
          pos = 3,
          cex = cex_labels,
          col = label_col
        )
      }
    }

    if (show_center) {
      graphics::points(0, 0, pch = 19, col = center_col, cex = cex_center)
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
      message("Em cada módulo, a regra local gera um único ciclo.")
    } else {
      message(sprintf(
        "Em cada módulo, a regra local gera %d ciclos independentes (gcd(n, k) = %d).",
        d, d
      ))
    }

    conexoes_txt <- paste0(
      "Módulo ", conexoes$grupo,
      ": Prego ", conexoes$indice_local_inicial,
      " -> Prego ", conexoes$indice_local_final
    )
    cat(paste(conexoes_txt, collapse = "\n"), "\n")
  }

  # ---------------------------------------------------------------------------
  # Retorno padronizado
  # ---------------------------------------------------------------------------
  res <- list(
    pregos = pregos_all,
    conexoes = conexoes,
    comprimento_total = comprimento_total,
    meta = list(
      figura = "radial",
      regra = "Em cada módulo: j = (i + k - 1) %% n + 1",
      parametros = list(
        n = n,
        k = k,
        m = m,
        r = r,
        spread = spread,
        col = col,
        lwd = lwd
      ),
      pacote = "stringArt"
    )
  )

  class(res) <- c("stringart_result", class(res))

  invisible(res)
}

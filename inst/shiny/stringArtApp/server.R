##################################
# stringArt Shiny App
# Standardized English Server
##################################

shinyServer(function(input, output, session) {

  # --------------------------------------------------
  # Helper operator
  # --------------------------------------------------
  `%||%` <- function(x, y) {
    if (is.null(x)) y else x
  }

  # --------------------------------------------------
  # Column-name helpers
  # --------------------------------------------------
  first_available <- function(x, candidates) {
    for (nm in candidates) {
      if (!is.null(x[[nm]])) {
        return(x[[nm]])
      }
    }
    NULL
  }

  rename_columns <- function(df, map) {
    for (old in names(map)) {
      new <- unname(map[[old]])
      if (old %in% names(df) && !new %in% names(df)) {
        names(df)[names(df) == old] <- new
      }
    }
    df
  }

  # --------------------------------------------------
  # Available figures
  # --------------------------------------------------
  available_figures <- c(
    "Circle",
    "Cardioid",
    "Ellipse",
    "Triangle",
    "Regular polygon",
    "Star",
    "Parabola",
    "Net",
    "Hexaflower",
    "Radial",
    "Lotus",
    "Rose",
    "Spiral",
    "Lissajous",
    "Region",
    "Rectangular grid",
    "Decimal"
  )

  # --------------------------------------------------
  # Fixed internal app parameters
  # --------------------------------------------------
  fixed_params <- reactive({
    req(input$figure)

    switch(
      input$figure,

      "Circle" = list(
        r = 5
      ),

      "Cardioid" = list(
        r = 4,
        rotate = 0
      ),

      "Ellipse" = list(
        a = 4,
        b = 2.5
      ),

      "Triangle" = list(
        side = 5
      ),

      "Regular polygon" = list(
        sides = 5,
        radius = 4,
        rotate = pi / 2
      ),

      "Star" = list(
        radius = 4,
        rotate = pi / 2,
        draw_polygon = FALSE
      ),

      "Parabola" = list(
        width = 6,
        height = 6,
        show_envelope = FALSE
      ),

      "Net" = list(
        length1 = 6,
        length2 = 6,
        angle = pi / 2,
        rotate = 0,
        show_envelope = FALSE
      ),

      "Hexaflower" = list(
        r = 3,
        scale_mid = 0.72,
        scale_inner = 0.42,
        offset_mid = 0,
        offset_inner = 0
      ),

      "Radial" = list(
        m = 6,
        r = 1.2,
        spread = pi / 5,
        rotate = 0,
        show_center = TRUE,
        center_col = "black",
        center_cex = 0.9
      ),

      "Lotus" = list(
        petals = 5,
        outer_radius = 4,
        petal_radius = 1.35,
        petal_center_radius = 1.35,
        inner_radius = 0.7,
        rotate = 0
      ),

      "Rose" = list(
        petals = 6,
        amplitude = 4,
        rotate = 0
      ),

      "Spiral" = list(
        turns = 3,
        spacing = 1.2,
        inner_radius = 0,
        rotate = 0
      ),

      "Lissajous" = list(
        a = 3,
        b = 2,
        phase = pi / 2,
        amplitude_x = 4,
        amplitude_y = 4,
        rotate = 0
      ),

      "Region" = list(
        contour = NULL
      ),

      "Rectangular grid" = list(
        width = 6,
        height = 4,
        rotate = 0
      ),

      "Decimal" = list(
        numerator = 1,
        denominator = 7,
        radius = 4,
        rotate = pi / 2,
        include_integer_part = TRUE
      )
    )
  })

  # --------------------------------------------------
  # Limits for n by figure
  # --------------------------------------------------
  min_n_by_figure <- reactive({
    req(input$figure)

    switch(
      input$figure,
      "Decimal" = 2L,
      "Region" = 4L,
      "Hexaflower" = 6L,
      "Regular polygon" = 5L,
      "Star" = 5L,
      3L
    )
  })

  max_n_by_figure <- reactive({
    req(input$figure)

    switch(
      input$figure,
      "Decimal" = 10L,
      300L
    )
  })

  # --------------------------------------------------
  # Suggested initial values by figure
  # --------------------------------------------------
  observeEvent(input$figure, {
    if (input$figure == "Star") {
      updateNumericInput(session, "n", min = 5, max = 300, value = 5)
      updateNumericInput(session, "k", min = 2, max = 2, value = 2)

    } else if (input$figure == "Regular polygon") {
      updateNumericInput(session, "n", min = 5, max = 300, value = 60)
      updateNumericInput(session, "k", min = 1, max = 59, value = 7)

    } else if (input$figure == "Decimal") {
      updateNumericInput(session, "n", min = 2, max = 10, value = 10)
      updateNumericInput(session, "k", min = 1, max = 20, value = 2)
    }
  }, ignoreInit = TRUE)

  # --------------------------------------------------
  # Effective number of pegs
  # --------------------------------------------------
  effective_n <- reactive({
    req(input$n, input$figure)

    n0 <- as.integer(input$n)

    if (input$figure == "Decimal") {
      return(max(2L, min(10L, n0)))
    }

    if (input$figure == "Region") {
      return(max(4L, n0))
    }

    if (input$figure == "Regular polygon") {
      return(max(5L, n0))
    }

    if (input$figure == "Star") {
      return(max(5L, n0))
    }

    if (input$figure == "Hexaflower") {
      n1 <- max(6L, as.integer(round(n0 / 6) * 6))
      return(n1)
    }

    max(3L, n0)
  })

  # --------------------------------------------------
  # Limits and effective value of k
  # --------------------------------------------------
  k_limits <- reactive({
    req(input$figure, effective_n())

    if (input$figure == "Decimal") {
      list(min = 1L, max = 20L)

    } else if (input$figure == "Star") {
      # For pedagogical visualization of star polygons, avoid k = 1,
      # which only gives the regular polygon, and avoid equivalent steps
      # greater than n / 2.
      list(
        min = 2L,
        max = max(2L, floor((effective_n() - 1L) / 2L))
      )

    } else {
      list(
        min = 1L,
        max = max(1L, effective_n() - 1L)
      )
    }
  })

  effective_k <- reactive({
    req(input$k, input$figure, effective_n())

    limits <- k_limits()
    k0 <- as.integer(round(input$k))

    max(limits$min, min(k0, limits$max))
  })

  # --------------------------------------------------
  # Standardized plot title
  # --------------------------------------------------
  plot_title <- reactive({
    req(input$figure, effective_n(), effective_k())

    paste0(
      input$figure,
      " - n = ", effective_n(),
      ", k = ", effective_k()
    )
  })

  # --------------------------------------------------
  # Dynamic adjustment of n
  # --------------------------------------------------
  observe({
    req(input$figure, input$n)

    n_eff <- effective_n()

    updateNumericInput(
      session,
      "n",
      min = min_n_by_figure(),
      max = max_n_by_figure(),
      value = n_eff
    )
  })

  # --------------------------------------------------
  # Dynamic adjustment of k
  # --------------------------------------------------
  observe({
    req(input$figure, effective_n(), input$k)

    limits <- k_limits()

    updateNumericInput(
      session,
      "k",
      min = limits$min,
      max = limits$max,
      value = effective_k()
    )
  })

  # --------------------------------------------------
  # Helper:
  # normalize package output for the Shiny app
  # --------------------------------------------------
  normalize_art_result <- function(res) {

    req(res)

    pegs <- first_available(res, c("pegs", "pregos"))
    con <- first_available(res, c("connections", "conexoes"))
    total_length <- first_available(res, c("total_length", "comprimento_total"))

    req(pegs)
    req(con)
    req(total_length)

    pegs <- as.data.frame(pegs)
    con <- as.data.frame(con)

    pegs <- rename_columns(
      pegs,
      c(
        indice = "index",
        modulo = "module",
        grupo = "group",
        camada = "layer",
        indice_local = "local_index",
        eixo = "axis",
        semirreta = "ray",
        lado = "side",
        digito = "digit"
      )
    )

    if (!"index" %in% names(pegs)) {
      pegs$index <- seq_len(nrow(pegs))
    }

    con <- rename_columns(
      con,
      c(
        indice_conexao = "connection_index",
        prego_inicial = "from",
        prego_final = "to",
        x_inicial = "x_from",
        y_inicial = "y_from",
        x_final = "x_to",
        y_final = "y_to",
        comprimento = "length",
        modulo = "module",
        grupo = "group",
        bloco = "block",
        setor = "sector",
        camada = "layer",
        varredura = "sweep",
        deslocamento = "offset",
        indice_local_inicial = "local_from",
        indice_local_final = "local_to",
        indice_local = "local_index",
        digito_inicial = "digit_from",
        digito_final = "digit_to",
        posicao = "position",
        cor = "color"
      )
    )

    if (!"connection_index" %in% names(con)) {
      con$connection_index <- seq_len(nrow(con))
    }

    if (!"from" %in% names(con) && "i" %in% names(con)) {
      con$from <- con$i
    }

    if (!"to" %in% names(con) && "j" %in% names(con)) {
      con$to <- con$j
    }

    if (!"x_from" %in% names(con) && "x1" %in% names(con)) {
      con$x_from <- con$x1
    }

    if (!"y_from" %in% names(con) && "y1" %in% names(con)) {
      con$y_from <- con$y1
    }

    if (!"x_to" %in% names(con) && "x2" %in% names(con)) {
      con$x_to <- con$x2
    }

    if (!"y_to" %in% names(con) && "y2" %in% names(con)) {
      con$y_to <- con$y2
    }

    if (!"length" %in% names(con) && all(c("x_from", "y_from", "x_to", "y_to") %in% names(con))) {
      con$length <- sqrt((con$x_to - con$x_from)^2 + (con$y_to - con$y_from)^2)
    }

    # Short aliases for compatibility with previous app versions.
    if ("from" %in% names(con)) con$i <- con$from
    if ("to" %in% names(con)) con$j <- con$to
    if ("x_from" %in% names(con)) con$x1 <- con$x_from
    if ("y_from" %in% names(con)) con$y1 <- con$y_from
    if ("x_to" %in% names(con)) con$x2 <- con$x_to
    if ("y_to" %in% names(con)) con$y2 <- con$y_to

    canonical_cols <- c(
      "connection_index",
      "from", "to",
      "x_from", "y_from",
      "x_to", "y_to",
      "length"
    )

    alias_cols <- c("i", "j", "x1", "y1", "x2", "y2")
    extra_cols <- setdiff(names(con), c(canonical_cols, alias_cols))

    con <- con[, c(
      canonical_cols[canonical_cols %in% names(con)],
      alias_cols[alias_cols %in% names(con)],
      extra_cols
    ), drop = FALSE]

    res$pegs <- pegs
    res$connections <- con
    res$total_length <- as.numeric(total_length)

    # Portuguese aliases kept for compatibility with older code.
    res$pregos <- pegs
    res$conexoes <- con
    res$comprimento_total <- as.numeric(total_length)

    res
  }

  # --------------------------------------------------
  # Rule labels and descriptions
  # --------------------------------------------------
  rule_label <- reactive({
    req(input$figure)

    switch(
      input$figure,
      "Circle" = "j = (i + k - 1) %% n + 1",
      "Cardioid" = "j = ((k * (i - 1)) %% n) + 1",
      "Ellipse" = "j = (i + k - 1) %% n + 1",
      "Triangle" = "j = (i + k - 1) %% n + 1",
      "Regular polygon" = "j = (i + k - 1) %% n + 1",
      "Star" = "{n/k}: j = (i + k - 1) %% n + 1",
      "Parabola" = "Peg i on the horizontal axis -> shifted peg i on the vertical axis",
      "Net" = "Peg i on the first ray -> shifted peg i on the second ray",
      "Hexaflower" = paste(
        "Block 1: outer contour;",
        "Blocks 2 and 3: j = (i + k - 1) %% n + 1;",
        "Block 4: outer vertices -> center."
      ),
      "Radial" = "Inside each module: j = (i + k - 1) %% n + 1",
      "Lotus" = "Inside each circular module: j = (i + k - 1) %% n + 1",
      "Rose" = "Points on a polar curve; j = (i + k - 1) %% n + 1",
      "Spiral" = "Points on an Archimedean spiral; j = (i + k - 1) %% n + 1",
      "Lissajous" = "Points on a parametric curve; j = (i + k - 1) %% n + 1",
      "Region" = "j = (i + floor(n / 2) + offset) %% n",
      "Rectangular grid" = "j = (i + k - 1) %% n + 1",
      "Decimal" = "Connects consecutive digits of the decimal expansion of a fraction"
    )
  })

  rule_description <- reactive({
    req(input$figure)

    switch(
      input$figure,
      "Circle" = "The pegs are uniformly distributed on a circle, and each peg is connected to the peg `k` positions ahead.",
      "Cardioid" = "The pegs are uniformly distributed on a circle, and each peg is connected using a multiplicative modular rule. For `k = 2`, the classic cardioid-like String Art effect appears.",
      "Ellipse" = "The pegs are uniformly distributed on the boundary of an ellipse, and each peg is connected to the peg `k` positions ahead.",
      "Triangle" = "The pegs are uniformly distributed along the boundary of an equilateral triangle, and each peg is connected to the peg `k` positions ahead.",
      "Regular polygon" = "The pegs are distributed along the boundary of a regular polygon. The construction can be used to explore central angles, internal angles, symmetry, and modular arithmetic.",
      "Star" = "The pegs are distributed on a circle and connected using step `k`, forming star polygons. The audit helps observe cycles, periods, and greatest common divisors.",
      "Parabola" = "The pegs are distributed on two perpendicular axes. The family of segments generates a visual envelope associated with the classical String Art parabola.",
      "Net" = "The pegs are distributed on two rays with a common origin. The figure generalizes the parabolic construction to different angles.",
      "Hexaflower" = "The pegs are distributed on three concentric hexagonal circuits and a center. The connections combine the outer contour, jumps between circuits, and links from outer vertices to the center.",
      "Radial" = "The figure is composed of triangular modules rotated around the center. In each module, the pegs are distributed along the triangular boundary and connected locally using step `k`.",
      "Lotus" = "The figure is formed by overlapping circular modules, including an outer circle, petals, and a central core.",
      "Rose" = "The pegs are positioned on a rose-type polar curve. The figure can be used to explore radial symmetry, trigonometry, and periodicity.",
      "Spiral" = "The pegs are positioned on an Archimedean spiral. The figure can be used to explore polar coordinates, growth, and parametrization.",
      "Lissajous" = "The pegs are positioned on a Lissajous curve. The figure can be used to explore frequency, phase, frequency ratios, and parametric curves.",
      "Region" = "The pegs are distributed along a closed contour and connected to approximately opposite points, visually filling the region.",
      "Rectangular grid" = "The pegs are distributed along the boundary of a rectangle and connected by a modular rule. The figure can be used to explore coordinates, slope, and symmetry.",
      "Decimal" = "The circle is divided into digits, and the figure connects consecutive digits of the decimal expansion of a rational number."
    )
  })

  # --------------------------------------------------
  # Geometric parameters in Markdown
  # --------------------------------------------------
  geometry_parameters_md <- reactive({
    req(input$figure)

    pars <- fixed_params()
    n_eff <- effective_n()

    switch(
      input$figure,

      "Circle" = paste0(
        "- **Radius:** ", pars$r, "\n"
      ),

      "Cardioid" = paste0(
        "- **Radius:** ", pars$r, "\n",
        "- **Applied rotation:** ", sprintf("%.2f", pars$rotate), " rad\n"
      ),

      "Ellipse" = paste0(
        "- **Semi-major axis (a):** ", pars$a, "\n",
        "- **Semi-minor axis (b):** ", pars$b, "\n"
      ),

      "Triangle" = paste0(
        "- **Triangle side length:** ", pars$side, "\n"
      ),

      "Regular polygon" = paste0(
        "- **Number of sides:** ", pars$sides, "\n",
        "- **Circumradius:** ", pars$radius, "\n"
      ),

      "Star" = paste0(
        "- **Star polygon notation:** {", n_eff, "/", effective_k(), "}\n",
        "- **Radius:** ", pars$radius, "\n"
      ),

      "Parabola" = paste0(
        "- **Width:** ", pars$width, "\n",
        "- **Height:** ", pars$height, "\n"
      ),

      "Net" = paste0(
        "- **Length of the first ray:** ", pars$length1, "\n",
        "- **Length of the second ray:** ", pars$length2, "\n",
        "- **Angle between rays:** ", sprintf("%.2f", pars$angle), " rad\n"
      ),

      "Hexaflower" = paste0(
        "- **Outer radius:** ", pars$r, "\n",
        "- **Middle scale:** ", pars$scale_mid, "\n",
        "- **Inner scale:** ", pars$scale_inner, "\n",
        "- **Middle offset:** ", pars$offset_mid, "\n",
        "- **Inner offset:** ", pars$offset_inner, "\n",
        "- **Effective number of pegs per circuit:** ", n_eff, "\n"
      ),

      "Radial" = paste0(
        "- **Number of modules (m):** ", pars$m, "\n",
        "- **Outer radius of each module:** ", pars$r, "\n",
        "- **Angular spread:** ", sprintf("%.2f", pars$spread), " rad\n"
      ),

      "Lotus" = paste0(
        "- **Number of petals:** ", pars$petals, "\n",
        "- **Outer radius:** ", pars$outer_radius, "\n",
        "- **Petal radius:** ", pars$petal_radius, "\n",
        "- **Central core radius:** ", pars$inner_radius, "\n"
      ),

      "Rose" = paste0(
        "- **Number of petals:** ", pars$petals, "\n",
        "- **Amplitude:** ", pars$amplitude, "\n"
      ),

      "Spiral" = paste0(
        "- **Number of turns:** ", pars$turns, "\n",
        "- **Radial spacing:** ", pars$spacing, "\n",
        "- **Initial radius:** ", pars$inner_radius, "\n"
      ),

      "Lissajous" = paste0(
        "- **Frequency in x (a):** ", pars$a, "\n",
        "- **Frequency in y (b):** ", pars$b, "\n",
        "- **Phase:** ", sprintf("%.2f", pars$phase), " rad\n"
      ),

      "Region" = paste0(
        "- **Contour:** package default contour\n"
      ),

      "Rectangular grid" = paste0(
        "- **Width:** ", pars$width, "\n",
        "- **Height:** ", pars$height, "\n"
      ),

      "Decimal" = paste0(
        "- **Fraction:** ", pars$numerator, "/", pars$denominator, "\n",
        "- **Base / number of digits:** ", n_eff, "\n",
        "- **Displayed repetitions:** ", effective_k(), "\n"
      )
    )
  })

  # --------------------------------------------------
  # Helper:
  # call the correct package function
  # --------------------------------------------------
  build_art <- function(plot_value = FALSE) {

    n_eff <- effective_n()
    pars <- fixed_params()
    template_value <- isTRUE(input$template)

    common <- list(
      n = n_eff,
      k = effective_k(),
      col = input$col,
      lwd = input$lwd,
      plot = plot_value,
      show_points = input$show_points,
      show_labels = input$show_labels,
      verbose = FALSE,
      template = template_value,
      main = plot_title()
    )

    if (input$figure == "Circle") {
      args <- c(common, pars)
      res <- do.call(stringArt::stcircle, args)

    } else if (input$figure == "Cardioid") {
      args <- c(common, pars)
      res <- do.call(stringArt::stcardioid, args)

    } else if (input$figure == "Ellipse") {
      args <- c(common, pars)
      res <- do.call(stringArt::stellipse, args)

    } else if (input$figure == "Triangle") {
      args <- c(common, pars)
      res <- do.call(stringArt::sttriangle, args)

    } else if (input$figure == "Regular polygon") {
      args <- c(common, pars)
      res <- do.call(stringArt::stpolygon, args)

    } else if (input$figure == "Star") {
      args <- c(common, pars)
      res <- do.call(stringArt::ststar, args)

    } else if (input$figure == "Parabola") {
      args <- c(common, pars)
      res <- do.call(stringArt::stparabola, args)

    } else if (input$figure == "Net") {
      args <- c(common, pars)
      res <- do.call(stringArt::stnet, args)

    } else if (input$figure == "Hexaflower") {
      args <- c(common, pars)
      res <- do.call(stringArt::sthexaflower, args)

    } else if (input$figure == "Radial") {
      args <- c(common, pars)
      res <- do.call(stringArt::stradial, args)

    } else if (input$figure == "Lotus") {
      args <- c(common, pars)
      res <- do.call(stringArt::stlotus, args)

    } else if (input$figure == "Rose") {
      args <- c(common, pars)
      res <- do.call(stringArt::strose, args)

    } else if (input$figure == "Spiral") {
      args <- c(common, pars)
      res <- do.call(stringArt::stspiral, args)

    } else if (input$figure == "Lissajous") {
      args <- c(common, pars)
      res <- do.call(stringArt::stlissajous, args)

    } else if (input$figure == "Region") {
      args <- c(common, pars)
      res <- do.call(stringArt::stregion, args)

    } else if (input$figure == "Rectangular grid") {
      args <- c(common, pars)
      res <- do.call(stringArt::stgrid, args)

    } else if (input$figure == "Decimal") {
      args <- c(common, pars)
      res <- do.call(stringArt::stdecimal, args)

    } else {
      stop("Unknown figure.", call. = FALSE)
    }

    res
  }

  # --------------------------------------------------
  # Helper:
  # build data without plotting
  # --------------------------------------------------
  build_art_data <- function() {
    normalize_art_result(build_art(plot_value = FALSE))
  }

  # --------------------------------------------------
  # Reactive data
  # --------------------------------------------------
  art_data <- reactive({
    build_art_data()
  })

  # --------------------------------------------------
  # Drawing function
  # --------------------------------------------------
  draw_stringart <- function() {
    invisible(build_art(plot_value = TRUE))
  }

  # --------------------------------------------------
  # Plot
  # --------------------------------------------------
  output$plot <- renderPlot({
    draw_stringart()
  }, res = 96)

  # --------------------------------------------------
  # Textual audit
  # --------------------------------------------------
  format_audit <- function(audit) {
    if (is.null(audit)) {
      return("No internal package audit available.")
    }

    paste(capture.output(print(audit)), collapse = "\n")
  }

  audit_text <- reactive({

    res <- art_data()
    req(res$connections)

    con <- res$connections

    connections_txt <- if ("module" %in% names(con) &&
                           all(c("local_from", "local_to") %in% names(con))) {

      paste0(
        "Module ", con$module,
        ": Peg ", con$local_from,
        " -> Peg ", con$local_to
      )

    } else if ("block" %in% names(con)) {

      paste0(
        "[", con$block, "] ",
        "Peg ", con$from,
        " -> Peg ", con$to
      )

    } else if (all(c("digit_from", "digit_to") %in% names(con))) {

      paste0(
        "Digit ", con$digit_from,
        " -> Digit ", con$digit_to
      )

    } else if (all(c("sweep", "local_from", "local_to") %in% names(con))) {

      paste0(
        "Sweep ", con$sweep,
        ": Peg ", con$local_from,
        " -> Peg ", con$local_to
      )

    } else {

      paste0(
        "Peg ", con$from,
        " -> Peg ", con$to
      )
    }

    max_show <- 250L
    connections_txt_show <- if (length(connections_txt) > max_show) {
      c(
        connections_txt[1:max_show],
        sprintf("... (%d additional connections omitted from this view)", length(connections_txt) - max_show)
      )
    } else {
      connections_txt
    }

    paste(
      "Construction Audit",
      "",
      paste0("Figure: ", input$figure),
      paste0("Effective number of pegs (n): ", effective_n()),
      paste0("Parameter k: ", effective_k()),
      paste0("Total string length: ",
             sprintf("%.2f", res$total_length), " units"),
      "",
      "Connection rule:",
      rule_label(),
      "",
      "Package audit summary:",
      format_audit(res$audit),
      "",
      "Connections:",
      paste(connections_txt_show, collapse = "\n"),
      sep = "\n"
    )
  })

  output$audit_text <- renderText({
    audit_text()
  })

  # --------------------------------------------------
  # Detailed audit in console
  # --------------------------------------------------
  observeEvent(
    {
      list(input$figure, input$n, input$k, input$col, input$lwd,
           input$show_points, input$show_labels, input$template, input$verbose)
    },
    {
      if (isTRUE(input$verbose)) {
        cat("\n============================\n")
        cat(audit_text())
        cat("\n============================\n")
      }
    },
    ignoreInit = TRUE
  )

  # --------------------------------------------------
  # Connections table
  # --------------------------------------------------
  output$connections_table <- DT::renderDataTable({

    res <- art_data()
    req(res$connections)

    con <- res$connections

    canonical_cols <- c(
      "connection_index",
      "from", "to",
      "x_from", "y_from",
      "x_to", "y_to",
      "length"
    )

    alias_cols <- c("i", "j", "x1", "y1", "x2", "y2")
    extra_cols <- setdiff(names(con), c(canonical_cols, alias_cols))

    display_cols <- c(
      canonical_cols[canonical_cols %in% names(con)],
      extra_cols
    )

    table_data <- con[, display_cols, drop = FALSE]

    DT::datatable(
      table_data,
      rownames = FALSE,
      options = list(
        pageLength = 10,
        scrollX = TRUE,
        scrollY = "420px",
        searching = FALSE,
        lengthChange = FALSE
      )
    )
  })

  # --------------------------------------------------
  # Downloads
  # --------------------------------------------------
  output$download_png <- downloadHandler(
    filename = function() {
      paste0("stringart_", Sys.Date(), ".png")
    },
    content = function(file) {
      png(file, width = 1200, height = 1200, res = 150)
      draw_stringart()
      dev.off()
    }
  )

  output$download_hd <- downloadHandler(
    filename = function() {
      paste0("stringart_HD_", Sys.Date(), ".png")
    },
    content = function(file) {
      png(file, width = 3000, height = 3000, res = 300)
      draw_stringart()
      dev.off()
    }
  )

  output$download_pdf <- downloadHandler(
    filename = function() {
      paste0("stringart_", Sys.Date(), ".pdf")
    },
    content = function(file) {
      grDevices::pdf(file, width = 8, height = 8)
      draw_stringart()
      dev.off()
    }
  )

  observeEvent(input$print_plot, {
    session$sendCustomMessage("printPlot", list())
  })

  # --------------------------------------------------
  # Technical summary
  # --------------------------------------------------
  output$summary_md <- renderUI({

    res <- art_data()
    req(res)

    n_eff <- effective_n()

    ideal_n <- switch(
      input$figure,

      "Circle" = if (n_eff < 30) {
        "For better visual definition, it is recommended to use at least 40 pegs."
      } else if (n_eff >= 40 && n_eff <= 120) {
        "The number of pegs is in a suitable range for good geometric definition."
      } else {
        "A large number of pegs produces high visual density and greater structural complexity."
      },

      "Cardioid" = if (n_eff < 60) {
        "For cardioid-like patterns, it is recommended to use at least 80 pegs for a smoother visual envelope."
      } else {
        "The number of pegs is suitable for observing the envelope produced by the multiplicative rule."
      },

      "Ellipse" = "For ellipses, it is recommended to use between 60 and 120 pegs for better perception of curvature.",

      "Triangle" = if (n_eff < 24) {
        "For triangles, at least 24 pegs are recommended for better distribution along the boundary."
      } else if (n_eff >= 24 && n_eff <= 90) {
        "The number of pegs is suitable for a balanced triangular construction."
      } else {
        "A large number of pegs produces a denser segment mesh and a more complex triangular structure."
      },

      "Regular polygon" = "For regular polygons, it is recommended to balance the number of pegs with the number of sides to preserve boundary readability.",

      "Star" = "For stars, the relationship between `n` and `k` is essential: if the greatest common divisor is 1, the figure forms a single cycle.",

      "Parabola" = "For the parabola, values between 30 and 80 pegs usually produce a clear visualization of the envelope.",

      "Net" = "For String Art nets, moderate values of `n` preserve the readability of the segments and of the envelope.",

      "Hexaflower" = "For Hexaflower, values of `n` that are multiples of 6, with at least 18 or 24 pegs per circuit, are recommended for better symmetry.",

      "Radial" = "For radial figures, the choice of `n` should balance visual density and module readability.",

      "Lotus" = "For the Lotus, values between 30 and 80 pegs per module usually provide a good balance between smoothness and readability.",

      "Rose" = "For rose curves, larger values of `n` are recommended when a smoother trigonometric curve is desired.",

      "Spiral" = "For spirals, larger values of `n` make the parametrization smoother and the structure more continuous.",

      "Lissajous" = "For Lissajous curves, larger values of `n` help represent the periodicity of the curve more accurately.",

      "Region" = "For regions, values between 80 and 150 pegs tend to produce a more uniform filling of the contour.",

      "Rectangular grid" = "For rectangular grids, the number of pegs should allow good distribution along the four sides.",

      "Decimal" = "In the decimal figure, `n` represents the base and the number of digits. For the usual decimal representation, use `n = 10`."
    )

    ideal_lwd <- if (input$lwd < 1) {
      "Smaller line widths produce more delicate drawings."
    } else if (input$lwd >= 1 && input$lwd <= 2) {
      "The selected line width is suitable for balanced visualization."
    } else {
      "Larger line widths produce a stronger visual effect."
    }

    note_hex <- if (input$figure == "Hexaflower" && !identical(as.integer(input$n), n_eff)) {
      paste0(
        "\n### Automatic adjustment\n\n",
        "- The value of `n` was automatically adjusted to **", n_eff,
        "** because the Hexaflower figure requires `n` to be a multiple of 6.\n\n"
      )
    } else {
      ""
    }

    note_decimal <- if (input$figure == "Decimal") {
      paste0(
        "\n### Note about the decimal figure\n\n",
        "- In this simplified Shiny version, the fraction is **1/7**, and `k` controls the number of displayed period repetitions.\n\n"
      )
    } else {
      ""
    }

    note_template <- if (isTRUE(input$template)) {
      paste0(
        "\n### Peg template mode\n\n",
        "- The **peg template without strings** mode is active. The app shows the pegs without drawing the connections.\n\n"
      )
    } else {
      ""
    }

    HTML(markdown::markdownToHTML(
      text = paste0(
        "### Generated Figure Specification\n\n",
        "- **Figure:** ", input$figure, "\n",
        "- **Effective number of pegs (n):** ", n_eff, "\n",
        "- **Parameter k:** ", effective_k(), "\n",
        geometry_parameters_md(),
        "- **Line width (lwd):** ", input$lwd, "\n",
        "- **Total string length:** ", sprintf("%.2f", res$total_length), " units\n\n",

        "### Construction Logic\n\n",
        rule_description(), "\n\n",
        "The connection rule is:\n\n",
        "`", rule_label(), "`\n\n",

        "### Technical Recommendations\n\n",
        "- ", ideal_n, "\n",
        "- ", ideal_lwd, "\n\n",

        note_hex,
        note_decimal,
        note_template,

        "### Academic Credits\n\n",
        "This Shiny application is part of the educational product developed by **Ivo Moreira Barbosa**, PROFMAT master's student.\n\n",
        "Advisor: **Fernando de Souza Bastos**.\n\n",
        "Defense year: **2026**."
      ),
      fragment.only = TRUE
    ))
  })
})

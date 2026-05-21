##################################
# stringArt Shiny App
# Simplified English UI
##################################

shinyUI(
  dashboardPage(

    skin = "green",

    dashboardHeader(
      title = "stringArt",
      titleWidth = 300
    ),

    dashboardSidebar(
      width = 300,

      sidebarMenu(

        tags$div(
          style = "text-align: center; padding: 15px;",
          img(src = "StringArt.png", width = 180)
        ),

        menuItem("Home", tabName = "home", icon = icon("home")),
        menuItem("String Art", tabName = "stringart", icon = icon("circle-nodes"))
      )
    ),

    dashboardBody(

      tags$script(HTML("
        Shiny.addCustomMessageHandler('printPlot', function(message) {
          window.print();
        });
      ")),

      tags$style(HTML("
        @media print {
          .main-sidebar,
          .main-header,
          .control-sidebar,
          .no-print {
            display: none !important;
          }

          .content-wrapper,
          .right-side,
          .main-footer {
            margin-left: 0 !important;
            padding: 0 !important;
          }

          .box {
            border: none !important;
            box-shadow: none !important;
          }

          #print-area {
            width: 100% !important;
            height: auto !important;
            margin: 0 auto !important;
          }

          #print-area img,
          #print-area canvas {
            display: block;
            margin: 0 auto;
          }
        }
      ")),

      tags$style(
        type = "text/css",
        "
        .shiny-output-error { visibility: hidden; }
        .shiny-output-error:before { visibility: hidden; }

        .equal-box {
          height: 650px;
        }

        .equal-box .box-body {
          height: 580px;
          overflow-y: auto;
        }

        .audit-box {
          height: 500px;
          overflow-y: auto;
          overflow-x: auto;
          padding-right: 10px;
        }

        .audit-box pre {
          white-space: pre-wrap;
          word-break: break-word;
        }

        .table-box {
          height: 500px;
          overflow-y: auto;
          overflow-x: auto;
        }
        "
      ),

      tabItems(

        tabItem(
          tabName = "home",

          fluidRow(
            box(
              width = 12,
              title = "Welcome to stringArt",
              status = "primary",
              solidHeader = TRUE,
              collapsible = FALSE,
              p("Interactive application for generating String Art figures."),
              p("The available figures include circle, ellipse, triangle, cardioid, polygons, stars, parabola, nets, hexaflowers, radial modules, lotus, rose, spiral, Lissajous curves, regions, rectangular grids, and decimal patterns."),
              p("Choose a figure and adjust the main parameters to generate the construction.")
            )
          )
        ),

        tabItem(
          tabName = "stringart",

          fluidRow(

            box(
              width = 3,
              class = "equal-box",
              title = "Parameters",
              status = "success",
              solidHeader = TRUE,

              selectInput(
                "figure",
                "Choose a figure:",
                choices = c(
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
                ),
                selected = "Circle"
              ),

              numericInput(
                "n",
                "Number of pegs:",
                value = 30,
                min = 3,
                max = 300,
                step = 1
              ),

              numericInput(
                "k",
                "Step / factor (k):",
                value = 5,
                min = 1,
                max = 100,
                step = 1
              ),

              colourInput(
                "col",
                "Line color:",
                value = "blue"
              ),

              sliderInput(
                "lwd",
                "Line width:",
                min = 0.5, max = 5, value = 1.2, step = 0.1
              ),

              checkboxInput(
                "show_points",
                "Show pegs",
                value = FALSE
              ),

              checkboxInput(
                "show_labels",
                "Show labels",
                value = FALSE
              ),

              checkboxInput(
                "template",
                "Show peg template without strings",
                value = FALSE
              ),

              checkboxInput(
                "verbose",
                "Show detailed audit in the console",
                value = FALSE
              )
            ),

            box(
              width = 9,
              class = "equal-box",
              title = "Visualization and Audit",
              status = "primary",
              solidHeader = TRUE,

              fluidRow(
                column(
                  12,
                  align = "right",
                  class = "no-print",
                  downloadButton("download_png", "Download PNG"),
                  downloadButton("download_hd", "Download high resolution"),
                  downloadButton("download_pdf", "Download PDF"),
                  actionButton("print_plot", "Print")
                )
              ),

              br(),

              tabsetPanel(
                id = "output_tabs",

                tabPanel(
                  "Figure",
                  br(),
                  div(
                    id = "print-area",
                    plotOutput("plot", height = "520px")
                  )
                ),

                tabPanel(
                  "Audit",
                  br(),
                  div(
                    class = "audit-box",
                    verbatimTextOutput("audit_text")
                  )
                ),

                tabPanel(
                  "Connections table",
                  br(),
                  div(
                    class = "table-box",
                    DT::dataTableOutput("connections_table")
                  )
                )
              )
            )
          ),

          fluidRow(
            box(
              width = 12,
              title = "Technical Summary",
              status = "info",
              solidHeader = TRUE,
              htmlOutput("summary_md")
            )
          )
        )
      )
    )
  )
)

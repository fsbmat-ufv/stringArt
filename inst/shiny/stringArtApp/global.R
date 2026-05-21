##################################
# stringArt Shiny App
# Packaged app global file
##################################

required_packages <- c(
  "shiny",
  "shinydashboard",
  "colourpicker",
  "DT",
  "markdown"
)

missing_packages <- required_packages[
  !vapply(required_packages, requireNamespace, quietly = TRUE, FUN.VALUE = logical(1))
]

if (length(missing_packages) > 0) {
  stop(
    "The following packages are required to run the stringArt Shiny app: ",
    paste(missing_packages, collapse = ", "),
    ". Please install them before launching the app.",
    call. = FALSE
  )
}

library(shiny)
library(shinydashboard)
library(colourpicker)

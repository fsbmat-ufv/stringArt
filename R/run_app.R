#' Launch the stringArt Shiny app
#'
#' Opens the interactive Shiny application included with the package.
#'
#' @param launch.browser Logical. If `TRUE`, the application is opened in the
#'   user's default browser.
#' @param ... Additional arguments passed to [shiny::runApp()].
#'
#' @return Invisibly returns the result of [shiny::runApp()].
#'
#' @examples
#' if (interactive()) {
#'   run_stringArt_app()
#' }
#'
#' @export
run_stringArt_app <- function(launch.browser = TRUE, ...) {
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
      ". Please install them with install.packages().",
      call. = FALSE
    )
  }

  app_dir <- system.file("shiny", "stringArtApp", package = "stringArt")

  if (!nzchar(app_dir)) {
    stop(
      "Could not find the Shiny app inside the installed stringArt package.",
      call. = FALSE
    )
  }

  shiny::runApp(
    appDir = app_dir,
    launch.browser = launch.browser,
    ...
  )
}

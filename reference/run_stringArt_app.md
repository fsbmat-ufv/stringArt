# Launch the stringArt Shiny app

Opens the interactive Shiny application included with the package.

## Usage

``` r
run_stringArt_app(launch.browser = TRUE, ...)
```

## Arguments

- launch.browser:

  Logical. If `TRUE`, the application is opened in the user's default
  browser.

- ...:

  Additional arguments passed to
  [`shiny::runApp()`](https://rdrr.io/pkg/shiny/man/runApp.html).

## Value

Invisibly returns the result of
[`shiny::runApp()`](https://rdrr.io/pkg/shiny/man/runApp.html).

## Examples

``` r
if (interactive()) {
  run_stringArt_app()
}
```

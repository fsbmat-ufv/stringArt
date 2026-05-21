# Generate a star polygon string art pattern

`ststar()` generates a classical star polygon `{n/k}` by placing `n`
pegs on a circle and connecting each peg to the peg `k` positions ahead.

## Usage

``` r
ststar(
  n = 5,
  k = 2,
  col = "blue",
  lwd = 1,
  plot = TRUE,
  show_points = TRUE,
  show_labels = FALSE,
  verbose = FALSE,
  radius = 1,
  rotate = pi/2,
  show_strings = TRUE,
  template = FALSE,
  draw_polygon = TRUE,
  border_col = "grey50",
  border_lwd = 1,
  point_col = "black",
  point_cex = 0.8,
  point_pch = 19,
  point_bg = "white",
  label_cex = 0.7,
  label_col = "black",
  bg = "white",
  main = NULL
)
```

## Arguments

- n:

  Integer. Number of pegs on the circle. Must be at least 3.

- k:

  Integer. Star step. Must satisfy `1 <= k <= n - 1`.

- col:

  String color passed to
  [`graphics::segments()`](https://rdrr.io/r/graphics/segments.html).

- lwd:

  Positive number. Line width used to draw the star.

- plot:

  Logical. If `TRUE`, draws the figure.

- show_points:

  Logical. If `TRUE`, draws the pegs.

- show_labels:

  Logical. If `TRUE`, draws peg labels.

- verbose:

  Logical. If `TRUE`, prints a short audit to the console.

- radius:

  Positive number. Circle radius.

- rotate:

  Numeric. Rotation angle in radians applied to the star.

- show_strings:

  Logical. If `TRUE`, draws the star connections.

- template:

  Logical. If `TRUE`, draws only the peg template, without star
  connections. This is equivalent to setting `show_strings = FALSE` and
  `show_points = TRUE`.

- draw_polygon:

  Logical. If `TRUE`, draws the underlying regular polygon.

- border_col:

  Polygon border color.

- border_lwd:

  Positive number. Border line width.

- point_col:

  Peg color.

- point_cex:

  Positive number. Peg size.

- point_pch:

  Plotting symbol used for pegs.

- point_bg:

  Peg background color when applicable.

- label_cex:

  Positive number. Label size.

- label_col:

  Label color.

- bg:

  Plot background color.

- main:

  Optional plot title. If `NULL`, no title is displayed.

## Value

Invisibly returns a list of class `stringart_result` with:

- pegs:

  A `data.frame` with columns `index`, `x`, and `y`.

- connections:

  A `data.frame` with columns `connection_index`, `from`, `to`,
  `x_from`, `y_from`, `x_to`, `y_to`, and `length`.

- total_length:

  Total string length.

- audit:

  A character vector with audit information.

- meta:

  A list with construction metadata.

## Details

The star polygon uses the additive modular rule

`to = ((from + k - 1) %% n) + 1`.

The greatest common divisor `gcd(n, k)` determines the number of cycles.
If `gcd(n, k) = 1`, the star is a single cycle. Otherwise, the
construction decomposes into several independent cycles.

## Examples

``` r
ststar()

ststar(n = 5, k = 2)
ststar(n = 7, k = 2)

ststar(n = 8, k = 3)

ststar(template = TRUE)

```

# Generate a rose-like string art pattern

`strose()` generates a rose-like string art figure based on a polar
curve of the form
`r(theta) = amplitude * (1 + cos(petals * theta)) / 2`.

## Usage

``` r
strose(
  n = 240,
  k = 17,
  col = "deeppink4",
  lwd = 0.7,
  plot = TRUE,
  show_points = FALSE,
  show_labels = FALSE,
  verbose = FALSE,
  petals = 6,
  amplitude = 1,
  rotate = 0,
  show_strings = TRUE,
  template = FALSE,
  draw_curve = TRUE,
  border_col = "grey50",
  border_lwd = 1,
  point_col = "black",
  point_cex = 0.5,
  point_pch = 19,
  point_bg = "white",
  label_cex = 0.6,
  label_col = "black",
  bg = "white",
  main = NULL
)
```

## Arguments

- n:

  Integer. Number of pegs sampled along the curve. Must be at least 3.

- k:

  Integer. Additive modular step used in the connection rule. Must
  satisfy `1 <= k <= n - 1`.

- col:

  String color passed to
  [`graphics::segments()`](https://rdrr.io/r/graphics/segments.html).

- lwd:

  Positive number. Line width used to draw the strings.

- plot:

  Logical. If `TRUE`, draws the figure.

- show_points:

  Logical. If `TRUE`, draws the pegs.

- show_labels:

  Logical. If `TRUE`, draws peg labels.

- verbose:

  Logical. If `TRUE`, prints a short audit to the console.

- petals:

  Integer. Number of petals in the rose-like curve.

- amplitude:

  Positive number. Maximum radial amplitude.

- rotate:

  Numeric. Rotation angle in radians.

- show_strings:

  Logical. If `TRUE`, draws the string connections.

- template:

  Logical. If `TRUE`, draws only the peg template, without string
  connections. This is equivalent to setting `show_strings = FALSE` and
  `show_points = TRUE`.

- draw_curve:

  Logical. If `TRUE`, draws the underlying rose-like curve.

- border_col:

  Curve color.

- border_lwd:

  Positive number. Curve line width.

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

Invisibly returns a list of class `stringart_result`.

## Details

The pegs are sampled from the polar curve
`r(theta) = amplitude * (1 + cos(petals * theta)) / 2`, which produces a
rose-like pattern with `petals` visible petals.

The connections follow the additive modular rule
`to = ((from + k - 1) %% n) + 1`.

## Examples

``` r
strose()

strose(petals = 6)
strose(petals = 8)

strose(template = TRUE)

```

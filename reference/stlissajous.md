# Generate a Lissajous string art pattern

`stlissajous()` generates a string art figure from pegs sampled on a
Lissajous curve.

## Usage

``` r
stlissajous(
  n = 300,
  k = 19,
  col = "purple",
  lwd = 0.7,
  plot = TRUE,
  show_points = FALSE,
  show_labels = FALSE,
  verbose = FALSE,
  a = 3,
  b = 2,
  phase = pi/2,
  amplitude_x = 1,
  amplitude_y = 1,
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

  Integer. Number of pegs sampled on the curve. Must be at least 3.

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

- a:

  Positive integer. Frequency in the x-coordinate.

- b:

  Positive integer. Frequency in the y-coordinate.

- phase:

  Numeric. Phase shift in radians.

- amplitude_x:

  Positive number. Horizontal amplitude.

- amplitude_y:

  Positive number. Vertical amplitude.

- rotate:

  Numeric. Rotation angle in radians.

- show_strings:

  Logical. If `TRUE`, draws the string connections.

- template:

  Logical. If `TRUE`, draws only the peg template.

- draw_curve:

  Logical. If `TRUE`, draws the underlying Lissajous curve.

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

  Optional plot title.

## Value

Invisibly returns a list of class `stringart_result`.

## Details

The curve is given by

`x(t) = amplitude_x * sin(a * t + phase)` and
`y(t) = amplitude_y * sin(b * t)`.

## Examples

``` r
stlissajous()

stlissajous(a = 3, b = 2)
stlissajous(a = 5, b = 4, phase = pi / 3)

stlissajous(template = TRUE)

```

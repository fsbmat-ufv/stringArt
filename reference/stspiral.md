# Generate a spiral string art pattern

`stspiral()` generates a string art pattern from pegs placed on an
Archimedean spiral.

## Usage

``` r
stspiral(
  n = 180,
  k = 13,
  col = "steelblue",
  lwd = 0.7,
  plot = TRUE,
  show_points = FALSE,
  show_labels = FALSE,
  verbose = FALSE,
  turns = 3,
  spacing = 0.6,
  inner_radius = 0,
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

  Integer. Number of pegs placed on the spiral. Must be at least 3.

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

- turns:

  Positive number. Number of spiral turns.

- spacing:

  Positive number. Radial growth per turn.

- inner_radius:

  Nonnegative number. Initial spiral radius.

- rotate:

  Numeric. Rotation angle in radians.

- show_strings:

  Logical. If `TRUE`, draws the string connections.

- template:

  Logical. If `TRUE`, draws only the peg template.

- draw_curve:

  Logical. If `TRUE`, draws the underlying spiral.

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

The spiral uses the polar form
`r(theta) = inner_radius + spacing * theta / (2 * pi)`.

## Examples

``` r
stspiral()

stspiral(turns = 4)

stspiral(template = TRUE)

```

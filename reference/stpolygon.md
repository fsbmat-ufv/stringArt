# Generate string art on a regular polygon

`stpolygon()` generates a string art figure on the boundary of a regular
polygon. Pegs are distributed uniformly along the polygon perimeter and
connected using an additive modular rule.

## Usage

``` r
stpolygon(
  n = 60,
  k = 7,
  col = "blue",
  lwd = 1,
  plot = TRUE,
  show_points = TRUE,
  show_labels = FALSE,
  verbose = FALSE,
  sides = 5,
  radius = 1,
  rotate = pi/2,
  show_strings = TRUE,
  template = FALSE,
  point_col = "black",
  point_cex = 0.8,
  point_pch = 19,
  point_bg = "white",
  label_cex = 0.7,
  label_col = "black",
  border_col = "grey50",
  border_lwd = 1,
  bg = "white",
  main = NULL
)
```

## Arguments

- n:

  Integer. Number of pegs placed along the polygon boundary. Must be at
  least 3 and at least `sides`.

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

- sides:

  Integer. Number of polygon sides. Must be at least 3.

- radius:

  Positive number. Circumradius of the polygon.

- rotate:

  Numeric. Rotation angle in radians applied to the polygon.

- show_strings:

  Logical. If `TRUE`, draws the string connections.

- template:

  Logical. If `TRUE`, draws only the peg template, without string
  connections. This is equivalent to setting `show_strings = FALSE` and
  `show_points = TRUE`.

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

- border_col:

  Polygon border color.

- border_lwd:

  Positive number. Polygon border line width.

- bg:

  Plot background color.

- main:

  Optional plot title. If `NULL`, no title is displayed.

## Value

Invisibly returns a list of class `stringart_result` with:

- pegs:

  A `data.frame` with columns `index`, `x`, `y`, `side`, and
  `local_index`.

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

The polygon is inscribed in a circle of radius `radius`. Pegs are
distributed uniformly along the full boundary, not only at the vertices.
The additive modular connection rule is:

`to = ((from + k - 1) %% n) + 1`.

This construction supports the exploration of regular polygons, central
and interior angles, symmetry, congruence, divisibility, and modular
arithmetic.

## Examples

``` r
stpolygon()

stpolygon(sides = 5)
stpolygon(sides = 6)

stpolygon(sides = 8)

stpolygon(n = 60, k = 7, sides = 5)

stpolygon(template = TRUE)

```

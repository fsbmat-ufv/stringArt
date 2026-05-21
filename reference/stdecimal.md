# Generate a string art path from a rational decimal expansion

`stdecimal()` places digit pegs on a circle and connects consecutive
digits in the expansion of a rational number. By default, the function
uses base 10, so the pegs are labeled from 0 to 9.

## Usage

``` r
stdecimal(
  numerator = 1,
  denominator = 7,
  n = 10,
  k = 2,
  col = "blue",
  lwd = 1,
  plot = TRUE,
  show_points = TRUE,
  show_labels = TRUE,
  verbose = FALSE,
  radius = 1,
  rotate = pi/2,
  include_integer_part = TRUE,
  show_strings = TRUE,
  template = FALSE,
  point_col = "black",
  point_cex = 0.9,
  point_pch = 21,
  point_bg = "white",
  label_cex = 0.8,
  label_col = "black",
  border_col = "grey50",
  border_lwd = 1,
  bg = "white",
  main = NULL
)
```

## Arguments

- numerator:

  Integer. Numerator of the rational number.

- denominator:

  Integer. Denominator of the rational number. Must be nonzero.

- n:

  Integer. Numeral base and number of digit pegs. Default is 10.

- k:

  Integer. Number of repetitions of the repetend shown in the plot when
  the expansion is repeating. Must be at least 1.

- col:

  String color passed to
  [`graphics::segments()`](https://rdrr.io/r/graphics/segments.html).

- lwd:

  Positive number. Line width used to draw the digit path.

- plot:

  Logical. If `TRUE`, draws the figure.

- show_points:

  Logical. If `TRUE`, draws the digit pegs.

- show_labels:

  Logical. If `TRUE`, draws digit labels.

- verbose:

  Logical. If `TRUE`, prints a short audit to the console.

- radius:

  Positive number. Circle radius.

- rotate:

  Numeric. Rotation angle in radians applied to the digit circle.

- include_integer_part:

  Logical. If `TRUE`, includes the integer part of the rational number
  in the displayed digit sequence.

- show_strings:

  Logical. If `TRUE`, draws the digit connections.

- template:

  Logical. If `TRUE`, draws only the digit template, without
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

  Border color of the digit circle.

- border_lwd:

  Positive number. Border line width.

- bg:

  Plot background color.

- main:

  Optional plot title. If `NULL`, no title is displayed.

## Value

Invisibly returns a list of class `stringart_result` with:

- pegs:

  A `data.frame` with columns `index`, `x`, `y`, and `digit`.

- connections:

  A `data.frame` with columns `connection_index`, `from`, `to`,
  `x_from`, `y_from`, `x_to`, `y_to`, `length`, `digit_from`,
  `digit_to`, and `position`.

- total_length:

  Total string length.

- audit:

  A character vector with audit information.

- meta:

  A list with construction metadata.

## Details

The function computes the base-`n` expansion of
`numerator / denominator` using exact long division. When the expansion
is repeating, the repetend is displayed `k` times after the preperiod.

For the default setting `n = 10`, the function is especially useful for
exploring decimal expansions, repeating decimals, periodicity, and
patterns in rational numbers.

## Examples

``` r
stdecimal()

stdecimal(1, 7)
stdecimal(1, 13)

stdecimal(22, 7)

stdecimal(template = TRUE)

```

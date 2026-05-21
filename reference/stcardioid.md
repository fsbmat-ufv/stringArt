# Generate a circular string art pattern with a cardioid effect

`stcardioid()` generates a circular string art pattern by placing
equally spaced pegs on a circle and connecting each peg to another peg
according to a multiplicative modular rule.

## Usage

``` r
stcardioid(
  n = 120,
  k = 2,
  col = "antiquewhite",
  lwd = 0.8,
  plot = TRUE,
  show_points = FALSE,
  show_labels = FALSE,
  verbose = FALSE,
  r = 1,
  rotate = 0,
  show_strings = TRUE,
  template = FALSE,
  point_col = "darkorange2",
  point_cex = 0.8,
  point_pch = 21,
  point_bg = "white",
  label_cex = 0.7,
  label_col = "black",
  border_col = "goldenrod3",
  border_lwd = 1.2,
  bg = "white",
  main = NULL
)
```

## Arguments

- n:

  Integer. Number of pegs placed on the circle. Must be at least 3.

- k:

  Integer. Multiplication factor used in the modular connection rule.
  Must satisfy `1 <= k <= n - 1`.

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

- r:

  Positive number. Radius of the circle.

- rotate:

  Numeric. Rotation angle in radians applied to the whole figure.

- show_strings:

  Logical. If `TRUE`, draws the string connections.

- template:

  Logical. If `TRUE`, draws only the peg template, without string
  connections. This is equivalent to setting `show_strings = FALSE` and
  `show_points = TRUE`.

- point_col:

  Peg border color.

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

  Circle border color.

- border_lwd:

  Positive number. Circle border line width.

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

The pegs are placed on a circle of radius `r`, centered at the origin.
Pegs are indexed from `1` to `n` in counterclockwise order, starting at
`(r, 0)`, after applying the optional rotation angle `rotate`.

The multiplicative modular connection rule is:

`to = ((k * (from - 1)) %% n) + 1`.

For `k = 2`, this rule produces the classical circular
multiplication-table pattern commonly associated with a cardioid-like
envelope.

## Examples

``` r
stcardioid()


res <- stcardioid(plot = FALSE)
head(res$pegs)
#>   index         x          y
#> 1     1 1.0000000 0.00000000
#> 2     2 0.9986295 0.05233596
#> 3     3 0.9945219 0.10452846
#> 4     4 0.9876883 0.15643447
#> 5     5 0.9781476 0.20791169
#> 6     6 0.9659258 0.25881905
head(res$connections)
#>   connection_index from to    x_from     y_from      x_to      y_to    length
#> 1                1    1  1 1.0000000 0.00000000 1.0000000 0.0000000 0.0000000
#> 2                2    2  3 0.9986295 0.05233596 0.9945219 0.1045285 0.0523539
#> 3                3    3  5 0.9945219 0.10452846 0.9781476 0.2079117 0.1046719
#> 4                4    4  7 0.9876883 0.15643447 0.9510565 0.3090170 0.1569182
#> 5                5    5  9 0.9781476 0.20791169 0.9135455 0.4067366 0.2090569
#> 6                6    6 11 0.9659258 0.25881905 0.8660254 0.5000000 0.2610524
res$total_length
#> [1] 152.78

stcardioid(n = 80, k = 2, col = "steelblue", lwd = 0.8)

stcardioid(n = 24, k = 5, show_points = TRUE, show_labels = TRUE)

stcardioid(template = TRUE)

```

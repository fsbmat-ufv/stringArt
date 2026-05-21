# Generate an elliptical string art pattern

`stellipse()` generates a string art pattern by placing equally spaced
pegs along an ellipse and connecting each peg to another peg according
to an additive modular rule.

## Usage

``` r
stellipse(
  n = 30,
  k = 5,
  col = "blue",
  lwd = 1,
  plot = TRUE,
  show_points = TRUE,
  show_labels = FALSE,
  verbose = FALSE,
  a = 2,
  b = 1,
  rotate = 0,
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

  Integer. Number of pegs placed on the ellipse. Must be at least 3.

- k:

  Integer. Step used in the modular connection rule. Must satisfy
  `1 <= k <= n - 1`.

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

  Positive number. Semi-major horizontal axis of the ellipse.

- b:

  Positive number. Semi-minor vertical axis of the ellipse.

- rotate:

  Numeric. Rotation angle in radians applied to the whole figure.

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

  Ellipse border color.

- border_lwd:

  Positive number. Ellipse border line width.

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

The pegs are placed along the parametric ellipse `x = a * cos(theta)`
and `y = b * sin(theta)`, centered at the origin. Pegs are indexed from
`1` to `n` in counterclockwise order, starting at `(a, 0)`, after
applying the optional rotation angle `rotate`.

The additive modular connection rule is:

`to = ((from + k - 1) %% n) + 1`.

When `gcd(n, k) = 1`, this rule generates a single cycle passing through
all pegs. When `gcd(n, k) > 1`, the figure decomposes into independent
cycles.

## Examples

``` r
stellipse()


res <- stellipse(plot = FALSE)
head(res$pegs)
#>   index        x         y
#> 1     1 2.000000 0.0000000
#> 2     2 1.956295 0.2079117
#> 3     3 1.827091 0.4067366
#> 4     4 1.618034 0.5877853
#> 5     5 1.338261 0.7431448
#> 6     6 1.000000 0.8660254
head(res$connections)
#>   connection_index from to   x_from    y_from       x_to      y_to   length
#> 1                1    1  6 2.000000 0.0000000  1.0000000 0.8660254 1.322876
#> 2                2    2  7 1.956295 0.2079117  0.6180340 0.9510565 1.530754
#> 3                3    3  8 1.827091 0.4067366  0.2090569 0.9945219 1.721489
#> 4                4    4  9 1.618034 0.5877853 -0.2090569 0.9945219 1.871816
#> 5                5    5 10 1.338261 0.7431448 -0.6180340 0.9510565 1.967312
#> 6                6    6 11 1.000000 0.8660254 -1.0000000 0.8660254 2.000000
res$total_length
#> [1] 46.25893

stellipse(n = 40, k = 7, a = 2.5, b = 1.2, col = "purple", lwd = 1)

stellipse(n = 24, k = 5, show_points = TRUE, show_labels = TRUE)

stellipse(template = TRUE)

```

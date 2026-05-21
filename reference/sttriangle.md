# Generate a triangular string art pattern

`sttriangle()` generates a string art pattern on the boundary of an
equilateral triangle. Pegs are distributed uniformly along the triangle
perimeter and connected using an additive modular rule.

## Usage

``` r
sttriangle(
  n = 30,
  k = 7,
  col = "blue",
  lwd = 1,
  plot = TRUE,
  show_points = TRUE,
  show_labels = FALSE,
  verbose = FALSE,
  side = 2,
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

  Integer. Number of pegs placed along the triangle boundary. Must be at
  least 3.

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

- side:

  Positive number. Side length of the equilateral triangle.

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

  Triangle border color.

- border_lwd:

  Positive number. Triangle border line width.

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

  A list with construction metadata, including the triangle vertices.

## Details

The triangle is centered at the origin using its centroid. The pegs are
placed uniformly along the perimeter in counterclockwise order.

The additive modular connection rule is:

`to = ((from + k - 1) %% n) + 1`.

When `gcd(n, k) = 1`, the modular rule forms a single cycle through all
pegs. When `gcd(n, k) > 1`, the construction decomposes into independent
cycles.

## Examples

``` r
sttriangle()


res <- sttriangle(plot = FALSE)
head(res$pegs)
#>   index    x          y
#> 1     1 -1.0 -0.5773503
#> 2     2 -0.8 -0.5773503
#> 3     3 -0.6 -0.5773503
#> 4     4 -0.4 -0.5773503
#> 5     5 -0.2 -0.5773503
#> 6     6  0.0 -0.5773503
head(res$connections)
#>   connection_index from to x_from     y_from x_to       y_to    length
#> 1                1    1  8   -1.0 -0.5773503  0.4 -0.5773503 1.4000000
#> 2                2    2  9   -0.8 -0.5773503  0.6 -0.5773503 1.4000000
#> 3                3    3 10   -0.6 -0.5773503  0.8 -0.5773503 1.4000000
#> 4                4    4 11   -0.4 -0.5773503  1.0 -0.5773503 1.4000000
#> 5                5    5 12   -0.2 -0.5773503  0.9 -0.4041452 1.1135529
#> 6                6    6 13    0.0 -0.5773503  0.8 -0.2309401 0.8717798
res$total_length
#> [1] 33.03866

sttriangle(n = 30, k = 7, side = 2, col = "blue", lwd = 1)
sttriangle(n = 18, k = 5, show_points = TRUE, show_labels = TRUE)

sttriangle(template = TRUE)

```

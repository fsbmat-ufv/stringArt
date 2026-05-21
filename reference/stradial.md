# Generate a radial string art pattern with triangular modules

`stradial()` generates a radial string art pattern composed of
triangular modules rotated around the origin. In each module, pegs are
placed along the triangular boundary and connected according to an
additive modular rule.

## Usage

``` r
stradial(
  n = 18,
  k = 5,
  col = "blue",
  lwd = 1,
  plot = TRUE,
  show_points = TRUE,
  show_labels = FALSE,
  verbose = FALSE,
  m = 6,
  r = 1.2,
  spread = pi/5,
  rotate = 0,
  show_strings = TRUE,
  template = FALSE,
  point_col = "black",
  point_cex = 0.8,
  point_pch = 21,
  point_bg = "white",
  label_cex = 0.7,
  label_col = "black",
  border_col = "grey50",
  border_lwd = 1,
  bg = "white",
  main = NULL,
  show_center = TRUE,
  center_col = "black",
  center_cex = 0.9
)
```

## Arguments

- n:

  Integer. Number of pegs in each triangular module. Must be at least 3.

- k:

  Integer. Additive modular step used inside each module. Must satisfy
  `1 <= k <= n - 1`.

- col:

  String color, or a vector of colors with length 1 or `m`, used to draw
  the string connections.

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

- m:

  Integer. Number of triangular modules.

- r:

  Positive number. Distance from the origin to the two outer vertices of
  each triangular module.

- spread:

  Positive number. Angular opening, in radians, of each module.

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

  Module border color.

- border_lwd:

  Positive number. Module border line width.

- bg:

  Plot background color.

- main:

  Optional plot title. If `NULL`, no title is displayed.

- show_center:

  Logical. If `TRUE`, highlights the origin.

- center_col:

  Color used to highlight the origin.

- center_cex:

  Positive number. Size of the highlighted origin.

## Value

Invisibly returns a list of class `stringart_result` with:

- pegs:

  A `data.frame` with columns `index`, `x`, `y`, `module`, and
  `local_index`.

- connections:

  A `data.frame` with columns `connection_index`, `from`, `to`,
  `x_from`, `y_from`, `x_to`, `y_to`, `length`, `module`, `local_from`,
  `local_to`, and `color`.

- total_length:

  Total string length.

- audit:

  A character vector with audit information.

- meta:

  A list with construction metadata.

## Details

Each module is a triangle with vertices at the origin and at two outer
points determined by `r` and `spread`. The base module is rotated `m`
times around the origin.

Within each module, the local connection rule is:

`to = ((from + k - 1) %% n) + 1`.

This means that each local peg is connected to the peg `k` positions
ahead, using modular indexing. The same local rule is applied
independently to all modules.

## Examples

``` r
stradial()


res <- stradial(plot = FALSE)
head(res$pegs)
#>   index         x           y module local_index
#> 1     1 0.0000000  0.00000000      1           1
#> 2     2 0.1659932 -0.05393447      1           2
#> 3     3 0.3319864 -0.10786893      1           3
#> 4     4 0.4979797 -0.16180340      1           4
#> 5     5 0.6639729 -0.21573787      1           5
#> 6     6 0.8299661 -0.26967233      1           6
head(res$connections)
#>   connection_index from to    x_from      y_from      x_to          y_to
#> 1                1    1  6 0.0000000  0.00000000 0.8299661 -2.696723e-01
#> 2                2    2  7 0.1659932 -0.05393447 0.9959593 -3.236068e-01
#> 3                3    3  8 0.3319864 -0.10786893 1.1412678 -3.490712e-01
#> 4                4    4  9 0.4979797 -0.16180340 1.1412678 -1.745356e-01
#> 5                5    5 10 0.6639729 -0.21573787 1.1412678 -1.387779e-16
#> 6                6    6 11 0.8299661 -0.26967233 1.1412678  1.745356e-01
#>      length module local_from local_to color
#> 1 0.8726780      1          1        6  blue
#> 2 0.8726780      1          2        7  blue
#> 3 0.8444613      1          3        8  blue
#> 4 0.6434142      1          4        9  blue
#> 5 0.5237874      1          5       10  blue
#> 6 0.5424292      1          6       11  blue
res$total_length
#> [1] 70.4474

stradial(n = 18, k = 5, m = 6, col = "steelblue", lwd = 0.8)

stradial(n = 12, k = 4, m = 5, show_points = TRUE, show_labels = TRUE)

stradial(template = TRUE)

```

# Generate a lotus-like string art pattern

`stlotus()` generates a stylized lotus-like string art figure by
combining one outer circular module, one central circular module, and
several petal modules arranged around the center.

## Usage

``` r
stlotus(
  n = 40,
  k = 11,
  col = "deeppink4",
  lwd = 0.8,
  plot = TRUE,
  show_points = FALSE,
  show_labels = FALSE,
  verbose = FALSE,
  petals = 5,
  outer_radius = 1,
  petal_radius = 0.34,
  petal_center_radius = 0.34,
  inner_radius = 0.18,
  rotate = 0,
  show_strings = TRUE,
  template = FALSE,
  point_col = "black",
  point_cex = 0.6,
  point_pch = 19,
  point_bg = "white",
  label_cex = 0.6,
  label_col = "black",
  border_col = "grey50",
  border_lwd = 1,
  bg = "white",
  main = NULL
)
```

## Arguments

- n:

  Integer. Number of pegs in each circular module. Must be at least 3.

- k:

  Integer. Additive modular step used in each module. Must satisfy
  `1 <= k <= n - 1`.

- col:

  String color passed to
  [`graphics::segments()`](https://rdrr.io/r/graphics/segments.html). It
  may have length 1 or `petals + 2`. If length is 1, the same color is
  used for all modules.

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

  Integer. Number of petal modules around the center.

- outer_radius:

  Positive number. Radius of the outer circular module.

- petal_radius:

  Positive number. Radius of each petal module.

- petal_center_radius:

  Positive number. Distance from the origin to each petal center.

- inner_radius:

  Positive number. Radius of the central circular module.

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

  Border color used for the module outlines.

- border_lwd:

  Positive number. Border line width.

- bg:

  Plot background color.

- main:

  Optional plot title. If `NULL`, no title is displayed.

## Value

Invisibly returns a list of class `stringart_result` with:

- pegs:

  A `data.frame` with peg coordinates and metadata.

- connections:

  A `data.frame` with columns `connection_index`, `from`, `to`,
  `x_from`, `y_from`, `x_to`, `y_to`, `length`, `module`, `local_from`,
  and `local_to`.

- total_length:

  Total string length.

- audit:

  A character vector with audit information.

- meta:

  A list with construction metadata.

## Details

The figure is built from `petals + 2` circular modules:

- one outer circular module;

- `petals` petal modules;

- one central circular module.

Each module contains `n` equally spaced pegs and uses the same additive
modular rule:

`to_local = ((from_local + k - 1) %% n) + 1`.

The final figure is obtained by superimposing all module connections.

## Examples

``` r
stlotus()


res <- stlotus(plot = FALSE)
head(res$pegs)
#>   index         x         y module layer local_index
#> 1     1 1.0000000 0.0000000  outer     0           1
#> 2     2 0.9876883 0.1564345  outer     0           2
#> 3     3 0.9510565 0.3090170  outer     0           3
#> 4     4 0.8910065 0.4539905  outer     0           4
#> 5     5 0.8090170 0.5877853  outer     0           5
#> 6     6 0.7071068 0.7071068  outer     0           6
head(res$connections)
#>   connection_index from to    x_from    y_from       x_to      y_to   length
#> 1                1    1 12 1.0000000 0.0000000 -0.1564345 0.9876883 1.520812
#> 2                2    2 13 0.9876883 0.1564345 -0.3090170 0.9510565 1.520812
#> 3                3    3 14 0.9510565 0.3090170 -0.4539905 0.8910065 1.520812
#> 4                4    4 15 0.8910065 0.4539905 -0.5877853 0.8090170 1.520812
#> 5                5    5 16 0.8090170 0.5877853 -0.7071068 0.7071068 1.520812
#> 6                6    6 17 0.7071068 0.7071068 -0.8090170 0.5877853 1.520812
#>   module local_from local_to
#> 1  outer          1       12
#> 2  outer          2       13
#> 3  outer          3       14
#> 4  outer          4       15
#> 5  outer          5       16
#> 6  outer          6       17
res$total_length
#> [1] 175.1975

stlotus(n = 50, k = 13, col = "deeppink4", lwd = 0.8)

stlotus(show_points = TRUE, show_labels = TRUE)

stlotus(template = TRUE)

```

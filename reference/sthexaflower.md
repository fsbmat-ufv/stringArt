# Generate a hexagonal flower string art pattern

`sthexaflower()` generates a string art pattern based on three
concentric hexagonal peg circuits and one central peg. The construction
is fully reproducible and returns both the peg coordinates and the
connection table.

## Usage

``` r
sthexaflower(
  n = 24,
  k = 5,
  col = c("black", "forestgreen", "darkorange", "deepskyblue4", "firebrick", "purple"),
  lwd = 1,
  plot = TRUE,
  show_points = TRUE,
  show_labels = FALSE,
  verbose = FALSE,
  r = 1,
  scale_mid = 0.72,
  scale_inner = 0.42,
  offset_mid = 0,
  offset_inner = 0,
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

  Integer. Number of pegs in each hexagonal circuit. Must be a multiple
  of 6 and at least 6.

- k:

  Integer. Step used in the local modular connection rule. Must satisfy
  `1 <= k <= n - 1`.

- col:

  String color passed to
  [`graphics::segments()`](https://rdrr.io/r/graphics/segments.html). It
  may have length 1 or 6. If length 6, colors are used by sector.

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

  Positive number. Radius of the outer hexagonal circuit.

- scale_mid:

  Positive number in `(0, 1)`. Scale of the middle hexagonal circuit
  relative to the outer circuit.

- scale_inner:

  Positive number in `(0, scale_mid)`. Scale of the inner hexagonal
  circuit relative to the outer circuit.

- offset_mid:

  Numeric value in `[0, 1)`. Discrete relative offset applied to the
  middle circuit along the peg sequence.

- offset_inner:

  Numeric value in `[0, 1)`. Discrete relative offset applied to the
  inner circuit along the peg sequence.

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

  Hexagonal border color.

- border_lwd:

  Positive number. Hexagonal border line width.

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
  `x_from`, `y_from`, `x_to`, `y_to`, `length`, `block`, and `sector`.

- total_length:

  Total string length.

- audit:

  A character vector with audit information.

- meta:

  A list with construction metadata.

## Details

The function builds three concentric hexagonal circuits with `n` pegs
each and one central peg.

The peg table contains the columns `index`, `x`, `y`, `group`, `layer`,
and `local_index`.

The construction uses four connection blocks:

- `outer_border`: consecutive connections on the outer hexagon.

- `outer_to_middle`: connections from the outer circuit to the middle
  circuit.

- `middle_to_inner`: connections from the middle circuit to the inner
  circuit.

- `vertices_to_center`: connections from the outer vertices to the
  central peg.

The local additive modular rule used in the two radial blocks is

`to_local = ((from_local + k - 1) %% n) + 1`.

## Examples

``` r
sthexaflower()


res <- sthexaflower(plot = FALSE)
head(res$pegs)
#>   index             x     y group layer local_index
#> 1     1  6.123234e-17 1.000 outer     1           1
#> 2     2 -2.165064e-01 0.875 outer     1           2
#> 3     3 -4.330127e-01 0.750 outer     1           3
#> 4     4 -6.495191e-01 0.625 outer     1           4
#> 5     5 -8.660254e-01 0.500 outer     1           5
#> 6     6 -8.660254e-01 0.250 outer     1           6
head(res$connections)
#>   connection_index from to        x_from y_from       x_to         y_to length
#> 1                1    1  2  6.123234e-17  1.000 -0.2165064 8.750000e-01   0.25
#> 2                2    2  3 -2.165064e-01  0.875 -0.4330127 7.500000e-01   0.25
#> 3                3    3  4 -4.330127e-01  0.750 -0.6495191 6.250000e-01   0.25
#> 4                4    4  5 -6.495191e-01  0.625 -0.8660254 5.000000e-01   0.25
#> 5                5    5  6 -8.660254e-01  0.500 -0.8660254 2.500000e-01   0.25
#> 6                6    6  7 -8.660254e-01  0.250 -0.8660254 3.053113e-16   0.25
#>          block sector
#> 1 outer_border      1
#> 2 outer_border      1
#> 3 outer_border      1
#> 4 outer_border      1
#> 5 outer_border      2
#> 6 outer_border      2
res$total_length
#> [1] 51.73785

sthexaflower(n = 30, k = 7, col = "steelblue", lwd = 0.8)

sthexaflower(n = 24, k = 5, show_points = TRUE, show_labels = TRUE)

sthexaflower(template = TRUE)

```

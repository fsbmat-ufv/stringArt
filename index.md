# stringArt

The **stringArt** package provides tools to generate, visualize, and
audit geometric string art figures for mathematics teaching.

String art figures are built from pegs and straight string connections,
creating visual patterns related to geometry, modular arithmetic,
symmetry, and visual mathematics.

The package is designed for educational use and can support classroom
activities in geometry, analytic geometry, modular arithmetic,
mathematical visualization, and graphical programming in R.

## Installation

You can install the development version of **stringArt** from GitHub
with:

``` r

# install.packages("devtools")
devtools::install_github("fsbmat-ufv/stringArt")
```

## Main idea

Each exported function generates a string art figure and returns a
standardized object containing:

- `pegs`: peg coordinates;
- `connections`: connection table;
- `total_length`: total string length;
- `audit`: audit information;
- `meta`: construction metadata.

``` r

library(stringArt)

res <- stcircle(plot = FALSE)

names(res)
head(res$pegs)
head(res$connections)
res$total_length
res$audit
res$meta
```

## Examples

### Circular string art

``` r

stcircle(
  n = 40,
  k = 7,
  col = "steelblue",
  lwd = 1
)
```

### Cardioid-like pattern

``` r

stcardioid(
  n = 120,
  k = 2,
  col = "darkorange",
  lwd = 0.8
)
```

### Elliptical string art

``` r

stellipse(
  n = 60,
  k = 9,
  a = 2,
  b = 1,
  col = "purple",
  lwd = 1
)
```

### Triangular string art

``` r

sttriangle(
  n = 45,
  k = 8,
  side = 2,
  col = "blue",
  lwd = 1
)
```

### Hexagonal flower

``` r

sthexaflower(
  n = 24,
  k = 5,
  show_points = TRUE
)
```

### Radial triangular modules

``` r

stradial(
  n = 18,
  k = 5,
  m = 6,
  col = "forestgreen",
  lwd = 1
)
```

### Region-based string art

``` r

stregion(
  n = 100,
  k = 4,
  col = "firebrick",
  lwd = 0.6
)
```

Custom contour:

``` r

custom_contour <- data.frame(
  x = c(0, 1, 0.6, -0.6, -1),
  y = c(1, 0.2, -0.9, -0.9, 0.2)
)

stregion(
  contour = custom_contour,
  n = 80,
  k = 3,
  col = "steelblue",
  lwd = 0.7
)
```

## Peg template without strings

All main functions support template mode. This is useful for generating
peg templates without drawing the string connections.

``` r

stcircle(template = TRUE)
sttriangle(template = TRUE)
stregion(template = TRUE)
```

## Showing pegs and labels

``` r

stcircle(
  n = 20,
  k = 3,
  show_points = TRUE,
  show_labels = TRUE
)
```

## Audit information

Audit information is returned as part of the object and can also be
printed to the console with `verbose = TRUE`.

``` r

res <- stcircle(n = 20, k = 3, plot = FALSE)

res$audit

stcircle(n = 20, k = 3, verbose = TRUE)
```

## Available functions

The current version includes:

- [`stcircle()`](https://fsbmat-ufv.github.io/stringArt/reference/stcircle.md):
  circular string art with additive modular step;
- [`stcardioid()`](https://fsbmat-ufv.github.io/stringArt/reference/stcardioid.md):
  circular multiplicative modular pattern;
- [`stellipse()`](https://fsbmat-ufv.github.io/stringArt/reference/stellipse.md):
  elliptical string art;
- [`sttriangle()`](https://fsbmat-ufv.github.io/stringArt/reference/sttriangle.md):
  triangular string art;
- [`sthexaflower()`](https://fsbmat-ufv.github.io/stringArt/reference/sthexaflower.md):
  concentric hexagonal circuits;
- [`stradial()`](https://fsbmat-ufv.github.io/stringArt/reference/stradial.md):
  rotated triangular modules;
- [`stregion()`](https://fsbmat-ufv.github.io/stringArt/reference/stregion.md):
  contour-based region filling.

## Development

To document, test, and check the package, use:

``` r

devtools::document()
devtools::test()
devtools::check()
```

To build the GitHub README, use:

``` r

devtools::build_readme()
```

## License

This package is licensed under the MIT License.

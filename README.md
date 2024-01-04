
<!-- README.md is generated from README.Rmd. Please edit that file -->

# cbbplotR: Your Gateway to CBB Visualization in R

Welcome to **`cbbplotR`**, an R package designed to bring the vibrant
world of college basketball into your data visualizations. This package,
an extension of **`nflplotR`**, is your go-to toolkit for incorporating
college basketball team and conference logos, player headshots, and team
colors into **`ggplot2`** plots and `gt` tables. While `cbbplotR` is
best paired with the [`cbbdata`
package](https://cbbdata.aweatherman.com), there is an extensive
internal name conversion function for teams and conferences that will
match some common sites. If you want to request adding specific name
conversions, [please open an issue on the package’s
GitHub.](https://github.com/andreweatherman/cbbplotR)

`cbbplotR` is an extension of the popular `{sport}plotR` family of
packages, including `nflplotR`, `nbaplotR`, `mlbplotR`, and `cfbplotR`.
This package was heavily adapted from `nflplotR` and `ggpath` from
developer Sebastian Carl.

## Installation

To get started with **`cbbplotR`**, you can install it using the
**`pak`** package.

``` r
if (!require("pak")) install.packages("pak")
pak::pak("andreweatherman/cbbplotR")
```

## Usage

[For information on usage and capabilities, please visit the ‘Getting
Started’
vignette.](https://cbbplotr.aweatherman.com/articles/getting_started.html)

## Support

If you have feature recommendations or run into bugs, [please open an
issue on GitHub](https://github.com/andreweatherman/cbbplotR). You can
also [contact me directly on
Twitter](https://twitter.com/andreweatherman), but I would prefer the
latter.

If you are looking for more college basketball R packages, [check out
`cbbdata`](https://cbbdata.aweatherman.com) – also developed and
maintained by me.

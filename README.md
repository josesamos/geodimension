
<!-- README.md is generated from README.Rmd. Please edit that file -->

# geodimension <a href="https://josesamos.github.io/geodimension/"><img src="man/figures/logo.png" align="right" height="139" alt="geodimension website" /></a>

<!-- badges: start -->

[![CRAN
status](https://www.r-pkg.org/badges/version/geodimension)](https://CRAN.R-project.org/package=geodimension)
[![R-CMD-check](https://github.com/josesamos/geodimension/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/josesamos/geodimension/actions/workflows/R-CMD-check.yaml)
[![Codecov test
coverage](https://codecov.io/gh/josesamos/geodimension/branch/master/graph/badge.svg)](https://app.codecov.io/gh/josesamos/geodimension?branch=master)
[![Downloads](http://cranlogs.r-pkg.org/badges/geodimension?color=brightgreen)](https://www.r-pkg.org:443/pkg/geodimension)
[![Downloads](http://cranlogs.r-pkg.org/badges/grand-total/geodimension?color=brightgreen)](https://www.r-pkg.org:443/pkg/geodimension)
<!-- badges: end -->

The *geographic dimension* plays a fundamental role in multidimensional
systems. To define a geographic dimension in a multidimensional star
schema, we need a table with attributes corresponding to the levels of
the dimension. Additionally, we will also need one or more geographic
layers to represent the data using this dimension.

We can obtain this data from available vector layers of geographic
information. In simple cases, one layer is enough. We often need several
layers related to each other. The relationships can be defined by common
attribute values or can be inferred from the respective geographic
information.

The goal of `geodimension` is to support the definition of geographic
dimensions from layers of geographic information that can be used in
multidimensional systems. In particular, through packages
[`rolap`](https://cran.r-project.org/package=rolap) and
[`geomultistar`](https://cran.r-project.org/package=geomultistar).

## Installation

You can install the released version of `geodimension` from
[CRAN](https://CRAN.R-project.org) with:

``` r
install.packages("geodimension")
```

And the development version from [GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("josesamos/geodimension")
```

## Example

This is a basic example which shows you how to generate a `geodimension`
from several vector layers of geographic information. It also shows how
to use it.

Suppose that, for the US, we want to define a geographic dimension at
the *state* level but also include the information at the predefined
higher organization levels: *region*, *division* and also *nation*. We
have obtained a geographic layer for *state* level (`layer_us_state`),
from it we can define all the levels. From each layer, we define a
`geolevel`.

Suppose that, for the US, we want to define a geographic dimension at
the *state* level but also include the information at the predefined
higher organization levels: *region*, *division* and also *nation*. We
have obtained geographic layers for each of these levels:
`layer_us_state`, `layer_us_region` and `layer_us_division`. From each
layer, we define a `geolevel`.

``` r
library(geodimension)
```


<!-- README.md is generated from README.Rmd. Please edit that file -->

# geodimension

<!-- badges: start -->

[![Travis build
status](https://travis-ci.com/josesamos/geodimension.svg?branch=master)](https://travis-ci.com/josesamos/geodimension)
<!-- badges: end -->

The *geographic dimension* plays a fundamental role in multidimensional
systems. To define a geographic dimension in a star schema, we need a
table with attributes corresponding to the levels of the dimension.
Additionally, we will also need one or more geographic layers to
represent the data using this dimension.

We can obtain this data from available vector layers of geographic
information. In simple cases, one layer is enough; but we often need
several layers related to each other. The relationships can be defined
by common attribute values or can be inferred from the respective
geographic information.

The goal of `geodimension` is to support the definition of geographic
dimensions from layers of geographic information that can be used in
multidimensional systems. In particular, through package
[`geomultistar`](https://cran.r-project.org/package=geomultistar) they
can be used directly.

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
have obtained geographic layers for each of these levels:
`layer_us_state`, `layer_us_region`, `layer_us_division` and
`layer_us_nation`. From each layer, we define a `geolevel`.

``` r
library(tidyr)
library(sf)
library(geodimension)

state <-
  geolevel(name = "state",
           layer = layer_us_state,
           key = c("geoid")) %>%
  complete_point_geometry()

region <-
  geolevel(name = "region",
           layer = layer_us_region,
           key = c("geoid"))

division <-
  geolevel(name = "division",
           layer = layer_us_division,
           key = c("geoid"))

nation <-
  geolevel(name = "nation",
           layer = layer_us_nation,
           key = c("geoid"))
```

We define a `geodimension` that includes all the levels in which we are
interested.

``` r
gd <-
  geodimension(name = "gd_us",
               level = region) %>%
  add_level(division) %>%
  add_level(state) %>%
  add_level(nation)
```

Next, we define the relationships that exist between the levels: some
based on common attributes, others on geographic relationships between
their instances.

``` r
gd <- gd %>%
  relate_levels(lower_level_name = "state",
                lower_level_attributes = c("division"),
                upper_level_name = "division") %>%
  relate_levels(lower_level_name = "division",
                upper_level_name = "region",
                by_geography = TRUE) %>%
  relate_levels(lower_level_name = "region",
                upper_level_name = "nation",
                by_geography = TRUE)
```

There are no restrictions on the relationships you define, as long as
the relationship can be established.

With these operations we have defined a `geodimension`. From it we can
obtain the data table to define a dimension in a star schema or the
layer or layers associated with that table at the level we need.

``` r
ld <- gd %>%
  get_level_data(level_name = "division")
names(ld)
#> [1] "division_key" "geoid"        "divisionce"   "affgeoid"     "name"        
#> [6] "lsad"         "aland"        "awater"

ld <- gd %>%
  get_level_data(level_name = "division",
                 inherited = TRUE)
names(ld)
#>  [1] "division_key"      "geoid"             "divisionce"       
#>  [4] "affgeoid"          "name"              "lsad"             
#>  [7] "aland"             "awater"            "NATION_nation_key"
#> [10] "NATION_geoid"      "NATION_affgeoid"   "NATION_name"      
#> [13] "REGION_region_key" "REGION_geoid"      "REGION_regionce"  
#> [16] "REGION_affgeoid"   "REGION_name"       "REGION_lsad"      
#> [19] "REGION_aland"      "REGION_awater"

ll <- gd %>%
  get_level_layer(level_name = "division",
                 inherited = TRUE)
names(ll)
#>  [1] "geoid"             "divisionce"        "affgeoid"         
#>  [4] "name"              "lsad"              "aland"            
#>  [7] "awater"            "NATION_nation_key" "NATION_geoid"     
#> [10] "NATION_affgeoid"   "NATION_name"       "REGION_region_key"
#> [13] "REGION_geoid"      "REGION_regionce"   "REGION_affgeoid"  
#> [16] "REGION_name"       "REGION_lsad"       "REGION_aland"     
#> [19] "REGION_awater"     "geom"
```

If we need the data at another level of detail, we can obtain it in a
similar way.

``` r
ld <- gd %>%
  get_level_data(level_name = "state",
                 inherited = TRUE)
names(ld)
#>  [1] "state_key"             "geoid"                 "region"               
#>  [4] "division"              "statefp"               "statens"              
#>  [7] "stusps"                "name"                  "lsad"                 
#> [10] "mtfcc"                 "funcstat"              "aland"                
#> [13] "awater"                "intptlat"              "intptlon"             
#> [16] "shape_length"          "shape_area"            "geoid_data"           
#> [19] "DIVISION_division_key" "DIVISION_geoid"        "DIVISION_divisionce"  
#> [22] "DIVISION_affgeoid"     "DIVISION_name"         "DIVISION_lsad"        
#> [25] "DIVISION_aland"        "DIVISION_awater"       "NATION_nation_key"    
#> [28] "NATION_geoid"          "NATION_affgeoid"       "NATION_name"          
#> [31] "REGION_region_key"     "REGION_geoid"          "REGION_regionce"      
#> [34] "REGION_affgeoid"       "REGION_name"           "REGION_lsad"          
#> [37] "REGION_aland"          "REGION_awater"

ll <- gd %>%
  get_level_layer(level_name = "state",
                  only_key = TRUE)

plot(ll)
```

<img src="man/figures/README-unnamed-chunk-6-1.png" width="100%" />

In addition to these functions, the package offers other support
functions to aid in the definition of levels (for example, to determine
the key attributes of a layer), to relate instances of levels whose
relationship is not immediately established, or to configure the
`geodimension` to obtain a customized output.

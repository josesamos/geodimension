# geodimension 2.0.0
* New datasets: level *place* instead of *city* in `gd_us` and  `gd_es`, *geodimension* for Spain.
* Definition of geographic information layers as *GeoPackage* in external data.
* New functions: `set_level_data()` and `get_level_data_geo()`.
* Add parameter `snake_case` to `geolevel()` and `geodimension()` functions.
* Delete parameter `use_intermediate_projected_crs` from function `complete_point_geometry()`.
* Delete parameter `surrogate_key` from function `get_level_layer()`.
* Include detailed error messages.
* New tests.

# geodimension 1.0.2
* Fix test problem.
* Use core pipe operator in tests.
* Use the `st_shift_longitude` function to plot the map of the US with Alaska.

# geodimension 1.0.2
* Fix test problem.
* Use core pipe operator in tests.
* Use the `st_shift_longitude` function to plot the map of the US with Alaska.

# geodimension 1.0.1
* Update documentation and website.
* Use core pipe operator.

# geodimension 1.0.0
* Initial functionality.
* Functions to define levels in a dimension with geographic information.
* Functions to define relationships between levels.
* Functions for obtaining tables and layers.


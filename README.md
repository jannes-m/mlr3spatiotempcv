
# mlr3spatiotemporal

Spatiotemporal extensions for mlr3

[![Travis build
status](https://travis-ci.org/mlr-org/mlr3spatiotemporal.svg?branch=master)](https://travis-ci.org/mlr-org/mlr3spatiotemporal)
[![CRAN
status](https://www.r-pkg.org/badges/version/mlr3spatiotemporal)](https://cran.r-project.org/package=mlr3spatiotemporal)
[![lifecycle](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://www.tidyverse.org/lifecycle/#experimental)
[![Coverage
status](https://codecov.io/gh/mlr-org/mlr3spatiotemporal/branch/master/graph/badge.svg)](https://codecov.io/github/mlr-org/mlr3spatiotemporal?branch=master)

This package extends the [mlr3](https://github.com/mlr-org/mlr3) package
framework by spatiotemporal resampling and visualization methods.

## Resampling methods

Currently, the following ones are implemented:

  - Spatial CV using k-means clustering after Brenning2012
    (ResampleCVKmeans) (package
    [sperrorest](https://github.com/pat-s/sperrorest)))

The following ones are planned:

  - “Spatial Blocking” (package
    [blockCV](https://github.com/rvalavi/blockCV))
  - “Environmental Blocking” (package
    [blockCV](https://github.com/rvalavi/blockCV))
  - “Buffering” (package [blockCV](https://github.com/rvalavi/blockCV))

## Spatial tasks

  - Task “ecuador” -\> `mlr_tasks$get("ecuador")`

## Visualization methods

# References

<div id="refs" class="references">

<div id="ref-Brenning2012">

———. 2012. “Spatial Cross-Validation and Bootstrap for the Assessment of
Prediction Rules in Remote Sensing: The R Package Sperrorest.” In *2012
IEEE International Geoscience and Remote Sensing Symposium*, 5372–5.
<https://doi.org/10.1109/IGARSS.2012.6352393>.

</div>

</div>

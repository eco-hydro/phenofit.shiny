
# phenofit.shiny

> **_This repository is not maintained any more. Please do not use package any more._**   
> You can find application cases in <https://github.com/eco-hydro/phenofit-scripts>.

<!-- badges: start -->
<!-- badges: end -->

phenofit.shiny is the shiny app of `phenofit` package. 
...

## Installation

``` r
install.packages("phenofit")
devtools::install_github("kongdd/phenofit.shiny")
```

## Example

``` r
library(phenofit.shiny)
## basic example code
shiny::runApp(system.file("phenofit", package = "phenofit"))
```

![title](man/figures/phenofit_shiny.png)   


# **References** 
> [1\] Dongdong Kong, R package: A state-of-the-art Vegetation Phenology extraction package, `phenofit` version 0.2.2, <https://github.com/kongdd/phenofit>

# Acknowledgements

Keep in mind that this repository is released under a GPL3 license, which permits commercial use but requires that the source code (of derivatives) is always open even if hosted as a web service.

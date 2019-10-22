# source("test/load_pkgs.R")
# source("../../R/fix_phenofit.R")
suppressMessages({
    library(data.table)
    library(magrittr)
    library(lubridate)
    library(purrr)
    library(plyr)
    library(glue)
    library(phenofit)
    library(stringr)
    library(Ipaper) # devtools::install_github("kongdd/Ipaper")
    library(sp2)    # devtools::install_github("kongdd/sp2")
    library(ncdf4)
    library(sp)
    library(raster)
    library(foreach)
    library(iterators)
})

PhenoMetrics = c("TRS2.sos", "TRS2.eos", "TRS5.sos", "TRS5.eos", "TRS6.sos", "TRS6.eos",
             "DER.sos", "DER.pop", "DER.eos",
              "UD", "SD", "DD", "RD", "Greenup", "Maturity", "Senescence", "Dormancy")

raster2SpatialPixel <- function(r) {
    as(r, "SpatialGridDataFrame")
}

#' resample_spatialPixel
#' 
#' @param df nrow(df) should be equal to nrow(r)
#' @param r raster
#' @param I_grid Index of selected pixel of raster (from raster::raster)
#' 
#' @importFrom raster values resample
#' @export
resample_spatialPixel <- function(df, r, r.new, I_grid, I_grid.new = NULL, verbose = TRUE) {
    if (is.null(I_grid.new)) I_grid.new <- 1:nrow(r.new)
    nvar = ncol(df)

    mat <- foreach(j = 1:nvar, .combine = cbind) %do% {
        if (verbose) runningId(j)
            
        values(r)    <- NA
        values(r)[I_grid] <- df[[j]] # begin from , 4
        rnew         <- resample(r, r.new)

        values(rnew)[I_grid.new]
    }
    mat
}

fill_raster <- function(r, I_grid, value){
    values(r)    <- NA
    values(r)[I_grid] <- value
    r
}

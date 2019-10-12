#' @importFrom sp2 prj84
#' @importFrom raster raster values
#' @export
get_grid2 <- function(range, cellsize){
   r <- raster(xmn = range[1], xmx = range[2], ymn = range[3], ymx = range[4], 
    resolution = cellsize, crs = sp2::prj84)
   values(r) <- 1:prod(dim(r))
   r
}

#' @export
write_tiff <- function(r, file) {
    rgdal::writeGDAL(r, file)
}


mask_notin <- function(r, I_grid){
    vals <- values(r)
    vals_new <- vals * NA
    vals_new[I_grid] <- vals[I_grid]
    values(r) <- vals_new
    r
}

#' extractId
#' 
#' @return 
#' - I_rect: Id of rectangle
#' 
#' @export
extractId <- function(r, sp, na.omit = TRUE, naa) {
    vals_old  = values(r)
    values(r) = 1:prod(dim(r))
  
    I_sp <- cellFromXY(r, sp)
    # I    <- extract(r, sp)
    # vals <- vals_old[I]
    vals = r[I_sp]
    ans = data.table(I_rect = I_sp, value = vals) 
    if (na.omit) ans =na.omit(ans)       
    # sp_sel = as(r[I], "SpatialPointsDataFrame") %>% plot()
    # plot(sp, add = TRUE)
    ans
}

# new I_grid of flipud 
fix_Igrid <- function(mat, I_grid){
    nrow = nrow(mat)
    ncol = ncol(mat)
    mat = matrix(1:(nrow*ncol), nrow, ncol) %>% flipud()
    I_fix = mat[I_grid]
    I_fix
}


source("test/main_pkgs.R")
# source("../phenofit/test/load_pkgs.R")
# load("INPUT/phenology_TP_AVHRR_multi-annual.rda")
#
#
# E:/Research/phenology/phenofit/inst/phenofit.shiny/inst/notes/ex-SPOT.Rmd

file_SPOT_112 <- "E:/SciData/pheno_SPOT_TP (1998-2013)/phenofit_SPOT_TP_112deg.RDS"
file_SPOT_010 <- "E:/SciData/pheno_SPOT_TP (1998-2013)/phenofit_SPOT_TP_010deg.RDS"

# colnames(df_pheno_avg)[1] <- "row"
tidy_preseason <- function(l){
    I <- map_lgl(l, ~!is.null(.x)) %>% which()
    mat_preseason <- purrr::transpose(l[I])
    mat_preseason[1:2] %<>% map(~do.call(rbind, .))
    mat_preseason$I <- I
    mat_preseason
}

s3_preseason = TRUE

# if (s3_preseason) {
load("data/00basement_TP.rda")
load(file_pheno_010)
# load(file_preseason)
# mete data are sampled according to I_grid_10
I_rem  <- match(I_grid2_10, I_grid_10)
I_time <- 1:(13879 - 365) # 2018, left 1981-2017

years = 1998:2013

## 1. resample to 0.1 deg ------------------------------------------------------
# grid_avhrr
load("N:/Research/phenology/phenology_TP/data/grid_avhrr.rda")
r2 = raster(grid_avhrr)

indir <- "E:/SciData/pheno_SPOT_TP (1998-2013)/csv"
files <- dir(indir, "*.csv", full.names = TRUE)
lst <- llply(files, function(file){
  d <- fread(file)
  d[, lapply(.SD, mean, na.rm = TRUE), .(I_grid, flag, origin), .SDcols = colnames(d)[-(1:4)]]
}, .progress = "text")

df <- do.call(rbind, lst)
grps = paste0(1998:2013, "_1")

lst_SPOT = foreach(grp = grps, i = icount()) %do% {
  runningId(i, prefix = grp)
  d <- df[flag == grp, ]
  I <- I_grid[d$I_grid]
  
  nvar = ncol(d) - 3
  resample_spatialPixel(df[, -(1:2)], r, r.new, I)
}

saveRDS(lst_SPOT, file = "phenofit_SPOT_TP_112deg.RDS")


## 2. tidy SPOT-----------------------------------------------------------------
# load("E:/Research/phenology/phenofit/inst/phenofit.shiny/lst_spot.rda")
lst_SPOT <- read_rds(file_SPOT_112) %>% set_names(years)

range    = c(73, 105, 25, 40)
cellsize = 1/10
r = get_grid2(range, cellsize) #%>% mask_notin(I_grid)

l_SPOT <- map(lst_SPOT, function(x){
    d <- x[I_grid2_10, ] %>% set_colnames(varnames) %>% as.data.table()
    d <- d[, .(SOS = TRS2.sos, EOS = TRS6.eos)]
    d[EOS > 365, EOS := NA]
    d[SOS < 0  , SOS := NA]
    d
})

df <- melt_list(l_SPOT, "year") %>% mutate(year = as.numeric(year))

d <- l_SPOT$`1998`
gridclip2_10@data <- data.frame(d)
spplot(gridclip2_10)

d <- df[, lapply(.SD, mean, na.rm = TRUE), .(year)]
ggplot(d, aes(year, EOS)) +
    geom_point() +
    geom_line()

saveRDS(l_SPOT, file = file_SPOT_010)

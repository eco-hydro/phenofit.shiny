source("test/load_pkgs.R")
devtools::load_all()

# 只到2015即可
date  = map(2000:2018, get_date_MODIS) %>% do.call(c, .)
ntime = length(date)

# NC file
load("INPUT/TP_MOD13C1_020deg_pixel (200002-201909).rda")
NDVI = NDVI[, 1:ntime]
QC   = QC[, 1:ntime]
QC[QC == 4] = 1 # From MODIS historic time series, treated as marginal

################################################################################
outdir   = "OUTPUT"
check_dir(outdir)
l_ind    <- chunk(seq_along(I_mod), 70)

InitCluster(12)
grps = 70:1
temp <- foreach(inds = l_ind[grps], grp = grps, icount()) %do% {
  runningId(grp)

  I_begin  = min(inds)
  n_time   = length(inds)

  mat_NDVI = t(NDVI[inds, ]/1e4)
  mat_qc   = QC[inds, ]   %>% t()

  # profvis::profvis({
  l_pheno = foreach(y = mat_NDVI, qc = mat_qc, j = icount()) %dopar% {
    Ipaper::runningId(j)
    # titlestr = "a"
    # titlestr = glue("[{j}]")
    titlestr = NULL
    d_qc = qc_summary(qc, wmin = 0.2)
    # browser()
    # if titlestr == NULL, then no figure produced; if show, then it will be open
    ans = tryCatch(
      phenofit_site(y = y[, 1], t = date, w = d_qc$w, QC_flag = d_qc$QC_flag,
                    nptperyear = 23,
                    prefix = "", titlestr = titlestr, show = FALSE),
      error = function(e){
        message(sprintf("e: [%04d] %s", j, e$message))
      }
    )
  }
  # })
  df = l_pheno %>% set_names(inds[1:length(l_pheno)]) %>% rm_empty() %>% melt_list("I_grid")
  outfile = sprintf("%s/phenofit_TP_MOD13C1_(2000-2018)_020deg_[%04d].csv", outdir, grp)
  fwrite(df, outfile)
}

## 2. POST-process -------------------------------------------------------------
indir <- "OUTPUT/"
files <- dir(indir, "*.csv", full.names = TRUE)
lst <- llply(files, function(file){
  d <- fread(file)
  d[, lapply(.SD, mean, na.rm = TRUE), .(I_grid, flag, origin), .SDcols = colnames(d)[-(1:4)]]
}, .progress = "text")

# only the first growing season used in this study
df <- do.call(rbind, lst)
df <- df[str_extract(flag, "_\\d") == "_1"]

I_grid   = I_mod
range    = c(73, 105, 25, 40)
cellsize = 1/20
r     = get_grid2(range, cellsize = 1/20)
r.new = get_grid2(range, cellsize = 1/10)
s     = as(r, "SpatialGridDataFrame")
s.new = as(r.new, "SpatialGridDataFrame")

lst_pheno = foreach(grp = grps, i = icount()) %do% {
  runningId(i, prefix = grp)
  d <- df[flag == grp, ]
  I <- I_grid[d$I_grid]
  # x@data <-
  # d2 <- d[, -(1:3)]
  # ans = resample_spatialPixel(d2, r, r.new, I, I_grid.new) %>% set_colnames(colnames(d2))
}

lst <- list(df = df, I_grid = I_mod, raster = r)
saveRDS(lst, file = "phenofit_MOD13C1_TP_020deg.RDS")

load("N:/Research/phenology/phenology_TP/OUTPUT/phenology_TP_AVHRR_phenofit_010deg.rda")
load("N:/Research/phenology/phenology_TP/data/grid_avhrr.rda")
I_grid.new = I_grid2_10

grps <- unique(df$flag) %>% set_names(., str_extract(., "\\d{1,}"))

lst_pheno = foreach(grp = grps, i = icount()) %do% {
  runningId(i, prefix = grp)
  d <- df[flag == grp, ]
  I <- I_grid[d$I_grid]
  d2 <- d[, -(1:3)]
  ans = resample_spatialPixel(d2, r, r.new, I, I_grid.new) %>% set_colnames(colnames(d2))
}


saveRDS(lst_pheno, file = "phenofit_MOD13C1_TP_010deg.RDS")




source("test/load_pkgs.R")

# add date
d_date <- fread("inst/extdata/dates_SPOT.txt")
date   = as.POSIXct(d_date$mid) %>% date()

# NC file
file = "inst/extdata/NDVI_SPOT_TP_GRID.nc"
fid  <- nc_open(file)

mat_grid <- ncvar_get(fid, "grid")
I_grid   <- fid$dim$Id$vals %>% set_dim(NULL)
l_ind    <- chunk(seq_along(I_grid), 1000)

outdir   = "OUTPUT"
check_dir(outdir)

InitCluster(12)

temp <- foreach(inds = l_ind[700:1000], i = icount()) %do% {
  I_begin  = min(inds)
  n_time   = length(inds)

  mat_NDVI = ncvar_get(fid, "NDVI"      , start = c(I_begin, 1), count = c(n_time, -1)) %>% t()
  mat_qc   = ncvar_get(fid, "VI_quality", start = c(I_begin, 1), count = c(n_time, -1)) %>% t()

  # browser()
  # profvis::profvis({
    l_pheno = foreach(y = mat_NDVI, qc = mat_qc, j = icount()) %dopar% {
      runningId(j)
      # titlestr = "a"N
      # titlestr = glue("[{i}] {site}")
      d_qc = qc_SPOT(qc, wmin = 0.2)
      # browser()
      # if titlestr == NULL, then no figure produced; if show, then it will be open
      ans = tryCatch(
        phenofit_site(y = y[, 1], t = date, w = d_qc$w, QC_flag = d_qc$QC_flag,
                      nptperyear = 36,
                      prefix = "", titlestr = titlestr, show = FALSE),
        error = function(e){
          message(sprintf("e: [%04d] %s", j, e$message))
        }
      )
    }
  # })
  df = l_pheno %>% set_names(inds) %>% rm_empty() %>% melt_list("I_grid")

  outfile = sprintf("{outdir}/phenofit_TP_SPOT_(1998-2013)_112deg_[%04d]", outdir, i)
  write(df, outfile)
}

# sites        <- unique(df$site)
# d            <- df[site == sitename] # get the first site data
# d_point      <- st[site == sitename]

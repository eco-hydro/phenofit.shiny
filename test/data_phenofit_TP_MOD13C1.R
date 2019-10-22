source("test/load_pkgs.R")
devtools::load_all()

#' middle of dn period
get_date_MODIS <- function(year, dn = 16){
    begin = ifelse(year == 2000, 49, 1)
    doy_begin = seq(begin     , 365, dn)
    doy_end   = seq(begin+dn-1, 365, dn) %>% c(leap_year(year)+365)

    doy   = round((doy_begin + doy_end)/2) # nearly floor
    dates = doy + year*1e3
    as.Date(as.character(dates), "%Y%j")
}

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

profvis::profvis(
    phenofit_site(y = y[, 1], t = date, w = d_qc$w, QC_flag = d_qc$QC_flag,
                  nptperyear = 36,
                  prefix = "", titlestr = titlestr, show = FALSE, use.julia = FALSE)
)

julia_init()

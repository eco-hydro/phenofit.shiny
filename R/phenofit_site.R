#' Extract Vegetation Phenology at site scale
#'
#' @inheritParams phenofit::check_input
#' @param ... other parameters to [phenofit::curvefits()]
#'
#' @export
phenofit_site <- function(y, t, w, QC_flag, nptperyear = 36,
    prefix = "", titlestr = NULL, show = FALSE, ...)
{
    # Parameters
    # lambda   <- 5    # non-parameter Whittaker, only suit for 16-day. Other time-scale should assign a lambda.
    # nptperyear <- 36   # How many points for a single year
    ymax_min   <- 0.1  # the maximum ymax shoud be greater than `ymax_min`
    rymin_less <- 0.8  # trough < ymin + A*rymin_less
    wFUN       <- wTSM #wTSM #wBisquare # Weights updating function, could be one of `wTSM`, 'wBisquare', `wChen` and `wSELF`.

    ## 2.1 load site data
    south      = FALSE
    print      = FALSE # whether print progress
    IsPlot     = FALSE  # for brks

    ## 2.2 Check input data
    d <- data.table(y, t, date = t, w, QC_flag)

    dnew  <- add_HeadTail(d, south, nptperyear = nptperyear) # add additional one year in head and tail
    INPUT <- check_input(dnew$t, dnew$y, dnew$w, dnew$QC_flag,
                         nptperyear, south,
                         maxgap = nptperyear/4, alpha = 0.02, wmin = 0.2)

    ## 2.3 Divide growing seasons
    lambda <- init_lambda(INPUT$y)

    brks2  <- season_mov(INPUT,
                       FUN = wWHIT, wFUN = wFUN,
                       maxExtendMonth = 6, r_min = 0.1,
                       IsPlot = IsPlot, IsPlot.OnlyBad = FALSE, print = print)

    ## 2.4 Curve fitting
    methods = c("AG", "Zhang", "Beck", "Elmore")
    fit  <- curvefits(INPUT, brks2,
                  methods = methods, #,"klos",, 'Gu'
                  wFUN = wFUN,
                  iters = 2,
                  nextend = 2, maxExtendMonth = 3, minExtendMonth = 1, minPercValid = 0.2,
                  print = print, verbose = FALSE, ...)

    ## check the curve fitting parameters
    l_param <- get_param(fit)
    d_fit   <- get_fitting(fit)
    # d_gof   <- get_GOF(fit)

    ## visualization
    if (show) {
        file_pdf = glue("Figure/{prefix}{titlestr}.pdf")
        check_dir(dirname(file_pdf))

        g <- plot_phenofit(d_fit, brks2, titlestr)
        # grid::grid.newpage(); grid::grid.draw(g)# plot to check the curve fitting
        write_fig(g, file_pdf, 11, 6)
    }

    ## 2.5 Extract phenology
    l_pheno <- get_pheno(fit, IsPlot = F) #%>% map(~melt_list(., "meth"))
    r <- l_pheno$doy %>% melt_list("meth")
    r
}

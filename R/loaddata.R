#' update all INPUT data according to `input` file.
#'
#' @param options should has children of `file_site`, and one of
#' `file_veg_rda` or `file_veg_text`.
#' @param rv return values to reactiveValues object.
#' @param ... ignored.
#'
#' @keywords internal
#' @importFrom utils find
#' @export
phenofit_loaddata <- function(options, rv, ...){
    file_type     <- options$file_type
    file_veg_rda  <- options$file_veg_rda
    file_veg_text <- options$file_veg_text
    file_site     <- options$file_site
    varname       <- options$var_y
    varQC         <- options$var_qc
    qcFUN         <- options$qcFUN
    nptperyear    <- options$nptperyear
    is_QC2w       <- options$is_QC2w
    wmin <- 0.2

    # browser()
    df    = NULL
    st    = NULL
    sites = NULL
    # sites has the possible of missing.
    if (file_type == "text") {
        if (check_file(file_veg_text)) {
            df    <- fread(file_veg_text) %>% check_datestr()
            sites <- unique(df$site) %>% sort()

            if (check_file(file_site)) {
                st <- fread(file_site, encoding = "UTF-8")
            } else {
                st <- data.table(ID = seq_along(sites), site = sites, lat = 30)
            }
        }
    } else {
        if (check_file(file_veg_rda)) {
            # options$file_veg_rda <- file_veg_rda
            load(file_veg_rda)
            df <- df %>% check_datestr()
            sites <- unique(df$site) %>% sort()
            # rv$st <- st
        }
    }

    if (!is.null(df) && !is.null(varname) && !(varname %in% c("", "y"))) {
        I <- match(varname, colnames(df))
        colnames(df)[I] <- "y"
    }

    if (!is.null(is_QC2w) && is_QC2w) {
        if (length(find(qcFUN, mode = "function")) > 0) {
        # if (is.function(qcFUN))
        if (is.character(varQC) && length(varQC) > 0) {
            if (!(varQC %in% colnames(df))){
                warning(sprintf("No QC variable %s in df! ", varQC))
            } else {
                eval(parse(text = sprintf('df[, c("QC_flag", "w") := %s(%s, wmin = %f)]',
                    qcFUN, varQC, wmin)))
            }
        }
        } else {
            warnings(sprintf('qcFUN: %s does not exist!', qcFUN))
        }
    }

    if (!missing(rv) && !is.null(df)) {
        rv$df <- df
        rv$st <- st
        rv$sites <- sites
        rv$nptperyear <- nptperyear
    }
    listk(df, st, sites, nptperyear)
}

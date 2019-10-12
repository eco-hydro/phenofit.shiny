#' @export
#' 
#' @references
#' Erwin Wolters, Else Swinnen, Carolien Toté, Sindy Sterckx. 
#' SPOT-VGT COLLECTION 3 PRODUCTS USER MANUAL V1.2, 2018, P47
qc_SPOT <- function (QA, wmin = 0.2, wmid = 0.5, wmax = 1) {
    QA <- getBits(QA, 0, 2) 

    w  <- rep(NA, length(QA))
    w[QA == 0] <- wmax

    # shadow, cloud and Ice
    w[QA %in% c(1, 3, 4)] <- wmin

    # undefined
    w[QA == 2 ] <- 0.5

    QC_flag <- factor(QA, 0:4, c("good", "shadow", "marginal", 
        "cloud", "snow"))
    list(QC_flag = QC_flag, w = w)
}


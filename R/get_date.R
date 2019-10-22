#' MODIS middle of dn period
#' 
#' @export
get_date_MODIS <- function(year, dn = 16){
    begin = ifelse(year == 2000, 49, 1)
    doy_begin = seq(begin     , 365, dn)
    doy_end   = seq(begin+dn-1, 365, dn) %>% c(leap_year(year)+365)

    doy   = round((doy_begin + doy_end)/2) # nearly floor
    dates = doy + year*1e3
    as.Date(as.character(dates), "%Y%j")
}

#' get_date_AVHRR
#'
#' Generate image dates from `year_begin` to `year_end`.
#' This function is only for AVHRR satellites.
#'
#' @param year_begin integer
#' @param year_end integer
#'
#' @importFrom lubridate days_in_month
#' @export
#'
#' @examples
#' date_AVHRR <- get_date_AVHRR()
get_date_AVHRR <- function(year_begin = 1982, year_end = 2015){
    dates <- seq(ymd(year_begin*1e4 + 0101), ymd(year_end*1e4 + 1231), "month")
    days  <- dates %>% days_in_month()

    dates_a <- dates + ddays(floor(days/4))
    dates_b <- dates + ddays(floor(days/4*3))
    dates <- c(dates_a, dates_b) %>% sort()

    d_dates <- data.table(I = seq_along(dates),
        date = dates, month = month(dates), dom = day(dates), doy = yday(dates))
    d_dates
}

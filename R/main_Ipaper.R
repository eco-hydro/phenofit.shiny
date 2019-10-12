# Make sure date character in \code{df} has been converted to \code{Date} object.
check_datestr <- function(df){
    var_times <-  intersect(c("t", "date"), colnames(df))
    for (i in seq_along(var_times)){
        varname <- var_times[i]
        df[[varname]] %<>% lubridate::ymd()
    }
    df
}

#' check_file
#' Check file whether exist. If not, then give a notification.
#' 
#' @importFrom shiny showNotification
#' @export
check_file <- function(file, duration = 10){
    filename <- deparse(substitute(file))

    if (length(file) == 0 || !is.character(file)) {
        return (FALSE)
    } else if (file.exists(file)) {
        return(TRUE)
    } else {
        session <- getDefaultReactiveDomain()
        if (!is.null(duration) && !is.null(session)){
            showNotification(sprintf("invalid %s: %s", filename, as.character(file)),
                duration = duration, type = "warning")
        }
        return(FALSE)
    }
}

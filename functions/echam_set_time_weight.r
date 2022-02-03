# r

echam_set_time_weight <- function(year_fr) {
    # year_fr = fraction of year
    options(digits=20)
    ntimes <- 52
    nyears <- 251
    iyear <- floor(year_fr) - 1849
    iweek <- floor((year_fr - floor(year_fr)) * ntimes) + 1
    err <- iweek > ntimes | iweek < 1 | iyear > nyears | iyear < 1
    err_tido <- iweek > ntimes | iweek < 1 | iyear > (nyears+1) | iyear < 1
    date <- yearsdec_to_ymdhms(year_fr)$text
    return(list(ntimes=ntimes, nyears=nyears,
                df=data.frame(year_fr=year_fr, iyear=iyear, iweek=iweek, date=date, err=err, err_tido=err_tido)))
} # echam_set_time_weight


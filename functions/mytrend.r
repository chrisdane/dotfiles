#!/usr/bin/env Rscript

# calc linear regression (stats::lm) on (lon,lat,time) data and save temporal trend and its significance as (lon,lat)

if (interactive()) { # test
    fin <- "/isibhv/projects/paleo_work/cdanek/post/echam5/yearsum/wisoaprt_d_post/cosmos-aso-wiso_Hol-T_wiso_mm_echam5_yearsum_wisoaprt_d_post_sellevel_2_global_yearsum_0004-7000.nc"
    #fout <- "/isibhv/projects/paleo_work/cdanek/post/echam5/yearsum/lm_wisoaprt_d_post_as_time_slope/cosmos-aso-wiso_Hol-T_wiso_mm_echam5_yearsum_mylm_wisoaprt_d_post_as_time_slope_sellevel_2_global_yearsum_0004-7000.nc"
    #fout <- "/isibhv/projects/paleo_work/cdanek/post/echam5/yearsum/lm_wisoaprt_d_post_as_time_slope/cosmos-aso-wiso_Hol-T_wiso_mm_echam5_yearsum_mylm_wisoaprt_d_post_as_time_slope_sig_0.01_sellevel_2_global_yearsum_0004-7000.nc"
    fout <- "/isibhv/projects/paleo_work/cdanek/post/echam5/yearsum/lm_wisoaprt_d_post_as_time_slope/cosmos-aso-wiso_Hol-T_wiso_mm_echam5_yearsum_mylm_wisoaprt_d_post_as_time_slope_sig_0.05_sellevel_2_global_yearsum_0004-7000.nc"
    lm_method <- "stats::lm"
    #significance <- 0.01
    significance <- 0.05
    trend_dt_unit <- "years"
    
} else {

    # get args
    args <- commandArgs(trailingOnly=F)
    me <- basename(sub("--file=", "", args[grep("--file=", args)]))
    help <- paste0("\nUsage:\n",
                   " $ ", me, " ",
                   "--fin=/path/to/input.nc ",
                   "--fout=/path/to/output.nc ",
                   "--lm_method=stats::lm ",
                   "--significance=0.01 ",
                   "--trend_dt_unit=years",
                   "\n")

    # check args 
    args <- commandArgs(trailingOnly=T)
    if (length(args) < 2) {
        message(help)
        quit()
    }
    if (any(grepl("--fin", args))) {
        fin <- sub("--fin=", "", args[grep("--fin=", args)])
    } else {
        stop("provide `--fin=/path/to/fin.nc`")
    }
    if (any(grepl("--fout", args))) {
        fout <- sub("--fout=", "", args[grep("--fout=", args)])
    } else {
        stop("provide `--fout=/path/to/fout.nc`")
    }
    if (any(grepl("--lm_method", args))) {
        lm_method <- sub("--lm_method=", "", args[grep("--lm_method=", args)])
    } else {
        lm_method <- "stats::lm" # default
    }
    if (any(grepl("--significance", args))) {
        significance <- sub("--significance=", "", args[grep("--significance=", args)])
    } else {
        significance <- 0.01 # default
    }
    if (any(grepl("--trend_dt_unit", args))) {
        trend_dt_unit <- sub("--trend_dt_unit=", "", args[grep("--trend_dt_unit=", args)])
    } else {
        trend_dt_unit <- "years" # default
    }

} # if interactive or not

## checks 
if (file.access(fin, mode=0) == -1) {
    stop("input file \"", fin, "\" does not exist")
}
if (file.access(fin, mode=4) == -1) {
    stop("input file \"", fin, "\" not readable")
}
fin <- normalizePath(fin)
outpath <- dirname(fout)
if (!dir.exists(outpath)) {
    dir.create(outpath, recursive=T)
    if (!dir.exists(outpath)) {
        stop("could not create path of `fout` = \"", outpath, "\"")
    }
}
if (file.access(outpath, mode=2) == -1) {
    stop("no permission to write in path of `fout` = \"", outpath, "\"")
}
outpath <- normalizePath(outpath)
fout <- paste0(outpath, "/", basename(fout))

if (!any(lm_method == c("stats::lm"))) {
    stop("`lm_method` = ", lm_method, " not implemented")
}

if (!is.finite(significance)) stop("provided `significance` = ", significance, " is not finite")
if (significance < 0) stop("provided `significance` = ", significance, " < 0 not allowed")
if (significance >= 1) stop("provided `significance` = ", significance, " >= 1 not allowed")

message("\nload ncdf4 package ...")
library(ncdf4)

## start
message("\nopen ", fin, " ...")
nc <- ncdf4::nc_open(fin)
dims <- names(nc$dim)

if (any(is.na(match(c("lon", "lat", "time"), dims)))) {
    stop("input file does not have \"lon\", \"lat\" and \"time\" dims")
}
dimids <- sapply(nc$dim, "[[", "id")
dimids <- dimids[c("lon", "lat", "time")]

# calc lm for all vars with lon, lat and time dims
vars <- names(nc$var)
for (vari in seq_along(vars)) {
    message("**********************************************\n",
            "variable ", vari, "/", length(vars), ": \"", vars[vari], "\" ", appendLF=F)
    dimids_of_var <- nc$var[[vari]]$dimids
    if (!all(!is.na(match(dimids, dimids_of_var)))) {
        message("--> this variable does not have lon, lat and time dims. skip variable.")
    } else {
        inds <- which(!is.na(match(dimids_of_var, dimids)))
        dims_of_var <- nc$var[[vari]]$size[inds]
        names(dims_of_var) <- names(dimids)
        message("\n--> dims: ", paste(paste0(names(dims_of_var), "=", dims_of_var), collapse=", "), " ...")
        if (dims_of_var["time"] == 1) {
            message("the time dim of this variable is of length 1. skip variable.")
        } else {
        
            # get time dim vals
            time <- nc$dim$time$vals
            timeunit <- nc$dim$time$units
            if (grepl(" since ", timeunit)) { # relative time
                origin <- substr(timeunit, 
                                 start=regexpr(" since ", timeunit) + 7,
                                 stop=nchar(timeunit))
                if (grepl("days since ", timeunit)) {
                    posixct <- as.POSIXct(time*86400, origin=origin, tz="UTC")
                } else {
                    stop("need to set mult factor for time unit ", timeunit)
                }
            } else { # absolute time
                stop("absolute time unit ", timeunit, " not implemented")
            }
            from <- as.POSIXlt(min(posixct))
            to <- as.POSIXlt(max(posixct))
            if (trend_dt_unit == "years") {
                trend_dt_val <- to$year - from$year + 1
            } else {
                stop("`trend_dt_unit` = ", trend_dt_unit, " not implemented yet")
            }
            
            # get variable unit and apply temporal trend multiplication
            varunit <- nc$var[[vari]]$units # e.g. "°C"
            varunit <- paste0(varunit, " / ", trend_dt_val, " ", trend_dt_unit) # e.g. "°C / 1000 years"
            message("calc temporal trends from ", from, " to ", to, 
                    " (", trend_dt_val, " ", trend_dt_unit, ") ...")

            # save result
            slope_mat <- std_error_mat <- t_val_mat <- p_val_mat <- array(NA, dim=dims_of_var[c("lon", "lat")])

            # load variable
            data <- ncdf4::ncvar_get(nc, vars[vari], collapse_degen=T) # dims of length 1 will be removed
            
            # open progress bar
            cnt <- 0
            pb <- utils::txtProgressBar(min=1, max=dims_of_var["lon"]*dims_of_var["lat"], 
                                        initial=1, style=3)
            
            # do for all lons and lats
            for (i in seq_len(dims_of_var["lon"])) {
                for (j in seq_len(dims_of_var["lat"])) {

                    cnt <- cnt + 1 # for progress bar

                    cmd <- rep("", t=length(dims_of_var))
                    cmd[which(names(dims_of_var) == "lon")] <- i
                    cmd[which(names(dims_of_var) == "lat")] <- j
                    cmd <- paste0("ts <- data[", paste(cmd, collapse=","), "]") 
                    eval(parse(text=cmd)) # e.g. `ts <- data[1,1,]`
                    
                    if (lm_method == "stats::lm") {
                        lm <- stats::lm(ts ~ time)
                        # get trend over whole time
                        if (T) {
                            slope <- lm$fitted.values[dims_of_var["time"]] - lm$fitted.values[1] # end of fit minus start of fit
                        } else if (F) {
                            # problem: non-equal dt throughout the time series
                            # but very similar as method above
                            slope <- lm$coefficients[2] * dims_of_var["time"] * mean(unique(diff(time))) # = slope * dt * nt
                        }
                        lm <- summary(lm)
                        if (i == 1 && j == 1) df <- lm$df[2]
                        std_error <- lm$coefficients[2,"Std. Error"]
                        t_val <- lm$coefficients[2,"t value"]
                        p_val <- lm$coefficients[2,"Pr(>|t|)"]
                    } else {
                        stop("`lm_method` = ", lm_method, " not implemented")
                    }

                    # save result if significant 
                    if (p_val < significance) { # e.g. < 0.01
                        slope_mat[i,j] <- slope
                        std_error_mat[i,j] <- std_error
                        t_val_mat[i,j] <- t_val
                        p_val_mat[i,j] <- p_val
                    }
        
                    # update progress bar
                    setTxtProgressBar(pb, value=cnt)

                } # for j nlat
            } # for i nlon
    
            # close progress bar
            close(pb)

            # output
            message("save ", fout, " ...")
            lon_dim <- nc$dim$lon
            lat_dim <- nc$dim$lat
            slope_var <- ncdf4::ncvar_def(name=paste0("lm_", vars[vari], "_as_time_slope"),
                                          units=varunit, dim=list(lon_dim, lat_dim))
            std_error_var <- ncdf4::ncvar_def(name=paste0("lm_", vars[vari], "_as_time_std_error"),
                                              units=varunit, dim=list(lon_dim, lat_dim))
            t_val_var <- ncdf4::ncvar_def(name=paste0("lm_", vars[vari], "_as_time_t_val"),
                                          units="", dim=list(lon_dim, lat_dim))
            p_val_var <- ncdf4::ncvar_def(name=paste0("lm_", vars[vari], "_as_time_p_val"),
                                          units="", dim=list(lon_dim, lat_dim))
            outnc <- ncdf4::nc_create(filename=fout, force_v4=T, 
                                      vars=list(slope_var, std_error_var, t_val_var, p_val_var))
            ncdf4::ncvar_put(nc=outnc, varid=slope_var, vals=slope_mat)
            ncdf4::ncvar_put(nc=outnc, varid=std_error_var, vals=std_error_mat)
            ncdf4::ncvar_put(nc=outnc, varid=t_val_var, vals=t_val_mat)
            ncdf4::ncvar_put(nc=outnc, varid=p_val_var, vals=p_val_mat)
            ncdf4::ncatt_put(nc=outnc, varid=0, "input", fin)
            ncdf4::ncatt_put(nc=outnc, varid=0, "lm_method", lm_method)
            ncdf4::ncatt_put(nc=outnc, varid=0, "lm_significance", significance)
            ncdf4::ncatt_put(nc=outnc, varid=0, "lm_df", df)
            ncdf4::nc_close(outnc)

        } # if ntime == 1 or not
    } # if vari has lon, lat and time dims
} # for vari


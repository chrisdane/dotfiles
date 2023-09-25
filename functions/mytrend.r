#!/usr/bin/env Rscript

# calc linear regression (stats::lm) save temporal trend and its std. error and p value

# dependencies: 
# - cdo: showname, showtimestamp, select, setgrid
# - nccopy (if rechunking is wanted and necessary)

if (interactive()) { # test
    rm(list=ls())
    me <- "mytrend.r"
    if (F) {
        args <- c("--fin=/work/ba1103/a270073/data/gregor_and_fay_2021/data/v2021.04/SeaFlux_v2021.04_spco2_SOCOM_unfilled_1982-2019.nc",
                  "--fout=/work/ba1103/a270073/data/gregor_and_fay_2021/post/v2021.04/SeaFlux_v2021.04_spco2_SOCOM_unfilled_lm_<varname>_as_time_1982-2019.nc"
                  , "--from_year=2009")
    } else if (F) {
        args <- c("--fin=/work/ba1103/a270073/post/CanESM5-CanOE/select/mlotst/CanESM5-CanOE_historical_and_ssp126_r1i1p2f1_CanESM5-CanOE_select_mlotst_global_Jan-Dec_1970-2019.nc",
                  "--fout=/work/ba1103/a270073/post/CanESM5-CanOE/select/mlotst/test_lm_<varname>_as_time.nc"
                  , "--spatialdimnames=i,j")
    } else if (F) {
        args <- c("--fin=/work/ba1103/a270073/post/EN.4.2.2/select/rho/EN.4.2.2.f.analysis.g10.202111_rho.nc",
                  "--fout=/work/ba1103/a270073/post/EN4.2.2/select/lm_rho_as_time/EN.4.2.2.f.analysis.g10.202111_<varname>.nc")
    } else if (F) {
        args <- c("--spatialdimnames=rgrid",
                  "--fin=~/test/era5_sf00_1M_era5_select_WS10_global_Jan-Dec_1970-1970.nc",
                  "--fout=~/test/fu_<varname>.nc")
    } else if (T) {
        args <- c("--fin=/work/ba1103/a270073/post/GFDL-ESM4/select/mldepthdensp030_m/GFDL-ESM4_historical_r1i1p1f1_GFDL-ESM4_select_mldepthdensp030_m_global_annual_1970-2014.nc",
                  "--fout=/work/ba1103/a270073/post/GFDL-ESM4/select/lm_mldepthdensp030_m/GFDL-ESM4_historical_r1i1p1f1_GFDL-ESM4_select_lm_<varname>_as_time_global_annual_1970-2014.nc")
    }
} else {
    args <- commandArgs(trailingOnly=F) # get args
    me <- basename(sub("--file=", "", args[grep("--file=", args)]))
    args <- commandArgs(trailingOnly=T)
} # if interactive or not

help <- paste0("\nUsage:\n",
               " $ ", me, " ",
               "[--varnames=varname1[,varname2,...]] ",
               "[--timedimname=time] ",
               "[--spatialdimnames=lon,lat] ",
               "[--from_year=from_year] ",
               "[--to_year=to_year] ",
               "[--lm_method=stats::lm] ",
               "[--auto_rechunk=true] ",
               "--fin=[/path/to/]input.nc ",
               "--fout=\"[/path/to/]output_<varname>.nc\" ",
               "\n\n",
               "e.g. ", me, " --fin=~/test.nc --fout=\"~/test_lm_<varname>_as_time.nc\"\n", 
               "     ", me, " --timedimname=Time --spatialdimnames=nodes_2d --from_year=2000 --fin=~/test.nc --fout=\"~/test_lm_<varname>_as_time.nc\"\n\n",
               "Dependencies: `cdo: showname, showtimestamp, select, setgrid`\n",
               "              `nccopy` if --auto_rechunk=true and rechunking is necessary\n")

# check args 
if (length(args) < 2 || length(args) > 9) {
    if (interactive()) {
        stop(help)
    } else {
        message(help)
        quit()
    }
}

warn <- options()$warn
options(warn=2) # error on warning

if (any(grepl("--timedimname", args))) {
    timedimname <- sub("--timedimname=", "", args[grep("--timedimname=", args)])
    message("provided `timedimname` = ", timedimname)
} else {
    timedimname <- "time"
    message("`--timedimname=<timedimname>` not provided --> use default: ", timedimname)
}

if (any(grepl("--spatialdimnames", args))) {
    spatialdimnames <- sub("--spatialdimnames=", "", args[grep("--spatialdimnames=", args)])
    spatialdimnames <- strsplit(spatialdimnames, ",")[[1]]
    message("provided `spatialdimnames` = \"", paste(spatialdimnames, collapse="\", \""), "\"")
} else {
    spatialdimnames <- c("lon", "lat")
    message("`--spatialdimnames=<spatialdimnames>` not provided --> use default: \"", paste(spatialdimnames, collapse="\", \""), "\"")
}
nspatialdims <- length(spatialdimnames)

if (any(grepl("--from_year", args))) {
    from_year <- sub("--from_year=", "", args[grep("--from_year=", args)])
    message("provided `from_year` = ", from_year)
} else {
    from_year <- NULL
    message("`--from_year=<from_year>` not provided --> use default: complete time series")
}

if (any(grepl("--to_year", args))) {
    to_year <- sub("--to_year=", "", args[grep("--to_year=", args)])
    message("provided `to_year` = ", to_year)
} else {
    to_year <- NULL
    message("`--to_year=<to_year>` not provided --> use default: complete time series")
}

if (any(grepl("--lm_method", args))) {
    lm_method <- sub("--lm_method=", "", args[grep("--lm_method=", args)])
    message("provided `lm_method` = ", lm_method)
} else {
    lm_method <- "stats::lm" # default
    message("`--lm_method=<lm_method>` not provided --> use default: ", lm_method)
}
if (!any(lm_method == c("stats::lm"))) {
    stop("`lm_method` = ", lm_method, " not implemented")
}

if (any(grepl("--auto_rechunk", args))) {
    auto_rechunk <- sub("--auto_rechunk=", "", args[grep("--auto_rechunk=", args)])
    message("provided `auto_rechunk` = ", auto_rechunk)
} else {
    auto_rechunk <- "true" # default
    message("`--auto_rechunk=<true or false>` not provided --> use default: ", auto_rechunk)
}
if (auto_rechunk == "true") {
    auto_rechunk <- T
} else if (auto_rechunk == "false") {
    auto_rechunk <- F
} else {
    stop("provided `auto_rechunk` = \"", auto_rechunk, "\" not provided. must be \"true\" or \"false\"")
}

if (any(grepl("--fin", args))) {
    fin <- sub("--fin=", "", args[grep("--fin=", args)])
    message("provided `fin` = ", fin)
} else {
    stop("provide `--fin=[/path/to/]fin.nc`")
}
if (file.access(fin, mode=0) == -1) stop("input file \"", fin, "\" does not exist")
if (file.access(fin, mode=4) == -1) stop("input file \"", fin, "\" not readable")
fin <- normalizePath(fin)

if (any(grepl("--fout", args))) {
    fout <- sub("--fout=", "", args[grep("--fout=", args)])
    message("provided `fout` = ", fout)
} else {
    stop("provide `--fout=[/path/to/]fout_<varname>.nc`")
}
if (!grepl("<varname>", fout)) stop("provide \"<varname>\" at wanted position in `fout` = ", fout)
outpath <- dirname(fout)
dir.create(outpath, recursive=T, showWarnings=F)
if (!dir.exists(outpath)) stop("could not create `outpath` = \"", outpath, "\"")
outpath <- normalizePath(outpath)

message("get cdo ... ", appendLF=F)
cdo <- Sys.which("cdo")
if (cdo == "") stop("could not find cdo")
message(cdo)

if (any(grepl("--varnames", args))) {
    varnames <- sub("--varnames=", "", args[grep("--varnames=", args)])
    varnames <- strsplit(varnames, ",")[[1]]
    message("provided `varnames` = \"", paste(varnames, collapse="\", \""), "\"")
} else {
    message("`--varnames=<varnames>` not provided --> use default")
    cmd <- paste0(cdo, " -s showname ", fin) # get varnames of fin
    message("run `", cmd, "` ...")
    varnames <- strsplit(trimws(system(cmd, intern=T)), " ")[[1]]
    if (length(varnames) == 0) stop("file has zero variables")
    message("--> file has ", length(varnames), " variables: ", paste(varnames, collapse=", "))
}
message("\nload ncdf4 package ...")
library(ncdf4)

# open file
message("open ", fin, " ...")
nc <- ncdf4::nc_open(fin)

# get complete time dim of file
if (!any(names(nc$dim) == timedimname)) stop("not any dim of this file has a dim named \"", timedimname, "\". rerun with `--timedimname=<time dim name>`")
time_all <- nc$dim[[timedimname]]$vals
if (length(time_all) <= 3) stop("time dim \"", timedimname, "\" of this file is of length ", length(time_all), " <= 3 --> too short for linear regression")
timedimid_nc <- nc$dim[[timedimname]]$id
posixct_all <- as.POSIXct(strsplit(trimws(system(paste0("cdo -s showtimestamp ", fin), intern=T)), "  ")[[1]], tz="UTC")
dimids_nc <- sapply(nc$dim, "[[", "id")
# e.g.    time     bnds        i        j vertices 
#            0        1        2        3        4
 
# calc lm for all variables
for (vari in seq_along(varnames)) {
    varname <- varnames[vari]
    message("calc trend of variable ", vari, "/", length(varnames), ": \"", varname, "\" ...")
    
    # check if trend of current variable already exists
    fouti <- sub("<varname>", varname, fout)
    if (file.exists(fouti)) {
        message("result of this variable already exists: ", fouti, " --> skip this variable")
    
    } else { # lm result does not exist yet
    
        # check variable dimnames
        dimids_var <- nc$var[[varname]]$dimids # e.g. 2 3 0 = i j time
        dimids_var_nc_inds <- match(dimids_var, dimids_nc) # e.g. 3 4 1 = i j time
        dimnames_var <- names(dimids_nc)[dimids_var_nc_inds]
        names(dimids_var) <- names(dimids_var_nc_inds) <- dimnames_var
        if (any(is.na(match(spatialdimnames, dimnames_var)))) {
            stop("provided ", nspatialdims, " spatial dims \"", paste(spatialdimnames, collapse="\", \""), 
                 "\" but these are not in the ", length(dimnames_var), " dims of variable ", varname, ": \"", 
                 paste(dimnames_var, collapse="\", \""), 
                 "\"\n--> provide `--spatialdimnames=<spatialdimname1[,spatialdimname2,...,spatialdimnameN]>` and rerun script")
        }

        # check spatial dim(s)
        spatial_dims <- vector("list", l=nspatialdims)
        names(spatial_dims) <- spatialdimnames
        spatial_dims_nc <- spatial_dims
        for (si in seq_len(nspatialdims)) {
            spatial_dims[[si]] <- list(len=nc$dim[[spatialdimnames[si]]]$len,
                                       vals=nc$dim[[spatialdimnames[si]]]$vals,
                                       units=nc$dim[[spatialdimnames[si]]]$units)
            spatial_dims_nc[[si]] <- nc$dim[[spatialdimnames[si]]]
        }
        nspatial_vals <- sapply(spatial_dims, "[[", "len")
        ntot <- prod(nspatial_vals)
        
        # check chunks of current variable
        chunksizes <- nc$var[[varname]]$chunksizes # for old ncdf4 < 1.19 versions this may return NA although variable is chunked
        msg <- NULL
        if (all(is.na(chunksizes))) { # variable is not chunked
            msg <- paste0("variable ", varname, " of this file is not chunked")
        } else { # variable is chunked
            names(chunksizes) <- dimnames_var
            # does the time dim chunk length equal the complete time dim length?
            if (chunksizes[which(dimnames_var == timedimname)] != nc$dim[[timedimname]]$len) {
                msg <- paste0("variable ", varname, " is chunked (", 
                              paste(paste0(names(chunksizes), "=", chunksizes), collapse=","), ")\n")
            }
        }
        if (!is.null(msg)) { # variable is not chunked or does not contain complete time dim
            msg_dims <- rep(1, t=nspatialdims)
            if (nspatialdims == 1) {
                msg_dims[1] <- spatial_dims[[1]]$len
            } else if (nspatialdims == 2) {
                # tested so far: chunk the longest of all spatial dims, e.g. (366,1,1440) and not (366,720,1)
                msg_dims[which.max(nspatial_vals)] <- nspatial_vals[which.max(nspatial_vals)]
            } else {
                stop(nspatialdims, "-dim case ", paste(spatialdimnames, collapse=","), " not defined")
            }
            msg <- paste0(msg, 
                          "--> this is inefficient for reading time series\n",
                          "--> rechunk data with `nccopy -u -w -c time/", nc$dim[[timedimname]]$len, ",",
                          paste(paste0(spatialdimnames, "/", msg_dims), collapse=","), " in out` and rerun script")
            message("\n", msg)
            if (auto_rechunk) {
                message("--> `auto_rechunk` = true --> rechunk variable ", varname, " ...")
                nccopy <- Sys.which("nccopy")
                if (nccopy == "") {
                    message("--> did not find program `nccopy` --> cannot rechunk input data --> continue with original fin ...")
                } else {
                    # rechunk with nccopy
                    fin_rechunk <- paste0(outpath, "/tmp_rechunk_pid", Sys.getpid(), "_", basename(fin))
                    if (file.exists(fin_rechunk)) {
                        message("tmp rechunked ", varname, " file already exists --> skip nccopy cmd ...")
                    } else {
                        # cmd 1: select variable
                        cmd_rechunk <- paste0(cdo, " -select,name=", varname, " ", fin, " ", fin_rechunk)
                        message("1: select ", varname, ": run `", cmd_rechunk, "` ...")
                        check <- system(cmd_rechunk)
                        if (check != 0) stop("cmd failed")
                        # cmd 2: rechunk
                        # -w: rechunk file completely in memory --> this is faster but needs large memory --> if OOM, run wout -w
                        cmd_rechunk <- paste0(nccopy, " -u -w -c ", timedimname, "/", nc$dim[[timedimname]]$len, ",",
                                              paste(paste0(spatialdimnames, "/", msg_dims), collapse=","), " ", fin, " ", fin_rechunk)
                        message("2: rechunk ", varname, ": run `", cmd_rechunk, "` ...")
                        check <- system(cmd_rechunk)
                        if (check != 0) stop("cmd failed")
                    }
                    # update fin
                    message("read rechunked data ", fin_rechunk, " ...")
                    ncdf4::nc_close(nc) # un- or wrongly chunked original fin
                    nc <- ncdf4::nc_open(fin_rechunk)
                    chunksizes <- nc$var[[varname]]$chunksizes
                    if (all(is.na(chunksizes))) {
                        message("chunksizes of variable ", varname, " are still NA. this should not happen (ncdf4 package version ", 
                            utils::packageDescription("ncdf4")$Version, " should be >= 1.19). continue with unchunked/wrongly chunked variable ...")
                    } else {
                        names(chunksizes) <- dimnames_var
                        message("--> chunksizes = (", paste(paste0(names(chunksizes), "=", chunksizes), collapse=","), ")")
                    }
                } # if nccopy is available
            } else { # if auto_rechunk is false
                message("--> or rerun script with `--auto_rechunk=true`")
            } # if auto_rechunk
        } # is msg is not null

        # make spatial index mapping for looping through locations
        message("\nmake mapping (", nspatialdims, "-spatial-dims inds) <--> (1-spatial-dim inds) for looping through all locations of arbitrary number of spatial dims ...")
        if (nspatialdims == 1) { # e.g. irregular fesom
            mapping_df <- as.data.frame(seq_len(nspatial_vals[1])) # (ntot,1)
        } else if (nspatialdims == 2) { # e.g. lon, lat
            mapping_df <- expand.grid(seq_len(nspatial_vals[1]), seq_len(nspatial_vals[2]), KEEP.OUT.ATTRS=F) # (ntot,2)
        } else {
            stop("mapping not defined yet for this ", nspatialdims, "-dim case")
        }
        names(mapping_df) <- spatialdimnames
        nloc <- ntot
        message("`location_inds` is NULL --> use all ", nloc, " locations ...")
        location_inds <- seq_len(nloc)
        
        # get time vals of variable
        timedimind_var <- which(sapply(nc$var[[varname]]$dim, "[[", "id") == timedimid_nc)
        time_vari <- nc$var[[varname]]$dim[[timedimind_var]]$vals
        time_vari_inds_in_all <- match(time_vari, time_all)
        posixct_vari <- posixct_all[time_vari_inds_in_all]
        posixlt_vari <- as.POSIXlt(posixct_vari)
        message("variable has ", length(time_vari), " timepoints from ", min(posixct_vari), " to ", max(posixct_vari))
        from_ind <- min(time_vari_inds_in_all)
        if (!is.null(from_year)) {
            message("--> provided `from_year` = ", from_year)
            from_ind <- which(posixlt_vari$year+1900L == from_year)
            if (length(from_ind) == 0) stop("did not find this year in years of input")
            from_ind <- min(from_ind)
            message("--> continue with time points from position ", from_ind)
        } else {
            message("--> `from_year` not provided --> continue with time points from position ", from_ind)
        }
        to_ind <- max(time_vari_inds_in_all)
        if (!is.null(to_year)) {
            message("--> provided `to_year` = ", to_year)
            to_ind <- which(posixlt_vari$year+1900L == to_year)
            if (length(to_ind) == 0) stop("did not find this year in years of input")
            to_ind <- max(to_ind)
            message("--> continue with time points until position ", to_ind)
        } else {
            message("--> `to_year` not provided --> continue with time points until position ", to_ind)
        }
        timeinds <- seq(from_ind, to_ind, b=1L)
        message("--> this yields ", length(timeinds), " wanted time points")
        if (length(timeinds) <= 3) {
            message("--> this is <= 3 --> too short for linear regression --> skip to next variable")
        } else {
            posixct_vari <- posixct_vari[timeinds]
            time_vari_sec <- as.numeric(posixct_vari)
            from_to <- posixlt_vari[range(timeinds)]
            nyears <- diff(from_to$year)+1
            message("\ncalc temporal trends over ", length(timeinds), " timepoints from ", 
                    from_to[1], " to ", from_to[2], " (", nyears, " years; are those dates correct?) at ", nloc, " locations from (",
                    paste(paste0(names(mapping_df), "=", mapping_df[1,]), collapse=","), ") to (", 
                    paste(paste0(names(mapping_df), "=", mapping_df[nloc,]), collapse=","), ") ...")

            ntime_all <- dt_day_all <- slope_day_all <- slope_day_err_all <- t_val_all <- p_val_all <- rep(NA, t=nloc) # allocate
            for (loci in seq_len(nloc)) { # loop through all locations
                locinds <- unlist(mapping_df[loci,])
                locvals <- rep(NA, t=nspatialdims)
                names(locvals) <- names(locinds)
                for (si in seq_len(nspatialdims)) {
                    locvals[si] <- spatial_dims[[si]]$vals[locinds[si]]
                }
                start <- rep(1, t=length(dimnames_var))
                names(start) <- dimnames_var
                count <- start
                start[match(spatialdimnames, dimnames_var)] <- locinds
                start[match(timedimname, dimnames_var)] <- from_ind
                count[match(timedimname, dimnames_var)] <- length(timeinds)
                if (T) {
                    if (loci == 1) pb <- utils::txtProgressBar(min=1, max=ntot, initial=1, style=3) # open progress bar
                    utils::setTxtProgressBar(pb, value=loci) # update progress bar
                } else if (F) {
                    message("loc ", loci, "/", nloc, " (tot loc ", location_inds[loci], "/", ntot, 
                            "), inds: (", paste(paste0(names(mapping_df), "=", locinds, "/", nspatial_vals), collapse=","), 
                            "), vals: (", paste(paste0(names(mapping_df), "=", locvals), collapse=","), 
                            "), start: (", paste(start, collapse=","), "), count=(", paste(count, collapse=","), ") ... ")
                }
                ts <- ncdf4::ncvar_get(nc, varname, start=start, count=count)
                inds <- which(!is.na(ts))
                ninds <- length(inds)
                ok <- F
                if (ninds > 3) {
                    if (lm_method == "stats::lm") {
                        lm <- stats::lm(ts[inds] ~ time_vari_sec[inds])
                        # catch potential warnings/errors
                        # --> e.g. constant time series
                        lms <- base::tryCatch(summary.lm(lm), error=function(e) e, warning=function(w) w) 
                        if (methods::is(lms, "error")) {
                        } else if (methods::is(lms, "warning")) {
                        } else {
                            if (!is.na(lms$coefficients[2,4])) { # if lm was successfull
                                ok <- T
                                dt_day <- as.numeric(difftime(posixct_vari[inds[ninds]], posixct_vari[inds[1]], units="days"))
                                slope_day_a <- lm$coefficients[2]*dt_day*86400
                                #slope_day_b <- lm$fitted.values[ninds] - lm$fitted.values[1]
                                # diff(a,b) = slope_day_a - slope_day_b ~ O(1e-13)
                                slope_day <- slope_day_a
                                slope_day_err <- lms$coefficients[2,"Std. Error"]*dt_day*86400
                                t_val <- lms$coefficients[2,"t value"]
                                p_val <- lms$coefficients[2,"Pr(>|t|)"]
                            }
                        }
                    } else {
                        stop("`lm_method` = ", lm_method, " not implemented")
                    }
                    if (ok) { # save result
                        ntime_all[loci] <- ninds
                        dt_day_all[loci] <- dt_day
                        slope_day_all[loci] <- slope_day
                        slope_day_err_all[loci] <- slope_day_err
                        t_val_all[loci] <- t_val
                        p_val_all[loci] <- p_val
                    }
                } # if more than 3 non-NA values in time series
            } # for loci
                
            # close progress bar
            if (T) base::close(pb)

            if (all(is.na(slope_day_all))) {
                message("all trends are NA. do not save any output. skip to next variable")
            } else { # save output
                save_trend_per_month <- save_trend_per_year <- save_trend_per_decade <- save_trend_per_century <- save_trend_per_millenium <- F # default
                slope_all_per_day <- slope_day_all/dt_day_all
                slope_err_all_per_day <- slope_day_err_all/dt_day_all
                save_trend_per_day <- T
                if (!is.na(any(dt_day_all > 31))) {
                    fac <- 365.25/12
                    slope_all_per_month <- slope_all_per_day*fac
                    slope_err_all_per_month <- slope_err_all_per_day*fac
                    save_trend_per_month <- T
                    if (all(dt_day_all > 31, na.rm=T)) {
                        message("all dt > 31 days --> do not save trend per day")
                        save_trend_per_day <- F 
                    }
                }
                if (!is.na(any(dt_day_all > 366))) {
                    fac <- 365.25
                    slope_all_per_year <- slope_all_per_day*fac
                    slope_err_all_per_year <- slope_err_all_per_day*fac
                    save_trend_per_year <- T
                    if (all(dt_day_all > 366, na.rm=T)) {
                        message("all dt > 366 days --> do not save trend per month")
                        save_trend_per_month <- F
                    }
                }
                if (any(dt_day_all > 10*366, na.rm=T)) {
                    slope_all_per_decade <- slope_all_per_year*10
                    slope_err_all_per_decade <- slope_err_all_per_year*10
                    save_trend_per_decade <- T
                }
                if (any(dt_day_all > 100*366, na.rm=T)) {
                    slope_all_per_century <- slope_all_per_decade*10
                    save_trend_per_century <- T
                }
                if (any(dt_day_all > 1000*366, na.rm=T)) {
                    slope_all_per_millenium <- slope_all_per_century*10
                    save_trend_per_millenium <- T
                }

                message("save ", fouti, " ...")
                varunit <- nc$var[[varname]]$units
                ncvars <- list()
                if (save_trend_per_day) {
                    slope_per_day_var <- ncdf4::ncvar_def(name=paste0(varname, "_per_day"),
                                                          units=paste0(varunit, " / day"), dim=spatial_dims_nc)
                    slope_err_per_day_var <- ncdf4::ncvar_def(name=paste0(varname, "_per_day_err"),
                                                              units=paste0(varunit, " / day"), dim=spatial_dims_nc)
                    ncvars <- c(ncvars, list(slope_per_day_var, slope_err_per_day_var))
                }
                if (save_trend_per_month) {
                    slope_per_month_var <- ncdf4::ncvar_def(name=paste0(varname, "_per_month"),
                                                            units=paste0(varunit, " / month"), dim=spatial_dims_nc)
                    slope_err_per_month_var <- ncdf4::ncvar_def(name=paste0(varname, "_per_month_err"),
                                                                units=paste0(varunit, " / month"), dim=spatial_dims_nc)
                    ncvars <- c(ncvars, list(slope_per_month_var, slope_err_per_month_var))
                }
                if (save_trend_per_year) {
                    slope_per_year_var <- ncdf4::ncvar_def(name=paste0(varname, "_per_year"),
                                                           units=paste0(varunit, " / year"), dim=spatial_dims_nc)
                    slope_err_per_year_var <- ncdf4::ncvar_def(name=paste0(varname, "_per_year_err"),
                                                               units=paste0(varunit, " / year"), dim=spatial_dims_nc)
                    ncvars <- c(ncvars, list(slope_per_year_var, slope_err_per_year_var))
                }
                if (save_trend_per_decade) {
                    slope_per_decade_var <- ncdf4::ncvar_def(name=paste0(varname, "_per_decade"),
                                                             units=paste0(varunit, " / decade"), dim=spatial_dims_nc)
                    slope_err_per_decade_var <- ncdf4::ncvar_def(name=paste0(varname, "_per_decade_err"),
                                                                 units=paste0(varunit, " / decade"), dim=spatial_dims_nc)
                    ncvars <- c(ncvars, list(slope_per_decade_var, slope_err_per_decade_var))
                }
                if (save_trend_per_century) {
                    slope_per_century_var <- ncdf4::ncvar_def(name=paste0(varname, "_per_century"),
                                                              units=paste0(varunit, " / century"), dim=spatial_dims_nc)
                    slope_err_per_century_var <- ncdf4::ncvar_def(name=paste0(varname, "_per_century_err"),
                                                                  units=paste0(varunit, " / century"), dim=spatial_dims_nc)
                    ncvars <- c(ncvars, list(slope_per_century_var, slope_err_per_century_var))
                }
                if (save_trend_per_millenium) {
                    slope_per_millenium_var <- ncdf4::ncvar_def(name=paste0(varname, "_per_millenium"),
                                                                units=paste0(varunit, " / millenium"), dim=spatial_dims_nc)
                    slope_err_per_millenium_var <- ncdf4::ncvar_def(name=paste0(varname, "_per_millenium"),
                                                                    units=paste0(varunit, " / millenium"), dim=spatial_dims_nc)
                    ncvars <- c(ncvars, list(slope_per_millenium_var, slope_err_per_millenium_var))
                }
                ntime_var <- ncdf4::ncvar_def(name=paste0(varname, "_ntime"),
                                              units="", dim=spatial_dims_nc)
                t_val_var <- ncdf4::ncvar_def(name=paste0(varname, "_t_val"),
                                              units="", dim=spatial_dims_nc)
                p_val_var <- ncdf4::ncvar_def(name=paste0(varname, "_p_val"),
                                              units="", dim=spatial_dims_nc)
                outnc <- ncdf4::nc_create(filename=fouti, force_v4=T, 
                                          vars=c(ncvars, list(ntime_var, t_val_var, p_val_var)))
                if (save_trend_per_day) {
                    message("save trends per day (min/max ", min(slope_all_per_day, na.rm=T), "/", max(slope_all_per_day, na.rm=T), ") ...")
                    ncdf4::ncvar_put(nc=outnc, varid=slope_per_day_var, vals=slope_all_per_day)
                    ncdf4::ncvar_put(nc=outnc, varid=slope_err_per_day_var, vals=slope_err_all_per_day)
                }
                if (save_trend_per_month) {
                    message("save trends per month (min/max ", min(slope_all_per_month, na.rm=T), "/", max(slope_all_per_month, na.rm=T), ") ...")
                    ncdf4::ncvar_put(nc=outnc, varid=slope_per_month_var, vals=slope_all_per_month)
                    ncdf4::ncvar_put(nc=outnc, varid=slope_err_per_month_var, vals=slope_err_all_per_month)
                }  
                if (save_trend_per_year) {
                    message("save trends per year (min/max ", min(slope_all_per_year, na.rm=T), "/", max(slope_all_per_year, na.rm=T), ") ...")
                    ncdf4::ncvar_put(nc=outnc, varid=slope_per_year_var, vals=slope_all_per_year)
                    ncdf4::ncvar_put(nc=outnc, varid=slope_err_per_year_var, vals=slope_err_all_per_year)
                }
                if (save_trend_per_decade) {
                    message("save trends per decade (min/max ", min(slope_all_per_decade, na.rm=T), "/", max(slope_all_per_decade, na.rm=T), ") ...")
                    ncdf4::ncvar_put(nc=outnc, varid=slope_per_decade_var, vals=slope_all_per_decade)
                    ncdf4::ncvar_put(nc=outnc, varid=slope_err_per_decade_var, vals=slope_err_all_per_decade)
                }
                if (save_trend_per_millenium) {
                    message("save trends per millenium (min/max ", min(slope_all_per_millenium, na.rm=T), "/", max(slope_all_per_millenium, na.rm=T), ") ...")
                    ncdf4::ncvar_put(nc=outnc, varid=slope_per_millenium_var, vals=slope_all_per_millenium)
                    ncdf4::ncvar_put(nc=outnc, varid=slope_err_per_millenium_var, vals=slope_err_all_per_millenium)
                }
                ncdf4::ncvar_put(nc=outnc, varid=ntime_var, vals=ntime_all)
                ncdf4::ncvar_put(nc=outnc, varid=t_val_var, vals=t_val_all)
                ncdf4::ncvar_put(nc=outnc, varid=p_val_var, vals=p_val_all)
                ncdf4::ncatt_put(nc=outnc, varid=0, "input", fin)
                ncdf4::ncatt_put(nc=outnc, varid=0, "lm_method", lm_method)
                ncdf4::nc_close(outnc)

                # apply griddes of fin
                cmd <- paste0(cdo, " setgrid,", fin, " ", fouti, " ", fouti, "_tmp && mv ", fouti, "_tmp ", fouti)
                message("\nset griddes: run `", cmd, "` ...")
                check <- system(cmd)
                if (check != 0) stop("cmd failed")

            } # if any trend is non-NA
        } # if current var has more than 3 timepoints
        
        if (file.exists(fin_rechunk)) {
            message("remove tmp rechunked ", varname, " file ", fin_rechunk, " ...")
            invisible(file.remove(fin_rechunk))
        }

    } # if trend result of current variable already exists

} # for vari

options(warn=warn) # restore default

message("\nfinished")


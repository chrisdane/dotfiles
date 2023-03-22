#!/usr/bin/env Rscript

# calc non-thermal and thermal components of pCO2 changes following 
# takahashi et al. 2002 (https://www.sciencedirect.com/science/article/pii/S0967064502000036)

if (interactive()) { # test

    rm(list=ls()); graphics.off()
    if (T) { # obs
        fpco2_replace_pattern <- "_spco2_"
        if (T) { # 1 year
            ftemp <- "/work/ba1103/a270073/post/EN.4.2.2/select/temperature/EN.4.2.2_EN.4.2.2_select_temperature_sellevidx_1_global_Jan-Dec_1995.nc"
            fpco2 <- "/work/ba1103/a270073/post/gregor_and_fay_2021/select/spco2/ensmean_gregor_and_fay_2021_select_spco2_global_Jan-Dec_1995.nc"
            outpath <- "/work/ba1103/a270073/post/gregor_and_fay_2021/select"
        } else if (F) { # ymonmean
            ftemp <- "/work/ba1103/a270073/post/EN.4.2.2/ymonmean/temperature/EN.4.2.2_EN.4.2.2_ymonmean_temperature_sellevidx_1_global_Jan-Dec_1982-2014.nc"
            fpco2 <- "/work/ba1103/a270073/post/gregor_and_fay_2021/ymonmean/spco2/ensmean_gregor_and_fay_2021_ymonmean_spco2_global_Jan-Dec_1982-2014.nc"
            outpath <- "/work/ba1103/a270073/post/gregor_and_fay_2021/ymonmean"
        } else if (F) { # many years
            ftemp <- "/work/ba1103/a270073/post/EN.4.2.2/select/temperature/EN.4.2.2_EN.4.2.2_select_temperature_sellevidx_1_global_Jan-Dec_1982-2014.nc"
            fpco2 <- "/work/ba1103/a270073/post/gregor_and_fay_2021/select/spco2/ensmean_gregor_and_fay_2021_select_spco2_global_Jan-Dec_1982-2014.nc"
            outpath <- "/work/ba1103/a270073/post/gregor_and_fay_2021/select"
        }
    } else if (F) { # awicm
        fpco2_replace_pattern <- "_pCO2s_"
        if (F) { # 1 year
            ftemp <- "/work/ba1103/a270073/post/fesom/select/tos/awi-esm-1-1-lr_kh800_historical2_regular_dx1.000_dy1.000_fesom_select_tos_global_Jan-Dec_1995.nc"
            fpco2 <- "/work/ba1103/a270073/post/recom/select/pCO2s/awi-esm-1-1-lr_kh800_historical2_regular_dx1.000_dy1.000_recom_select_pCO2s_global_Jan-Dec_1995.nc"
            outpath <- "/work/ba1103/a270073/post/recom/select"
        } else if (F) { # ymonmean
            ftemp <- "/work/ba1103/a270073/post/fesom/ymonmean/tos/awi-esm-1-1-lr_kh800_historical2_regular_dx1.000_dy1.000_fesom_ymonmean_tos_global_Jan-Dec_1981-2014.nc"
            fpco2 <- "/work/ba1103/a270073/post/recom/ymonmean/pCO2s/awi-esm-1-1-lr_kh800_historical2_regular_dx1.000_dy1.000_recom_ymonmean_pCO2s_global_Jan-Dec_1981-2014.nc"
            outpath <- "/work/ba1103/a270073/post/recom/ymonmean"
        } else if (T) { # many years
            ftemp <- "/work/ba1103/a270073/post/fesom/select/tos/awi-esm-1-1-lr_kh800_historical2_regular_dx1.000_dy1.000_fesom_select_tos_global_Jan-Dec_1981-2014.nc"
            fpco2 <- "/work/ba1103/a270073/post/recom/select/pCO2s/awi-esm-1-1-lr_kh800_historical2_regular_dx1.000_dy1.000_recom_select_pCO2s_global_Jan-Dec_1981-2014.nc"
            outpath <- "/work/ba1103/a270073/post/recom/select"
        }
    }
    temporal_mean <- "timmean" # "timmean" "yearmean"
    #temporal_mean <- "yearmean"
    clean <- T

} else {
    
    stop("asd")
    
} # if interactive or not

message("\ncalculate thermal and non-thermal pCO2 components as in Takahashi et al. 2002:\n",
        "https://www.sciencedirect.com/science/article/pii/S0967064502000036 ...\n")

dir.create(outpath, recursive=T, showWarnings=F)
if (!dir.exists(outpath)) stop("could not create outpath ", outpath)
outpath <- normalizePath(outpath)

cdo <- Sys.which("cdo")
if (cdo == "") stop("could not find cdo")

gamma_T <- 0.0423 # °C^{-1}

# check input
if (length(gregexpr(fpco2_replace_pattern, fpco2)[[1]]) != 1) {
    stop("did not find pattern \"", fpco2_replace_pattern, "\" once in fpco2 ", fpco2)
}
dt_temp <- dt_pco2 <- NULL
for (fi in seq_len(2)) {
    if (fi == 1) cmd <- paste0(cdo, " -s -tinfo ", ftemp)
    if (fi == 2) cmd <- paste0(cdo, " -s -tinfo ", fpco2)
    message("run `", cmd, "` ...")
    dt <- system(cmd, intern=T)
    dt <- dt[which(grepl(" Increment           :", dt))] # e.g. " Increment           :  10 years"
    message("--> \"", dt, "\"")
    dt <- strsplit(dt, ":")[[1]][2] # e.g. "  10 years"
    dt <- trimws(dt) # e.g. "10 years" 
    dt <- gsub("\\s+", "", dt) # e.g. "10years" --> usable by `cdo shifftime`
    message("--> dt = ", dt)
    if (dt == "0seconds") { # `cdo tinfo` only 1 timestep or no success
        message("--> data has only 1 timestep or no proper \"time\" dim found. skip")
    } else { # `cdo tinfo` success
        if (fi == 1) dt_temp <- dt
        if (fi == 2) dt_pco2 <- dt
    } # if `cdo showtimestamp` returned something
} # for fi
if (is.null(dt_temp) || is.null(dt_pco2)) stop("provide temp and pco2 files with a time dim of len > 1")
if (dt_temp != dt_pco2) stop("dt_temp = ", dt_temp, " != dt_pco2 = ", dt_pco2, " not implemented")
if (temporal_mean == "yearmean") {
    if (grepl("year", dt_temp)) {
        stop("wanted `temporal_mean` = \"", temporal_mean, "\" but dt_temp = ", dt_temp, "\n",
             "--> not clear how `\\DeltaT = <T>_", temporal_mean, " - T(", dt_temp, ")` (or vice versa) should be calculated")
    }
}
ntime_temp <- system(paste0(cdo, " -s ntime ", ftemp), intern=T)
ntime_pco2 <- system(paste0(cdo, " -s ntime ", fpco2), intern=T)
message("\n`cdo ntime` temp = \"", ntime_temp, "\"; ntime pco2 = \"", ntime_pco2, "\"; convert to integer ...")
warn <- options()$warn
options(warn=2) # stop on warning
ntime_temp <- as.integer(ntime_temp)
ntime_pco2 <- as.integer(ntime_pco2)
options(warn=warn)
message("--> ntime temp = ", ntime_temp, "; ntime pco2 = ", ntime_pco2)
if (ntime_temp != ntime_pco2) stop("they must be equal")

# calc temp and pco2 temporal means and repeat if necessary
message("\n`temporal_mean` = ", temporal_mean, " --> calc temp and pco2 temporal means and repeat if necessary ...")
for (i in seq_len(2)) {
    if (i == 1) { 
        message(i, ": temp")
        f <- ftemp
        dt <- dt_temp
    } else if (i == 2) {
        message("\n", i, ": pco2")
        f <- fpco2
        dt <- dt_pco2
    }
    dir.create(paste0(outpath, "/T02_", temporal_mean), recursive=T, showWarnings=F)
    f_mean <- paste0(outpath, "/T02_", temporal_mean, "/", tools::file_path_sans_ext(basename(f)), "_T02_", temporal_mean, ".", tools::file_ext(f))
    if (i == 1) ftemp_mean <- f_mean
    if (i == 2) fpco2_mean <- f_mean
    if (file.exists(f_mean)) {
        message("mean file ", f_mean, " already exists")
    } else {
        if (!any(temporal_mean == c("timmean", "yearmean"))) {
            stop("`temporal_mean` = \"", temporal_mean, "\" not implemented. must be \"timmean\" or \"yearmean\"")
        }
        cmd <- paste0(cdo, " -s ", temporal_mean, " ", f, " ", f_mean)
        message("run `", cmd, "` ...")
        check <- system(cmd)
        if (check != 0) stop("cmd failed")

        # repeat temporal means (e.g. annual) for each timepoint of input time series (e.g. monthly)
        # --> if e.g. temporal_mean is "timmean", this is not necessary since this one timmean is automatically repeated by cdo
        # --> if e.g. temporal_mean is "yearmean" and the input time is 1 year, this is also not necessary since ntime_mean = 1
        # --> if e.g. temporal_mean is "yearmean" and the input time is > 1 year, this is necessary
        time_mean <- cdo_showtimestamp(f_mean)
        time_mean <- as.POSIXlt(time_mean)
        message("--> ", length(time_mean), " temporal mean times from ", min(time_mean), " to ", max(time_mean))
        if (length(time_mean) > 1) {
            message("`temporal_mean` = ", temporal_mean, " and dt of the input time series = ", dt, 
                    " --> there are ", length(time_mean), " > 1 timpoints of the temporal mean\n",
                    "--> repeat temporal means for each timepoint of input time series ...")
            outpath_tmp <- paste0(outpath, "/T02_", temporal_mean, "/tmp_", Sys.getpid())
            dir.create(outpath_tmp, recursive=T, showWarnings=F)
            time <- cdo_showtimestamp(f)
            time <- as.POSIXlt(time)
            message("--> ", length(time), " times from ", min(time), " to ", max(time))
            ftmps <- vector("list", l=length(time_mean))
            message("this may take some time for ", length(time_mean), " mean dates ...")
            for (ti in seq_along(time_mean)) {
                if (temporal_mean == "yearmean") {
                    tinds <- which(time$year == time_mean$year[ti])
                } else {
                    stop("`temporal_mean` = ", temporal_mean, " not implemented")
                }
                if (length(tinds) == 0) stop("this should not happen")
                ftmps_ti <- rep(NA, t=length(tinds))
                for (tj in seq_along(ftmps_ti)) {
                    ftmps_ti[tj] <- paste0(outpath_tmp, "/tmp_", basename(f_mean), 
                                           "_ti_", ti, "_of_", length(time_mean), "_time_tj_", tj, "_of_", length(tinds))
                    if (!file.exists(ftmps_ti[tj])) {
                        cmd <- paste0(cdo, " -s",
                                      " -setdate,", format(time[tinds[tj]], "%Y-%m-%d"), 
                                      " -settime,", format(time[tinds[tj]], "%H:%M:%S"), 
                                      " -seltimestep,", ti, " ", f_mean, " ", ftmps_ti[tj])
                        if (F) message("ti ", ti, "/", length(time_mean), ", tj ", tj, "/", length(tinds), ": ", time[tinds[tj]])
                        check <- system(cmd, ignore.stderr=T)
                        if (check != 0) stop("cmd failed")
                    }
                } # for ti
                ftmps[[ti]] <- ftmps_ti
            } # for ti
            # cat all files together
            ftmps <- unlist(ftmps)
            cmd <- paste0(cdo, " -s -O -mergetime ", paste(ftmps, collapse=" "), " ", f_mean) # overwrite
            message("`cdo -s -O -mergetime <", length(ftmps), " repeated ", temporal_mean, " files> ", f_mean, "` ...")
            check <- system(cmd)
            if (check != 0) stop("cmd failed")
            invisible(file.remove(ftmps))
            if (i == 2) invisible(file.remove(outpath_tmp))
        } # if length(time_mean) > 1
    } # if f_mean already exists
} # for i 1:2

# calc non-thermal component `p(<T>) = p(T) exp((<T>-T)gamma)` (T02 eq. 1)
message("\nnon-thermal component (T02 eq. 1) ...")
fnt <- basename(fpco2)
fnt <- sub(fpco2_replace_pattern, paste0("_T02_", temporal_mean, "_nt_"), fnt)
fnt <- paste0(outpath, "/T02_", temporal_mean, "_nt/", fnt)
dir.create(dirname(fnt), recursive=T, showWarnings=F)
if (file.exists(fnt)) {
    message("non-thermal result ", fnt, " already exists")
} else {
    dir.create(paste0(outpath, "/T02_", temporal_mean, "_minus_temp"), recursive=T, showWarnings=F)
    ftemp_mean_minus_temp <- paste0(outpath, "/T02_", temporal_mean, "_minus_temp/", 
                                    tools::file_path_sans_ext(basename(ftemp)), 
                                    "_T02_", temporal_mean, "_minus_temp.", tools::file_ext(ftemp))
    cmd <- paste0(cdo, " -s sub ", ftemp_mean, " ", ftemp, " ", ftemp_mean_minus_temp)
    message("\\DeltaT = <T>_", temporal_mean, " - T(", dt_temp, ")`:\nrun `", cmd, "` ...")
    check <- system(cmd)
    if (check != 0) stop("cmd failed")
    ftmp <- paste0(outpath, "/T02_", temporal_mean, "_minus_temp/", 
                   tools::file_path_sans_ext(basename(fpco2)), 
                   "_T02_", temporal_mean, "_tmp.", tools::file_ext(fpco2))
    cmd <- paste0(cdo, " -s mulc,", gamma_T, " ", ftemp_mean_minus_temp, " ", ftmp)
    message("`\\DeltaT * ", gamma_T, "`:\nrun `", cmd, "` ...")
    check <- system(cmd)
    if (check != 0) stop("cmd failed")
    cmd <- paste0(cdo, " -s exp ", ftmp, " ", ftmp, "_tmp && mv ", ftmp, "_tmp ", ftmp)
    message("`exp(\\DeltaT * ", gamma_T, ")`:\nrun `", cmd, "` ...")
    check <- system(cmd)
    if (check != 0) stop("cmd failed")
    cmd <- paste0(cdo, " -s",
                  " -setattribute,T02_", temporal_mean, "_nt@temp_file:s=", ftemp, 
                  " -setattribute,T02_", temporal_mean, "_nt@pco2_file:s=", fpco2, 
                  " -setattribute,T02_", temporal_mean, "_nt@temporal_mean:s=", temporal_mean, 
                  " -setname,T02_", temporal_mean, "_nt -mul ", fpco2, " ", ftmp, " ", fnt)
    message("`p(", dt_pco2, ") * exp(\\DeltaT * ", gamma_T, ")`:\nrun `", cmd, "` ...")
    check <- system(cmd)
    if (check != 0) stop("cmd failed")
    if (clean) invisible(file.remove(c(ftemp_mean_minus_temp, ftmp)))
} # if fnt already exists

# calc thermal component `p(T) = p(<T>) exp((T-<T>)gamma)` (T02 eq. 2)
message("\nthermal component (T02 eq. 2) ...")
fth <- basename(fpco2)
fth <- sub(fpco2_replace_pattern, paste0("_T02_", temporal_mean, "_th_"), fth)
fth <- paste0(outpath, "/T02_", temporal_mean, "_th/", fth)
dir.create(dirname(fth), recursive=T, showWarnings=F)
if (file.exists(fth)) {
    message("thermal result ", fth, " already exists")
} else {
    dir.create(paste0(outpath, "/T02_temp_minus_", temporal_mean), recursive=T, showWarnings=F)
    ftemp_temp_minus_mean <- paste0(outpath, "/T02_temp_minus_", temporal_mean, "/",
                                    tools::file_path_sans_ext(basename(ftemp)),
                                    "_T02_temp_minus_", temporal_mean, ".", tools::file_ext(ftemp))
    cmd <- paste0(cdo, " -s sub ", ftemp, " ", ftemp_mean, " ", ftemp_temp_minus_mean)
    message("`\\DeltaT = T(", dt_temp, ") - <T>_", temporal_mean, "`:\nrun `", cmd, "` ...")
    check <- system(cmd)
    if (check != 0) stop("cmd failed")
    ftmp <- paste0(outpath, "/T02_temp_minus_", temporal_mean, "/", 
                   tools::file_path_sans_ext(basename(fpco2)),
                   "_T02_", temporal_mean, "_tmp.", tools::file_ext(fpco2))
    cmd <- paste0(cdo, " -s mulc,", gamma_T, " ", ftemp_temp_minus_mean, " ", ftmp)
    message("`\\DeltaT * ", gamma_T, "`:\nrun `", cmd, "` ...")
    check <- system(cmd)
    if (check != 0) stop("cmd failed")
    cmd <- paste0(cdo, " -s exp ", ftmp, " ", ftmp, "_tmp && mv ", ftmp, "_tmp ", ftmp)
    message("`exp(\\DeltaT * ", gamma_T, ")`:\nrun `", cmd, "` ...")
    check <- system(cmd)
    if (check != 0) stop("cmd failed")
    cmd <- paste0(cdo, " -s",
                  " -setattribute,T02_", temporal_mean, "_th@temp_file:s=", ftemp, 
                  " -setattribute,T02_", temporal_mean, "_th@pco2_file:s=", fpco2, 
                  " -setattribute,T02_", temporal_mean, "_th@temporal_mean:s=", temporal_mean, 
                  " -setname,T02_", temporal_mean, "_th -mul ", fpco2_mean, " ", ftmp, " ", fth)
    message("`<p>_", temporal_mean, " * exp(\\DeltaT * ", gamma_T, ")`:\nrun `", cmd, "` ...")
    check <- system(cmd)
    if (check != 0) stop("cmd failed")
    if (clean) invisible(file.remove(c(ftemp_temp_minus_mean, ftmp)))
} # if fth already exists

# calc temporal min/max of non-thermal and thermal components (T02 eq. 3 and 4)
message("\ncalc temporal min/max and their anomaly = max minus min of non-thermal and thermal components (T02 eq. 3 and 4) ...")
fnt_timmin <- basename(fpco2)
fnt_timmin <- sub(fpco2_replace_pattern, paste0("_T02_", temporal_mean, "_nt_timmin_"), fnt_timmin)
fnt_timmin <- paste0(outpath, "/T02_", temporal_mean, "_nt_timmin/", fnt_timmin)
dir.create(dirname(fnt_timmin), recursive=T, showWarnings=F)
cmd <- paste0(cdo, " -s timmin ", fnt, " ", fnt_timmin)
message("run `", cmd, "` ...")
check <- system(cmd)
if (check != 0) stop("cmd failed")
fnt_timmax <- basename(fpco2)
fnt_timmax <- sub(fpco2_replace_pattern, paste0("_T02_", temporal_mean, "_nt_timmax_"), fnt_timmax)
fnt_timmax <- paste0(outpath, "/T02_", temporal_mean, "_nt_timmax/", fnt_timmax)
dir.create(dirname(fnt_timmax), recursive=T, showWarnings=F)
cmd <- paste0(cdo, " -s timmax ", fnt, " ", fnt_timmax)
message("run `", cmd, "` ...")
check <- system(cmd)
if (check != 0) stop("cmd failed")
fnt_timmax_minus_timmin <- basename(fpco2)
fnt_timmax_minus_timmin <- sub(fpco2_replace_pattern, paste0("_T02_", temporal_mean, "_nt_timmax_minus_timmin_"), fnt_timmax_minus_timmin)
fnt_timmax_minus_timmin <- paste0(outpath, "/T02_", temporal_mean, "_nt_timmax_minus_timmin/", fnt_timmax_minus_timmin)
dir.create(dirname(fnt_timmax_minus_timmin), recursive=T, showWarnings=F)
cmd <- paste0(cdo, " -s -setname,T02_", temporal_mean, "_nt_timmax_minus_timmin -sub ", 
              fnt_timmax, " ", fnt_timmin, " ", fnt_timmax_minus_timmin)
message("run `", cmd, "` ...")
check <- system(cmd)
if (check != 0) stop("cmd failed")

fth_timmin <- basename(fpco2)
fth_timmin <- sub(fpco2_replace_pattern, paste0("_T02_", temporal_mean, "_th_timmin_"), fth_timmin)
fth_timmin <- paste0(outpath, "/T02_", temporal_mean, "_th_timmin/", fth_timmin)
dir.create(dirname(fth_timmin), recursive=T, showWarnings=F)
cmd <- paste0(cdo, " -s timmin ", fth, " ", fth_timmin)
message("run `", cmd, "` ...")
check <- system(cmd)
if (check != 0) stop("cmd failed")
fth_timmax <- basename(fpco2)
fth_timmax <- sub(fpco2_replace_pattern, paste0("_T02_", temporal_mean, "_th_timmax_"), fth_timmax)
fth_timmax <- paste0(outpath, "/T02_", temporal_mean, "_th_timmax/", fth_timmax)
dir.create(dirname(fth_timmax), recursive=T, showWarnings=F)
cmd <- paste0(cdo, " -s timmax ", fth, " ", fth_timmax)
message("run `", cmd, "` ...")
check <- system(cmd)
if (check != 0) stop("cmd failed")
fth_timmax_minus_timmin <- basename(fpco2)
fth_timmax_minus_timmin <- sub(fpco2_replace_pattern, paste0("_T02_", temporal_mean, "_th_timmax_minus_timmin_"), fth_timmax_minus_timmin)
fth_timmax_minus_timmin <- paste0(outpath, "/T02_", temporal_mean, "_th_timmax_minus_timmin/", fth_timmax_minus_timmin)
dir.create(dirname(fth_timmax_minus_timmin), recursive=T, showWarnings=F)
cmd <- paste0(cdo, " -s -setname,T02_", temporal_mean, "_th_timmax_minus_timmin -sub ", 
              fth_timmax, " ", fth_timmin, " ", fth_timmax_minus_timmin)
message("run `", cmd, "` ...")
check <- system(cmd)
if (check != 0) stop("cmd failed")

# calc anomaly = thermal minus non-thermal and ratio = thermal/non-thermal (T02 eq. 5)
message("\ncalc anomaly non-thermal minus thermal effects = (timmax-timmin)_thermal minus (timmax-timmin)_non-thermal (and ratio) (T02 eq. 5) ...")
fth_minus_fnt <- basename(fpco2)
fth_minus_fnt <- sub(fpco2_replace_pattern, paste0("_T02_", temporal_mean, "_th_minus_nt_"), fth_minus_fnt)
fth_minus_fnt <- paste0(outpath, "/T02_", temporal_mean, "_th_minus_nt/", fth_minus_fnt)
dir.create(dirname(fth_minus_fnt), recursive=T, showWarnings=F)
cmd <- paste0(cdo, " -s -setname,T02_", temporal_mean, "_th_minus_nt -sub ", 
              fth_timmax_minus_timmin, " ", fnt_timmax_minus_timmin, " ", fth_minus_fnt)
message("run `", cmd, "` ...")
check <- system(cmd)
if (check != 0) stop("cmd failed")
fth_over_fnt <- basename(fpco2)
fth_over_fnt <- sub(fpco2_replace_pattern, paste0("_T02_", temporal_mean, "_th_over_nt_"), fth_over_fnt)
fth_over_fnt <- paste0(outpath, "/T02_", temporal_mean, "_th_over_nt/", fth_over_fnt)
dir.create(dirname(fth_over_fnt), recursive=T, showWarnings=F)
cmd <- paste0(cdo, " -s -setname,T02_", temporal_mean, "_th_over_nt -div ", 
              fth_timmax_minus_timmin, " ", fnt_timmax_minus_timmin, " ", fth_over_fnt)
message("run `", cmd, "` ...")
check <- system(cmd)
if (check != 0) stop("cmd failed")

if (clean) invisible(file.remove(c(ftemp_mean, fpco2_mean, 
                                   fnt_timmin, fnt_timmax, 
                                   fth_timmin, fth_timmax)))

message("\nfinished\n")


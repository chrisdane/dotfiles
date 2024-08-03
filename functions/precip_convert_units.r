#!/usr/bin/env Rscript

if (interactive()) {
    files <- list.files("/work/ab0246/a270073/data/dwd/data/GPCC/full_data_monthly_v2022/10", pattern=glob2rx("full_data_monthly_*.nc"), full.names=T)
    mode <- "mm_month2day"
    outdir <- paste0("/work/ab0246/a270073/data/dwd/post/GPCC/full_data_monthly_v2022/10/", mode)
} else {
    args <- commandArgs(trailingOnly=F)
    me <- basename(sub("--file=", "", args[grep("--file=", args)]))
    args <- commandArgs(trailingOnly=T)

    # check which mode is executed
    if (grepl("mm_month2day", me)) {
        mode <- "mm_month2day"
    } else if (grepl("mm_day2month", me)) {
        mode <- "mm_day2month"
    } else {
        stop("script ", me, " not implemented")
    }
    if (any(grepl("^outdir=", args))) {
        ind <- which(grepl("^outdir=", args))
        if (length(ind) != 1) stop("found ", length(ind), " args that start with \"outdir=\". must be 1")
        outdir <- sub("outdir=", "", args[ind])
        args <- args[-ind]
    } else {
        stop("provide `outdir=/path/to/save/result")
    }
    files <- args
} # interactive

cdo <- Sys.which("cdo")
if (cdo == "") stop("could not find cdo")
warn <- options()$warn
dir.create(outdir, recursive=T, showWarnings=F)
if (!dir.exists(outdir)) stop("could not create outdir ", outdir)

cdo_showtimestamp <- function(file) {
    if (!file.exists(file)) stop("provided `file` = ", file, " does not exist")
    cdo <- Sys.which("cdo")
    if (cdo == "") stop("could not find cdo")
    if (getRversion() < "3.2.0") stop("R must be >= 3.2.0 to have base::trimws()")
    return(as.POSIXct(strsplit(trimws(system(paste0(cdo, " -s showtimestamp ", file), intern=T)), "  ")[[1]], format="%Y-%m-%dT%H:%M:%S", tz="UTC"))
} # cdo_showtimestamp
is.leap <- function(years) {
    return(((years %% 4 == 0) & (years %% 100 != 0)) | (years %% 400 == 0))
}
dpm <- c(Jan=31, Feb=28, Mar=31, Apr=30, May=31, Jun=30, Jul=31, Aug=31, Sep=30, Oct=31, Nov=30, Dec=31)

######################################################################

for (fi in seq_along(files)) {
    message("*********************************************************\n",
            "file ", fi, "/", length(files), ": ", files[fi], " ...")
    time <- cdo_showtimestamp(files[fi])
    message(length(time), " time points from ", min(time), " to ", max(time), ":")
    cat(capture.output(str(time)), sep="\n")
    time <- as.POSIXlt(time)
    fouts <- rep(NA, t=length(time))
    
    for (ti in seq_along(time)) {
        ndays_per_month <- dpm[time[ti]$mon+1]
        if (time[ti]$mon+1 == 2 && is.leap(time[ti]$year+1900)) ndays_per_month <- 29
        if (T) message("time ", ti, "/", length(time), ": ", time[ti], " has ", ndays_per_month, " days per month")
        
        fout <- paste0(outdir, "/", tools::file_path_sans_ext(basename(files[fi])), 
                       "_", mode, "_ti_", sprintf(paste0("%0", nchar(length(time)), "i"), ti), "_of_", length(time), ".", tools::file_ext(files[fi]))
        fouts[ti] <- fout
        if (file.exists(fout)) {
            message("fout ", fout, " already exists. skip")
        } else {
            if (mode == "mm_day2month") { # mm day-1 --> mm month-1
                cmd <- paste0(cdo, " -s -setunit,'mm month-1' -mulc,", ndays_per_month)
            } else if (mode == "mm_month2day") { # mm month-1 --> mm day-1
                cmd <- paste0(cdo, " -s -setunit,'mm day-1' -divc,", ndays_per_month)
            }
            cmd <- paste0(cmd, " -seltimestep,", ti, " ", files[fi], " ", fout)
            message("run `", cmd, "` ...")
            check <- system(cmd)
            if (check != 0) stop("error")

        } # if fout already exists
    } # for ti

    # cat result
    message("\ncat ", length(fouts), " files ...")
    fout <- paste0(outdir, "/", tools::file_path_sans_ext(basename(files[fi])), "_", mode, ".", tools::file_ext(files[fi]))
    if (file.exists(fout)) {
        message("fout ", fout, " already exists. skip")
    } else {
        cmd <- paste0(cdo, " cat ", paste(fouts, collapse=" "), " ", fout)
        message("run `", cmd, "` ...")
        check <- system(cmd)
        if (check != 0) stop("error")
    }

    # clean
    invisible(file.remove(fouts))

} # for fi

message("\nfinished\n")


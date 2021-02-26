#!/usr/bin/env Rscript

args <- commandArgs(trailingOnly=F)
me <- basename(sub("--file=", "", args[grep("--file=", args)]))
args <- commandArgs(trailingOnly=T)

usage <- paste0("\nUsage:\n $ ", me, " season in.nc out.nc\n",
                "\n",
                "    season = e.g. \"12,1,2\" or \"11,12,1,2,3\" (the '\"' are not needed) for annual averages over ",
                "these year-crossing seasons\n",
                "\n",
                "  The order of `season` is important: \"1,2,12\" will average over months 1,2,12 of year n, while\n",
                "  \"12,1,2\" calculates the average of year n over month 12 of year n-1 and the months 1 and 2 of year n.\n",
                "  If season = e.g. \"1,2,3\", i.e. if annual averages over a non-year-crossing season is requested, \n",
                "  the default `cdo seasmean,1,2,3 in.nc out.nc` is executed.",
                "\n")
# check
if (length(args) != 3) {
    message(usage)
    quit()
}

# check 1st arg: season
season <- args[1]
if (grepl(",", season)) { # "," in season
    season <- strsplit(season, ",")[[1]]
}
season <- as.integer(season)
if (any(is.na(season))) {
    stop("conversion of provided season = \"", args[1], "\" to integer failed.")
}
if (any(is.na(match(season, 1:12L)))) {
    errinds <- which(is.na(match(season, 1:12L)))
    stop("provided season month", ifelse(length(errinds) > 1, "s", ""), " ", 
         paste(season[errinds], collapse=","), " not in (1,2,...,11,12).")
}
# check if wanted season is year-crossing
if (any(diff(season) < 0)) { # season is year-crossing
    year_crossing_season <- T
    message("wanted season \"", paste(season, collapse=","), "\" is year-crossing")
    if (length(which(diff(season) < 0)) > 1) {
        stop("however, ", length(which(diff(season) < 0)), " year-crossing months were detected by ",
             "`diff(", paste(season, collapse=","), ") = ", paste(diff(season), collapse=","), 
             " < 0`. must be one.")
    }
} else {
    year_crossing_season <- F
    season <- paste(unique(season), collapse=",")
    message("wanted season \"", season, "\" is not year-crossing")
}

# check input file
fin <- args[2]
if (!file.exists(fin)) stop("fin = \"", fin, "\" does not exist")
indir <- normalizePath(dirname(fin))

# check output file
fout <- args[3]
if (file.exists(fout)) stop("fout = \"", fout, "\" already exists")
outdir <- normalizePath(dirname(fout))
if (file.access(outdir, mode=0) == -1) { # not existing
    message("outdir = \"", outdir, "\" does not exist. try to create ... ", appendLF=F)
    dir.create(outdir, recursive=T)
    if (!file.exists(outdir)) {
        stop("not successful. error msg:")
    } else {
        message("ok")
    }
} else { # outdir exists
    if (file.access(outdir, mode=2) == -1) { # not writable
        stop("provided outdir = \"", outdir, "\" not writeable.")
    }
}
outdir <- normalizePath(outdir)
#message("--> outdir = \"", outdir, "\"")

# check if cdo exists
if (Sys.which("cdo") == "") stop("could not find cdo program")
cdo_version <- paste0("cdo --version 2>&1")
cdo_version <- system(cdo_version, intern=T)
cdo_version <- cdo_version[1] # e.g. "Climate Data Operators version 1.7.0 (http://mpimet.mpg.de/cdo)"
cdo_version <- strsplit(cdo_version, " ")[[1]]
cdo_version <- sapply(cdo_version, strsplit, split="\\.")
cdo_version <- suppressWarnings(lapply(cdo_version, as.numeric))
tmp <- sapply(cdo_version, is.na)
tmp <- lapply(tmp, "==", "FALSE")
tmp <- sapply(tmp, any)
if (any(tmp)) {
    if (length(which(tmp)) == 1) {
        cdo_version <- cdo_version[[which(tmp)]] # numeric vector of length 3; e.g.: 1 7 0
    } else {
        stop("the case of more than 1 as.numeric() of `cdo --version?` is not implemented here")
    }
} else {
    stop("the case of 0 as.numeric() of `cdo --version?` is not implemented here")
}
#message("cdo_version = ", paste(cdo_version, collapse="."))

# calc seasonal mean
if (!year_crossing_season) {
    cmd <- paste0("cdo -seasmean,", season, " ", fin, " ", fout)

} else if (year_crossing_season) {
    cross_ind <- which(diff(season) < 0)
    months_year_nminus1 <- season[1:cross_ind]
    months_year_n <- season[(cross_ind+1):length(season)]
    if (length(months_year_nminus1) == 0) {
        stop("found zero months of year n-1. this should not happen")
    }
    if (length(months_year_n) == 0) {
        stop("found zero months of year n. this should not happen")
    }
    # remove any duplicated 
    months_year_nminus1 <- unique(months_year_nminus1)
    months_year_n <- unique(months_year_n)
    message("  --> ", length(months_year_nminus1), " months of year n-1: ", paste(months_year_nminus1, collapse=","))
    message("  --> ", length(months_year_n), " months of year n: ", paste(months_year_n, collapse=","))

    # get years of input
    # from R > 3.2 in case used R version is <= 3.2
    trimws <- function (x, which = c("both", "left", "right"), whitespace = "[ \t\r\n]")
    {
        which <- match.arg(which)
        mysub <- function(re, x) sub(re, "", x, perl = TRUE)
        switch(which, left = mysub(paste0("^", whitespace, "+"),
            x), right = mysub(paste0(whitespace, "+$"), x), both = mysub(paste0(whitespace,
            "+$"), mysub(paste0("^", whitespace, "+"), x)))
    }
    # in case of many years and old R versions, `cdo showyear` may raise warning:
    #    line 1 may be truncated in call to system(, intern = TRUE)
    # --> prevent warnings with `suppressWarnings`
    # --> in this case, the result is chunked based on some internal `n_max`
    cmd <- paste0("cdo -s showyear ", fin)
    message("get input years with `", cmd, "` ...")
    years <- suppressWarnings(system(cmd, intern=T))
    if (length(years) > 1) years <- paste(years, collapse=" ") # in case the warning was rasied 
    years <- as.integer(strsplit(trimws(years), "\\s+")[[1]])
    year_range <- range(years)
    message("  --> input years from ", year_range[1], " to ", year_range[2])
    if (any(is.na(year_range))) stop("this should not happen")

    # construct cdo command:
    # for 11,12,1,2,3:
    # cdo -shifttime,3mo -yearmean -selyear,6970/6999 -selmon,8,9,10,11,12 -shifttime,-3mo in_6971-7000.nc out.nc
    # for 12,1,2:
    # cdo -shifttime,2mo -yearmean -selyear,6970/6999 -selmon,10,11,12 -shifttime,-2mo in_6971-7000.nc out.nc
    shifttime_mo <- max(months_year_n)
    months_year_nminus1_shifttime <- months_year_nminus1 - shifttime_mo
    if (any(months_year_nminus1_shifttime < 0)) {
        stop("months of year n-1 minus ", shifttime_mo, " months = ", 
             paste(months_year_nminus1_shifttime, collapse=","), " < 0. this is not allowed.")
    }    
    months_year_n_shifftime <- months_year_n + 12L - shifttime_mo
    
    cmd <- paste0("cdo -shifttime,", shifttime_mo, "mo ",
                  "-yearmean ",
                  "-selyear,", year_range[1]-1, "/", year_range[2]-1, " ",
                  "-selmon,", paste(months_year_nminus1_shifttime, collapse=","),
                         ",", paste(months_year_n_shifftime, collapse=","), " ",
                  "-shifttime,-", shifttime_mo, "mo ",
                  fin, " ", fout)
}

# run cdo command 
message("run `", cmd, "` ...")
system(cmd)


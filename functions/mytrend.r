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
    } else if (T) {
        args <- c("--fin=/work/ba1103/a270073/post/CanESM5-CanOE/select/fgco2/CanESM5-CanOE_historical_and_ssp126_r1i1p2f1_gn_CanESM5-CanOE_select_fgco2_global_Jan-Dec_1982-2019.nc,/work/ba1103/a270073/post/CanESM5-CanOE/select/mlotst/CanESM5-CanOE_historical_and_ssp126_r1i1p2f1_gn_CanESM5-CanOE_select_mlotst_global_Jan-Dec_1982-2019.nc",
                  "--fout=/work/ba1103/a270073/post/CanESM5-CanOE/select/fgco2/test_lm_<varname1>_as_<varname2>.nc",
                  "--spatialdimnames=i,j"
                  , "--from_year=2015"
                  , "--operator=\"*,/\""
                  , "--operator_value=-1,-1")
    } else if (F) {
        args <- c("--fin=/work/ba1103/a270073/post/EN.4.2.2/select/rho/EN.4.2.2.f.analysis.g10.202111_rho.nc",
                  "--fout=/work/ba1103/a270073/post/EN4.2.2/select/lm_rho_as_time/EN.4.2.2.f.analysis.g10.202111_<varname>.nc")
    } else if (F) {
        args <- c("--spatialdimnames=rgrid",
                  "--fin=~/test/era5_sf00_1M_era5_select_WS10_global_Jan-Dec_1970-1970.nc",
                  "--fout=~/test/fu_<varname>.nc")
    } else if (F) {
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
if (any(grepl("--operator", args))) {
    operator <- sub("--operator=", "", args[grep("--operator=", args)])
    message("provided `operator` = ", operator)
    operator <- strsplit(operator, ",")[[1]] # "a" or "a,a"
    operator <- sub("\"", "", operator)
    operator <- sub("'", "", operator)
} else {
    operator <- NULL # default
    message("`--operator=<+,-,*,/>` not provided --> use default: none")
}
if (any(grepl("--operator_value", args))) {
    operator_value <- sub("--operator_value=", "", args[grep("--operator_value=", args)])
    message("provided `operator_value` = ", operator_value)
    operator_value <- strsplit(operator_value, ",")[[1]] # "a" or "a,a"
} else {
    operator_value <- NULL # default
    message("`--operator_value=<number>` not provided --> use default: none")
}
if (!is.null(operator) && is.null(operator_value)) stop("`operator` provided but not `operator_value`")
if (is.null(operator) && !is.null(operator_value)) stop("`operator_value` provided but not `operator`")
if (!is.null(operator)) {
    if (length(operator) != length(operator_value)) stop("`operator` and `operator_value` must be of same length")
    for (opi in seq_along(operator)) {
        if (operator[opi] == "none" && operator_value[opi] != "none") {
            stop("`operator[", opi, "]` = ", operator[opi], " but `operator_value[", opi, "]` = ", operator_value[opi])
        }
    }
}

if (any(grepl("--fin", args))) {
    fin <- sub("--fin=", "", args[grep("--fin=", args)])
    message("provided `fin` = ", fin)
    fin <- strsplit(fin, ",")[[1]]
} else {
    stop("provide `--fin=[/path/to/]fin.nc`")
}
if (any(!file.exists(fin))) stop("some input file does not exist")
fin <- normalizePath(fin)
if (length(fin) == 1) {
    mode <- "var_vs_time" # all varnames vs time
} else if (length(fin) == 2) {
    mode <- "var1_vs_var2" # varnames[1] vs varnames[2]
    source("~/scripts/r/functions/myfunctions.r") # identical_list
    if (!is.null(operator)) {
        if (length(operator) != 2) stop("`operator` and `operator_value` need to be provided for every file. e.g. `operator=\"none,*\"` is possible")
    }
} else {
    stop("`--fin=/path/to/file[,/path/to/file2]` must be either one or two files")
}

if (any(grepl("--fout", args))) {
    fout <- sub("--fout=", "", args[grep("--fout=", args)])
    message("provided `fout` = ", fout)
} else {
    stop("provide `--fout=[/path/to/]fout_<varname>.nc`")
}
if (mode == "var_vs_time") {
    if (!grepl("<varname>", fout)) stop("provide \"<varname>\" at wanted position in `fout` = ", fout)
} else if (mode == "var1_vs_var2") {
    if (!grepl("<varname1>", fout)) stop("provide \"<varname1>\" at wanted position in `fout` = ", fout)
    if (!grepl("<varname2>", fout)) stop("provide \"<varname2>\" at wanted position in `fout` = ", fout)
}
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
    if (mode == "var1_vs_var2") {
        if (length(varnames) != 2) stop("2 files are provided but not 2 varnames (`varnames` = ", 
                                        paste(varnames, collapse=", "), 
                                        "). provide `varnames=varname_of_file1,varname_of_file2`.")
    }
} else {
    message("`--varnames=<varnames>` not provided --> use default")
    varnames <- vector("list", l=length(fin))
    for (fi in seq_along(fin)) {
        cmd <- paste0(cdo, " -s showname ", fin[fi]) # get varnames of fin
        message("run `", cmd, "` ...")
        varnames[[fi]] <- strsplit(trimws(system(cmd, intern=T)), " ")[[1]]
        if (length(varnames[[fi]]) == 0) stop("file has zero variables")
        message("--> file ", fi, " has ", length(varnames[[fi]]), " variables: ", paste(varnames[[fi]], collapse=", "))
    }
    if (mode == "var1_vs_var2") { # length(fin) == 2
        if (!all(sapply(varnames, length) == 1)) {
            stop("when 2 files are provided each of them must have only one variable")
        }
    }
    varnames <- unlist(varnames)
}
message("\nload ncdf4 package ...")
library(ncdf4)

# open file(s)
nc <- time_all <- timedimid_nc <- posixct_all <- dimids_nc <- vector("list", l=length(fin))
for (fi in seq_along(fin)) {
    message("open ", fin[fi], " ...")
    nc[[fi]] <- ncdf4::nc_open(fin[fi])

    # get complete time dim of file
    if (!any(names(nc[[fi]]$dim) == timedimname)) stop("not any dim of this file has a dim named \"", timedimname, "\". rerun with `--timedimname=<time dim name>`")
    time_all[[fi]] <- nc[[fi]]$dim[[timedimname]]$vals
    if (length(time_all[[fi]]) <= 3) stop("time dim \"", timedimname, "\" of this file is of length ", length(time_all[[fi]]), " <= 3 --> too short for linear regression")
    timedimid_nc[[fi]] <- nc[[fi]]$dim[[timedimname]]$id
    posixct_all[[fi]] <- as.POSIXct(strsplit(trimws(system(paste0("cdo -s showtimestamp ", fin[fi]), intern=T)), "  ")[[1]], tz="UTC")
    dimids_nc[[fi]] <- sapply(nc[[fi]]$dim, "[[", "id")
    # e.g.    time     bnds        i        j vertices 
    #            0        1        2        3        4
} # for fi

# calc lm for all variables
if (mode == "var_vs_time") {
    nit <- length(varnames)
} else if (mode == "var1_vs_var2") {
    nit <- 1
}
for (vari in seq_len(nit)) {
    if (mode == "var_vs_time") {
        varname <- varnames[vari]
        message("calc regression of variable ", vari, "/", length(varnames), ": \"", varname, "\" against time (temporal trend) ...")
    } else if (mode == "var1_vs_var2") {
        message("calc regression of variable1 ", varnames[1], " against variable 2 ", varnames[2], " ...")
    }
    # check if trend of current variable already exists
    if (mode == "var_vs_time") {
        fouti <- sub("<varname>", varname, fout)
    } else if (mode == "var1_vs_var2") {        
        fouti <- sub("<varname1>", varnames[1], fout)
        fouti <- sub("<varname2>", varnames[2], fouti)
    }
    if (file.exists(fouti)) {
        message("result ", fouti, " already exists. skip")
    
    } else { # lm result does not exist yet
    
        # check variable dimnames
        dimids_var <- dimids_var_nc_inds <- dimnames_var <- vector("list", l=length(fin))
        for (fi in seq_along(fin)) {
            if (mode == "var_vs_time") {
                dimids_var[[fi]] <- nc[[fi]]$var[[varname]]$dimids # e.g. 2 3 0 = i j time
            } else if (mode == "var1_vs_var2") {
                dimids_var[[fi]] <- nc[[fi]]$var[[varnames[fi]]]$dimids # e.g. 2 3 0 = i j time
            }
            dimids_var_nc_inds[[fi]] <- match(dimids_var[[fi]], dimids_nc[[fi]]) # e.g. 3 4 1 = i j time
            dimnames_var[[fi]] <- names(dimids_nc[[fi]])[dimids_var_nc_inds[[fi]]]
            names(dimids_var[[fi]]) <- names(dimids_var_nc_inds[[fi]]) <- dimnames_var[[fi]]
            if (any(is.na(match(spatialdimnames[[fi]], dimnames_var[[fi]])))) {
                msg <- paste0("provided ", nspatialdims, " spatial dims \"", paste(spatialdimnames[[fi]], collapse="\", \""), 
                              "\" but these are not in the ", length(dimnames_var[[fi]]), " dims of variable ")
                if (mode == "var_vs_time") {
                    msg <- paste0(msg, varname)
                } else if (mode == "var1_vs_var2") {
                    msg <- paste0(msg, varnames[fi])
                }
                msg <- paste0(msg, ": \"", paste(dimnames_var[[fi]], collapse="\", \""), 
                              "\"\n--> provide `--spatialdimnames=<spatialdimname1[,spatialdimname2,...,spatialdimnameN]>` and rerun script")
                stop(msg)
            }
        } # for fi

        # `dimids_var`, `dimids_var_nc_inds`, `dimnames_var` are of length `length(fin)`

        # check spatial dim(s)
        spatial_dims <- vector("list", l=length(fin))
        spatial_dims_nc <- spatial_dims
        for (fi in seq_along(fin)) {
            tmp <- vector("list", l=nspatialdims)
            names(tmp) <- spatialdimnames
            tmp2 <- tmp
            for (si in seq_len(nspatialdims)) {
                tmp[[si]] <- list(len=nc[[fi]]$dim[[spatialdimnames[si]]]$len,
                                  vals=nc[[fi]]$dim[[spatialdimnames[si]]]$vals,
                                  units=nc[[fi]]$dim[[spatialdimnames[si]]]$units)
                tmp2[[si]] <- nc[[fi]]$dim[[spatialdimnames[si]]]
            } # for si
            spatial_dims[[fi]] <- tmp
            spatial_dims_nc[[fi]] <- tmp2
        } # for fi
        if (mode == "var1_vs_var2") {
            if (!identical_list(spatial_dims)) stop("not all spatial dims are equal across the files")
        }
        spatial_dims <- spatial_dims[[1]]
        spatial_dims_nc <- spatial_dims_nc[[1]]
        nspatial_vals <- sapply(spatial_dims, "[[", "len")
        ntot <- prod(nspatial_vals)
        
        # check chunks of current variable
        fin_rechunk <- rep(NA, t=length(fin))
        for (fi in seq_along(fin)) {
            if (mode == "var_vs_time") {
                chunksizes <- nc[[fi]]$var[[varname]]$chunksizes # for old ncdf4 < 1.19 versions this may return NA although variable is chunked
            } else if (mode == "var1_vs_var2") {
                chunksizes <- nc[[fi]]$var[[varnames[fi]]]$chunksizes # for old ncdf4 < 1.19 versions this may return NA although variable is chunked
            }
            if (mode == "var_vs_time") {
                msg <- paste0("variable ", varname, " of this file")
            } else if (mode == "var1_vs_var2") {
                msg <- paste0("variable ", varnames[fi], " of file ", fi)
            }
            ok <- T
            if (all(is.na(chunksizes))) { # variable is not chunked
                msg <- paste0(msg, " is not chunked")
                ok <- F
            } else { # variable is chunked
                names(chunksizes) <- dimnames_var[[fi]]
                msg <- paste0(msg, " is chunked (", 
                              paste(paste0(names(chunksizes), "=", chunksizes), collapse=","), ")\n")
                # does the time dim chunk length equal the complete time dim length?
                if (chunksizes[which(dimnames_var[[fi]] == timedimname)] != nc[[fi]]$dim[[timedimname]]$len) {
                    ok <- F
                }
            }
            if (!ok) { # variable is not chunked or does not contain complete time dim
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
                              "--> rechunk data with `nccopy -u -w -c time/", nc[[fi]]$dim[[timedimname]]$len, ",",
                              paste(paste0(spatialdimnames, "/", msg_dims), collapse=","), " in out` and rerun script")
                message("\n", msg)
                if (auto_rechunk) {
                    message("--> `auto_rechunk` = true --> rechunk with nccopy ...")
                    nccopy <- Sys.which("nccopy")
                    if (nccopy == "") {
                        message("--> did not find `nccopy` --> cannot rechunk input data --> continue with original file ...")
                    } else {
                        # rechunk with nccopy
                        fin_rechunk[fi] <- paste0(outpath, "/tmp_rechunk_pid", Sys.getpid(), "_", basename(fin[fi]))
                        if (file.exists(fin_rechunk[fi])) {
                            message("tmp rechunked file ", fin_rechunk[fi], " already exists --> skip nccopy cmd ...")
                        } else {
                            # cmd 1: select variable
                            if (mode == "var_vs_time") {
                                cmd_rechunk <- paste0(cdo, " -select,name=", varname, " ", fin[fi], " ", fin_rechunk[fi])
                            } else if (mode == "var1_vs_var2") {
                                cmd_rechunk <- paste0(cdo, " -select,name=", varnames[fi], " ", fin[fi], " ", fin_rechunk[fi])
                            }
                            message("1: select: run `", cmd_rechunk, "` ...")
                            check <- system(cmd_rechunk)
                            if (check != 0) stop("cmd failed")
                            # cmd 2: rechunk
                            # -w: rechunk file completely in memory --> this is faster but needs large memory --> if OOM, run wout -w
                            cmd_rechunk <- paste0(nccopy, " -u -w -c ", timedimname, "/", nc[[fi]]$dim[[timedimname]]$len, ",",
                                                  paste(paste0(spatialdimnames, "/", msg_dims), collapse=","), " ", fin[fi], " ", fin_rechunk[fi])
                            message("2: rechunk: run `", cmd_rechunk, "` ...")
                            check <- system(cmd_rechunk)
                            if (check != 0) stop("cmd failed")
                        }
                        # update fin
                        message("read rechunked data ", fin_rechunk[fi], " ...")
                        ncdf4::nc_close(nc[[fi]]) # un- or wrongly chunked original fin
                        nc[[fi]] <- ncdf4::nc_open(fin_rechunk[fi])
                        if (mode == "var_vs_time") {
                            chunksizes <- nc[[fi]]$var[[varname]]$chunksizes
                        } else if (mode == "var1_vs_var2") {
                            chunksizes <- nc[[fi]]$var[[varnames[fi]]]$chunksizes
                        }
                        if (all(is.na(chunksizes))) {
                            message("chunksizes are still NA. this should not happen (ncdf4 package version ", 
                                    utils::packageDescription("ncdf4")$Version, " should be >= 1.19). continue with unchunked/wrongly chunked variable ...")
                        } else {
                            names(chunksizes) <- dimnames_var[[fi]]
                            message("--> chunksizes = (", paste(paste0(names(chunksizes), "=", chunksizes), collapse=","), ")")
                        }
                    } # if nccopy is available
                } else { # if auto_rechunk is false
                    message("--> or rerun script with `--auto_rechunk=true`")
                } # if auto_rechunk
            } # is msg is not null
        } # for fi

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
        timedimind_var <- time_vari <- time_vari_inds_in_all <- posixct_vari <- posixlt_vari <- from_ind <- timeinds <- vector("list", l=length(fin))
        for (fi in seq_along(fin)) {
            if (mode == "var_vs_time") {
                timedimind_var[[fi]] <- which(sapply(nc[[fi]]$var[[varname]]$dim, "[[", "id") == timedimid_nc[[fi]])
                time_vari[[fi]] <- nc[[fi]]$var[[varname]]$dim[[timedimind_var[[fi]]]]$vals
            } else if (mode == "var1_vs_var2") {
                timedimind_var[[fi]] <- which(sapply(nc[[fi]]$var[[varnames[fi]]]$dim, "[[", "id") == timedimid_nc[[fi]])
                time_vari[[fi]] <- nc[[fi]]$var[[varnames[fi]]]$dim[[timedimind_var[[fi]]]]$vals
            }
            time_vari_inds_in_all[[fi]] <- match(time_vari[[fi]], time_all[[fi]])
            posixct_vari[[fi]] <- posixct_all[[fi]][time_vari_inds_in_all[[fi]]]
            posixlt_vari[[fi]] <- as.POSIXlt(posixct_vari[[fi]])
            message("variable has ", length(time_vari[[fi]]), " timepoints from ", min(posixct_vari[[fi]]), " to ", max(posixct_vari[[fi]]))
            from_ind[[fi]] <- min(time_vari_inds_in_all[[fi]])
            if (!is.null(from_year)) {
                message("--> provided `from_year` = ", from_year)
                from_ind[[fi]] <- which(posixlt_vari[[fi]]$year+1900L == from_year)
                if (length(from_ind[[fi]]) == 0) stop("did not find this year in years of input")
                from_ind[[fi]] <- min(from_ind[[fi]])
                message("--> continue with time points from position ", from_ind[[fi]])
            } else {
                message("--> `from_year` not provided --> continue with time points from position ", from_ind[[fi]])
            }
            to_ind <- max(time_vari_inds_in_all[[fi]])
            if (!is.null(to_year)) {
                message("--> provided `to_year` = ", to_year)
                to_ind <- which(posixlt_vari[[fi]]$year+1900L == to_year)
                if (length(to_ind) == 0) stop("did not find this year in years of input")
                to_ind <- max(to_ind)
                message("--> continue with time points until position ", to_ind)
            } else {
                message("--> `to_year` not provided --> continue with time points until position ", to_ind)
            }
            timeinds[[fi]] <- seq(from_ind[[fi]], to_ind, b=1L)
            message("--> this yields ", length(timeinds[[fi]]), " wanted time points")
        } # for fi

        ntimeinds <- sapply(timeinds, length)
        if (length(unique(ntimeinds)) != 1) {
            stop("--> number of wanted time points differs between files")
        }

        if (any(ntimeinds <= 3)) {
            message("--> time dim length of some variable is <= 3 --> too short for linear regression. skip")
        } else {
            time_vari_sec <- from_to <- nyears <- vector("list", l=length(fin))
            for (fi in seq_along(fin)) {
                posixct_vari[[fi]] <- posixct_vari[[fi]][timeinds[[fi]]]
                posixlt_vari[[fi]] <- posixlt_vari[[fi]][timeinds[[fi]]]
                time_vari_sec[[fi]] <- as.numeric(posixct_vari[[fi]])
                from_to[[fi]] <- range(posixlt_vari[[fi]])
                nyears[[fi]] <- diff(from_to[[fi]]$year)+1
            } # for fi
            message("from_to:")
            cat(capture.output(str(from_to)), sep="\n")
            if (mode == "var1_vs_var2") {
                if (!identical_list(from_to)) {
                    if (from_to[[1]][1]$year == from_to[[2]][1]$year && # froms year identical
                        from_to[[1]][1]$mon == from_to[[2]][1]$mon &&   # froms mon identical
                        from_to[[1]][2]$year == from_to[[2]][2]$year && # tos year identical
                        from_to[[1]][2]$mon == from_to[[2]][2]$mon) {   # tos mon identical
                        message("--> same year and same month. assume they represent the same time")
                    } else if (abs(difftime(from_to[[1]][1], from_to[[2]][1], units="day")) < 15 && # froms are close
                               abs(difftime(from_to[[1]][2], from_to[[2]][2], units="day")) < 15) { # tos are close
                        message("--> dt between froms and tos is < 15 days. assume they represent the same time")
                    } else {
                        stop("time dim vals of the 2 variables are not identical. set `from_year` and/or `to_year` to find common time dim vals across files")
                    }
                }
            }
            posixct_vari <- posixct_vari[[1]]
            posixlt_vari <- posixct_vari[[1]]
            time_vari_sec <- time_vari_sec[[1]]
            from_to <- from_to[[1]]
            nyears <- nyears[[1]]
            message("\ncalc temporal trends over ", length(timeinds[[1]]), " timepoints from ", 
                    from_to[1], " to ", from_to[2], " (", nyears, " years; are those dates correct?) at ", nloc, " locations from (",
                    paste(paste0(names(mapping_df), "=", mapping_df[1,]), collapse=","), ") to (", 
                    paste(paste0(names(mapping_df), "=", mapping_df[nloc,]), collapse=","), ") ...")
            if (!is.null(operator)) {
                for (fi in seq_along(fin)) {
                    if (operator[fi] != "none") {
                        message("will run `ts <- ts ", operator[fi], " ", operator_value[fi], "` for every time series of file ", fi)
                    }
                }
            }
            
            ntime_all <- slope_all <- slope_err_all <- t_val_all <- p_val_all <- rep(NA, t=nloc) # allocate
            if (mode == "var_vs_time") dt_day_all <- slope_day_all <- slope_day_err_all <- ntime_all
            for (loci in seq_len(nloc)) { # loop through all locations
                locinds <- unlist(mapping_df[loci,])
                locvals <- rep(NA, t=nspatialdims)
                names(locvals) <- names(locinds)
                for (si in seq_len(nspatialdims)) {
                    locvals[si] <- spatial_dims[[si]]$vals[locinds[si]]
                }
                
                if (T) { # normal verbose
                    if (loci == 1) pb <- utils::txtProgressBar(min=1, max=ntot, initial=1, style=3) # open progress bar
                    utils::setTxtProgressBar(pb, value=loci) # update progress bar
                } else if (F) { # very verbose
                    message("fi ", fi, " loc ", loci, "/", nloc, " (tot loc ", location_inds[loci], "/", ntot, 
                            "), inds: (", paste(paste0(names(mapping_df), "=", locinds, "/", nspatial_vals), collapse=","), 
                            "), vals: (", paste(paste0(names(mapping_df), "=", locvals), collapse=","), 
                            "), start: (", paste(start, collapse=","), "), count=(", paste(count, collapse=","), ") ... ")
                }

                ts <- vector("list", l=length(fin))
                for (fi in seq_along(fin)) {
                    start <- rep(1, t=length(dimnames_var[[fi]]))
                    names(start) <- dimnames_var[[fi]]
                    count <- start
                    start[match(spatialdimnames, dimnames_var[[fi]])] <- locinds
                    start[match(timedimname, dimnames_var[[fi]])] <- from_ind[[fi]]
                    count[match(timedimname, dimnames_var[[fi]])] <- length(timeinds[[fi]])
                    if (mode == "var_vs_time") {
                        ts[[fi]] <- ncdf4::ncvar_get(nc[[fi]], varname, start=start, count=count)
                    } else if (mode == "var1_vs_var2") {
                        ts[[fi]] <- ncdf4::ncvar_get(nc[[fi]], varnames[fi], start=start, count=count)
                    }
                } # for fi
                inds <- lapply(ts, function(x) which(!is.na(x)))
                if (mode == "var1_vs_var2") inds <- base::intersect(inds[[1]], inds[[2]])
                ninds <- length(inds)
                ok <- F
                if (all(ninds > 3)) {
                    for (fi in seq_along(fin)) {
                        if (!is.null(operator[fi])) {
                            if (operator[fi] != "none") {
                                cmd <- paste0("ts[[", fi, "]] <- ts[[", fi, "]] ", operator[fi], " ", operator_value[fi])
                                #message("run `", cmd, "` ...")
                                eval(parse(text=cmd))
                            }
                        }
                    }
                    # get common inds in case of two variables
                    if (lm_method == "stats::lm") {
                        if (mode == "var_vs_time") {
                            lm <- stats::lm(ts[inds] ~ time_vari_sec[inds])
                        } else if (mode == "var1_vs_var2") {
                            lm <- stats::lm(ts[[1]][inds] ~ ts[[2]][inds])
                        }
                        # catch potential warnings/errors
                        # --> e.g. constant time series
                        lms <- base::tryCatch(summary.lm(lm), error=function(e) e, warning=function(w) w) 
                        if (methods::is(lms, "error")) {
                            # check
                        } else if (methods::is(lms, "warning")) {
                            # check
                        } else if (length(attributes(lms$coefficients)$dimnames[[1]]) == 1 &&
                                   attributes(lms$coefficients)$dimnames[[1]] == "(Intercept)") {
                            # only intercept exists because of predictor singularities, e.g. predictor is constant
                        } else {
                            if (!is.na(lms$coefficients[2,4])) { # if lm was successfull
                                ok <- T
                                slope <- lm$coefficients[2]
                                slope_err <- lms$coefficients[2,"Std. Error"]
                                t_val <- lms$coefficients[2,"t value"]
                                p_val <- lms$coefficients[2,"Pr(>|t|)"]
                                if (mode == "var_vs_time") {
                                    dt_day <- as.numeric(difftime(posixct_vari[inds[ninds]], posixct_vari[inds[1]], units="days"))
                                    slope_day_a <- slope*dt_day*86400
                                    #slope_day_b <- lm$fitted.values[ninds] - lm$fitted.values[1]
                                    # diff(a,b) = slope_day_a - slope_day_b ~ O(1e-13)
                                    slope_day <- slope_day_a
                                    slope_day_err <- slope_err*dt_day*86400
                                }
                            }
                        }
                    } else {
                        stop("`lm_method` = ", lm_method, " not implemented")
                    }
                    if (ok) { # save result
                        ntime_all[loci] <- ninds
                        slope_all[loci] <- slope
                        slope_err_all[loci] <- slope_err
                        t_val_all[loci] <- t_val
                        p_val_all[loci] <- p_val
                        if (mode == "var_vs_time") {
                            dt_day_all[loci] <- dt_day
                            slope_day_all[loci] <- slope_day
                            slope_day_err_all[loci] <- slope_day_err
                        }
                    }
                } # if more than 3 non-NA values in time series
            } # for loci
                
            # close progress bar
            if (T) base::close(pb)

            if (all(is.na(slope_all))) {
                message("all regression slopes are NA. do not save any output. skip")
            } else { # save output
                
                message("save ", fouti, " ...")
                if (mode == "var_vs_time") {
                    names <- paste0(varname, "_ntime", "_slope", "_t_val", "_p_val")
                    varunits <- nc[[1]]$var[[varname]]$units
                    slope_unit <- paste0(varunits, " / time")
                } else if (mode == "var1_vs_var2") {
                    names <- c("ntime", paste0("slope_", varnames[1], "_per_", varnames[2]), "t_val", "p_val")
                    varunits <- c(nc[[1]]$var[[varnames[1]]]$units, nc[[2]]$var[[varnames[2]]]$units)
                    slope_unit <- paste0(varunits[1], " / ", varunits[2])
                }
                ntime_var <- ncdf4::ncvar_def(name=names[1],
                                              units="", dim=spatial_dims_nc)
                slope_var <- ncdf4::ncvar_def(name=names[2],
                                              units=slope_unit, dim=spatial_dims_nc)
                t_val_var <- ncdf4::ncvar_def(name=names[3],
                                              units="", dim=spatial_dims_nc)
                p_val_var <- ncdf4::ncvar_def(name=names[4],
                                              units="", dim=spatial_dims_nc)
                ncvars <- list(ntime_var, slope_var, t_val_var, p_val_var)
                if (mode == "var_vs_time") {
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

                    if (save_trend_per_day) {
                        slope_per_day_var <- ncdf4::ncvar_def(name=paste0(varname, "_per_day"),
                                                              units=paste0(varunits, " / day"), dim=spatial_dims_nc)
                        slope_err_per_day_var <- ncdf4::ncvar_def(name=paste0(varname, "_per_day_err"),
                                                                  units=paste0(varunits, " / day"), dim=spatial_dims_nc)
                        ncvars <- c(ncvars, list(slope_per_day_var, slope_err_per_day_var))
                    }
                    if (save_trend_per_month) {
                        slope_per_month_var <- ncdf4::ncvar_def(name=paste0(varname, "_per_month"),
                                                                units=paste0(varunits, " / month"), dim=spatial_dims_nc)
                        slope_err_per_month_var <- ncdf4::ncvar_def(name=paste0(varname, "_per_month_err"),
                                                                    units=paste0(varunits, " / month"), dim=spatial_dims_nc)
                        ncvars <- c(ncvars, list(slope_per_month_var, slope_err_per_month_var))
                    }
                    if (save_trend_per_year) {
                        slope_per_year_var <- ncdf4::ncvar_def(name=paste0(varname, "_per_year"),
                                                               units=paste0(varunits, " / year"), dim=spatial_dims_nc)
                        slope_err_per_year_var <- ncdf4::ncvar_def(name=paste0(varname, "_per_year_err"),
                                                                   units=paste0(varunits, " / year"), dim=spatial_dims_nc)
                        ncvars <- c(ncvars, list(slope_per_year_var, slope_err_per_year_var))
                    }
                    if (save_trend_per_decade) {
                        slope_per_decade_var <- ncdf4::ncvar_def(name=paste0(varname, "_per_decade"),
                                                                 units=paste0(varunits, " / decade"), dim=spatial_dims_nc)
                        slope_err_per_decade_var <- ncdf4::ncvar_def(name=paste0(varname, "_per_decade_err"),
                                                                     units=paste0(varunits, " / decade"), dim=spatial_dims_nc)
                        ncvars <- c(ncvars, list(slope_per_decade_var, slope_err_per_decade_var))
                    }
                    if (save_trend_per_century) {
                        slope_per_century_var <- ncdf4::ncvar_def(name=paste0(varname, "_per_century"),
                                                                  units=paste0(varunits, " / century"), dim=spatial_dims_nc)
                        slope_err_per_century_var <- ncdf4::ncvar_def(name=paste0(varname, "_per_century_err"),
                                                                      units=paste0(varunits, " / century"), dim=spatial_dims_nc)
                        ncvars <- c(ncvars, list(slope_per_century_var, slope_err_per_century_var))
                    }
                    if (save_trend_per_millenium) {
                        slope_per_millenium_var <- ncdf4::ncvar_def(name=paste0(varname, "_per_millenium"),
                                                                    units=paste0(varunits, " / millenium"), dim=spatial_dims_nc)
                        slope_err_per_millenium_var <- ncdf4::ncvar_def(name=paste0(varname, "_per_millenium"),
                                                                        units=paste0(varunits, " / millenium"), dim=spatial_dims_nc)
                        ncvars <- c(ncvars, list(slope_per_millenium_var, slope_err_per_millenium_var))
                    }
                } # if var_vs_time
                
                outnc <- ncdf4::nc_create(filename=fouti, force_v4=T, vars=ncvars)
                ncdf4::ncvar_put(nc=outnc, varid=ntime_var, vals=ntime_all)
                ncdf4::ncvar_put(nc=outnc, varid=slope_var, vals=slope_all)
                ncdf4::ncvar_put(nc=outnc, varid=t_val_var, vals=t_val_all)
                ncdf4::ncvar_put(nc=outnc, varid=p_val_var, vals=p_val_all)
                if (mode == "var_vs_time") {
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
                } # if var_vs_time
                
                if (mode == "var_vs_time") {
                    ncdf4::ncatt_put(nc=outnc, varid=0, "input", fin)
                    ncdf4::ncatt_put(nc=outnc, varid=0, "varname", varname)
                } else if (mode == "var1_vs_var2") {
                    ncdf4::ncatt_put(nc=outnc, varid=0, "input1", fin[1])
                    ncdf4::ncatt_put(nc=outnc, varid=0, "input2", fin[2])
                    ncdf4::ncatt_put(nc=outnc, varid=0, "varname1", varnames[1])
                    ncdf4::ncatt_put(nc=outnc, varid=0, "varname2", varnames[2])
                }
                if (!is.null(operator)) {
                    if (mode == "var_vs_time") {
                        if (operator != "none") {
                            ncdf4::ncatt_put(nc=outnc, varid=0, "operator", operator)
                            ncdf4::ncatt_put(nc=outnc, varid=0, "operator_value", operator_value)
                        }
                    } else if (mode == "var1_vs_var2") {
                        if (operator[1] != "none") {
                            ncdf4::ncatt_put(nc=outnc, varid=0, "operator1", operator[1])
                            ncdf4::ncatt_put(nc=outnc, varid=0, "operator_value1", operator_value[1])
                        }
                        if (operator[2] != "none") {
                            ncdf4::ncatt_put(nc=outnc, varid=0, "operator2", operator[2])
                            ncdf4::ncatt_put(nc=outnc, varid=0, "operator_value2", operator_value[2])
                        }
                    }
                }
                ncdf4::ncatt_put(nc=outnc, varid=0, "lm_method", lm_method)
                ncdf4::nc_close(outnc)

                # apply griddes of fin
                cmd <- paste0(cdo, " setgrid,", fin[1], " ", fouti, " ", fouti, "_tmp && mv ", fouti, "_tmp ", fouti)
                message("\nset griddes: run `", cmd, "` ...")
                check <- system(cmd)
                if (check != 0) stop("cmd failed")

            } # if any trend is non-NA
        } # if current var has more than 3 timepoints
        
        if (any(file.exists(fin_rechunk))) {
            message("remove tmp rechunked files\n", paste(fin_rechunk, collapse="\n"), "\n...")
            invisible(file.remove(fin_rechunk))
        }

    } # if trend result of current variable already exists

} # for vari nit

options(warn=warn) # restore default

message("\nfinished")


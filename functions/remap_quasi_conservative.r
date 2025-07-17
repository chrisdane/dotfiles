#!/usr/bin/env Rscript

# quasi-conservative (i.e. fldsums of input and remapped fields as equal as possible) remapping from source to target grid
# https://github.com/FESOM/spheRlab/issues/10

rm(list=ls()); graphics.off()

# methods
# default: - normalize interpolated values locally: divide interpolated field by its fldsum and multiply by fldsum of input field
# R14:     - as in section 2.4.3 "Residual flux redistribution" in Rackow 2014 (https://media.suub.uni-bremen.de/handle/elib/829) 
# todo: oasis: global, glbpos, gsspos, basbal, baspos, bsspos
    
# defaults
#method <- "default" # "default" or "R14"
method <- "R14"
cdo_remap <- "remapycon"
target_grid <- "global_1"
target_grid_file <- NULL
if (interactive()) { # rest
    if (F) {
        fin <- "/work/ba1103/a270073/bc/input4MIPs/wfo_input4MIPs_surfaceFluxes_FAFMIP_NCAS-2-1-0_gn.nc"
        varname <- "wfo"
        outdir <- dirname(fin)
    } else if (F) {
        fin <- "/work/ba1103/a270073/bc/FESOM1/oae/core/lime_mask_cao_2040-high.nc_timestep_1030-1032"
        varname <- "lime_mask"
        target_grid <- "fesom1_core"
        target_grid_file <- "/pool/data/AWICM/FESOM1/MESHES/core/griddes.nc"
        outdir <- "/work/ba1103/a270073/bc/FESOM1/fwf/core"
    } else if (F) {
        fin <- "/work/ab1095/a270073/out/awicm-1.0-recom/awi-esm-1-1-lr_kh800/piControl2/outdata/post/fesom/setgrid/wnet_fesom_18500101_setgrid_shifttime_1day.nc"
        varname <- "wnet"
        target_grid <- "global_1"
        outdir <- "/work/ab1095/a270073/out/awicm-1.0-recom/awi-esm-1-1-lr_kh800/piControl2/outdata/post/fesom/remap_quasi_conservative"
    } else if (F) {
        fin <- "/work/ab1095/a270073/out/awicm-1.0-recom/sofia/fwf_01/outdata/post/fesom/setgrid/fwf_fesom_18700101_setgrid_shifttime_1day.nc"
        varname <- "fwf"
        target_grid <- "global_1"
        outdir <- "/work/ab1095/a270073/out/awicm-1.0-recom/sofia/fwf_01/outdata/post/fesom/remap_quasi_conservative"
    } else if (T) {
        fin <- "/work/ab1095/a270073/out/awicm-1.0-recom/awi-esm-1-1-lr_kh800/historical3/outdata/post/fesom/levelwise/so_fesom_20140101_levelwise_0-5900m.nc"
        varname <- "so"
        target_grid <- "global_1"
        outdir <- "/work/ab1095/a270073/out/awicm-1.0-recom/awi-esm-1-1-lr_kh800/historical3/outdata/post/cmor/sofia/CMIP6/CMIP/AWI/AWI-ESM-1-REcoM/historical/r1i2p1f1/Omon/so/gr1adj/v20240107"
    }

} else if (!interactive()) {
    me <- "remap_quasi_conservative.r"
    help <- paste0("Usage:\n",
                   " $ ", me, " --fin=/file/to/remap --varname=varname --outdir=/path/to/out [--method=R14] [--cdo_remap=remapycon] [--target_grid=global_1] [--target_grid_file]\n")
    args <- commandArgs(trailingOnly=F)
    me <- basename(sub("--file=", "", args[grep("--file=", args)]))
    args <- commandArgs(trailingOnly=T)

    # check args 
    if (length(args) < 3) {
        message(help)
        quit()
    }
    if (any(grepl("--fin", args))) {
        fin <- sub("--fin=", "", args[grep("--fin=", args)])
    } else {
        stop("provide `--fin=/file/to/remap`")
    }
    if (any(grepl("--varname", args))) {
        varname <- sub("--varname=", "", args[grep("--varname=", args)])
    } else {
        stop("provide `--varname=varname`")
    }
    if (any(grepl("--outdir", args))) {
        outdir <- sub("--outdir=", "", args[grep("--outdir=", args)])
    } else {
        stop("provide --outdir=/path/save/out")
    }
    if (any(grepl("--method", args))) {
        method <- sub("--method=", "", args[grep("--method=", args)])
    }
    if (any(grepl("--cdo_remap", args))) {
        cdo_remap <- sub("--cdo_remap=", "", args[grep("--cdo_remap=", args)])
    }
    if (any(grepl("--target_grid", args))) {
        target_grid <- sub("--target_grid=", "", args[grep("--target_grid=", args)])
    }
    if (any(grepl("--target_grid_file", args))) {
        target_grid_file <- sub("--target_grid_file=", "", args[grep("--target_grid_file=", args)])
    }

} # if interactive

####################################################################
    
if (!any(method == c("default", "R14"))) stop("`method` must be one of \"default\", \"R14\"")
cdo <- Sys.which("cdo")
if (cdo == "") stop("could not find cdo")
#if (T) cdo <- "/sw/spack-levante/cdo-2.2.2-4z4icb/bin/cdo" # cdo 2.0.5 remapycon broken? 
if (length(fin) != 1) stop("`fin` must be of length 1")
if (file.info(fin)$isdir) stop("`fin` ", fin, " is directory and not a file")
if (!file.exists(fin)) stop("`fin` ", fin, " does not exist")
if (file.access(fin, mode=4) == -1) stop("`fin` ", fin, " is not readable")
dir.create(outdir, recursive=T, showWarnings=F)
if (!dir.exists(outdir)) stop("could not create outdir ", outdir)
if (!is.null(target_grid_file)) {
    if (file.info(target_grid_file)$isdir) stop("`target_grid_file` ", target_grid_file, " is directory and not a file")
    if (!file.exists(target_grid_file)) stop("`target_grid_file` ", target_grid_file, " does not exist")
    if (file.access(target_grid_file, mode=4) == -1) stop("`target_grid_file` ", target_grid_file, " is not readable")
}
message("load ncdf4 package ...")
library(ncdf4)
warn <- options()$warn
nproc <- system("nproc", intern=T)
tol <- sqrt(.Machine$double.eps) # default of base::all.equal
clean <- T

message("\nget quasi-conservative remapped field with method = ", method, " ...")
if (method == "default") {

    # remap input field to target grid
    message("\nremap input field with cdo_remap = ", cdo_remap, " ...")
    fout <- paste0(outdir, "/", basename(fin), "_", cdo_remap, "_", target_grid)
    if (!file.exists(fout)) {
        cmd <- paste0(cdo, " --pedantic -s -P ", nproc, " -", cdo_remap, ",")
        if (!is.null(target_grid_file)) {
            cmd <- paste0(cmd, target_grid_file)
        } else {
            cmd <- paste0(cmd, target_grid)
        }
        cmd <- paste0(cmd," -select,name=", varname, " ", fin, " ", fout)
        message("run `", cmd, "` ...")
        check <- system(cmd)
        if (check != 0) stop("cmd not successful")
    }

    # get fldsum of input field
    message("\nget fldsum of input field ...")
    fin_fldsum <- paste0(outdir, "/", basename(fin), "_fldsum")
    if (!file.exists(fin_fldsum)) {
        cmd <- paste0(cdo, " --pedantic -s -fldsum -select,name=", varname, " ", fin, " ", fin_fldsum)
        message("run `", cmd, "` ...")
        check <- system(cmd)
        if (check != 0) stop("cmd not successful")
    }
    message("open ", fin_fldsum, " ...")
    fin_fldsum_nc <- ncdf4::nc_open(fin_fldsum)
    fin_fldsum_num <- as.numeric(ncdf4::ncvar_get(fin_fldsum_nc, varname))
    if (!is.vector(fin_fldsum_num)) { # fldsum result has more than 1 dim
        stop("fldsum result has more than 1 dim. this case is not impolemented")
    }
    ntime <- length(fin_fldsum_num)
    message("--> fldsums of ", ntime, " input fields:")
    print(summary(fin_fldsum_num))

    # get fldsum of interpolated field
    message("\nget fldsum of interpolated field ...")
    fout_fldsum <- paste0(fout, "_fldsum")
    if (!file.exists(fout_fldsum)) {
        cmd <- paste0(cdo, " --pedantic -s -fldsum ", fout, " ", fout_fldsum)
        message("run `", cmd, "` ...")
        check <- system(cmd)
        if (check != 0) stop("cmd not successful")
    }
    fout_fldsum_nc <- ncdf4::nc_open(fout_fldsum)
    fout_fldsum_num <- as.numeric(ncdf4::ncvar_get(fout_fldsum_nc, varname))
    message("--> fldsums of ", ntime, " interpolated fields:")
    print(summary(fout_fldsum_num))
    
    fout_adjusted <- paste0(fout, "_adjusted_", method)
    if (!file.exists(fout_adjusted)) {
        
        # normalize interpolated field by dividing by fldsum of interpolated field
        # and multiply by fldsum of input field
        # --> only necessary if fldsum != 0
        message("\nnormalize interpolated field: divide interpolated field by fldsum of interpolated field and multiply with fldsum of input field ...")
        fouts_normalized <- fouts_adjusted <- fout_normalized_fldsum_nums <- fldsums_adjusted_checks <- rep(NA, t=ntime)
        for (ti in seq_along(fout_fldsum_num)) {
            message("********************** ti ", ti, "/", ntime, " **********************")
            fout_normalized <- paste0(fout, "_normalized_ti_", ti, "_of_", length(fout_fldsum_num), "_pid_", Sys.getpid())
            fouts_normalized[ti] <- fout_normalized
            fout_adjusted_ti <- paste0(fout_normalized, "_adjusted_", method)
            fouts_adjusted[ti] <- fout_adjusted_ti
            if (!file.exists(fout_normalized)) {
                if (fout_fldsum_num[ti] == 0) { # normalization not needed (and not possible); just select time step
                    message("fldsum of remapped field is zero --> normalization not possible due to division by zero")
                    cmd <- paste0(cdo, " --pedantic -s -seltimestep,", ti, " ", fout, " ", fout_adjusted_ti)
                } else {
                    cmd <- paste0(cdo, " --pedantic -s -divc,", fout_fldsum_num[ti], " -seltimestep,", ti, " ", fout, " ", fout_normalized)
                }
                message("run `", cmd, "` ...")
                check <- system(cmd)
                if (check != 0) stop("cmd not successful")
            }

            # check if fldsum of normalized interpolated field equals 1
            if (fout_fldsum_num[ti] != 0) {
                fout_normalized_fldsum <- paste0(fout_normalized, "_fldsum")
                if (!file.exists(fout_normalized_fldsum)) {
                    cmd <- paste0(cdo, " --pedantic -s -fldsum ", fout_normalized, " ", fout_normalized_fldsum)
                    message("run `", cmd, "` ...")
                    check <- system(cmd)
                    if (check != 0) stop("cmd not successful")
                }
                fout_normalized_fldsum_nc <- ncdf4::nc_open(fout_normalized_fldsum)
                fout_normalized_fldsum_num <- as.vector(ncdf4::ncvar_get(fout_normalized_fldsum_nc, varname))
                fout_normalized_fldsum_nums[ti] <- fout_normalized_fldsum_num
                message("--> fldsum of normalized interpolated field = ", fout_normalized_fldsum_num)
                check <- all.equal(fout_normalized_fldsum_num, 1, tol=tol)
                if (isTRUE(check)) { # global fldsum equals 1 to tolerance level
                    message("--> = 1 with tolerance = `sqrt(.Machine$double.eps)` = ", tol)
                    fldsums_adjusted_checks[ti] <- T
                } else { # global fldsum does not equal 1 to tolerance level
                    message("--> ", check)
                    message("--> != 1 with tolerance = `sqrt(.Machine$double.eps)` = ", tol)
                    fldsums_adjusted_checks[ti] <- F
                }
                invisible(file.remove(fout_normalized_fldsum))

                # multiply normalized interpolated field by fldsum of input
                if (!file.exists(fout_adjusted_ti)) {
                    cmd <- paste0(cdo, " --pedantic -s -mulc,", fin_fldsum_num[ti], " ", fout_normalized, " ", fout_adjusted_ti)
                    message("run `", cmd, "` ...")
                    check <- system(cmd)
                    if (check != 0) stop("cmd not successful")
                }
            } # if fldsum at time ti != 0
        } # for ti

        # merge adjusted files
        nadjusted <- length(which(fout_fldsum_num != 0))
        nnot_adjusted <- ntime - nadjusted
        message("\nmerge ", nadjusted, "/", ntime, " adjusted and ", nnot_adjusted, "/", ntime, " non-adjusted interpolated fields ...")
        cmd <- paste0(cdo, " --pedantic -cat ", paste(fouts_adjusted, collapse=" "), " ", fout_adjusted)
        scriptname <- paste0(fout_adjusted, "_script.txt")
        writeLines(cmd, con=scriptname)
        cmd_source <- paste0(". ", scriptname)
        message("run `", cmd_source, "` ...")
        check <- system(cmd_source)
        if (check != 0) stop("cmd not successful")
        invisible(file.remove(scriptname))
    
        if (clean) {
            message("\n`clean` = true --> rm tmp files ...")
            inds <- which(file.exists(fouts_normalized))
            if (length(inds) > 0) fouts_normalized <- fouts_normalized[inds]
            invisible(file.remove(c(fouts_normalized,
                                    fouts_adjusted)))
        }

    } # if fout_adjusted already exists or not

    # check
    message("\ncheck ...")
    fin_fldsum_timmean <- paste0(fin_fldsum, "_timmean")
    if (!file.exists(fin_fldsum_timmean)) {
        cmd <- paste0(cdo , " --pedantic -s -timmean ", fin_fldsum, " ", fin_fldsum_timmean)
        message("run `", cmd, "` ...")
        check <- system(cmd)
        if (check != 0) stop("cmd not successful")
    }
    fin_fldsum_timmean_nc <- ncdf4::nc_open(fin_fldsum_timmean)
    fin_fldsum_timmean_num <- as.numeric(ncdf4::ncvar_get(fin_fldsum_timmean_nc, varname))
    
    fout_fldsum_timmean <- paste0(fout_fldsum, "_timmean")
    if (!file.exists(fout_fldsum_timmean)) {
        cmd <- paste0(cdo , " --pedantic -s -timmean ", fout_fldsum, " ", fout_fldsum_timmean)
        message("run `", cmd, "` ...")
        check <- system(cmd)
        if (check != 0) stop("cmd not successful")
    }
    fout_fldsum_timmean_nc <- ncdf4::nc_open(fout_fldsum_timmean)
    fout_fldsum_timmean_num <- as.numeric(ncdf4::ncvar_get(fout_fldsum_timmean_nc, varname))
    
    fout_adjusted_fldsum_timmean <- paste0(fout_adjusted, "_fldsum_timmean")
    if (!file.exists(fout_adjusted_fldsum_timmean)) {
        cmd <- paste0(cdo , " --pedantic -s -fldsum -timmean ", fout_adjusted, " ", fout_adjusted_fldsum_timmean)
        message("run `", cmd, "` ...")
        check <- system(cmd)
        if (check != 0) stop("cmd not successful")
    }
    fout_adjusted_fldsum_timmean_nc <- ncdf4::nc_open(fout_adjusted_fldsum_timmean)
    fout_adjusted_fldsum_timmean_num <- as.numeric(ncdf4::ncvar_get(fout_adjusted_fldsum_timmean_nc, varname))
    
    message("--> fldsum timmean fin ", fin_fldsum_timmean_num)
    bias <- fout_fldsum_timmean_num - fin_fldsum_timmean_num
    sign <- ifelse(bias >= 0, "+", "-")
    message("--> fldsum timmean ", cdo_remap, " = ", fout_fldsum_timmean_num, " (", sign, abs(bias), " ~ ", sign, abs(bias)/abs(fin_fldsum_timmean_num)*100, "% wrt input)")
    bias <- fout_adjusted_fldsum_timmean_num - fin_fldsum_timmean_num
    sign <- ifelse(bias >= 0, "+", "-")
    message("--> fldsum timmean ", cdo_remap, " adjusted = ", fout_adjusted_fldsum_timmean_num, " (", sign, abs(bias), " ~ ", sign, abs(bias)/abs(fout_adjusted_fldsum_timmean_num)*100, "% wrt input)")
    
    if (clean) {
        message("\n`clean` = true --> rm tmp files ...")
        invisible(file.remove(c(fout,
                                fin_fldsum_timmean,
                                fout_fldsum_timmean,
                                fout_adjusted_fldsum_timmean)))
    } # if clean

} else if (method == "R14") { # original R14 method
    
    # remap input field to target grid
    message("\nremap input field with cdo_remap = ", cdo_remap, " ...")
    fout <- paste0(outdir, "/", basename(fin), "_", cdo_remap, "_", target_grid)
    if (!file.exists(fout)) {
        cmd <- paste0(cdo, " --pedantic -s -P ", nproc, " -", cdo_remap, ",")
        if (!is.null(target_grid_file)) {
            cmd <- paste0(cmd, target_grid_file)
        } else {
            cmd <- paste0(cmd, target_grid)
        }
        cmd <- paste0(cmd, " -select,name=", varname, " ", fin, " ", fout)
        message("run `", cmd, "` ...")
        check <- system(cmd)
        if (check != 0) stop("cmd not successful")
    }
    
    fout_adjusted <- paste0(fout, "_adjusted_", method)
    if (!file.exists(fout_adjusted)) {
    
        # get fldint of input field for residual 
        message("\nget fldint of intput field (R14:2.47) ...")
        fldint_fin <- paste0(outdir, "/", basename(fin), "_fldint")
        if (!file.exists(fldint_fin)) {
            cmd <- paste0(cdo, " --pedantic -s -fldint -select,name=", varname, " ", fin, " ", fldint_fin)
            message("run `", cmd, "` ...")
            check <- system(cmd)
            if (check != 0) stop("cmd not successful")
        }

        # get fldint of remapped field for residual 
        message("\nget fldint of remapped field (R14:2.48) ...")
        fldint_fout <- paste0(outdir, "/", basename(fin), "_", cdo_remap, "_", target_grid, "_fldint")
        if (!file.exists(fldint_fout)) {
            cmd <- paste0(cdo, " --pedantic -s fldint ", fout, " ", fldint_fout)
            message("run `", cmd, "` ...")
            check <- system(cmd)
            if (check != 0) stop("cmd not successful")
        }

        # get residual of fldints
        message("\nget residual of fldints (R14:2.49; switch order to keep target grid) ...")
        fldint_residual_orig_minus_remap <- paste0(outdir, "/", basename(fin), "_fldint_minus_", target_grid, "_fldint_residual")
        if (!file.exists(fldint_residual_orig_minus_remap)) {
            # (interp minus remap)*-1 and not (orig minus remap) to get lon/lat from target_grid
            cmd <- paste0(cdo, " -s -mulc,-1 -sub ", fldint_fout, " ", fldint_fin, " ", fldint_residual_orig_minus_remap) 
            message("run `", cmd, "` ...")
            check <- system(cmd)
            if (check != 0) stop("cmd not successful")
        }
       
        # get ntime
        message("\nget ntime ...")
        cmd <- paste0(cdo, " --pedantic -s ntime ", fldint_residual_orig_minus_remap)
        message("run `", cmd, " ...")
        ntime <- system(cmd, intern=T)
        message("--> ntime = \"", ntime, "\"")
        message("convert to integer ... ", appendLF=F)
        options(warn=2); ntime <- as.integer(ntime); options(warn=warn)
        message("ok: ", ntime)

        # calc and apply weights for every time step
        message("\ncalc and apply weights for ", ntime, " time steps (R14:2.51 and 2.50) ...")
        fouts_adjusted <- fouts_target_grid_weights <- rep(NA, t=ntime)
        for (ti in seq_len(ntime)) {

            message("********************** ti ", ti, "/", ntime, " **********************")
            
            # get weights
            fout_target_grid_weights <- paste0(fout, "_target_weights_ti_", ti, "_of_", ntime, "_pid_", Sys.getpid())
            fouts_target_grid_weights[ti] <- fout_target_grid_weights
            if (!file.exists(fout_target_grid_weights)) {

                # get fldint of absolute flux on target grid
                cmd <- paste0(cdo, " --pedantic -s -output -fldint -abs -seltimestep,", ti, " ", fout)
                message("get fldint of absolute flux on target grid: run `", cmd, "` ...")
                fldint_abs_target_num <- system(cmd, intern=T)
                fldint_abs_target_num <- gsub(" ", "", fldint_abs_target_num, fixed=T) # remove all white space
                # e.g. (for all depths)
                # [1] "1.29268e+16" "1.29289e+16" "1.29404e+16" "1.292e+16"   "1.29053e+16"
                # [6] "1.28909e+16" "1.27472e+16" "1.26813e+16" "1.26539e+16" "1.26295e+16"
                message("--> fldint[ abs( X_target(t=", ti, ") ) ] = \"", paste(fldint_abs_target_num, collapse=" "), "\"")
                message("convert to numeric ... ", appendLF=F)
                options(warn=2); fldint_abs_target_num <- as.numeric(fldint_abs_target_num); options(warn=warn)
                message("ok: ", paste(fldint_abs_target_num, collapse=" "))

                if (any(fldint_abs_target_num != 0)) { # R14:2.51 case 1
                    if (length(fldint_abs_target_num) != 1) stop("3D case not implemented")
                    cmd <- paste0(cdo, " --pedantic -s -divc,", fldint_abs_target_num, " -abs -seltimestep,", ti, " ", fout, " ", fout_target_grid_weights)
                    message("calc weight case 1: run `", cmd, "` ...")
                    check <- system(cmd)
                    if (check != 0) stop("cmd not successful")

                } else if (all(fldint_abs_target_num == 0)) { # R14:2.51 case 2
                    stop("not implemented")
                } # if R14:2.51 case 1 or 2

            } # if target_grid_weights already exists or not
        
            # apply weights 
            fout_adjusted_ti <- paste0(fout, "_adjusted_", method, "_ti_", ti, "_of_", ntime, "_pid_", Sys.getpid())
            fouts_adjusted[ti] <- fout_adjusted_ti
            if (!file.exists(fout_adjusted_ti)) {
                
                # get residual
                cmd <- paste0(cdo, " --pedantic -s -output -seltimestep,", ti, " ", fldint_residual_orig_minus_remap)
                message("get residual: run `", cmd, "` ...")
                fldint_residual_num <- system(cmd, intern=T)
                fldint_residual_num <- gsub(" ", "", fldint_residual_num, fixed=T) # remove all white space
                message("--> residual = \"", fldint_residual_num, "\"")
                message("convert to numeric ... ", appendLF=F)
                options(warn=2); fldint_residual_num <- as.numeric(fldint_residual_num); options(warn=warn)
                message("ok: ", fldint_residual_num)

                # get adjusted fout
                cmd <- paste0(cdo, " --pedantic -s -add -seltimestep,", ti, " ", fout, " [ -mulc,", fldint_residual_num, " ", fout_target_grid_weights, " ] ", fout_adjusted_ti)
                message("apply weights: run `", cmd, "` ...")
                check <- system(cmd)
                if (check != 0) stop("cmd not successful")
        
            } # if fout_adjusted aready exists or not
        
        } # for ti
        
        # merge adjusted files
        message("\nmerge ", ntime, " adjusted adjusted interpolated fields ...")
        cmd <- paste0(cdo, " -cat ", paste(fouts_adjusted, collapse=" "), " ", fout_adjusted)
        scriptname <- paste0(fout_adjusted, "_script.txt")
        writeLines(cmd, con=scriptname)
        cmd_source <- paste0(". ", scriptname)
        message("run `", cmd_source, "` ...")
        check <- system(cmd_source)
        if (check != 0) stop("cmd not successful")
        invisible(file.remove(scriptname))
        
        if (clean) {
            message("\n`clean` = true --> rm tmp files ...")
            invisible(file.remove(c(fouts_target_grid_weights,
                                    fouts_adjusted,
                                    fldint_fin,
                                    fldint_fout,
                                    fldint_residual_orig_minus_remap)))
        }

    } # if fout_adjusted already exists or not

    # check timmean and fldint of input and adjusted output
    message("\ncheck timmean and fldint of input, output and adjusted output ...")
    fldint_timmean_fin <- paste0(outdir, "/", basename(fin), "_timmean_fldint")
    if (!file.exists(fldint_timmean_fin)) {
        cmd <- paste0(cdo, " -s -fldint -timmean ", fin, " ", fldint_timmean_fin)
        check <- system(cmd)
        if (check != 0) stop("cmd not successful")
    }
    fldint_timmean_fout <- paste0(fout, "_timmean_fldint")
    if (!file.exists(fldint_timmean_fout)) {
        cmd <- paste0(cdo, " -s -fldint -timmean ", fout, " ", fldint_timmean_fout)
        check <- system(cmd)
        if (check != 0) stop("cmd not successful")
    }
    fldint_timmean_fout_adjusted <- paste0(fout_adjusted, "_timmean_fldint")
    if (!file.exists(fldint_timmean_fout_adjusted)) {
        cmd <- paste0(cdo, " -s -fldint -timmean ", fout_adjusted, " ", fldint_timmean_fout_adjusted)
        check <- system(cmd)
        if (check != 0) stop("cmd not successful")
    }
    fldint_timmean_fin_nc <- ncdf4::nc_open(fldint_timmean_fin)
    fldint_timmean_fin_num <- as.vector(ncdf4::ncvar_get(fldint_timmean_fin_nc, varname))
    fldint_timmean_fout_nc <- ncdf4::nc_open(fldint_timmean_fout)
    fldint_timmean_fout_num <- as.vector(ncdf4::ncvar_get(fldint_timmean_fout_nc, varname))
    fldint_timmean_fout_adjusted_nc <- ncdf4::nc_open(fldint_timmean_fout_adjusted)
    fldint_timmean_fout_adjusted_num <- as.vector(ncdf4::ncvar_get(fldint_timmean_fout_adjusted_nc, varname))
    message("--> fldint timmean fin = ", fldint_timmean_fin_num)
    bias <- fldint_timmean_fout_num - fldint_timmean_fin_num
    sign <- ifelse(bias >= 0, "+", "-")
    message("--> fldint timmean remap ", cdo_remap, " = ", fldint_timmean_fout_num, " (", sign, abs(bias), " ~ ", sign, abs(bias)/abs(fldint_timmean_fin_num)*100, "% wrt input)")
    bias <- fldint_timmean_fout_adjusted_num - fldint_timmean_fin_num
    sign <- ifelse(bias >= 0, "+", "-")
    message("--> fldint timmean remap ", cdo_remap, " adjusted = ", fldint_timmean_fout_adjusted_num, " (", sign, abs(bias), " ~ ", sign, abs(bias)/abs(fldint_timmean_fout_adjusted_num)*100, "% wrt input)")

    if (clean) {
        message("\n`clean` = true --> rm tmp files ...")
        invisible(file.remove(c(fout,
                                fldint_timmean_fin,
                                fldint_timmean_fout,
                                fldint_timmean_fout_adjusted)))
    } # if clean

} # which method

message("\nfinished\n")


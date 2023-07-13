# r

# quasi-conservative (i.e. fldsums of input and remapped fields as equal as possible) remapping from source to target grid

rm(list=ls()); graphics.off()

# which method
# default:  - normalize interpolated values locally: divide interpolated field by its fldsum and multiply by fldsum of input field
# R14:      - as in section 2.4.3 "Residual flux redistribution" in Rackow 2014 (https://media.suub.uni-bremen.de/handle/elib/829) 
#           - due to the global correction, this method yields values != 0 everywhere
#           - those values != 0 can also be of unwanted sign, e.g. negative for a concentration
method <- "default" # "default" or "R14"
#fin <- c("wfo"="/work/ba1103/a270073/forcing/input4MIPs/wfo_input4MIPs_surfaceFluxes_FAFMIP_NCAS-2-1-0_gn.nc")
#fin <- c("lime_mask"="/work/ba1103/a270120/alkalinization_masks/lime_mask_cao_2040-high.nc")
#fin <- c("lime_mask"="/work/ba1103/a270073/forcing/FESOM1/core/lime_mask_cao_2040-high.nc_timestep_1030-1032")
fin <- c("FWFGRNL"="/work/ab0246/a270073/data/bamber_etal_12/data/PalMod_GrnlHosing_5kmGrid.nc")
target_griddes <- c("fesom1_core"="/pool/data/AWICM/FESOM1/MESHES/core/griddes.nc")
cdo <- Sys.which("cdo")
cdo <- "/sw/spack-levante/cdo-2.0.6-jkuh4i/bin/cdo" # cdo 2.0.5 on levante broken?
cdo_remap <- "remapycon"
outdir <- "/work/ba1103/a270073/forcing/FESOM1/core"
clean <- T

####################################################################

if (!any(method == c("default", "R14"))) stop("`method` must be one of \"default\", \"R14\"")
if (cdo == "") stop("could not find cdo")
if (method == "R14") {
    ncap2 <- Sys.which("ncap2")
    if (ncap2 == "") stop("could not find ncap2")
    ncwa <- Sys.which("ncwa")
    if (ncwa == "") stop("could not find ncwa")
}
if (length(fin) != 1) stop("`fin` must be of length 1")
if (is.null(names(fin))) stop("`fin` must be a named vector with the name indicating the variable that shall be remapped") 
if (file.info(fin)$isdir) stop("`fin` ", fin, " is directory and not a file")
if (!file.exists(fin)) stop("`fin` ", fin, " does not exist")
if (file.access(fin, mode=4) == -1) stop("fin ", fin, " is not readable")
if (is.null(names(target_griddes))) stop("`target_griddes` must be a named vector with the name of fesom mesh; used for output filenames")
message("load ncdf4 package ...")
library(ncdf4)
warn <- options()$warn

message("\nget quasi-conservative remapped field with method = ", method, " ...")
if (method == "default") {
    
    # get fldsum of input field
    message("\nget fldsum of input field ...")
    fin_fldsum <- paste0(outdir, "/", basename(fin), "_fldsum")
    if (!file.exists(fin_fldsum)) {
        cmd <- paste0(cdo, " -s -fldsum -select,name=", names(fin), " ", fin, " ", fin_fldsum)
        message("run `", cmd, "` ...")
        check <- system(cmd)
        if (check != 0) stop("cmd not successful")
    }
    message("open ", fin_fldsum, " ...")
    fin_fldsum_nc <- ncdf4::nc_open(fin_fldsum)
    fin_fldsum_num <- as.numeric(ncdf4::ncvar_get(fin_fldsum_nc, names(fin)))
    if (!is.vector(fin_fldsum_num)) { # fldsum result has more than 1 dim
        stop("fldsum result has more than 1 dim. this case is not impolemented")
    }
    ntime <- length(fin_fldsum_num)
    message("--> fldsums of ", ntime, " input fields:")
    print(summary(fin_fldsum_num))

    # remap input field to target grid
    message("\nremap input field with cdo_remap = ", cdo_remap, " ...")
    fout <- paste0(outdir, "/", basename(fin), "_", names(target_griddes))
    if (!file.exists(fout)) {
        cmd <- paste0(cdo, " -s -P ", system("nproc", intern=T), " -", cdo_remap, ",", target_griddes, " -select,name=", names(fin), " ", fin, " ", fout)
        message("run `", cmd, "` ...")
        check <- system(cmd)
        if (check != 0) stop("cmd not successful")
    }

    # get fldsum of interpolated field
    message("\nget fldsum of interpolated field ...")
    fout_fldsum <- paste0(fout, "_fldsum")
    if (!file.exists(fout_fldsum)) {
        cmd <- paste0(cdo, " -s -fldsum ", fout, " ", fout_fldsum)
        message("run `", cmd, "` ...")
        check <- system(cmd)
        if (check != 0) stop("cmd not successful")
    }
    fout_fldsum_nc <- ncdf4::nc_open(fout_fldsum)
    fout_fldsum_num <- as.numeric(ncdf4::ncvar_get(fout_fldsum_nc, names(fin)))
    message("--> fldsums of ", ntime, " interpolated fields:")
    print(summary(fout_fldsum_num))

    # normalize interpolated field by dividing by fldsum of interpolated field
    # and multiply by fldsum of input field
    # --> only necessary if fldsum != 0
    message("\nnormalize interpolated field: divide interpolated field by fldsum of interpolated field and multiply with fldsum of input field ...")
    fouts_normalized <- fouts_adjusted <- rep(NA, t=ntime)
    for (ti in seq_along(fout_fldsum_num)) {
        message("********************** ti ", ti, "/", ntime, " **********************")
        fout_normalized <- paste0(fout, "_normalized_ti_", ti, "_of_", length(fout_fldsum_num), "_pid_", Sys.getpid())
        fouts_normalized[ti] <- fout_normalized
        fout_adjusted <- paste0(fout_normalized, "_adjusted")
        fouts_adjusted[ti] <- fout_adjusted
        if (!file.exists(fout_normalized)) {
            if (fout_fldsum_num[ti] == 0) { # normalization not needed (and not possible); just select time step
                message("fldsum of remapped field is zero --> normalization not possible due to division by zero")
                cmd <- paste0(cdo, " -s -seltimestep,", ti, " ", fout, " ", fout_adjusted)
            } else {
                cmd <- paste0(cdo, " -s -divc,", fout_fldsum_num[ti], " -seltimestep,", ti, " ", fout, " ", fout_normalized)
            }
            message("run `", cmd, "` ...")
            check <- system(cmd)
            if (check != 0) stop("cmd not successful")
        }

        # check if fldsum of normalized interpolated field equals 1
        if (fout_fldsum_num[ti] != 0) {
            fout_normalized_fldsum <- paste0(fout_normalized, "_fldsum")
            if (!file.exists(fout_normalized_fldsum)) {
                cmd <- paste0(cdo, " -s -fldsum ", fout_normalized, " ", fout_normalized_fldsum)
                message("run `", cmd, "` ...")
                check <- system(cmd)
                if (check != 0) stop("cmd not successful")
            }
            fout_normalized_fldsum_nc <- ncdf4::nc_open(fout_normalized_fldsum)
            fout_normalized_fldsum_num <- as.vector(ncdf4::ncvar_get(fout_normalized_fldsum_nc, names(fin)))
            message("--> fldsum of normalized interpolated field = ", fout_normalized_fldsum_num)
            tol <- sqrt(.Machine$double.eps) # default of base::all.equal
            if (!all.equal(fout_normalized_fldsum_num, 1, tol=tol)) { 
                stop("--> != 1 with tolerance = `sqrt(.Machine$double.eps)` = ", tol)
            } else {
                message("--> = 1 with tolerance = `sqrt(.Machine$double.eps)` = ", tol)
            }
            invisible(file.remove(fout_normalized_fldsum))

            # multiply normalized interpolated field by fldsum of input
            if (!file.exists(fout_adjusted)) {
                cmd <- paste0(cdo, " -s -mulc,", fin_fldsum_num[ti], " ", fout_normalized, " ", fout_adjusted)
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
    fout_adjusted <- paste0(fout, "_adjusted")
    if (!file.exists(fout_adjusted)) {
        cmd <- paste0(cdo, " select,name=", names(fin), " ", paste(fouts_adjusted, collapse=" "), " ", fout_adjusted)
        scriptname <- paste0(fout_adjusted, "_script.txt")
        writeLines(cmd, con=scriptname)
        cmd_source <- paste0(". ", scriptname)
        message("run `", cmd_source, "` ...")
        check <- system(cmd_source)
        if (check != 0) stop("cmd not successful")
        invisible(file.remove(scriptname))
    }

    # check
    message("\ncheck ...")
    fin_fldsum_timsum <- paste0(fin_fldsum, "_timsum")
    if (!file.exists(fin_fldsum_timsum)) {
        cmd <- paste0(cdo , " -s -timsum ", fin_fldsum, " ", fin_fldsum_timsum)
        message("run `", cmd, "` ...")
        check <- system(cmd)
        if (check != 0) stop("cmd not successful")
    }
    fin_fldsum_timsum_nc <- ncdf4::nc_open(fin_fldsum_timsum)
    fin_fldsum_timsum_num <- as.numeric(ncdf4::ncvar_get(fin_fldsum_timsum_nc, names(fin)))
    
    fout_fldsum_timsum <- paste0(fout_fldsum, "_timsum")
    if (!file.exists(fout_fldsum_timsum)) {
        cmd <- paste0(cdo , " -s -timsum ", fout_fldsum, " ", fout_fldsum_timsum)
        message("run `", cmd, "` ...")
        check <- system(cmd)
        if (check != 0) stop("cmd not successful")
    }
    fout_fldsum_timsum_nc <- ncdf4::nc_open(fout_fldsum_timsum)
    fout_fldsum_timsum_num <- as.numeric(ncdf4::ncvar_get(fout_fldsum_timsum_nc, names(fin)))
    
    fout_adjusted_fldsum_timsum <- paste0(fout_adjusted, "_fldsum_timsum")
    if (!file.exists(fout_adjusted_fldsum_timsum)) {
        cmd <- paste0(cdo , " -s -fldsum -timsum ", fout_adjusted, " ", fout_adjusted_fldsum_timsum)
        message("run `", cmd, "` ...")
        check <- system(cmd)
        if (check != 0) stop("cmd not successful")
    }
    fout_adjusted_fldsum_timsum_nc <- ncdf4::nc_open(fout_adjusted_fldsum_timsum)
    fout_adjusted_fldsum_timsum_num <- as.numeric(ncdf4::ncvar_get(fout_adjusted_fldsum_timsum_nc, names(fin)))
    
    message("--> fldsum timsum fin ", fin_fldsum_timsum_num)
    bias <- fout_fldsum_timsum_num - fin_fldsum_timsum_num
    sign <- ifelse(bias >= 0, "+", "-")
    message("--> fldsum timsum ", cdo_remap, " = ", fout_fldsum_timsum_num, " (", sign, abs(bias), " ~ ", sign, abs(bias)/abs(fin_fldsum_timsum_num)*100, "% wrt input)")
    bias <- fout_adjusted_fldsum_timsum_num - fin_fldsum_timsum_num
    sign <- ifelse(bias >= 0, "+", "-")
    message("--> fldsum timsum ", cdo_remap, " adjusted = ", fout_adjusted_fldsum_timsum_num, " (", sign, abs(bias), " ~ ", sign, abs(bias)/abs(fout_adjusted_fldsum_timsum_num)*100, "% wrt input)")
    
    if (clean) {
        message("\n`clean` = true --> rm tmp files ...")
        inds <- which(file.exists(fouts_normalized))
        if (length(inds) > 0) fouts_normalized <- fouts_normalized[inds]
        invisible(file.remove(c(fin_fldsum,
                                fout_fldsum,
                                fouts_normalized,
                                fouts_adjusted,
                                fin_fldsum_timsum,
                                fout_fldsum_timsum,
                                fout_adjusted_fldsum_timsum)))
    } # if clean

} else if (method == "R14") { # original R14 method

    # get ntime of input
    message("\nget ntime of input ...")
    cmd <- paste0(cdo, " -s ntime -select,name=", names(fin), " ", fin)
    message("run `", cmd, " ...")
    ntime <- system(cmd, intern=T)
    message("--> ntime = \"", ntime, "\"")
    message("convert to integer ... ", appendLF=F)
    options(warn=2); ntime <- as.integer(ntime); options(warn=warn)
    message("ok: ", ntime)

    # get size of target grid
    message("\nget size of target grid ...")
    cmd <- paste0(cdo, " -s -griddes -select,name=cell_area ", target_griddes)
    message("run `", cmd, "` ...")
    target_griddes_num <- system(cmd, intern=T)
    message("find one line with pattern \"^gridsize  \" ...")
    ind <- grep("^gridsize  = ", target_griddes_num)
    if (length(ind) != 1) stop("found ", length(ind), " lines")
    gridsize <- target_griddes_num[ind] # e.g. "gridsize  = 126859"
    gridsize <- trimws(strsplit(gridsize, "=")[[1]][2])
    message("--> gridsize = \"", gridsize, "\"")
    message("convert to integer ... ", appendLF=F)
    options(warn=2); gridsize <- as.integer(gridsize); options(warn=warn)
    message("ok: ", gridsize)

    # get maximum cell_area of target grid to calculcate weights of target grid
    message("\nget cell_area fldsum of target grid ", target_griddes, " ...")
    target_grid_cell_area_fldsum <- paste0(outdir, "/", names(target_griddes), "_cell_area_fldsum")
    if (!file.exists(target_grid_cell_area_fldsum)) {
        cmd <- paste0(cdo, " -s -fldsum -select,name=cell_area ", target_griddes, " ", target_grid_cell_area_fldsum)
        message("run `", cmd, "` ...")
        check <- system(cmd)
        if (check != 0) stop("cmd not successful")
    }
    message("open ", target_grid_cell_area_fldsum, " ...")
    target_grid_cell_area_fldsum_nc <- ncdf4::nc_open(target_grid_cell_area_fldsum)
    target_grid_cell_area_fldsum_num <- ncdf4::ncvar_get(target_grid_cell_area_fldsum_nc, "cell_area")
    message("--> ", target_grid_cell_area_fldsum_num)

    # calculate target grid weights
    message("\ncreate target grid weights = cell_area/", target_grid_cell_area_fldsum_num, " ...")
    target_grid_weights <- paste0(outdir, "/", names(target_griddes), "_weights")
    if (!file.exists(target_grid_weights)) {
        cmd <- paste0(cdo, " -s -setname,cell_area_weight -divc,", target_grid_cell_area_fldsum_num, " -select,name=cell_area ", target_griddes, " ", target_grid_weights)
        message("run `", cmd, "` ...")
        check <- system(cmd)
        if (check != 0) stop("cmd not successful")

        # check if fldsum of weights equal 1
        message("\ncheck if fldsum of weights equals 1 ...")
        target_grid_weights_fldsum <- paste0(outdir, "/", names(target_griddes), "_weights_fldsum")
        if (!file.exists(target_grid_weights_fldsum)) {
            cmd <- paste0(cdo , " -s -fldsum ", target_grid_weights, " ", target_grid_weights_fldsum)
            message("run `", cmd, "` ...")
            check <- system(cmd)
            if (check != 0) stop("cmd not successful")
        }
        message("open ", target_grid_weights_fldsum, " ...")
        target_grid_weights_fldsum_nc <- ncdf4::nc_open(target_grid_weights_fldsum)
        target_grid_weights_fldsum_num <- as.vector(ncdf4::ncvar_get(target_grid_weights_fldsum_nc, "cell_area_weight"))
        message("--> target_grid_weights_fldsum_num = ", target_grid_weights_fldsum_num)
        tol <- sqrt(.Machine$double.eps) # default of base::all.equal
        if (!all.equal(target_grid_weights_fldsum_num, 1, tol=tol)) { 
            stop("--> != 1 with tolerance = ", tol , " (= `sqrt(.Machine$double.eps)`)")
        } else {
            message("--> = 1 with tolerance = ", tol, " (= `sqrt(.Machine$double.eps)`)")
        }

        # replicate weights in time
        message("\nreplicate weights in time ...")
        cmd <- paste0(ncap2, " -O -s 'defdim(\"time\",", ntime, "); cell_area_weight[time,ncells]=cell_area_weight' ", target_grid_weights, " ", target_grid_weights)
        message("run `", cmd, "` ...")
        check <- system(cmd)
        if (check != 0) stop("cmd not successful")
    }

    # remap input field to target grid
    message("\nremap input field with cdo_remap = ", cdo_remap, " ...")
    fout <- paste0(outdir, "/", basename(fin), "_", names(target_griddes))
    if (!file.exists(fout)) {
        cmd <- paste0(cdo, " -s -P ", system("nproc", intern=T), " -", cdo_remap, ",", target_griddes, " -select,name=", names(fin), " ", fin, " ", fout)
        message("run `", cmd, "` ...")
        check <- system(cmd)
        if (check != 0) stop("cmd not successful")
    }

    # get fldsums of input and remapped fields for residual 
    message("\nget fldsum of intput field (R14:2.47) ...")
    fldsum_fin <- paste0(outdir, "/", basename(fin), "_fldsum")
    if (!file.exists(fldsum_fin)) {
        cmd <- paste0(cdo, " -s -fldsum -select,name=", names(fin), " ", fin, " ", fldsum_fin)
        message("run `", cmd, "` ...")
        check <- system(cmd)
        if (check != 0) stop("cmd not successful")
    }
    message("\nget fldsum of remapped field (R14:2.48) ...")
    fldsum_fout <- paste0(outdir, "/", basename(fin), "_", names(target_griddes), "_fldsum")
    if (!file.exists(fldsum_fout)) {
        cmd <- paste0(cdo, " -s fldsum ", fout, " ", fldsum_fout)
        message("run `", cmd, "` ...")
        check <- system(cmd)
        if (check != 0) stop("cmd not successful")
    }

    # get residual of fldsums
    message("\nget residual of fldsums (R14:2.49; switch order to keep target grid) ...")
    fldsum_residual_orig_minus_remap <- paste0(outdir, "/", basename(fin), "_fldsum_minus_", names(target_griddes), "_fldsum_residual")
    if (!file.exists(fldsum_residual_orig_minus_remap)) {
        # (interp minus remap)*-1 and not (orig minus remap) to get lon/lat from `target_griddes`
        cmd <- paste0(cdo, " -s -mulc,-1 -sub ", fldsum_fout, " ", fldsum_fin, " ", fldsum_residual_orig_minus_remap) 
        message("run `", cmd, "` ...")
        check <- system(cmd)
        if (check != 0) stop("cmd not successful")

        # repliacate residual in space
        message("\nreplicate residual in space ...")
        cmd <- paste0(ncwa, " -O -a ncells ", fldsum_residual_orig_minus_remap, " ", fldsum_residual_orig_minus_remap) # remove redundant dim ncells of length 1
        message("run `", cmd, "` ...")
        check <- system(cmd)
        if (check != 0) stop("cmd not successful")
        cmd <- paste0(ncap2, " -O -s 'defdim(\"ncells\",", gridsize, "); lon[ncells]=lon; lat[ncells]=lat; ", names(fin), "[time,ncells]=", names(fin), "' ", 
                      fldsum_residual_orig_minus_remap, " ", fldsum_residual_orig_minus_remap)
        message("run `", cmd, "` ...")
        check <- system(cmd)
        if (check != 0) stop("cmd not successful")
        cmd <- paste0(cdo, " -s -setgrid,", target_griddes, " ", fldsum_residual_orig_minus_remap, " ", fldsum_residual_orig_minus_remap, "_tmp && mv ", 
                      fldsum_residual_orig_minus_remap, "_tmp ", fldsum_residual_orig_minus_remap)
        message("run `", cmd, "` ...")
        check <- system(cmd)
        if (check != 0) stop("cmd not successful")
    }

    # calculate modified remapped field for all time points 
    message("\ncalculate remapped field adjusted by weights (R14:2.50) ... (todo: decide cases as in R14:2.51)")
    fout_adjusted <- paste0(fout, "_adjusted") 
    if (!file.exists(fout_adjusted)) {
        cmd <- paste0(cdo, " -s -add ", fout, " -mul [ ", target_grid_weights, " ", fldsum_residual_orig_minus_remap, " ] ", fout_adjusted)
        message("run `", cmd, "` ...")
        check <- system(cmd)
        if (check != 0) stop("cmd not successful")
    }

    # check timsum and fldsum of input and adjusted output
    message("\ncheck timsum and fldsum of input, output and adjusted output ...")
    fldsum_timsum_fin <- paste0(outdir, "/", basename(fin), "_timsum_fldsum")
    if (!file.exists(fldsum_timsum_fin)) {
        cmd <- paste0(cdo, " -s -fldsum -timsum ", fin, " ", fldsum_timsum_fin)
        check <- system(cmd)
        if (check != 0) stop("cmd not successful")
    }
    fldsum_timsum_fout <- paste0(fout, "_timsum_fldsum")
    if (!file.exists(fldsum_timsum_fout)) {
        cmd <- paste0(cdo, " -s -fldsum -timsum ", fout, " ", fldsum_timsum_fout)
        check <- system(cmd)
        if (check != 0) stop("cmd not successful")
    }
    fldsum_timsum_fout_adjusted <- paste0(fout_adjusted, "_timsum_fldsum")
    if (!file.exists(fldsum_timsum_fout_adjusted)) {
        cmd <- paste0(cdo, " -s -fldsum -timsum ", fout_adjusted, " ", fldsum_timsum_fout_adjusted)
        check <- system(cmd)
        if (check != 0) stop("cmd not successful")
    }
    fldsum_timsum_fin_nc <- ncdf4::nc_open(fldsum_timsum_fin)
    fldsum_timsum_fin_num <- as.vector(ncdf4::ncvar_get(fldsum_timsum_fin_nc, names(fin)))
    fldsum_timsum_fout_nc <- ncdf4::nc_open(fldsum_timsum_fout)
    fldsum_timsum_fout_num <- as.vector(ncdf4::ncvar_get(fldsum_timsum_fout_nc, names(fin)))
    fldsum_timsum_fout_adjusted_nc <- ncdf4::nc_open(fldsum_timsum_fout_adjusted)
    fldsum_timsum_fout_adjusted_num <- as.vector(ncdf4::ncvar_get(fldsum_timsum_fout_adjusted_nc, names(fin)))
    message("--> fldsum timsum fin = ", fldsum_timsum_fin_num)
    bias <- fldsum_timsum_fout_num - fldsum_timsum_fin_num
    sign <- ifelse(bias >= 0, "+", "-")
    message("--> fldsum timsum remap ", cdo_remap, " = ", fldsum_timsum_fout_num, " (", sign, abs(bias), " ~ ", sign, abs(bias)/abs(fldsum_timsum_fin_num)*100, "% wrt input)")
    bias <- fldsum_timsum_fout_adjusted_num - fldsum_timsum_fin_num
    sign <- ifelse(bias >= 0, "+", "-")
    message("--> fldsum timsum remap ", cdo_remap, " adjusted = ", fldsum_timsum_fout_adjusted_num, " (", sign, abs(bias), " ~ ", sign, abs(bias)/abs(fldsum_timsum_fout_adjusted_num)*100, "% wrt input)")

    if (clean) {
        message("\n`clean` = true --> rm tmp files ...")
        invisible(file.remove(c(target_grid_cell_area_fldsum, 
                                target_grid_weights,
                                target_grid_weights_fldsum,
                                fldsum_fin,
                                fldsum_fout,
                                fldsum_residual_orig_minus_remap,
                                fout,
                                fldsum_timsum_fin,
                                fldsum_timsum_fout,
                                fldsum_timsum_fout_adjusted)))
    } # if clean

} # which method

message("\nfinished\n")


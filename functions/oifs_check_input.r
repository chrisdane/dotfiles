#!/usr/bin/env Rscript

# r

me <- "oifs_check_input.r"
help <- paste0("\nUsage:\n",
               " $ ", me, " /out/path ICM*\n")
    
# get args
if (interactive()) {
    args <- c(".", "ICM*")
} else {
    args <- commandArgs(trailingOnly=F)
    me <- basename(sub("--file=", "", args[grep("--file=", args)]))
    args <- commandArgs(trailingOnly=T)
}

# check args 
if (length(args) < 2) {
    message(help)
    if (interactive()) {
        stop("error")
    } else {
        quit()
    }
}

cdo_gaussian_reduced2regular <- "-setgridtype,regular"
#cdo_spectral2gaussian <- "-sp2gpl" # linear Gaussian
cdo_spectral2gaussian <- "-sp2gp" # quadratic Gaussian
#grib2nc <- "cdo"
grib2nc <- "grib_to_netcdf" # better preserves variable names & attributes

# checks
cdo <- Sys.which("cdo")
if (cdo == "") stop("could not find cdo")

outpath <- args[1]
dir.create(outpath, recursive=T, showWarnings=F)
if (!dir.exists(outpath)) stop("could not create outpath ", outpath)

fs <- args[2:length(args)]
if (interactive()) { # apply potential wildcards
    tmp <- list.files(dirname(fs), pattern=basename(fs), full.names=T)
    if (length(tmp) == 0) {
        tmp <- list.files(dirname(fs), pattern=glob2rx(basename(fs)), full.names=T)
        if (length(tmp) == 0) {
            stop("interactive found zero files")
        }
    }
    fs <- tmp
}

# convert and/or regrid input files
if (length(fs) == 0) {
    message("found zero files with pattern \"", args, "\"")
} else {
    for (fi in seq_along(fs)) {
        message("****************************************\n",
                "file ", fi, "/", length(fs), ": ", fs[fi])
        fout <- paste0(outpath, "/", fs[fi], "_regular.nc")
        if (file.exists(fout)) {
            message("fout ", fout, " already exists. skip")
        } else if (!file.exists(fout)) {
            
            # get grid type
            cmd <- paste0(cdo, " griddes ", fs[fi])
            message("run `", cmd, "` ...")
            griddes <- system(cmd, intern=T)
            if (T) message(paste(griddes[1:min(length(griddes), 15)], collapse="\n"))
            grid_type <- griddes[grep("^gridtype  = ", griddes)]
            cdo_regrid <- ""
            if (grepl("= gaussian_reduced", grid_type)) {
                cdo_regrid <- cdo_gaussian_reduced2regular
            } else if (grepl("= spectral", grid_type)) {
                cdo_regrid <- cdo_spectral2gaussian
            }
            message("--> `cdo_regrid` = \"", cdo_regrid, "\"")
            
            # get file format
            cmd <- paste0(cdo, " showformat ", fs[fi])
            message("run `", cmd, "` ...")
            format <- system(cmd, intern=T)
            convert_grib2nc <- F
            if (grepl("GRIB", format)) {
                convert_grib2nc <- T
            }
            message("--> `format` = \"", format, "\" --> `convert_grib2nc` = ", convert_grib2nc)
            
            if (!convert_grib2nc && cdo_regrid == "") { # nothing to do
                message("--> fin is already nc and regrid not necessary")
            } else {
                
                # regrid (and convert if wanted with cdo)
                cmd <- ""
                if (cdo_regrid != "") cmd <- paste0(cmd, " -P $(nproc) ", cdo_regrid)
                if (convert_grib2nc && grib2nc == "cdo") cmd <- paste0(cmd, " -t ecmwf -f nc")
                if (cmd != "") {
                    cmd <- paste0(cdo, " ", cmd, " ", fs[fi], " ", fout)
                    message("run `", cmd, "` ...")
                    check <- system(cmd)
                    if (check != 0) stop("error")
                } else {
                    if (convert_grib2nc && grib2nc == "grib_to_netcdf") { # make input for next step below
                        cmd <- paste0("ln -s ", fs[fi], " ", fout)
                        message("run `", cmd, "` ...")
                        check <- system(cmd)
                        if (check != 0) stop("error")
                    }
                }

                # convert to nc via grib_to_netcdf
                if (convert_grib2nc && grib2nc == "grib_to_netcdf") {
                    grib_to_netcdf <- Sys.which("grib_to_netcdf")
                    if (grib_to_netcdf == "") stop("could not find grib_to_netcdf")
                    
                    cmd <- paste0(grib_to_netcdf, " -o ", fout, "_tmp ", fout, " && mv ", fout, "_tmp ", fout)
                    message("run `", cmd, "` ...")
                    check <- system(cmd)
                    if (check != 0) {
                        message("some error occured (if grib_to_netcdf complains that the input is not distinct, maybe there are duplicated entries?)\n",
                                "--> will use cdo to convert to nc ...")
                        cmd <- paste0(cdo, " -t ecmwf -f nc copy ", fout, " ", fout, "_tmp && mv ", fout, "_tmp ", fout)
                        message("run `", cmd, "` ...")
                        check <- system(cmd)
                        if (check != 0) stop("error")
                    }
                }

            } # fin already nc and regrid not necessary
        } # if fout already exists
    } # for fi
} # of some files found


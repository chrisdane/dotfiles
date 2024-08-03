#!/usr/bin/env Rscript

if (interactive()) {
    rm(list=ls())
    me <- "my_gsw_O2sol_SP_pt.r"
    if (T) { # non-levelwise
        args <- c("thetao=/work/ba1103/a270073/out/awicm-1.0-recom/awi-esm-1-1-lr_kh800/historical2/outdata/post/fesom/monmean/thetao_fesom_20140101.nc",
                  "so=/work/ba1103/a270073/out/awicm-1.0-recom/awi-esm-1-1-lr_kh800/historical2/outdata/post/fesom/monmean/so_fesom_20140101.nc",
                  "nod3d.out=/pool/data/AWICM/FESOM1/MESHES/core/nod3d.out",
                  "unit_out=mmol m-3",
                  "fout=/work/ba1103/a270073/out/awicm-1.0-recom/awi-esm-1-1-lr_kh800/historical2/outdata/post/recom/monmean/o2sat_2014.nc")
    } else if (F) { # levelwise
        args <- c("thetao=/work/ba1103/a270073/out/awicm-1.0-recom/awi-esm-1-1-lr_kh800/esm-hist/outdata/post/fesom/levelwise/thetao_fesom_20140101_levelwise_0-5900m_setgrid.nc",
                  "so=/work/ba1103/a270073/out/awicm-1.0-recom/awi-esm-1-1-lr_kh800/esm-hist/outdata/post/fesom/levelwise/so_fesom_20140101_levelwise_0-5900m_setgrid.nc",
                  "unit_out=mmol m-3",
                  "fout=/work/ba1103/a270073/out/awicm-1.0-recom/awi-esm-1-1-lr_kh800/esm-hist/outdata/post/recom/levelwise/o2sat_remin_20140101_levelwise_0-5900m_setgrid.nc")
    }
} else {
    args <- commandArgs(trailingOnly=F)
    me <- basename(sub("--file=", "", args[grep("--file=", args)]))
    args <- commandArgs(trailingOnly=T)
}

usage <- paste0("\nUsage:\n",
                " $ ", me, " thetao_varname=/path/to/thetao so_varname=/path/to/so [nod3d.out=/path/to/nod3d.out/if/fesom1/non-levelwise] [unit_out=mmol m-3] fout=/path/to/fout\n",
                "\n",
                "e.g. nod3d.out=/pool/data/AWICM/FESOM1/MESHES/core/nod3d.out\n",
                "\n")

# check
if (length(args) < 3) {
    message(usage)
    if (interactive()) {
        stop("stop interactive call")
    } else {
        quit()
    }
}

if (!interactive()) {
    library(ncdf4)
    library(gsw)
}
cdo <- Sys.which("cdo")
if (cdo == "") stop("could not find cdo")

thetao_varname <- strsplit(args[1], "=")[[1]]
if (length(thetao_varname) != 2) stop("1st arg must be `thetao_varname=/path/to/file`")
thetao_file <- thetao_varname[2]
if (!file.exists(thetao_file)) {
    if (interactive()) {
        stop("thetao file", thetao_file, " does not exist")
    } else {
        message("thetao file", thetao_file, " does not exist")
        quit()
    }
}
thetao_varname <- thetao_varname[1]

so_varname <- strsplit(args[2], "=")[[1]]
if (length(so_varname) != 2) stop("2nd arg must be `so_varname=/path/to/file`")
so_file <- so_varname[2]
if (!file.exists(so_file)) {
    if (interactive()) {
        stop("so file", so_file, " does not exist")
    } else {
        message("so file", so_file, " does not exist")
        quit()
    }
}
so_varname <- so_varname[1]

if (any(grepl("^fout=", args))) {
    ind <- which(grepl("^fout=", args))
    if (length(ind) != 1) stop("found ", length(ind), " args that start with \"fout\". must be 1")
    fout <- sub("fout=", "", args[ind])
    args <- args[-ind]
} else {
    stop("provide `fout=/path/to/fout")
}
if (file.exists(fout)) {
    if (interactive()) {
        stop("fout ", fout, " already exists. skip") 
    } else {
        message("fout ", fout, " already exists. skip")
        quit()
    }
}
dir.create(dirname(fout), recursive=T, showWarnings=F)
if (!dir.exists(dirname(fout))) stop("could not create output dir ", dirname(fout))

known_units <- c("µmol kg-1", "mmol m-3")
if (any(grepl("^unit_out=", args))) {
    ind <- which(grepl("^unit_out=", args))
    if (length(ind) != 1) stop("found ", length(ind), " args that start with \"out_unit\". must be 1 if provided")
    unit_out <- sub("unit_out=", "", args[ind])
    if (!any(unit_out == known_units)) {
        stop("`unit_out` = ", unit_out, " must be one of ", paste(known_units, collapse=", "))
    }
    args <- args[-ind]
} else {
    unit_out <- "µmol kg-1" # gsw default
}

nod3d_file <- NULL
if (any(grepl("^nod3d.out=", args))) {
    ind <- which(grepl("^nod3d.out=", args))
    if (length(ind) != 1) stop("found ", length(ind), " args that start with \"nod3.out\". must be 1 if provided")
    nod3d_file <- sub("nod3d.out=", "", args[ind])
    if (!file.exists(nod3d_file)) stop("provided nod3d.out file ", nod3d_file, " does not exist")
    args <- args[-ind]
}

#############################################################################

message("\nopen thetao file ", thetao_file, " ...")
thetao_nc <- ncdf4::nc_open(thetao_file)
if (so_file != thetao_file) {
    message("open so file ", so_file, " ...")
    so_nc <- ncdf4::nc_open(so_file)
}
message("\nread thetao varname ", thetao_varname, " ...")
thetao <- ncdf4::ncvar_get(thetao_nc, thetao_varname)
message("read so varname ", so_varname, " ...")
if (so_file != thetao_file) {
    so <- ncdf4::ncvar_get(so_nc, so_varname)
} else {
    so <- ncdf4::ncvar_get(thetao_nc, so_varname)
}
message("\nmin/max thetao = ", min(thetao, na.rm=T), "/", max(thetao, na.rm=T), " °C")
message("min/max so = ", min(so, na.rm=T), "/", max(so, na.rm=T), " g kg-1")

message("\ndim(thetao)    = (", paste(dim(thetao), collapse=","), ")\n",
        "dim(so)        = (", paste(dim(so), collapse=","), ")")

if (is.null(nod3d_file)) {
    if (any(names(thetao_nc$dim) == "lon")) {
        lon <- thetao_nc$dim$lon$vals
    } else if (any(names(thetao_nc$var) == "lon")) {
        lon <- ncdf4::ncvar_get(thetao_nc, "lon")
    } else {
        stop("could not find dim or var called lon")
    }
    if (any(names(thetao_nc$dim) == "lat")) {
        lat <- thetao_nc$dim$lat$vals
    } else if (any(names(thetao_nc$var) == "lat")) {
        lat <- ncdf4::ncvar_get(thetao_nc, "lat")
    } else {
        stop("could not find dim or var called lat")
    }
    if (any(names(thetao_nc$dim) == "depth")) {
        depth <- thetao_nc$dim$depth$vals
    } else if (any(names(thetao_nc$var) == "depth")) {
        depth <- ncdf4::ncvar_get(thetao_nc, "depth")
    } else {
        stop("could not find dim or var called depth")
    }
    if (all(depth >= 0)) depth <- depth*-1
    if (length(dim(thetao)) == 3) { # (nhoriz,ndepth,ntime)
        lon_all <- base::replicate(lon, n=dim(thetao)[2]) # (nhoriz,ndepth)
        lon_all <- base::replicate(lon_all, n=dim(thetao)[3]) # (nhoriz,ndepth,ntime)
        lat_all <- base::replicate(lat, n=dim(thetao)[2]) # (nhoriz,ndepth)
        lat_all <- base::replicate(lat_all, n=dim(thetao)[3]) # (nhoriz,ndepth,ntime)
        depth_all <- base::replicate(depth, n=dim(thetao)[3]) # (ndepth,ntime)
        depth_all <- base::replicate(depth_all, n=dim(thetao)[1]) # (ndepth,ntime,nhoriz)
        depth_all <- base::aperm(depth_all, c(3, 1, 2)) # (nhoriz,ndepth,ntime)
    } else if (length(dim(thetao)) == 4) { # (nlon,nlat,ndepth,ntime)
        lon_all <- base::replicate(lon, n=dim(thetao)[2]) # (nlon,nlat)
        lon_all <- base::replicate(lon_all, n=dim(thetao)[3]) # (nlon,nlat,ndepth)
        lon_all <- base::replicate(lon_all, n=dim(thetao)[4]) # (nlon,nlat,ndepth,ntime)
        lat_all <- base::replicate(lat, n=dim(thetao)[2]) # (nlat,nlon)
        lat_all <- base::aperm(lat_all, n=c(2, 1)) # (nlon,nlat)
        lat_all <- base::replicate(lat_all, n=dim(thetao)[3]) # (nlon,nlat,ndepth)
        lat_all <- base::replicate(lat_all, n=dim(thetao)[4]) # (nlon,nlat,ndepth,ntime)
        depth_all <- base::replicate(depth, n=dim(thetao)[4]) # (ndepth,ntime)
        depth_all <- base::replicate(depth_all, n=dim(thetao)[1]) # (ndepth,ntime,nlon)
        depth_all <- base::replicate(depth_all, n=dim(thetao)[2]) # (ndepth,ntime,nlon,nlat)
        depth_all <- base::aperm(depth_all, c(3, 4, 1, 2)) # (nlon,nlat,ndepth,ntime)
    } else {
        stop("levelwise case `dim(thetao) = (", paste(dim(thetao), collapse=","), ") not defined")
    }

} else if (!is.null(nod3d_file)) {
    message("\narg `nod3.out` was provided --> read nod3d file ", nod3d_file, " ...")
    data.table_check <- suppressWarnings(library(data.table, logical.return=T))
    if (data.table_check) {
        nod3d <- data.table::fread(nod3d_file, skip=1, showProgress=F)
        nod3d <- as.matrix(nod3d)
    } else { # use base::scan
        message("could not find data.table package, will use `base::scan()`. this is slower")
        nod3d <- base::scan(nod3d_file, skip=1, quiet=T)
        nod3d <- matrix(nod3d, nrow=n3, byrow=T)
    }
    lon <- nod3d[,2]
    lat <- nod3d[,3]
    depth <- nod3d[,4]
    if (all(depth >= 0)) depth <- depth*-1
    if (length(dim(thetao)) == 2) { # (n3,ntime)
        lon_all <- replicate(lon, n=dim(thetao)[2]) # (n3,ntime)
        lat_all <- replicate(lat, n=dim(thetao)[2]) # (n3,ntime)
        depth_all <- replicate(depth, n=dim(thetao)[2]) # (n3,ntime)
    } else {
        stop("non-levelwise case `dim(thetao) = (", paste(dim(thetao), collapse=","), ") not defined")
    }
} # if nod3d

message("dim(lon_all)   = (", paste(dim(lon_all), collapse=","), ")\n",
        "dim(lat_all)   = (", paste(dim(lat_all), collapse=","), ")\n",
        "dim(depth_all) = (", paste(dim(depth_all), collapse=","), ")")

# calc
message("\ncalc O2 saturation in µmol kg-1 ...")
o2sat <- gsw::gsw_O2sol_SP_pt(SP=so, pt=thetao)
#o2sat <- gsw::gsw_O2sol(SA=SA, CT=CT, p=p_dbar, longitude=lon_all, latitude=lat_all) # gsw::gsw_O2sol_SP_pt = gsw::gsw_O2sol
message("min/max = ", min(o2sat, na.rm=T), "/", max(o2sat, na.rm=T), " µmol kg-1")

if (unit_out == "mmol m-3") {
    message("\nunit_out = \"", unit_out, "\" --> get seawater density ...")
    
    message("\ncalc pressure ...")
    p_dbar <- gsw::gsw_p_from_z(z=depth_all, latitude=lat_all)
    message("min/max = ", min(p_dbar, na.rm=T), "/", max(p_dbar, na.rm=T), " dbar")

    message("\ncalc absolute salinity ...")
    SA <- gsw::gsw_SA_from_SP(SP=so, p=p_dbar, longitude=lon_all, latitude=lat_all)
    message("min/max = ", min(SA, na.rm=T), "/", max(SA, na.rm=T), " g kg-1")

    message("\ncalc conservative temperature ...")
    CT <- gsw::gsw_CT_from_pt(SA=SA, pt=thetao)
    message("min/max = ", min(CT, na.rm=T), "/", max(CT, na.rm=T), " °C")

    # todo: oxygen is in-situ, i.e. mass to volume conversion needs in-situ density?
    message("\ncalc in-situ density ...")
    rho_insitu <- gsw::gsw_rho(SA=SA, CT=CT, p=p_dbar)
    message("min/max = ", min(rho_insitu, na.rm=T), "/", max(rho_insitu, na.rm=T), " kg m-3")

    message("\ncalc O2 saturation from µmol kg-1 to mmol m-3 ...")
    o2sat <- o2sat*rho_insitu/1e3
    message("min/max = ", min(o2sat, na.rm=T), "/", max(o2sat, na.rm=T), " mmol m-3")
}

# save
message("\nsave ", fout, " ...")
ncvar <- thetao_nc$var[[thetao_varname]]
ncvar$name <- "o2sat"
ncvar$units <- unit_out
ncvar$longname <- "Oxygen concentration for seawater that is in equilibrium with vapour-saturated air at standard atmospheric pressure (101.325 kPa, i.e. for sea pressure of 0dbar); gsw_O2sol_SP_pt"
outnc <- ncdf4::nc_create(fout, vars=ncvar, force_v4=T)
ncdf4::ncvar_put(outnc, ncvar, o2sat)
if (is.null(nod3d_file)) {
    ncdf4::ncatt_put(outnc, "depth", "axis", "Z") # necessary for `cdo griddes` below; todo: why doesn't that work automatically?
}
ncdf4::nc_close(outnc)

# apply same griddes as input
if (is.null(nod3d_file)) {
    message("\napply griddes ...")
    cmd <- paste0(cdo, " -setgrid,", thetao_file, " ", fout, " ", fout, "_tmp && mv ", fout, "_tmp ", fout)
    message("run `", cmd, "` ...")
    check <- system(cmd)
    if (check != 0) stop("error")
}

message("\nfinished\n")


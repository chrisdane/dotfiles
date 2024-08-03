#!/usr/bin/env Rscript

# dependencies: myfunctions.r:identical_list()

if (interactive()) {
    rm(list=ls()); graphics.off()
    me <- "mycat_moc.r"
    args <- c("atlantic_arctic_ocean=/work/ab1095/a270073/post/fesom/moc_depth/MOCw/awi-esm-1-1-lr_kh800_piControl2_fesom_moc_depth_MOCw_0-5900m_atlantic_arctic_ocean_Jan-Dec_1850-1851.nc",
              "indian_pacific_ocean=/work/ab1095/a270073/post/fesom/moc_depth/MOCw/awi-esm-1-1-lr_kh800_piControl2_fesom_moc_depth_MOCw_0-5900m_indian_pacific_ocean_Jan-Dec_1850-1851.nc",
              "/work/ab1095/a270073/post/fesom/moc_depth/MOCw/awi-esm-1-1-lr_kh800_piControl2_fesom_moc_depth_MOCw_0-5900m_atl_arc_ind_pac_ocean_Jan-Dec-1850-1851.nc")

} else { # not interactive
    args <- commandArgs(trailingOnly=F)
    me <- basename(sub("--file=", "", args[grep("--file=", args)]))
    args <- commandArgs(trailingOnly=T)
}

usage <- paste0("\nUsage: ", me, " \"basin1=f1\" \"basin2=f2\" [\"basinN=fN\"] fout",
                "\n")

# check
if (length(args) < 3) { # basin1=f1 basin2=f2 [basinN=fN] fout
    if (interactive()) {
        stop(usage)
    } else {
        message(usage)
        quit()
    }
}

fs <- args[1:(length(args)-1)]
if (any(regexpr("=", fs) == -1)) stop(usage)
fs <- strsplit(fs, "=")
sectors <- sapply(fs, "[", 1)
fs <- sapply(fs, "[", 2)
message(length(fs), " input files:")
print(data.frame(sector=sectors, file=fs), width=500)
fout <- args[length(args)]

#################################################################################################

if (file.exists(fout)) {
    message("\nfout ", fout, " already exists. skip")
} else {

    # check
    if (any(!file.exists(fs))) stop("some of input files do not exist")
    dir.create(dirname(fout), recursive=T, showWarnings=F)
    if (!dir.exists(dirname(fout))) stop("could not create outdir ", dirname(fout))
    if (!interactive()) {
        library(ncdf4)
        source("~/scripts/r/functions/myfunctions.r")
    }

    # read moc
    moc <- vector("list", l=length(fs))
    for (fi in seq_along(moc)) {
        message("read file ", fi, "/", length(fs), ": ", fs[fi], " ...")
        nc <- nc_open(fs[fi])
        moc[[fi]] <- list(time=cdo_showtimestamp(nc$filename),
                          lat=ncvar_get(nc, "moc_reg_lat"),
                          depth=nc$dim$depth$vals,
                          moc=ncvar_get(nc, "MOCw"))
    } # for fi

    # common time
    if (identical_list(lapply(moc, "[[", "time"))) {
        time_dim <- nc$dim$time
    } else {
        stop("not implemented")
    }
    
    # common depth
    if (identical_list(lapply(moc, "[[", "depth"))) {
        depth_dim <- nc$dim$depth
        depth_dim$vals <- abs(depth_dim$vals) # negative --> positive
    } else {
        stop("not implemented")
    }

    # common lat
    dlat <- lapply(lapply(lapply(moc, "[[", "lat"), diff), unique)
    if (length(unique(sapply(dlat, unique))) != 1) stop("found different dlat")
    dlat <- unique(sapply(dlat, unique))
    latlim <- range(lapply(moc, "[[", "lat"))
    lat <- seq(latlim[1], latlim[2], b=dlat)
    lat_dim <- ncdim_def("lat", units="degrees north", vals=lat)
        
    # basin dim
    basin_dim <- ncdim_def("basin", units="", vals=seq_along(sectors), create_dimvar=F)
    
    # sector string dim
    strlen_dim <- ncdim_def("strlen", units="", vals=seq_len(max(nchar(sectors))), create_dimvar=F)

    # sector var
    sector_var <- ncvar_def(name="sector", units="", dim=list(strlen_dim, basin_dim), prec="char")

    # moc var
    moc_var <- ncvar_def(name="MOCw", units="Sv", dim=list(lat_dim, depth_dim, basin_dim, time_dim))
    
    # fout
    message("save fout ", fout, " ...")
    outnc <- nc_create(fout, vars=list(sector_var, moc_var), force_v4=T)

    # add sectors to nc
    ncvar_put(outnc, sector_var, sectors)

    # add moc to nc
    for (basini in seq_along(sectors)) {
        rhs <- replicate(moc[[basini]]$moc, n=1) # add basin dim
        rhs <- aperm(rhs, c(1, 2, 4, 3)) # lat, lev, time, basin --> lat, lev, basin, time (will yield reverse nc dims time, basin, lev, lat)
        latinds <- which(!is.na(match(lat, moc[[basini]]$lat)))
        if (length(latinds) != dim(rhs)[1]) stop("did not find correct latitude mapping for basin ", basini)
        lhs <- array(NA, dim=c(length(lat), dim(rhs)[2:4]))
        lhs[latinds,,,] <- rhs
        ncvar_put(outnc, moc_var, lhs, start=c(1, 1, basini, 1), count=dim(lhs))
    } # for basini
    
    ncdf4::ncatt_put(outnc, "depth", "positive", "down")
    ncdf4::ncatt_put(outnc, "depth", "axis", "Z")

    nc_close(outnc)

} # if fout already exists or not

message("\nfinished\n")


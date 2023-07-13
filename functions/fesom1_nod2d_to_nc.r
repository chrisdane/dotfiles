# r

rm(list=ls()); graphics.off()

if (!interactive()) stop("must run interactive")
    
griddes <- "/pool/data/AWICM/FESOM1/MESHES/core/griddes.nc"

################

# get mesh infos
message("open ", griddes, " ...")
griddes_nc <- ncdf4::nc_open(griddes)
lon <- ncdf4::ncvar_get(griddes_nc, "lon")
lat <- ncdf4::ncvar_get(griddes_nc, "lat")
coast <- ncdf4::ncvar_get(griddes_nc, "coast") # 0 or 1
depth_lev <- ncdf4::ncvar_get(griddes_nc, "depth_lev") # e.g. 3 to 46
cell_area <- ncdf4::ncvar_get(griddes_nc, "cell_area")

if (T) { # make fwf
    if (F) { # 0.1 sv freshwater fwf sofia
        fwf_val <- 0.1
        fwf_unit <- "Sv"
        fout <- "/work/ba1103/a270073/mesh/fesom/core/fwf_sofia_antwater_0.1_Sv.nc"
        inds <- which(lat < -60 & coast == 1)
    } else if (T) { # 0.1 sv globally
        fwf_val <- 0.1
        fwf_unit <- "Sv"
        fout <- "/work/ba1103/a270073/mesh/fesom/core/fwf_global_0.1_Sv.nc"
        inds <- seq_along(lon)
    }

    if (length(inds) == 0) stop("asdasd")
    message("make ", fwf_val, " ", fwf_unit, " freshwater fwf at ", length(inds), " inds...")
    fwf <- lon
    fwf[] <- 0
    fwf[inds] <- fwf_val/length(inds) # total amount distributed equally over all identified nodes
    
    # monthly climatology
    time <- seq.POSIXt(as.POSIXct("1970-1-1", tz="UTC"), as.POSIXct("1970-12-31"), b="1 mon")
    fwf <- replicate(fwf, n=length(time)) 
   
    # output
    message("save ", fout, " ...")
    nod2d_dim <- griddes_nc$dim$ncells
    time_dim <- ncdf4::ncdim_def("time", units="seconds since 1970-1-1", vals=as.numeric(time))
    ncvar <- ncdf4::ncvar_def(name="fwf", units=fwf_unit, dim=list(nod2d_dim, time_dim), missval=NaN,
                              longname=paste0(fwf_val, " ", fwf_unit, " freshwater flux distributed equally at ", length(inds), " surface nodes"))
    outnc <- ncdf4::nc_create(fout, vars=ncvar, force_v4=T)
    ncdf4::ncvar_put(outnc, ncvar, fwf)
    ncdf4::ncatt_put(outnc, ncvar, "created by", "~/bin/fesom1_nod2d_to_nc.r")
    ncdf4::nc_close(outnc)
    
    # apply griddes
    cmd <- paste0("cdo setgrid,", griddes, " ", fout, " ", fout, "_tmp && mv ", fout, "_tmp ", fout)
    message("run `", cmd, "` ...")
    check <- system(cmd)
    if (check != 0) stop("cmd failed")

} # fwf

# plot
if (T) {
    plot(lon, lat, t="n", xaxt="n", yaxt="n")
    axis(1, at=pretty(lon, n=10))
    axis(2, at=pretty(lat, n=10), las=2)
    points(lon, lat, pch=".")
    points(lon[inds], lat[inds], pch=16, col=2, cex=0.33)
}


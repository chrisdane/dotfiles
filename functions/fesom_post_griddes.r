# r

rm(list=ls()); graphics.off()

if (!interactive()) stop("must run interactive")
    
#griddes <- c("node"="/pool/data/AWICM/FESOM1/MESHES/core/griddes.nc")
#griddes <- c("node"="/albedo/work/projects/p_pool_recom/meshes/fesom2/core2/core2_griddes_nodes.nc")
griddes <- c("node"="/pool/data/AWICM/FESOM2/MESHES_FESOM2.1/core2/core2_griddes_nodes.nc")
#griddes <- c("elem"="/pool/data/AWICM/FESOM2/MESHES_FESOM2.1/core2/core2_griddes_elements.nc")

################

# get type of griddes: node or elem
griddes_type <- names(griddes)
if (is.null(griddes_type)) {
    stop("`griddes` must be named vector with the name indicating if the griddes is for nodes or elems")
}
if (is.na(match(griddes_type, c("node", "elem")))) stop("griddes-type must be one of node, elem")

# get mesh infos
message("open ", griddes_type, "-griddes ", griddes, " ...")
nc <- ncdf4::nc_open(griddes)
lon <- list(ncdf4::ncvar_get(nc, "lon"))
lat <- list(ncdf4::ncvar_get(nc, "lat"))
coast <- list(ncdf4::ncvar_get(nc, "coast")) # 0 or 1
depth_lev <- list(ncdf4::ncvar_get(nc, "depth_lev")) # e.g. 3 to 46
area <- list(ncdf4::ncvar_get(nc, "cell_area")) # m2
triag_nodes <- list(ncvar_get(nc, "triag_nodes"))

names(lon)[1] <- names(lat)[1] <- names(coast)[1] <- names(depth_lev)[1] <- 
    names(area)[1] <- names(triag_nodes)[1] <- griddes_type

if (griddes_type == "node") {
    reso <- 2*sqrt(area$node/pi)
} else if (griddes_type == "elem") {
    #reso <- 
}
reso <- list(reso)
names(reso)[1] <- griddes_type

message("\nreso_node:")
print(summary(reso$node)/1e3)
#message("\nreso_elem:")
#print(summary(reso$elem)/1e3)

if (griddes_type == "node") { # bring lon lat from nod2d to triag_nodes
    message("\nbring lon/lat from nod2d --> triag_nodes ...")
    lon_e2 <- lat_e2 <- rep(NA, t=ncol(triag_nodes$node))
    for (ei in seq_along(lon_e2)) {
        inds <- triag_nodes$node[,ei]
        lon_e2[ei] <- mean(lon$node[inds])
        lat_e2[ei] <- mean(lat$node[inds])
    } # for ei

} else if (griddes_type == "elem") { # bring resolution from triag_nodes to nod2d
    message("\nbring resolution from triag_nodes --> nod2d ...")
    mync <- nc_open("/work/ba1103/a270073/mesh/fesom/core/derivatives/mesh_core_deriv_2d_geo.nc")
    myarea_e2 <- ncvar_get(mync, "voltriangle")
    myres_e2 <- ncvar_get(mync, "resolution")
    myres_n2 <- nelems <- rep(0, t=length(lon$node))
    for (ei in seq_len(ncol(triag_nodes$elem))) {
        inds <- triag_nodes$elem[,ei]
        res_n2[inds] <- res_n2[inds] + rep(myres_e2[ei], t=3)
        nelems[inds] <- nelems[inds] + 1
    }
    myres_n2 <- myres_n2/nelems
}

if (F) { # plot nodes
    plot(lon$node, lat$node, t="n", xaxt="n", yaxt="n")
    axis(1, at=pretty(lon$node, n=10))
    axis(2, at=pretty(lat$node, n=10), las=2)
    points(lon$node, lat$node, pch=".")
}

if (F) { # make fwf
    if (F) { # 0.1 sv freshwater fwf sofia
        fwf_val <- 0.1
        fwf_unit <- "Sv"
        fout <- "/work/ba1103/a270073/mesh/fesom/core/fwf_sofia_antwater_0.1_Sv.nc"
        inds <- which(lat$node < -60 & coast$node == 1)
    } else if (T) { # 0.1 sv globally
        fwf_val <- 0.1
        fwf_unit <- "Sv"
        fout <- "/work/ba1103/a270073/mesh/fesom/core/fwf_global_0.1_Sv.nc"
        inds <- seq_along(lon$node) # all
    }

    if (length(inds) == 0) stop("asdasd")
    message("make ", fwf_val, " ", fwf_unit, " freshwater fwf at ", length(inds), " inds...")
    fwf <- lon$node
    fwf[] <- 0
    fwf[inds] <- fwf_val/length(inds) # total amount distributed equally over all identified nodes
    
    # monthly climatology
    time <- seq.POSIXt(as.POSIXct("1970-1-1", tz="UTC"), as.POSIXct("1970-12-31"), b="1 mon")
    fwf <- replicate(fwf, n=length(time)) 
   
    # output
    message("save ", fout, " ...")
    nod2d_dim <- nc$dim$ncells
    time_dim <- ncdf4::ncdim_def("time", units="seconds since 1970-1-1", vals=as.numeric(time))
    ncvar <- ncdf4::ncvar_def(name="fwf", units=fwf_unit, dim=list(nod2d_dim, time_dim), missval=NaN,
                              longname=paste0(fwf_val, " ", fwf_unit, " freshwater flux distributed equally at ", length(inds), " surface nodes"))
    outnc <- ncdf4::nc_create(fout, vars=ncvar, force_v4=T)
    ncdf4::ncvar_put(outnc, ncvar, fwf)
    ncdf4::ncatt_put(outnc, ncvar, "created by", "~/dotfiles/functions/fesom_post_griddes.r")
    ncdf4::nc_close(outnc)
    
    # apply griddes
    cmd <- paste0("cdo setgrid,", griddes, " ", fout, " ", fout, "_tmp && mv ", fout, "_tmp ", fout)
    message("run `", cmd, "` ...")
    check <- system(cmd)
    if (check != 0) stop("cmd failed")
    
    if (!is.null(dev.list())) {
        points(lon$node[inds], lat$node[inds], pch=16, col=2, cex=0.33)
    }

} # fwf


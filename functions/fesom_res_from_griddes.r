#!/usr/bin/env Rscript

# calc horizontal resolution based on griddes (from https://github.com/FESOM/spheRlab.git:sl.grid.writeCDO.R)
# following Danilov 2022: https://agupubs.onlinelibrary.wiley.com/doi/full/10.1029/2022MS003177
# --> dmax = sqrt(2*area_elem) ~ sqrt(area_node) # call it 'dmax' as in cmip6 dr document

if (interactive()) {
    me <- "fesom_get_res_from_griddes.r"
    args <- c("--node=/work/ba1103/a270073/mesh/fesom/core/griddes_node.nc",
              "--elem=/work/ba1103/a270073/mesh/fesom/core/griddes_elem.nc",
              "--pathout=/work/ba1103/a270073/mesh/fesom/core")
} else {
    args <- commandArgs(trailingOnly=F)
    me <- basename(sub("--file=", "", args[grep("--file=", args)]))
    args <- commandArgs(trailingOnly=T)
}

usage <- paste0("\nUsage:\n",
                " $ ", me, " --node=/path/to/griddes_node --elem=/path/to/griddes_elem --pathout=/path/where/dmax_{node,elem}.nc/will/be/saved\n",
                "\n",
                " Provide either --node or --elem or both\n",
                "\n")

# check
if (length(args) == 0 || length(args) > 3) {
    message(usage)
    quit()
}

griddes_node <- griddes_elem <- NULL
if (any(grepl("--node", args))) {
    griddes_node <- sub("--node=", "", args[grep("--node=", args)])
}
if (any(grepl("--elem", args))) {
    griddes_elem <- sub("--elem=", "", args[grep("--elem=", args)])
}
if (is.null(griddes_node) && is.null(griddes_elem)) stop(usage)
if (!is.null(griddes_node)) {
    if (!file.exists(griddes_node)) stop("griddes_node ", griddes_node, " does not exist")
}
if (!is.null(griddes_elem)) {
    if (!file.exists(griddes_elem)) stop("griddes_elem ", griddes_elem, " does not exist")
}
if (any(grepl("--pathout", args))) {
    pathout <- sub("--pathout=", "", args[grep("--pathout=", args)])
    dir.create(pathout, recursive=T, showWarnings=F)
    if (!dir.exists(pathout)) stop("could not create pathout")
} else {
    stop("provide pathout")
}

options(warn=2)

# dmax_node
if (!is.null(griddes_node)) {
    # dmax_node in km
    fout_node <- paste0(pathout, "/dmax_node.nc")
    if (file.exists(fout_node)) {
        message("fout on nodes ", fout_node, " does already exist. skip")
    } else {
        cmd <- paste0("cdo -setunit,km -expr,'dmax=sqrt(cell_area)/1e3' ", griddes_node, " ", fout_node)
        message("\nrun `", cmd, "` ...")
        check <- system(cmd)
        if (check != 0) stop("error: ", check)
    }
} # dmax_node

# dmax_elem
if (!is.null(griddes_elem)) {
    # dmax_elem in km
    fout_elem <- paste0(pathout, "/dmax_elem.nc")
    if (file.exists(fout_elem)) {
        message("fout on elems ", fout_elem, " does already exist. skip")
    } else {
        cmd <- paste0("cdo -setunit,km -expr,'dmax=sqrt(2*cell_area)/1e3' ", griddes_elem, " ", fout_elem)
        message("run `", cmd, "` ...")
        check <- system(cmd)
        if (check != 0) stop("error: ", check)
    }
} # dmax_elem

# dmax from km to deg
message("\nget dmax in deg lon and lat ...")
radius_earth_km <- 6371 # todo: spheRlab::sl.grid.readFESOM uses Rearth=6371000 m; must be same to be consistent with griddes?
pi_35_digits <- 3.14159265358979323846264338327950288 # from cdo: const.h:#define M_PI 3.14159265358979323846264338327950288
circ_earth_km <- 2*pi_35_digits*radius_earth_km
one_deg_lat_km <- circ_earth_km/360 # = 111.194926644559 with r=6371
if (!interactive()) library(ncdf4)
for (i in seq_len(2)) { # node,elem
    if (i == 1) { # node
        griddes <- griddes_node
        horiz_dimname <- "ncells"
        dmax_km <- fout_node
        fout_deg_lon <- paste0(pathout, "/dmax_node_deg_lon.nc")
        fout_deg_lat <- paste0(pathout, "/dmax_node_deg_lat.nc")
    } else if (i == 2) { # elem
        griddes <- griddes_elem
        horiz_dimname <- "ntriags"
        dmax_km <- fout_elem
        fout_deg_lon <- paste0(pathout, "/dmax_elem_deg_lon.nc")
        fout_deg_lat <- paste0(pathout, "/dmax_elem_deg_lat.nc")
    }
    message("\nfrom dmax ", dmax_km, " ...")

    # dmax in deg lon
    if (file.exists(fout_deg_lon)) {
        message("dmax in deg lon ", fout_deg_lon, " does already exist. skip")
    } else {
        # ddeg_lon = dmax/(111.1km * cos(lat*pi/180)) # actually dx, not dmax
        if (T) { # todo: there is no way to get lat as a variable from dimension, as e.g. `cdo gridarea` -> `cdo latitude`
                 # --> create temporary file with latitude
            message("  get lat from griddes ", griddes, " ...")
            nc <- ncdf4::nc_open(griddes)
            lat <- ncdf4::ncvar_get(nc, "lat")
            ncvar <- ncdf4::ncvar_def(name="latvar", units="degrees north", dim=nc$dim[[horiz_dimname]], missval=NA)
            fout_deg_lat_tmp <- paste0(fout_deg_lat, "_tmp")
            message("  save tmp file ", fout_deg_lat_tmp, " ...")
            outnc <- ncdf4::nc_create(fout_deg_lat_tmp, vars=ncvar, force_v4=T)
            ncdf4::ncvar_put(outnc, ncvar, lat)
            ncdf4::nc_close(outnc)
            cmd <- paste0("cdo merge ", dmax_km, " ", fout_deg_lat_tmp, " ", fout_deg_lat_tmp, "2 && mv ", fout_deg_lat_tmp, "2 ", fout_deg_lat_tmp)
            message("  run `", cmd, "` ...")
            check <- system(cmd)
            if (check != 0) stop("error: ", check)
            cmd <- paste0("cdo -setunit,'degrees longitude' -expr,'dmax_lon=dmax/(", one_deg_lat_km, "*cos(latvar*M_PI/180))' ", fout_deg_lat_tmp, " ", fout_deg_lon)
            message("run `", cmd, "` ...")
            check <- system(cmd)
            if (check != 0) stop("error: ", check)
            invisible(file.remove(fout_deg_lat_tmp))
        }
    }

    # dmax in deg lat
    if (file.exists(fout_deg_lat)) {
        message("dmax in deg lon ", fout_deg_lat, " does already exist. skip")
    } else {
        # ddeg_lat = dmax/(111.1km) # actually dy, not dmax
        cmd <- paste0("cdo -setunit,'degrees latitude' -expr,'dmax_lat=dmax/", one_deg_lat_km, "' ", dmax_km, " ", fout_deg_lat)
        message("run `", cmd, "` ...")
        check <- system(cmd)
        if (check != 0) stop("error: ", check)
    }

} # for i

message("\nfinished")


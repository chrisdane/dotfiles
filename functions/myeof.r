#!/usr/bin/env Rscript

args <- commandArgs(trailingOnly=F)
me <- basename(sub("--file=", "", args[grep("--file=", args)]))
args <- commandArgs(trailingOnly=T)

# check
usage <- paste0("\nUsage:\n $ ", me, " ",
                "--neof=3 ",
                "--anom_file=<anom_file> ",
                "--varname=`cdo showname anom_file` ",
                "--outdir=`dirname(anom_file)` ",
                "--method=base::svd\n")
if (length(args) == 0) {
    message(usage)
    quit()
}

# check neof
if (any(grepl("--neof", args))) {
    neof <- sub("--neof=", "", args[grep("--neof=", args)])
    if (!is.numeric(neof)) stop("provided neof = ", neof, " is not numeric")
} else {
    neof <- 3
    message("`--neof` not provided --> use default ", neof) 
}

# check anom_file
if (!any(grepl("--anom_file", args))) {
    stop("must provide `--anom_file=<anom_file>` argument", usage)
} else {
    anom_file <- sub("--anom_file=", "", args[grep("--anom_file=", args)])
    if (!file.exists(anom_file)) stop("anom_file = \"", anom_file, "\" does not exist")
}

# check varname
if (!any(grepl("--varname", args))) {
    message("`--varname=<varname>` or `--varname=varname1,varname2` not provided --> run `cdo showname ", 
            anom_file, "` ...")
    if (Sys.which("cdo") == "") stop("did not find cdo command")
    cmd <- paste0("cdo -s showname ", anom_file)
    varnames <- system(cmd, intern=T)
    varnames <- strsplit(varnames, " ")[[1]]
} else {
    varnames <- sub("--varname=", "", args[grep("--varname=", args)])
    varnames <- strsplit(varnames, ",")[[1]]
}
if (any(varnames == "")) varnames <- varnames[-which(varnames == "")]
message("--> varnames = ", paste(varnames, collapse=", "))
if (length(varnames) == 0) stop("found zero varnames")

# check outdir
if (!any(grepl("--outdir", args))) { # outdir not provided
    outdir <- dirname(anom_file)
    message("`--outdir` not provided --> use `dirname(anom_file)` = \"", outdir, "\"")
} else { # outdir provided
    outdir <- sub("--outdir=", "", args[grep("--outdir=", args)])
}
if (file.access(outdir, mode=0) == -1) { # not existing
    message("outdir = \"", outdir, "\" does not exist. try to create ... ", appendLF=F)
    dir.create(outdir, recursive=T)
    if (!file.exists(outdir)) {
        stop("not successful. error msg:")
    } else {
        message("success")
    }
} else { # outdir exists
    if (file.access(outdir, mode=2) == -1) { # not writable
        stop("provided outdir = \"", outdir, "\" not writeable.")
    }
}
outdir <- normalizePath(outdir)
message("--> outdir = \"", outdir, "\"")

# check method
known_methods <- c("base::svd", "RSectra::svds")
if (!any(grepl("--method", args))) { # method not provided
    method <- "base::svd"
} else { # outdir provided
    method <- sub("--method=", "", args[grep("--method=", args)])
    if (!any(method == known_methods)) {
        stop("method ", method, " must be one of ", paste(known_methods, collapse=", "))
    }
}

# open file
library(ncdf4)
message("open ", anom_file, " ...")
ncin <- nc_open(anom_file)

# load wanted data
data <- vector("list", l=length(varnames))
for (vi in seq_along(data)) {
    dimnames <- sapply(ncin$var[[varnames[vi]]]$dim, "[[", "name")
    message("load var ", vi, "/", length(data), ": ", varnames[vi], 
            " (", paste(dimnames, collapse=","), ") ... ", appendLF=F)
    data[[vi]]$data <- ncvar_get(ncin, varnames[vi])
    names(data)[vi] <- varnames[vi]
    message(format(utils::object.size(data[[vi]]$data), units = "auto"), appendLF=F)
    cat(capture.output(str(data[[vi]]$data)), sep="\n")
    data[[vi]]$dims <- dimnames
} # for vi

# get unique needed dims
needed_dims <- lapply(data, "[[", "dims")
needed_dims <- unique(unlist(needed_dims))

# load needed dims
dims <- list()
for (di in seq_along(needed_dims)) {
    message("load dim ", di, "/", length(needed_dims), ": ", needed_dims[di], " ...", appendLF=F)
    dims[[needed_dims[di]]] <- ncin$dim[[needed_dims[di]]]$vals
    cat(capture.output(str(dims[[needed_dims[di]]])), sep="\n")
}

# figure out spatial and temporal dims
known_time_dimnames <- c("time", "TIME")
temporal_inds <- spatial_inds <- c()
for (di in seq_along(dims)) {
    if (any(names(dims)[di] == known_time_dimnames)) {
        temporal_inds <- c(temporal_inds, di)
    } else { # all other dims are considered spatial dims
        spatial_inds <- c(spatial_inds, di)
    }
}
if (length(temporal_inds) != 1) {
    stop("found ", length(temporal_inds), " != 1 time dimension", 
         ifelse(length(temporal_inds) >1 , "s", ""), " based on `known_time_dimnames` = \"", 
         paste(known_time_dimnames, collapse="\", \""), "\"")
}
if (length(spatial_inds) == 0) {
    stop("did not find a spatial dimension")
}
message("detected temporal dim: \"", names(dims)[temporal_inds], "\"")
message("detected spatial dim", ifelse(length(spatial_inds) > 1, "s", ""), 
        " (all others than time dim", ifelse(length(temporal_inds) > 1, "s", ""), 
        "): \"", paste(names(dims)[spatial_inds], collapse="\", \""), "\"")

# apply latitudinal weights
known_lat_dimnames <- c("lat", "LAT", "latitude", "LATITUDE")
if (any(match(names(dims)[spatial_inds], known_lat_dimnames))) {
    lat_dimind <- which(!is.na(match(names(dims)[spatial_inds], known_lat_dimnames)))
    if (length(lat_dimind) != 1) {
        stop("found ", length(lat_dimind), " latitude dims based on `known_lat_dimnames` = \"",
             paste(known_lat_dimnames[lat_dimind], collapse="\", \""), "\"")
    }
    lat_dimname <- names(dims)[lat_dimind]
    message("detected latitude dim \"", lat_dimname, "\" --> apply latitudinal weights = sqrt(cos(lat)) ...")
    weights <- sqrt(cos(dims[[lat_dimname]]*pi/180))
    for (vi in seq_along(data)) {
        repeat_diminds <- which(data[[vi]]$dims != lat_dimname)
        #message("repeat weights along \"", 
        #        paste(data[[vi]]$dims[repeat_diminds], collapse="\", \""), "\" dims ...")
        weights_arr <- array(weights, dim=c(length(weights), sapply(dims, length)[repeat_diminds]))
        # correct dim order
        weights_arr <- aperm(weights_arr, seq_along(dim(weights_arr))[c(lat_dimind, repeat_diminds)])
        # apply weights
        data[[vi]]$data <- data[[vi]]$data*weights_arr
    } # for vi
} # apply latitudinal weights

# order data to (time,space) if necessary
for (vi in seq_along(data)) {
    temporal_diminds <- match(data[[vi]]$dims, names(dims)[temporal_inds])
    temporal_diminds <- which(!is.na(temporal_diminds))
    ntime <- prod(sapply(dims, length)[temporal_diminds])
    spatial_diminds <- match(data[[vi]]$dims, names(dims)[spatial_inds])
    spatial_diminds <- which(!is.na(spatial_diminds))
    nspace <- prod(sapply(dims, length)[spatial_diminds])
    
    # reorder (dim1,dim2,...) to (spacedim1,spacedim2,...,timedim1,timedim2,...) if necessary
    if (length(dim(data[[vi]]$data)) != 2) {
        dimorder <- c(spatial_diminds, temporal_diminds) # keep this order for the nect vectorization step
        if (!all(dimorder == seq_along(dimorder))) {
            message("reorder ", names(data)[vi], " from (", paste(data[[vi]]$dim, collapse=","), 
                    ") to (", paste(data[[vi]]$dim[dimorder], collapse=",") , ") ...")
            data[[vi]]$data <- aperm(data[[vi]]$data, perm=dimorder)
            data[[vi]]$dims <- data[[vi]]$dims[dimorder]
        }

        # vectorize space dim if necessary
        message("vectorize spatial dims of ", names(data)[vi], " from (", 
                paste(dim(data[[vi]]$data), collapse=","), ") to ", appendLF=F) 
        data[[vi]]$data <- array(data[[vi]]$data, dim=c(nspace, ntime))
        message("(", paste(dim(data[[vi]]$data), collapse=","), ")")
    }

    # transpose to time,space
    if (dim(data[[vi]]$data)[1] != ntime) {
        message("transpose ", names(data)[vi], " data from (", 
                paste(dim(data[[vi]]$data), collapse=","), ") to ", appendLF=F)
        data[[vi]]$data <- t(data[[vi]]$data)
        message("(", paste(dim(data[[vi]]$data), collapse=","), ")")
    }
} # for vi

# run eof method
eofs <- vector("list", l=length(data))
names(eofs) <- names(data)
for (vi in seq_along(eofs)) {
    message("run `", method, "` on ", names(data)[vi], " data ...")

    if (method == "base::svd") {
        eof <- base::svd(data[[vi]]$data, nu=neof, nv=neof)
        eofs[[vi]] <- list(eigenval=eof$d^2, eigenvec=eof$v, pc=eof$u)

    } else if (method == "RSpectra::svds") {
        stop("not implemented yet")
    
    } # which eof method
    
    # eigenvals as pcnt described variance
    eofs[[vi]]$eigenval_pcnt <- eofs[[vi]]$eigenval / sum(eofs[[vi]]$eigenval) * 100
    for (eofi in seq_len(min(neof, length(eofs[[vi]]$eigenval)))) {
        message("EOF", eofi, ": ", round(eofs[[vi]]$eigenval_pcnt[eofi]), "%")
    }
} # for vi 

# reorder eigenvecs to correct spatial dimensions
for (vi in seq_along(eofs)) {
    spatial_diminds <- match(data[[vi]]$dims, names(dims)[spatial_inds])
    spatial_diminds <- which(!is.na(spatial_diminds))
    nspace <- sapply(dims, length)[spatial_diminds]

    if (length(nspace) > 1) {
        message("reorder ", names(data)[vi], " eigenvecs from (",
                paste(dim(eofs[[vi]]$eigenvec), collapse=","), ") to ", appendLF=F)
        eofs[[vi]]$eigenvec <- array(eofs[[vi]]$eigenvec, 
                                     dim=c(nspace, dim(eofs[[vi]]$eigenvec)[2]))
        message("(", paste(dim(eofs[[vi]]$eigenvec), collapse=","), ")")
    }
} # for vi

# save result as nc
for (vi in seq_along(data)) {
    fout <- paste0(outdir, "/", basename(anom_file), "_", names(data)[vi], 
                   "_eof_", neof, "_", gsub("::", "_", method), ".nc")
    message("save results to ", fout, " ...")
    dimname <- names(dims)[temporal_inds]
    nc_time_dim <- ncin$dim[[dimname]]
    nc_spatial_dims <- vector("list", l=length(spatial_inds))
    for (i in seq_along(spatial_inds)) {
        dimname <- names(dims)[spatial_inds[i]]
        nc_spatial_dims[[i]] <- ncin$dim[[dimname]]
    }
    nc_eof_dim <- ncdim_def(name="eof", units="", vals=seq_len(neof))                          
    eigenval_var <- ncvar_def(name="eigenval", units="", dim=nc_time_dim)
    eigenval_pcnt_var <- ncvar_def(name="eigenval_pcnt", units="%", dim=nc_time_dim)
    eigenvec_var <- ncvar_def(name="eigenvec", units="", dim=c(nc_spatial_dims, list(nc_eof_dim)))
    pc_var <- ncvar_def(name="pc", units="", dim=list(nc_time_dim, nc_eof_dim))
    outnc <- nc_create(filename=fout, force_v4=T,
                       vars=list(eigenval_var, eigenval_pcnt_var, eigenvec_var, pc_var))
    ncatt_put(outnc, 0, "anom_file", anom_file)
    ncatt_put(outnc, 0, "weights", "sqrt(cos(lat))")
    ncatt_put(outnc, 0, "method", method)
    ncvar_put(nc=outnc, varid=eigenval_var, vals=eofs[[vi]]$eigenval)
    ncvar_put(nc=outnc, varid=eigenval_pcnt_var, vals=eofs[[vi]]$eigenval_pcnt)
    ncvar_put(nc=outnc, varid=eigenvec_var, vals=eofs[[vi]]$eigenvec)
    ncvar_put(nc=outnc, varid=pc_var, vals=eofs[[vi]]$pc)
    nc_close(outnc)
} # for vi

message("finished")


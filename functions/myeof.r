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
                "--method=base::svd ",
                "--weight=cos(lat)\n")
if (length(args) == 0) {
    message(usage)
    quit()
}

# check neof
if (any(grepl("--neof", args))) {
    neof <- sub("--neof=", "", args[grep("--neof=", args)])
    neof <- as.numeric(neof)
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
    message("`--varname=<varname>` or `--varname=varname1,varname2` not provided --> run `cdo showname` ...") 
    if (Sys.which("cdo") == "") stop("did not find cdo command")
    cmd <- paste0("cdo -s showname ", anom_file)
    varnames <- system(cmd, intern=T)
    varnames <- strsplit(varnames, " ")[[1]]
} else {
    varnames <- sub("--varname=", "", args[grep("--varname=", args)])
    varnames <- strsplit(varnames, ",")[[1]]
}
if (any(varnames == "")) varnames <- varnames[-which(varnames == "")]
message("--> varnames = \"", paste(varnames, collapse="\", \""), "\"")
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
    message("`--method` not provided --> use default \"", method, "\"")
} else { # methid provided
    method <- sub("--method=", "", args[grep("--method=", args)])
    if (!any(method == known_methods)) {
        stop("method ", method, " unknown. must be one of ", paste(known_methods, collapse=", "))
    }
}

# check weight
known_weights <- c("", "cos(lat)")
if (!any(grepl("--method", args))) { # method not provided
    weight <- "cos(lat)"
    message("`--weights` not provided --> use default \"", weight, "\"")
} else { # weight provided
    weight <- sub("--=", "", args[grep("--weight=", args)])
    if (!any(weight == known_weights)) {
        stop("weight ", weight, " unknown. must be one of ", paste(known_weights, collapse=", "))
    }
}
   
# add more here if necessary
known_time_dimnames <- c("time", "TIME")
known_lat_dimnames <- c("lat", "LAT", "latitude", "LATITUDE")

# open file
library(ncdf4)
message("open ", anom_file, " ...")
ncin <- nc_open(anom_file)

# for all wanted variables
for (vi in seq_along(data)) {
    
    # load data
    data <- list()
    dimnames <- sapply(ncin$var[[varnames[vi]]]$dim, "[[", "name")
    data$units <- ncin$var[[varnames[vi]]]$units
    message("load var ", vi, "/", length(varnames), ": ", varnames[vi], " (", 
            paste(dimnames, collapse=","), ") in ", data$units, " ... ", appendLF=F)
    data$data <- ncvar_get(ncin, varnames[vi])
    message(format(utils::object.size(data$data), units = "auto"), appendLF=F)
    cat(capture.output(str(data$data)), sep="\n")
    data$dims <- dimnames

    # get unique needed dims
    needed_dims <- unique(data$dims)

    # load needed dims
    dims <- list()
    for (di in seq_along(needed_dims)) {
        message("load dim ", di, "/", length(needed_dims), ": ", needed_dims[di], " ...", appendLF=F)
        dims[[needed_dims[di]]] <- ncin$dim[[needed_dims[di]]]$vals
        cat(capture.output(str(dims[[needed_dims[di]]])), sep="\n")
    }

    # figure out spatial and temporal dims
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
    
    # distinguish time and space dims
    temporal_diminds <- match(data$dims, names(dims)[temporal_inds])
    temporal_diminds <- which(!is.na(temporal_diminds))
    ntime <- prod(sapply(dims, length)[temporal_diminds])
    spatial_diminds <- match(data$dims, names(dims)[spatial_inds])
    spatial_diminds <- which(!is.na(spatial_diminds))
    nspace <- prod(sapply(dims, length)[spatial_diminds])
   
    # check if neof is possible
    if (neof > min(ntime, nspace)) {
        stop("neof = ", neof, " > min(ntime,nspace) = min(", 
             ntime, ",", nspace, ") = ", min(ntime, nspace))
    }

    # apply weights if necessary
    if (weight != "") {
        message("weight = `", weight, "` are defined")
        if (grepl("lat", weight)) {
            message("weight depend on latitude")
            if (any(match(names(dims)[spatial_inds], known_lat_dimnames))) { 
                message("detected latitude dimension based `known_lat_dimnames` = \"",
                        paste(known_lat_dimnames, collapse="\", \""), "\"")
                lat_dimind <- which(!is.na(match(names(dims)[spatial_inds], known_lat_dimnames)))
                if (length(lat_dimind) != 1) {
                    stop("found ", length(lat_dimind), " and not 1 latitude dimension based on `known_lat_dimnames` = \"",
                         paste(known_lat_dimnames, collapse="\", \""), "\". dont know how to handle this")
                }
                lat_dimname <- names(dims)[lat_dimind]
                if (weight == "cos(lat)") {
                    message(lat_dimname, " =", appendLF=F) 
                    cat(capture.output(str(dims[[lat_dimname]])), sep="\n")
                    message("calc cos(", lat_dimname, "*pi/180) ...")
                    weights <- cos(dims[[lat_dimname]]*pi/180)
                } else {
                    stop("weight = ", weight, " not defined yet")
                }
                message("weights =", appendLF=F)
                cat(capture.output(str(weights)), sep="\n")
            } else {
                stop("did not detect any latitude dimension based on `known_lat_dimnames` = \"",
                     paste(known_lat_dimnames[lat_dimind], collapse="\", \""), 
                     "\". cannot calc weight = ", weight)
            }
        } else {
            stop("weight = ", weight, " not defined")
        }
        
        # make array of weights
        repeat_diminds <- which(data$dims != lat_dimname)
        #message("repeat weights along \"", paste(data$dims[repeat_diminds], collapse="\", \""), "\" dims ...")
        weights_arr <- array(weights, dim=c(length(weights), sapply(dims, length)[repeat_diminds]))
        # correct dim order
        weights_arr <- aperm(weights_arr, seq_along(dim(weights_arr))[c(lat_dimind, repeat_diminds)])
        # apply weights
        message("calc sqrt(", weight, ")*data ...")
        data$dataw <- sqrt(weights_arr) * data$data

    } else { 
        message("weight is not defined. do not apply weight")
    } # if weights are defined or not

    # reorder (dim1,dim2,...) to (spacedim1,spacedim2,...,timedim) if necessary
    if (length(dim(data$data)) != 2) { # input data has more than 2 dims
        dimorder <- c(spatial_diminds, temporal_diminds) # keep this order for the vectorization below
        if (!all(dimorder == seq_along(dimorder))) {
            message("reorder ", varnames[vi], " from (", paste(data$dim, collapse=","), 
                    ") to (", paste(data$dim[dimorder], collapse=",") , ") ...")
            data$data <- aperm(data$data, perm=dimorder)
            if (!is.null(data$dataw)) data$dataw <- aperm(data$dataw, perm=dimorder)
            data$dims <- data$dims[dimorder]
        } # if reordering is necessary

        # vectorize space dim if necessary
        message("vectorize spatial dims of ", varnames[vi], " from (", 
                paste(dim(data$data), collapse=","), ") to (", nspace, ",", ntime, ") ...")
        data$data <- array(data$data, dim=c(nspace, ntime))
        if (!is.null(data$dataw)) data$dataw <- array(data$dataw, dim=c(nspace, ntime))
    } else {
        stop("todo: check if dimorder is correct on two dims")
    }

    # transpose to time,space
    if (dim(data$data)[1] != ntime) {
        message("transpose ", varnames[vi], " data from (", 
                paste(dim(data$data), collapse=","), ") to (", ntime, ",", nspace, ") ...")
        data$data <- t(data$data)
        if (!is.null(data$dataw)) data$dataw <- t(data$dataw)
    }

    # run eof method
    eof <- list()
    message("run `", method, "` on ", varnames[vi], " data ... (this may take some time)")

    if (method == "base::svd") {
        if (!is.null(data$dataw)) {
            eof <- base::svd(data$dataw, nu=neof, nv=neof)
        } else {
            eof <- base::svd(data$data, nu=neof, nv=neof)
        }
        eof <- list(eigenval_abs=eof$d^2/ntime, eigenvec=eof$v, svd_u=eof$u)

    } else if (method == "RSpectra::svds") {
        stop("not implemented yet")
    
    } # which eof method
    
    # eigenvals as pcnt described variance
    eof$eigenval_pcnt <- eof$eigenval_abs / sum(eof$eigenval_abs) * 100
    for (eofi in seq_len(min(neof, length(eof$eigenval_abs)))) {
        message("EOF", eofi, ": ", round(eof$eigenval_pcnt[eofi]), "%", appendLF=F)
        if (eofi > 1) {
            message(" (", round(sum(eof$eigenval_pcnt[1:eofi])), "%)")
        } else {
            message()
        }
    }
    
    # normalized eigenvecs: eigenvec_normalized_eofi = sqrt(eigenval_eofi) * e_eofi
    # eq. 13.22 von stoch and zwiers 1999
    # --> normalized eigenvectors have same units as data
    # --> normalized eigenvectors represent a "typical event"
    eof$eigenvec_normalized <- array(NA, dim=dim(eof$eigenvec))
    for (i in seq_len(neof)) {
        eof$eigenvec_normalized[,i] <- sqrt(eof$eigenval_abs[i]) * eof$eigenvec[,i]
        if (i <= min(neof, 3)) message("min/max normalized eigenvec", i, " = ", 
                                       paste(round(range(eof$eigenvec_normalized[,i])), collapse="/"),
                                       " ", data$units)
    }

    # calculate own principal components
    # from `cdo eofcoef` docu:
    # "this operator calculates a non weighted dot product of the fields in infile1 and infile2"
    # --> infile1 =  eof_file = eigenvec
    # --> infile2 = anom_file = data
    # kelley book p. 180: pc a1 <- (u %*% e$vectors[,1])[,1] # u: 2 dims=time,dist
    if (any(names(eof) == "svd_u")) {
        message("calculate principal components PCs = ", appendLF=F)
        if (!is.null(data$dataw)) {
            message("`sqrt(", weight, ")*data x eigenvec`", appendLF=F)
        } else {
            message("`data x eigenvec`", appendLF=F)
        }
        message(" ...")
        pc <- array(NA, dim=dim(eof$svd_u))
        for (j in seq_len(neof)) {
            # `cdo eofcoeff` does not apply a weight here
            # --> neglect weights here to get PCs closer to the ones of `cdo eofcoeff`
            # --> however, then, the data reconstruction `eigenvec*pc` is incomplete
            if (!is.null(data$dataw)) {
                pc[,j] <- data$dataw %*% eof$eigenvec[,j] # (ntime,nspace) x (nspace,1) = (ntime)
            } else {
                stop("test")
                pc[,j] <- data$data %*% eof$eigenvec[,j] # (ntime,nspace) x (nspace,1) = (ntime)
            }
        }
        eof$pc <- pc # continue with own pc
    } # calc own PCs if `svd_u` is present

    # until here, PCs must be found
    if (!any(names(eof) == "pc")) stop("no proper PC defined")

    # normalized PCs: pc_normalized_eofi = pc_eofi / sqrt(eigenval_eofi)
    # eq. 13.21 von stoch and zwiers 1999
    # --> var(normalized pcs) = 1
    # --> normalized pcs +-1 represent "typical events"
    eof$pc_normalized <- array(NA, dim=dim(eof$pc))
    for (i in seq_len(neof)) {
        eof$pc_normalized[,i] <- eof$pc[,i] / sqrt(eof$eigenval_abs[i])
        if (i <= min(neof, 3)) message("var(normalized PC", i, ") = ", var(eof$pc_normalized[,i]))
    }

    # scaled PCs: (x-µ)/sd
    eof$pc_scaled <- array(NA, dim=dim(eof$pc))
    for (i in seq_len(neof)) {
        eof$pc_scaled[,i] <- scale(eof$pc[,i])
    }
    
    # reorder eigenvecs to correct spatial dimensions
    spatial_diminds <- match(data$dims, names(dims)[spatial_inds])
    spatial_diminds <- which(!is.na(spatial_diminds))
    nspace <- sapply(dims, length)[spatial_diminds]

    if (length(nspace) > 1) {
        message("reorder eigenvecs from (",
                paste(dim(eof$eigenvec), collapse=","), ") to ", appendLF=F)
        eof$eigenvec <- array(eof$eigenvec,
                              dim=c(nspace, dim(eof$eigenvec)[2]))
        message("(", paste(dim(eof$eigenvec), collapse=","), ")")
        eof$eigenvec_normalized <- array(eof$eigenvec_normalized,
                                         dim=c(nspace, dim(eof$eigenvec_normalized)[2]))
    } # if more than one spatial dim 1

    # save result as nc
    fout <- paste0(outdir, "/", basename(anom_file), "_", varnames[vi], 
                   "_eof_", neof, "_", gsub("::", "_", method), ".nc")
    message("save results to ", fout, " ...")
    nc_eigenval_dim <- ncdim_def(name="eigenval_no", units="", vals=seq_along(eofs[[vi]]$eigenval))                          
    dimname <- names(dims)[temporal_inds]
    nc_time_dim <- ncin$dim[[dimname]]
    nc_spatial_dims <- vector("list", l=length(spatial_inds))
    for (i in seq_along(spatial_inds)) {
        dimname <- names(dims)[spatial_inds[i]]
        nc_spatial_dims[[i]] <- ncin$dim[[dimname]]
    }
    nc_eof_dim <- ncdim_def(name="eof", units="", vals=seq_len(neof))                          
    eigenval_abs_var <- ncvar_def(name="eigenval_abs", units="", dim=nc_time_dim)
    eigenval_pcnt_var <- ncvar_def(name="eigenval_pcnt", units="%", dim=nc_time_dim)
    eigenvec_var <- ncvar_def(name="eigenvec", units="", dim=c(nc_spatial_dims, list(nc_eof_dim)))
    eigenvec_normalized_var <- ncvar_def(name="eigenvec_normalized", units=data$units, 
                                         dim=c(nc_spatial_dims, list(nc_eof_dim)))
    # save each PC as own variable that it can be viewed separatly with ncview
    pc_var <- pc_normalized_var <- pc_scaled_var <- vector("list", l=min(neof, 10)) # save maximum first 10 PCs
    for (i in seq_along(pc_var)) {
        pc_var[[i]] <- ncvar_def(name=paste0("pc", i), units="", dim=nc_time_dim)
        pc_normalized_var[[i]] <- ncvar_def(name=paste0("pc", i, "_normalized"), units="", dim=nc_time_dim)
        pc_scaled_var[[i]] <- ncvar_def(name=paste0("pc", i, "_scaled"), units="", dim=nc_time_dim)
    }
    outnc <- nc_create(filename=fout, force_v4=T,
                       vars=c(list(eigenval_abs_var, eigenval_pcnt_var, 
                                   eigenvec_var, eigenvec_normalized_var), 
                              pc_var, pc_normalized_var, pc_scaled_var))
    ncatt_put(outnc, 0, "anom_file", anom_file)
    ncatt_put(outnc, 0, "weights", weight)
    ncatt_put(outnc, 0, "method", method)
    ncatt_put(outnc, 0, "eigenvec_normalized", "sqrt(eigenval_eofi) * eigenvec_eofi")
    ncatt_put(outnc, 0, "pc_normalized", "pc_eofi / sqrt(eigenval_eofi)")
    ncatt_put(outnc, 0, "pc_scaled", "(pc_eofi - mu_pc_eofi)/sd(pc_eofi)")
    ncvar_put(nc=outnc, varid=eigenval_abs_var, vals=eof$eigenval_abs)
    ncvar_put(nc=outnc, varid=eigenval_pcnt_var, vals=eof$eigenval_pcnt)
    ncvar_put(nc=outnc, varid=eigenvec_var, vals=eof$eigenvec)
    ncvar_put(nc=outnc, varid=eigenvec_normalized_var, vals=eof$eigenvec_normalized)
    for (i in seq_along(pc_var)) {
        ncvar_put(nc=outnc, varid=pc_var[[i]], vals=eof$pc[,i])
        ncvar_put(nc=outnc, varid=pc_normalized_var[[i]], vals=eof$pc_normalized[,i])
        ncvar_put(nc=outnc, varid=pc_scaled_var[[i]], vals=eof$pc_scaled[,i])
    }
    nc_close(outnc)

} # for vi

message("finished")


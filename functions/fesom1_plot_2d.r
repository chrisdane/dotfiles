#!/usr/bin/env Rscript

rm(list=ls()); graphics.off()
warn <- options()$warn

#plot_ext <- "x11"
plot_ext <- "png"
#plot_ext <- "pdf"
plot_type <- "nodes"
#plot_type <- "elements"
ignore_varnames <- c("lon_bnds", "lat_bnds")
fdata <- fgriddes_node <- ftriag_nodes <- NULL

if (F) {
    fdata <- "/work/ba1103/a270073/mesh/fesom/core/dmax_elem.nc"
    fgriddes_node <- ftriag_nodes <- "/work/ba1103/a270073/mesh/fesom/core/griddes_core_node.nc" 
    plot_type <- "elements"
} else if (T) {
    fdata <- "/work/ab1095/a270073/out/awiesm3-develop-cc/cold2/run_19000101-19001231/work/DIC.fesom.1900.nc"
    fgriddes_node <- "/work/ab0246/a270092/input//fesom2//core3/mesh_cavity.nc"
    plot_type <- "nodes"
}

#################################################################################################

if (!interactive()) {
    message("load ncdf4 package ...")
    library(ncdf4)
}

if (is.null(fdata)) stop("must provide `fdata`")
message("\nopen `fdata` = ", fdata, " ...")
nc_data <- ncdf4::nc_open(fdata)
varnames <- names(nc_data$var)
message("\nvarnames of fdata: ", paste(varnames, collapse=", "))

if (!is.null(fgriddes_node)) { # get node lon/lat from griddes if provided

    message("\nprovided `fgriddes_node` = ", fgriddes_node, " --> open ...")
    nc_griddes_node <- ncdf4::nc_open(fgriddes_node)
    message("get lon ...")
    if (!any(names(nc_griddes_node$var) == "lon")) stop("could not find varname \"lon\" in this file")
    lon_node <- as.numeric(ncdf4::ncvar_get(nc_griddes_node, "lon"))
    message("get lat ...")
    if (!any(names(nc_griddes_node$var) == "lat")) stop("could not find varname \"lat\" in this file")
    lat_node <- as.numeric(ncdf4::ncvar_get(nc_griddes_node, "lat"))
    #message("get cell_area ...")
    #if (!any(names(nc_griddes_node$var) == "cell_area")) stop("could not find varname \"cell_area\" in this file")
    #cell_area_node <- ncdf4::ncvar_get(nc_griddes_node, "cell_area")
    #message("--> min/max = ", min(cell_area_node), "/", max(cell_area_node))

} else if (is.null(fgriddes_node)) { # get node lon/lat from data if griddes not provided

    message("\n`fgriddes_node` not provided --> try to get lon/lat from fdata ...")
    message("get lon ...")
    if (!any(varnames == "lon")) stop("could not find varname \"lon\" in this file")
    lon_node <- as.numeric(ncdf4::ncvar_get(nc_data, "lon"))
    if (!any(varnames == "lat")) stop("could not find varname \"lat\" in this file")
    lat_node <- as.numeric(ncdf4::ncvar_get(nc_data, "lat"))

}

message("lon_node:")
cat(capture.output(str(lon_node)), sep="\n")
message("min/max = ", min(lon_node), "/", max(lon_node))
message("lat_node:")
cat(capture.output(str(lat_node)), sep="\n")
message("min/max = ", min(lat_node), "/", max(lat_node))

# get variables to plot
inds <- na.omit(match(c("lon", "lat", ignore_varnames), varnames))
if (length(inds) > 0) varnames <- varnames[-inds]
message("\ncontinue with ", length(varnames), " varnames: ", paste(varnames, collapse=", "))
if (length(varnames) == 0) stop("file has zero varnames next to lon, lat and `ignore_varnames` = ", paste(ignore_varnames, collapse=", "))

if (plot_type == "elements") { # get lon lat on nodes and triag_nodes
    stop("update")
    message("\n`plot_type` = \"elements\" --> get lon and lat on nodes and triag_nodes ...")
    if (fgriddes_node == fdata) {
        lon_node <- ncdf4::ncvar_get(nc, "lon")
        lat_node <- ncdf4::ncvar_get(nc, "lat")
    } else {
        nc_lonlat_node <- ncdf4::nc_open(fgriddes_node)
        lon_node <- ncdf4::ncvar_get(nc_lonlat_node, "lon")
        lat_node <- ncdf4::ncvar_get(nc_lonlat_node, "lat")
    }
    if (ftriag_nodes == fdata) {
        triag_nodes <- ncdf4::ncvar_get(nc, "triag_nodes")
    } else if (ftriag_nodes == fgriddes_node) {
        triag_nodes <- ncdf4::ncvar_get(nc_lonlat_node, "triag_nodes")
    } else {
        nc_triag_nodes <- ncdf4::nc_open(ftriag_nodes)
        triag_nodes <- ncdf4::ncvar_get(nc_triag_nodes, "triag_nodes")
    }
    message("lon_node:")
    cat(capture.output(str(lon_node)), sep="\n")
    message("lat_node:")
    cat(capture.output(str(lat_node)), sep="\n")
    message("triag_nodes:")
    cat(capture.output(str(triag_nodes)), sep="\n")
    
    # remove cyclic elements
    inds_cycl <- which(abs(lon_node[triag_nodes[3,]] - lon_node[triag_nodes[2,]]) > 170 |
                       abs(lon_node[triag_nodes[2,]] - lon_node[triag_nodes[1,]]) > 170 |
                       abs(lon_node[triag_nodes[3,]] - lon_node[triag_nodes[1,]]) > 170) 
    if (length(inds_cycl) > 0) { # if there is a cyclic element
        message("\nremove ", length(inds_cycl), " cyclic elements which would spread over the whole plot ...")
        triag_nodes_cycl <- triag_nodes[,inds_cycl]
        triag_nodes <- triag_nodes[,-inds_cycl]
        auxxc1 <- auxyc1 <- auxxc2 <- auxyc2 <- array(NA, dim(triag_nodes_cycl))
        for (threei in seq_len(3)) {
            auxxc1[threei,] <- lon_node[triag_nodes_cycl[threei,]]
            auxyc1[threei,] <- lat_node[triag_nodes_cycl[threei,]]
            auxxc2[threei,] <- lon_node[triag_nodes_cycl[threei,]]
            auxyc2[threei,] <- lat_node[triag_nodes_cycl[threei,]]
        }
        mid <- (max(lon_node) + min(lon_node))/2
        for (elemi in seq_along(inds_cycl)) {
            inds2 <- which(auxxc1[,elemi] > mid)
            auxxc1[inds2,elemi] <- auxxc1[inds2,elemi] - 360
            inds2 <- which(auxxc2[,elemi] < mid)
            auxxc2[inds2,elemi] <- auxxc2[inds2,elemi] + 360
        }
        auxxc1[auxxc1 < -180] <- -180
        auxxc2[auxxc2 > 180] <- 180
    } # if there is a cyclic element
    
    # get lon lat on elements
    lon_elem <- lat_elem <- array(NA, dim(triag_nodes))
    for (threei in seq_len(3)) {
        lon_elem[threei,] <- lon_node[triag_nodes[threei,]]
        lat_elem[threei,] <- lat_node[triag_nodes[threei,]]
    } 
    if (length(inds_cycl) > 0) { # if there is a cyclic element
        lon_elem <- cbind(lon_elem, auxxc1, auxxc2)
        lat_elem <- cbind(lat_elem, auxyc1, auxyc2)
        triag_nodes <- cbind(triag_nodes, triag_nodes_cycl, triag_nodes_cycl)
    }
    lon_elem_poly <- as.vector(rbind(lon_elem, NA))
    lat_elem_poly <- as.vector(rbind(lat_elem, NA))
    xlim <- range(lon_elem)
    ylim <- range(lat_elem)

} # if plot_type == "elements"


# plot
message("\nplot ...")
xlim <- range(lon_node)
ylim <- range(lat_node)
message("load fields package ...")
library(fields)
message("load ~/scripts/r/functions/image.plot.pre.r ...")
source("~/scripts/r/functions/image.plot.pre.r")

# for all variables
for (vari in seq_along(varnames)) {

    # check for lon/lat dim
    message("\nget dims of variable ", vari, "/", length(varnames), ": ", varnames[vari], " ...")
    data_dims <- nc_data$var[[varnames[vari]]]$size
    data_dim_inds <- nc_data$var[[varnames[vari]]]$dimids + 1
    data_dim_names <- names(nc_data$dim)[data_dim_inds]
    message("--> (", paste(data_dim_names, collapse=","), "):")
    cat(capture.output(str(data_dims)), sep="\n")
    node_dim_ind <- which(data_dims == length(lon_node)) # todo: this does not work for fesom1 nod3d data
    if (length(node_dim_ind) != 1) stop("found ", length(node_dim_ind), " dims that are of same length as lon_node/lat_node. must be exactly 1")

    inds <- NULL
    if (!is.vector(data)) { # data has time and/or depth dim
        message("\ndata has more than one dim: select time/depth slice from dims (", 
                paste(paste0(data_dim_names[-node_dim_ind], ":", data_dims[-node_dim_ind]), collapse=","), "), e.g.: ",
                paste(rep(1, times=length(data_dims)-1), collapse=","))
        if (interactive()) {
            inds <- base::readline()
        } else {
            inds <- base::readLines("stdin", n=1)
        }
        inds <- strsplit(inds, ",")[[1]]
        if (length(inds) != length(data_dims) - 1) stop("you must select a slice from ", length(data_dims) - 1, " dims, not ", length(inds))
        message("--> your input is (", paste(paste0(data_dim_names[-node_dim_ind], ":", inds), collapse=","), ")")
        message("--> convert to integer ...")
        options(warn=2); inds <- as.integer(inds); options(warn=warn)
        message("--> ok: ", paste(inds, collapse=", "))
    }

    # load variable
    start <- rep(1, times=length(data_dims))
    count <- rep(-1, times=length(data_dims)) # default: read all values
    if (!is.null(inds)) {
        start[-node_dim_ind] <- inds
        count[-node_dim_ind] <- 1
    }
    message("\nread variable ", vari, "/", length(varnames), ": ", varnames[vari], 
            " from start=(", paste(paste0(data_dim_names, ":", start), collapse=","), 
            ") and count=(", paste(paste0(data_dim_names, ":", count), collapse=","), ") ...")
    data_node <- as.numeric(ncdf4::ncvar_get(nc_data, varnames[vari], start=start, count=count)) # ncvar_get will squeeze dims of len 1; as.numeric will turn array to vector
    cat(capture.output(str(data_node)), sep="\n")
    zlim <- range(data_node, na.rm=T)
    message("min/max = ", zlim[1], "/", zlim[2])

    # get colorbar
    ncols <- 10
    if (F) { # equal distribution of levels
        levels <- seq(zlim[1], zlim[2], length.out=ncols+1)
    } else if (T) { # zoom
        levels <- stats::quantile(data_node, probs=c(0.05, 0.95), na.rm=T)
        levels <- c(zlim[1], pretty(levels, n=ncols), zlim[2])
    }
    if (any(levels < zlim[1])) levels <- levels[-which(levels < zlim[1])]
    if (any(levels > zlim[2])) levels <- levels[-which(levels > zlim[2])]
    
    message("\nrun myfunction `image.plot.pre` ...")
    ip <- image.plot.pre(zlim=zlim, zlevels=levels)
    if (F) { # invert colors
        ip$cols <- rev(ip$cols)
    }

    message("\nrun `base::findInterval` ...")
    colinds <- base::findInterval(data_node, ip$levels, all.inside=T) 
    
    if (plot_ext == "x11") {
        # nothing do to
    } else {
        plotname <- paste0("~/", basename(fdata), "_", varnames[vari])
        if (!is.null(inds)) {
            plotname <- paste0(plotname, "_", paste(paste0(data_dim_names[-node_dim_ind], "_", inds), collapse="_"))
        }
        plotname <- paste0(plotname, ".", plot_ext)
        message("\nopen ", plotname)
        if (plot_ext == "png") {
            #grDevices::png(plotname, width=1500, height=1500, res=200, family="sans") # on levante, default sans (Helvetica) is broken
            grDevices::png(plotname, width=1500, height=1500, res=200, family="Droid Sans")
        } else {
            stop("asd")
        }
    }

    par(mar=c(5.1, 4.1, 4.1, 7.1))
    main <- varnames[vari]
    if (!is.null(inds)) main <- paste0(main, "(", paste(paste0(data_dim_names[-node_dim_ind], ":", inds), collapse=","), ")")
    base::plot(0, type="n", xlim=xlim, ylim=ylim,
               xlab="lon", ylab="lat", 
               xaxt="n", yaxt="n",
               xaxs="i", yaxs="i",
               main=main)
    axis(1, pretty(xlim, n=10))
    axis(2, pretty(ylim, n=10), las=2)
    if (T) { # background color = land
        usr <- par("usr")
        rect(usr[1], usr[3], usr[2], usr[4], col="gray", border=NA)
    }
    if (plot_type == "nodes") {
        message("\nrun `graphics::points` ...")
        graphics::points(lon_node, lat_node, col=ip$cols[colinds], pch=16, cex=0.33)
    } else if (plot_type == "elements") {
        message("\nrun `graphics::polygon` ...")
        stop("update")
        polygon(lon_elem_poly, lat_elem_poly, col=ip$cols[colinds], border=NA)
    }
    
    # add colorbar
    if (F) { # uneven colorbar levels appear uneven
        suppressWarnings( # due to partial arg matching of fields::image.plot --> fields::fields::image.plot
            fields::image.plot(zlim=zlim, add=T, 
                               breaks=levels, col=ip$cols, legend.only=T,
                               legend.mar=par("mar")[4] + 0.1,
                               axis.args=list(at=ip$axis.at, labels=ip$axis.labels))
        )
    } else if (T) { # uneven colorbar levels appear even
        suppressWarnings( # due to partial arg matching of fields::image.plot --> fields::fields::image.plot
            fields::image.plot(zlim=c(1, ip$nlevels), add=T, 
                               breaks=seq_len(ip$nlevels), col=ip$cols, legend.only=T,
                               legend.mar=par("mar")[4] + 0.1,
                               axis.args=list(at=ip$axis.at.ind, labels=ip$axis.labels))
        )
    }

    if (plot_ext != "x11") {
        dev.off()
    }

} # for vari

message("\nfinished\n")


#!/usr/bin/env Rscript

rm(list=ls()); graphics.off()

#plot_ext <- "x11"
plot_ext <- "png"
#plot_ext <- "pdf"
plot_type <- "nodes"
#plot_type <- "elements"
ignore_varnames <- c("lon_bnds", "lat_bnds")
flonlat_node <- ftriag_nodes <- NULL # only needed if plot_type == "elements"

if (T) {
    fdata <- "/work/ba1103/a270073/mesh/fesom/core/dmax_elem.nc"
    plot_type <- "elements"
    flonlat_node <- ftriag_nodes <- "/work/ba1103/a270073/mesh/fesom/core/griddes_core_node.nc" 
}

#################################################################################################

if (plot_type == "elements" && 
    (is.null(flonlat_node) || is.null(ftriag_nodes))) {
    warning("if `plot_type` = \"elements\", provide `flonlat_node` and `ftriag_nodes` (can be the same)\n",
            "--> continue with `plot_type` = \"nodes\" ...")
    plot_type <- "nodes"
}

if (!interactive()) {
    message("load ncdf4 package ...")
    library(ncdf4)
    message("load fields package ...")
    library(fields)
}
message("load ~/scripts/r/functions/image.plot.pre.r ...")
source("~/scripts/r/functions/image.plot.pre.r")

message("\nopen ", fdata, " ...")
nc <- ncdf4::nc_open(fdata)
varnames <- names(nc$var)
if (!any(varnames == "lon")) stop("could not find varname \"lon\" in this file. apply griddes.")
if (!any(varnames == "lat")) stop("could not find varname \"lat\" in this file. apply griddes.")
varnames <- varnames[-match(c("lon", "lat"), varnames)]
varnames <- varnames[-match(ignore_varnames, varnames)]
if (length(varnames) == 0) stop("file has zero varnames next to lon, lat and ", paste(ignore_varnames, collapse=", "))
message("--> ", length(varnames), " varnames: ", paste(varnames, collapse=", "))
lon <- as.numeric(ncdf4::ncvar_get(nc, "lon"))
lat <- as.numeric(ncdf4::ncvar_get(nc, "lat"))
xlim <- range(lon)
ylim <- range(lat)

if (plot_type == "elements") { # get lon lat on nodes and triag_nodes
    
    message("\n`plot_type` = \"elements\" --> get lon and lat on nodes and triag_nodes ...")
    if (flonlat_node == fdata) {
        lon_node <- ncdf4::ncvar_get(nc, "lon")
        lat_node <- ncdf4::ncvar_get(nc, "lat")
    } else {
        nc_lonlat_node <- ncdf4::nc_open(flonlat_node)
        lon_node <- ncdf4::ncvar_get(nc_lonlat_node, "lon")
        lat_node <- ncdf4::ncvar_get(nc_lonlat_node, "lat")
    }
    if (ftriag_nodes == fdata) {
        triag_nodes <- ncdf4::ncvar_get(nc, "triag_nodes")
    } else if (ftriag_nodes == flonlat_node) {
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

for (vari in seq_along(varnames)) {
    
    message("read variable ", vari, "/", length(varnames), ": ", varnames[vari], " ...")
    data <- as.numeric(ncdf4::ncvar_get(nc, varnames[vari])) # ncvar_get will squeeze dims of len 1; as.numeric will turn array to vector
    message("data:")
    cat(capture.output(str(data)), sep="\n")
    if (!is.vector(data)) {
        message("data must have one dim. skip to next variable")
    } else {
        message("plot ", varnames[vari], " on ", length(data), " surface nodes or elems ...")
        
        zlim <- range(data, na.rm=T)
        if (F) { # equal distribution of levels
            levels <- seq(zlim[1], zlim[2], l=ncols+1)
        } else if (T) { # zoom
            levels <- stats::quantile(data, probs=c(0.05, 0.95), na.rm=T)
            levels <- c(zlim[1], pretty(levels, n=10), zlim[2])
        }
        if (any(levels < zlim[1])) levels <- levels[-which(levels < zlim[1])]
        if (any(levels > zlim[2])) levels <- levels[-which(levels > zlim[2])]
        ip <- image.plot.pre(zlim=zlim, zlevels=levels)
        if (T) { # invert colors
            ip$cols <- rev(ip$cols)
        }
        colinds <- base::findInterval(data, ip$levels, all.inside=T) 
        
        if (plot_ext == "x11") {
            # nothing do to
        } else {
            plotname <- paste0("~/", basename(fdata), "_", varnames[vari], ".", plot_ext)
            message("save ", plotname)
            if (plot_ext == "png") {
                png(plotname, width=1500, height=1500, res=200, family="sans")
            } else {
                stop("asd")
            }
        }
        par(mar=c(5.1, 4.1, 4.1, 7.1))
        plot(0, t="n", xlim=xlim, ylim=ylim,
             xlab="lon", ylab="lat", 
             xaxt="n", yaxt="n",
             xaxs="i", yaxs="i",
             main=varnames[vari])
        axis(1, pretty(xlim, n=10))
        axis(2, pretty(ylim, n=10), las=2)
        if (T) { # background color = land
            usr <- par("usr")
            rect(usr[1], usr[3], usr[2], usr[4], col="gray", border=NA)
        }
        if (plot_type == "nodes") {
            points(lon, lat, col=ip$cols[colinds], pch=16, cex=0.33)
        } else if (plot_type == "elements") {
            polygon(lon_elem_poly, lat_elem_poly, col=ip$cols[colinds], border=NA)
        }
        fields::image.plot(zlim=zlim, add=T, 
                           breaks=levels, col=ip$cols, legend.only=T,
                           legend.mar=par("mar")[4] + 0.1,
                           axis.args=list(at=ip$axis.at, labels=ip$axis.labels))
        if (plot_ext != "x11") {
            dev.off()
        }
    } # if variable has one dim
} # for vari

message("\nfinished\n")


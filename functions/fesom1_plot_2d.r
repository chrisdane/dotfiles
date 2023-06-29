#!/usr/bin/env Rscript

rm(list=ls()); graphics.off()

if (interactive()) {
    me <- "fesom1_plot_2d.r"
    args <- c("/pool/data/AWICM/FESOM1/MESHES/core/nod2d.out",
              "/work/ba1103/a270073/forcing/FESOM1/core/lime_mask_cao_2040-high.nc_fesom1_core_adjusted_timestep_1032")
} else {
    args <- commandArgs(trailingOnly=F)
    me <- basename(sub("--file=", "", args[grep("--file=", args)]))
    args <- commandArgs(trailingOnly=T)
}

usage <- paste0("\nUsage:\n $ ", me, 
                " /path/to/nod2d.out /path/to/2d_variable_on_unstructured_grid.nc\n",
                "\n",
                " with e.g. /pool/data/AWICM/FESOM1/MESHES/core/nod2d.out\n\n")

# check
if (length(args) != 2) { # nod2d.out, landice_nodes
    message(usage)
    quit()
}

nod2d.out <- args[1]
if (file.access(nod2d.out, mode=0) == -1) { # not existing
    stop("provided nod2d.out = \"", nod2d.out, "\" does not exist")
}
if (file.access(nod2d.out, mode=4) == -1) { # not readable
    stop("provided nod2d.out = \"", nod2d.out, "\" not readable.")
}
nod2d.out <- normalizePath(nod2d.out)
message("nod2d.out = ", nod2d.out)

data_2d.nc <- args[2]
if (file.access(data_2d.nc, mode=0) == -1) { # not existing
    stop("provided data_2d.nc = \"", data_2d.nc, "\" does not exist")
}
if (file.access(data_2d.nc, mode=4) == -1) { # not readable
    stop("provided data_2d.nc = \"", data_2d.nc, "\" not readable.")
}
data_2d.nc <- normalizePath(data_2d.nc)
message("data_2d.nc = ", data_2d.nc)

message("load ncdf4 package ...")
library(ncdf4)
message("load RColorBrewer package ...")
library(RColorBrewer)
message("load fields package ...")
library(fields)
cdo <- Sys.which("cdo")

################

if (cdo == "") stop("could not find cdo")

message("read ", nod2d.out, " ...")
n2 <- base::scan(nod2d.out, what=integer(), n=1, quiet=T)
nod2d <- base::scan(nod2d.out, skip=1, quiet=T)
nod2d <- matrix(nod2d, nrow=n2, byrow=T)
colnames(nod2d) <- c("no", "lon", "lat", "coast")

message("read ", data_2d.nc, " ...")
nc <- ncdf4::nc_open(data_2d.nc)

# get colors
palname <- "Spectral"
nmax <- RColorBrewer:::maxcolors[palname]
cols <- rev(RColorBrewer::brewer.pal(n=min(10, nmax), name=palname))
ncols <- length(cols)

varnames <- strsplit(trimws(system(paste0(cdo, " -s showname ", data_2d.nc), intern=T)), " ")[[1]]
message("--> ", length(varnames), " varnames: ", paste(varnames, collapse=", "))

for (vari in seq_along(varnames)) {
    
    message("open variable ", vari, "/", length(varnames), ": ", varnames[vari], " ...")
    data_2d <- as.numeric(ncdf4::ncvar_get(nc, varnames[vari]))
    message("data_2d:")
    cat(capture.output(str(data_2d)), sep="\n")
    if (!is.vector(data_2d)) {
        message("data must have one dim. skip to next variable")
    } else {
        plotname <- paste0("~/", basename(data_2d.nc), "_", varnames[vari], ".png")
        message("plot ", varnames[vari], " on ", n2, " surface nodes ...")
        if (!interactive()) png(plotname, width=1500, height=1500, res=200)
        par(mar=c(5.1, 4.1, 4.1, 7.1))
        plot(nod2d[,"lon"], nod2d[,"lat"], t="n",
             xlab="lon", ylab="lat", yaxt="n",
             main=varnames[vari])
        axis(1, pretty(nod2d[,"lon"], n=10))
        axis(2, pretty(nod2d[,"lat"], n=10), las=2)
        levels <- seq(min(data_2d, na.rm=T), max(data_2d, na.rm=T), l=ncols+1)
        colinds <- base::findInterval(data_2d, levels, all.inside=T) 
        points(nod2d[,"lon"], nod2d[,"lat"], col=cols[colinds], pch=16, cex=0.33)
        labels <- levels
        if (ncols > 20) labels <- pretty(levels, n=15)
        fields::image.plot(zlim=range(data_2d), add=T, 
                           breaks=levels, col=cols, legend.only=T,
                           legend.mar=par("mar")[4] + 0.1,
                           axis.args=list(at=labels, labels=format(labels, digits=3)))
        if (!interactive()) {
            message("save ", plotname)
            dev.off()
        }
    } # if variable has one dim
} # for vari

message("\nfinished\n")


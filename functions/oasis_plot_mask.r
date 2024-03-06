#!/usr/bin/env Rscript

rm(list=ls()); graphics.off()

me <- "oasis_plot_mask.r"
help <- paste0("Usage:\n",
               " $ ", me, " /path/to/{grids.nc,masks.nc}\n")
    
# get args
if (interactive()) {
    args <- "/work/ba1103/a270073/mesh/oifs/tl159/oasis3mct/masks.nc"
} else {
    args <- commandArgs(trailingOnly=F)
    me <- basename(sub("--file=", "", args[grep("--file=", args)]))
    args <- commandArgs(trailingOnly=T)
}

# check args 
if (length(args) != 1) {
    message(help)
    quit()
}

# checks
masks_file <- args[1]
if (file.info(masks_file)$isdir) stop("provided masks file ", masks_file, " is a directory")
if (!file.exists(masks_file)) stop("provided masks file ", masks_file, " does not exist")
masks_file <- normalizePath(masks_file)

if (!interactive()) library(ncdf4)

message("open provided masks file ", masks_file, " ...")
masks_nc <- nc_open(masks_file)
varnames <- names(masks_nc$var)
gridnames <- varnames[grepl(".msk", varnames)] # e.g. A009.msk L009.msk R009.msk
if (length(gridnames) == 0) stop("found zero variables called \"<name>.msk\" in masks file ", masks_file)
gridnames <- gsub(".msk", "", gridnames)

# check if lon,lat are available for masks
lonnames <- gsub(".lon", "", varnames[grep(".lon", varnames)])
latnames <- gsub(".lat", "", varnames[grep(".lat", varnames)])
lonlatnames <- union(lonnames, latnames)
inds <- which(!is.na(match(gridnames, lonlatnames)))
if (length(inds) == 0) {
    stop("lon/lat was not provided for any of these masks: ", paste(gridnames, collapse=", "))
} else if (length(inds) != length(gridnames)) {
    message("continue with ", length(inds), "/", length(gridnames), " grids for which lon,lat was found ...")
}
gridnames <- gridnames[inds]

plotname <- paste0("masks_", paste(gridnames, collapse="_"), ".pdf")
message("plot ", plotname, " ...")
pdf(plotname, family="sans")
for (gi in seq_along(gridnames)) {

    lon <- ncvar_get(masks_nc, paste0(gridnames[gi], ".lon"))
    lat <- ncvar_get(masks_nc, paste0(gridnames[gi], ".lat"))
    mask <- ncvar_get(masks_nc, paste0(gridnames[gi], ".msk"))

    xat <- pretty(lon, n=20)
    yat <- pretty(lat, n=20)
    plot(0, t="n", xlim=range(lon), ylim=range(lat),
         xlab="lon", ylab="lat", xaxt="n", yaxt="n")
    axis(1, at=xat)
    axis(2, at=yat, las=2)
    title(paste0(gridnames[gi], " (black: msk=1)"))
    mtext(masks_nc$filename, cex=0.75)
    inds <- which(mask == 0)
    if (length(inds) > 0) points(lon[inds], lat[inds], pch=".", cex=0.5, col="gray")
    inds <- which(mask == 1)
    if (length(inds) > 0) points(lon[inds], lat[inds], pch=".", cex=0.5, col="black")

} # for gi
dev.off()


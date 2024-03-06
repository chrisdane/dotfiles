#!/usr/bin/env Rscript

rm(list=ls()); graphics.off()
options(warn=2) # stop on warnings    

plottype <- "png" # "png" "pdf"
exclude <- NULL
if (F) exclude <- c("glacier")
if (T) exclude <- c("glacier", "bare land (1-veg_ratio_max)")

#########################################

known_varnames <- c("pft_fract_box")

if (interactive()) {
    #args <- "/work/ba1103/a270073/post/jsbach/fldsum/pft_fract_box/awi-esm-1-1-lr_kh800_esm-piControl2_jsbach_fldsum_pft_fract_box_global_annual_1850-1872.nc"
    #args <- "/work/ba1103/a270073/post/jsbach/fldsum/pft_fract_box/awi-esm-1-1-lr_kh800_esm-piControl_wout_talk_rest2_jsbach_fldsum_pft_fract_box_global_annual_3208-3945.nc"
    args <- "/work/ba1103/a270073/post/jsbach/fldsum/pft_fract_box/awi-esm-1-1-lr_kh800_piControl_and_esm-piControl_jsbach_fldsum_pft_fract_box_global_annual_1950-3945.nc"
    #args <- "/work/ba1103/a270073/post/jsbach/fldsum/pft_fract_box/awi-esm-1-1-lr_kh800_piControl_and_esm-piControl_jsbach_fldsum_pft_fract_box_global_annual_1950-4527.nc"
} else {

    # get args
    args <- commandArgs(trailingOnly=F)
    me <- basename(sub("--file=", "", args[grep("--file=", args)]))
    help <- paste0("Usage:\n",
                   " $ ", me, " <result_of_jsbach_pft_wrt_box.r>\n")

    # check args 
    args <- commandArgs(trailingOnly=T)
    if (length(args) != 1) {
        message(help)
        quit()
    }
    message("\nstart ", me, " ...\n")
    source("~/scripts/r/functions/myfunctions.r") # mycols(), plot_sizes()

} # if interactive or not

# checks
fin <- args[1]
if (!file.exists(fin)) stop("fin ", fin, " does not exist")

message("load ncdf4 package ...")
library(ncdf4)
message("load abind package ...")
library(abind)

# open file
message("\nopen ", fin, " ...")
nc <- ncdf4::nc_open(fin)

# check if input is valid
ind <- na.omit(match(known_varnames, names(nc$var)))
if (length(ind) != 1) stop("must find exactly one of allowed varnames: ", paste(known_varnames, collapse=", "))
varname <- names(nc$var)[ind]
atts <- ncdf4::ncatt_get(nc, varname)
message("--> found variable ", varname, ":")
cat(capture.output(str(atts)), sep="\n")
units <- atts$units
if (units == "fraction") {
    ylab <- "PFT fraction"
} else if (units == "area") {
    ylab <- "PFT area" 
} else {
    stop("`units` = ", units, " not defined")
}
vardimids <- nc$var[[varname]]$dimids
dimids <- sapply(nc$dim, "[[", "id")
dimlengths <- sapply(nc$dim, "[[", "len")
vardimlengths <- dimlengths[match(vardimids, dimids)]
levname <- names(vardimlengths)[na.omit(match(c("lev", "sfc"), names(vardimlengths)))]
if (length(levname) == 0) stop("one dim of variable ", varname, " must be \"lev\" or \"sfc\"")

# get pft levels
inds <- which(substr(names(atts), 1, 3) == "lev")
if (length(inds) == 0) stop("there should be `lev*` attribute names")
levs <- unlist(atts[inds])
levs <- strsplit(levs, ";")
levs <- sapply(levs, "[", 1)
levs <- strsplit(levs, "=")
levs <- sapply(levs, "[", 2)

# read data
message("\nread (", paste(paste0(sapply(nc$var[[varname]]$dim, "[[", "name"), ":", nc$var[[varname]]$size), collapse=" x "), 
        ") variable \"", varname, "\" ...") 
data <- ncdf4::ncvar_get(nc, varname)
inds <- which(vardimlengths != 1)
attributes(data)$dimname <- names(vardimlengths)[inds]
message("data:")
cat(capture.output(str(data)), sep="\n")
if (length(dim(data)) != 2) stop("data must have 2 dims")
levind <- na.omit(match(attributes(data)$dimname, levname))
dimnames(data)[[levind]] <- levs

# get time
message("time:")
time <- as.POSIXct(strsplit(trimws(system(paste0("cdo -s showtimestamp ", fin), intern=T)), "  ")[[1]], tz="UTC")
cat(capture.output(str(time)), sep="\n")
xlab <- "year"
if (T) {
    message("\nspecial: set new time ...")
    time <- as.POSIXlt(time)
    if (F) {
        #time$year <- time$year - time$year[1] - 1900 # make first year zero
        time$year <- time$year - time$year[1] - 1900 + 1 # make first year 1
        xlab <- "piControl year"
    } else if (T) {
        time$year <- time$year - 3000
        xlab <- "piControl/esm-piControl year"
    }
}

# exclude certain pfts
if (!is.null(exclude)) {
    message("\n`exclude` = ", paste(exclude, collapse=", "))
    inds <- match(exclude, levs)
    if (!any(is.na(inds))) {
        message("remove ", length(inds), " levels: ", paste(paste0(inds, ":\"", levs[inds], "\""), collapse=", "))
        data <- abind::asub(data, idx=seq_along(levs)[-inds], dims=levind)
    } else {
        stop("those levels are not found in the data")
    }
    levs <- dimnames(data)[[1]] # update
}

# aggregate PFTs
if (T) {
    message("\naggreagte PFTs for plot ...")
    mapping <- list(list(pfts_in=c("C3 grass", "C4 grass"),
                         newname="grass (C3+C4)"),
                    list(pfts_in=c("C3 pasture", "C4 pasture"),
                         newname="pasture (C3+C4)"),
                    list(pfts_in=c("C3 crops", "C4 crops"),
                         newname="crops (C3+C4)"))
    if (T) {
        mapping[[length(mapping)+1]] <- list(pfts_in=c("tropical broadleaf evergreen", "tropical broadleaf deciduous"),
                                             newname="tropical tree")
        mapping[[length(mapping)+1]] <- list(pfts_in=c("extra-tropical evergreen", "extra-tropical deciduous"),
                                             newname="extra-tropical tree")
    }
    for (mi in seq_along(mapping)) {
        inds <- match(mapping[[mi]]$pfts_in, levs)
        if (!any(is.na(inds))) {
            message("aggregate ", length(inds), " PFTs together: ", paste(levs[inds], collapse=", "))
            tmp <- apply(data[inds,], 2, sum)
            data[inds[1],] <- tmp
            data <- abind::asub(data, idx=seq_along(levs)[-inds[2]], dims=levind)
            dimnames(data)[[1]][inds[1]] <- mapping[[mi]]$newname 
            levs <- dimnames(data)[[1]] # update
        }
    } # for gi
}

nlev <- dim(data)[levind]

# normalize
if (T && varname == "pft_fract_box") {
    message("\nvarname is pft_fract_box --> normalize by total land points ...")
    nland <- atts$slm_nland
    data <- data/nland*100
    ylab <- "PFT cover of global land [%]"
}

# plot
plotname <- getwd()
if (file.access(plotname, mode=2) == -1) stop("can not save plot to path ", plotname)
plotname <- paste0(plotname, "/", basename(fin))
if (plottype == "png") plotname <- paste0(plotname, ".png")
if (plottype == "pdf") plotname <- paste0(plotname, ".pdf")
message("\nsave plot ", plotname, " ...")
pp <- plot_sizes()
if (plottype == "png") {
    png(plotname, width=pp$png_width_px, height=pp$png_height_px,
        res=pp$png_ppi, family="Droid Sans")
} else if (plottype == "pdf") {
    pdf(plotname, width=pp$pdf_width_in, height=pp$pdf_height_in,
        family="sans")
} else {
    stop("plottype == ", plottype, " not defined")
}
ylim <- range(data, na.rm=T)
ltys <- rep(1:10, e=3, l=nlev)
cols <- rep(1:8, t=10, l=nlev)
if (T) cols <- mycols(nlev)
plot(time, rep(NA, t=length(time)), 
     ylim=ylim, xlab=xlab, ylab=ylab,
     xaxt="n", yaxt="n")
if (F) {
    axis.POSIXct(1, at=pretty(time, n=10))
} else if (T) {
    axis.POSIXct(1, at=pretty(time, n=30), format="%Y")
}
axis(2, pretty(ylim, n=10), las=2)
for (i in seq_len(dim(data)[levind])) {
    tmp <- abind::asub(data, idx=i, dims=levind)
    lines(time, tmp, lty=ltys[i], col=cols[i])
}
legend("topright", dimnames(data)[[levind]], lty=ltys, col=cols,
       bty="n", x.intersp=0.5, cex=0.66, ncol=3)
invisible(dev.off())

message("\nfinished\n")


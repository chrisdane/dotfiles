#!/usr/bin/env Rscript

rm(list=ls()); graphics.off()
#options(warn=2) # stop on warnings    
options(warn=0) # do not stop on warnings    

plottype <- "png" # "png" "pdf"

# exclude?
exclude <- NULL
if (F) exclude <- c("glacier")
if (T) exclude <- c("glacier", "bare land (=1-veg_ratio_max)")

# map?
mapping <- list()
if (F) { # all: should be 100%
    mapping[[length(mapping)+1]] <- list(pfts_in=c("tropical broadleaf evergreen", "tropical broadleaf deciduous",
                                                   "extra-tropical evergreen", "extra-tropical deciduous",
                                                   "raingreen shrubs", "deciduous shrubs",
                                                   "C3 grass", "C4 grass",
                                                   "C3 pasture", "C4 pasture",
                                                   "C3 crops", "C4 crops",
                                                   "glacier",
                                                   "bare land (1-veg_ratio_max)"),
                                         pft_out="all",
                                         col="black", lty=1)
}
if (F) {
    mapping[[length(mapping)+1]] <- list(pfts_in=c("tropical broadleaf evergreen", "tropical broadleaf deciduous"),
                                         pft_out="tropical tree",
                                         col="#173c00", lty=1)
    mapping[[length(mapping)+1]] <- list(pfts_in=c("extra-tropical evergreen", "extra-tropical deciduous"),
                                         pft_out="extra-tropical tree",
                                         col="#10a350", lty=1)
}
if (T) {
    mapping[[length(mapping)+1]] <- list(pfts_in=c("tropical broadleaf evergreen", "tropical broadleaf deciduous", "extra-tropical evergreen", "extra-tropical deciduous"),
                                         pft_out="tree",
                                         col="#173c00", lty=1)
}
if (T) {
    mapping[[length(mapping)+1]] <- list(pfts_in=c("raingreen shrubs", "deciduous shrubs"),
                                         pft_out="shrub",
                                         col="#0f8387", lty=1)
}
if (F) {
    mapping[[length(mapping)+1]] <- list(pfts_in=c("C3 grass", "C4 grass"),
                                         pft_out="grass (C3+C4)",
                                         col="#44ef47", lty=1)
    mapping[[length(mapping)+1]] <- list(pfts_in=c("C3 pasture", "C4 pasture"),
                                         pft_out="pasture (C3+C4)",
                                         col="#b5b91b", lty=1)
    mapping[[length(mapping)+1]] <- list(pfts_in=c("C3 crops", "C4 crops"),
                                         pft_out="cropland (C3+C4)",
                                         col="#ffb660", lty=1)
}
if (T) {
    mapping[[length(mapping)+1]] <- list(pfts_in=c("C3 grass", "C4 grass"),
                                         pft_out="grass",
                                         col="#44ef47", lty=1)
    mapping[[length(mapping)+1]] <- list(pfts_in=c("C3 pasture", "C4 pasture", "C3 crops", "C4 crops"),
                                         pft_out="pasture + cropland",
                                         col="#ffb660", lty=1)
}

#########################################

if (interactive()) {
    me <- "jsbach_plot_pft.r"
    #args <- "/work/ab1095/a270073/post/jsbach/fldsum/pft_area_box/awi-esm-1-1-lr_kh800_historical2_jsbach_fldsum_pft_area_box_global_Jan-Dec_1850-2014.nc"
    #args <- "/work/ab1095/a270073/post/jsbach/fldsum/pft_area_box/awi-esm-1-1-lr_kh800_historical2_jsbach_fldsum_pft_area_box_global_annual_1850-2014.nc"
    #args <- "/work/ba1103/a270073/post/jsbach/fldsum/pft_fract_box/awi-esm-1-1-lr_kh800_esm-piControl2_jsbach_fldsum_pft_fract_box_global_annual_1850-1872.nc"
    #args <- "/work/ba1103/a270073/post/jsbach/fldsum/pft_fract_box/awi-esm-1-1-lr_kh800_esm-piControl_wout_talk_rest2_jsbach_fldsum_pft_fract_box_global_annual_3208-3945.nc"
    #args <- "/work/ba1103/a270073/post/jsbach/fldsum/pft_fract_box/awi-esm-1-1-lr_kh800_piControl_and_esm-piControl_jsbach_fldsum_pft_fract_box_global_annual_1950-3945.nc"
    #args <- "/work/ba1103/a270073/post/jsbach/fldsum/pft_fract_box/awi-esm-1-1-lr_kh800_piControl_and_esm-piControl_jsbach_fldsum_pft_fract_box_global_annual_1950-4527.nc"
    #args <- "/work/ba1103/a270073/post/jsbach/fldsum/pft_fract_box/awi-esm-1-1-lr_kh800_historical2_jsbach_fldsum_pft_fract_box_global_Jan-Dec_1850-2014.nc"
    args <- "/work/ba1103/a270073/post/jsbach/fldsum/pft_area_box/awi-esm-1-1-lr_kh800_historical2_jsbach_fldsum_pft_area_box_global_Jan-Dec_1850-2014.nc"

} else {
    args <- commandArgs(trailingOnly=F)
    me <- basename(sub("--file=", "", args[grep("--file=", args)]))
    args <- commandArgs(trailingOnly=T)

} # if interactive or not

# checks
help <- paste0("Usage:\n",
               " $ ", me, " <fldsum_of_jsbach_select_pft_wrt_box.r>\n")
if (length(args) != 1) {
    message(help)
    quit()
}
fin <- args[1]
if (!file.exists(fin)) stop("fin ", fin, " does not exist")
fin <- normalizePath(fin)

message("load ncdf4 package ...")
library(ncdf4)
message("load abind package ...")
library(abind)

source("~/scripts/r/functions/myfunctions.r") # mycols(), plot_sizes()

# open file
message("\nopen ", fin, " ...")
nc <- ncdf4::nc_open(fin)

# check if input is valid
known_varnames <- c("pft_fract_box", "pft_area_box")
ind <- na.omit(match(known_varnames, names(nc$var)))
if (length(ind) != 1) stop("must find exactly one of allowed varnames: ", paste(known_varnames, collapse=", "))
varname <- names(nc$var)[ind]
atts <- ncdf4::ncatt_get(nc, varname)
message("--> found variable ", varname, ":")
cat(capture.output(str(atts)), sep="\n")
units <- atts$units
if (units == "fraction") {
    ylab <- "PFT fraction"
} else if (units == "m2") {
    ylab <- "PFT area" 
} else {
    stop("`units` = ", units, " not defined")
}
vardimids <- nc$var[[varname]]$dimids
dimids <- sapply(nc$dim, "[[", "id")
dimlengths <- sapply(nc$dim, "[[", "len")
vardimlengths <- dimlengths[match(vardimids, dimids)]
if (!any(names(vardimlengths) == "pft")) stop("one dim of variable ", varname, " must be \"pft\"")

# get pfts
inds <- which(substr(names(atts), 1, 3) == "pft")
if (length(inds) == 0) stop("there should be `pft*` attribute names of variable ", varname)
pfts <- unlist(atts[inds])
pfts <- strsplit(pfts, ";")
pfts <- sapply(pfts, "[", 1)
pfts <- strsplit(pfts, "name=")
pfts <- sapply(pfts, "[", 2)

# read data
message("\nread (", paste(paste0(sapply(nc$var[[varname]]$dim, "[[", "name"), ":", nc$var[[varname]]$size), collapse=" x "), 
        ") variable \"", varname, "\" ...") 
data <- ncdf4::ncvar_get(nc, varname)
inds <- which(vardimlengths != 1)
attributes(data)$dimname <- names(vardimlengths)[inds]
message("data:")
cat(capture.output(str(data)), sep="\n")
if (length(dim(data)) != 2) stop("data must have 2 dims") # (pft, time) (or (time, pft))
pftind <- na.omit(match(attributes(data)$dimname, "pft"))
if (length(pftind) != 1) stop("this should not happen")
dimnames(data)[[pftind]] <- pfts
npfts <- dim(data)[pftind]
message("--> ", npfts, " PFTs")

# default colors/ltys
cols <- seq_len(npfts)
ltys <- rep(1, times=npfts)
if (T) { # my default colors
    cols[which(pfts == "tropical broadleaf evergreen")] <- "#173c00"
    cols[which(pfts == "tropical broadleaf deciduous")] <- "#10a350"
    cols[which(pfts == "extra-tropical evergreen")] <- "#173c00"
    ltys[which(pfts == "extra-tropical evergreen")] <- 2
    cols[which(pfts == "extra-tropical deciduous")] <- "#10a350"
    ltys[which(pfts == "extra-tropical deciduous")] <- 2
    cols[which(pfts == "raingreen shrubs")] <- "#0f8387"
    cols[which(pfts == "deciduous shrubs")] <- "#27c6b2"
    cols[which(pfts == "C3 grass")] <- "#44ef47"
    cols[which(pfts == "C4 grass")] <- "#66bb6a"
    cols[which(pfts == "C3 pasture")] <- "#b5b91b"
    cols[which(pfts == "C4 pasture")] <- "#faf368"
    cols[which(pfts == "C3 crops")] <- "#ffb660"
    cols[which(pfts == "C4 crops")] <- "#ed6130"
    cols[which(pfts == "glacier")] <- "#90a4ae"
    cols[which(pfts == "bare land (=1-veg_ratio_max)")] <- "#6d4c41"
} # my default cols/ltys

# get time
message("time:")
time <- as.POSIXct(strsplit(trimws(system(paste0("cdo -s showtimestamp ", fin), intern=T)), "  ")[[1]], tz="UTC")
cat(capture.output(str(time)), sep="\n")
xlab <- "year"
if (F) { # piControl time
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
    inds <- match(exclude, pfts)
    if (!any(is.na(inds))) {
        message("remove ", length(inds), " pfts: ", paste(paste0(inds, ":\"", pfts[inds], "\""), collapse=", "))
        data <- abind::asub(data, idx=seq_len(npfts)[-inds], dims=pftind)
    } else {
        stop("those pfts are not found in the data")
    }
    pfts <- dimnames(data)[[pftind]]
    cols <- cols[-inds]
    ltys <- ltys[-inds]
    npfts <- dim(data)[pftind] # update
    message("--> ", npfts, " PFTs")
}

# aggregate PFTs
if (length(mapping) > 0) {
    message("\n`mapping`:")
    cat(capture.output(str(mapping)), sep="\n")
    newdims <- dim(data)
    newdims[pftind] <- length(mapping)
    data2 <- array(NA, newdims)
    pfts2 <- cols2 <- ltys2 <- rep(NA, times=length(mapping))
    mapinds <- vector("list", length=length(mapping))
    for (mi in seq_along(mapping)) {
        inds <- match(mapping[[mi]]$pfts_in, pfts)
        if (anyNA(inds)) stop("some of `mapping[[", mi, "]]$pfts_in` are not defined in `pfts` = \n", paste(pfts, collapse=", "))
        mapinds[[mi]] <- inds 
        message("aggregate (sum) ", length(inds), " PFTs (", paste(pfts[inds], collapse=", "), ") to new PFT ", mapping[[mi]]$pft_out, " ...")
        tmp <- apply(data[inds,], seq_len(2)[-pftind], sum) # sum over pft dim = keep non-pft dim
        data2[mi,] <- tmp
        pfts2[mi] <- mapping[[mi]]$pft_out
        cols2[mi] <- mapping[[mi]]$col
        ltys2[mi] <- mapping[[mi]]$lty
    } # for mi
    mapinds <- unlist(mapinds)
    if (any(duplicated(mapinds))) stop("some input PFT was mapped to more than 1 new PFT --> check `mapping`")
    
    # remove all input pfts that were mapped to new pfts 
    data <- abind::asub(data, idx=seq_len(npfts)[-mapinds], dims=pftind) # arbitrary subsetting
    pfts <- pfts[-mapinds]
    cols <- cols[-mapinds]
    ltys <- ltys[-mapinds]

    # continue with new pfts and, if any remaining, input pfts
    if (dim(data)[pftind] == 0) { # no inpt pfts remains --> replace by new pfts
        data <- data2
        pfts <- pfts2
        cols <- cols2
        ltys <- ltys2
    } else { # some input pfts remain --> merge input and new pfts
        if (pftind == 1) {
            data <- rbind(data, data2)
        } else if (pftind == 2) {
            data <- cbind(data, data2)
        } else {
            stop("not defined")
        }
        pfts <- c(pfts, pfts2)
        cols <- c(cols, cols2)
        ltys <- c(ltys, ltys2)
    }
    rm(data2, pfts2, cols2, ltys2)
    dimnames(data)[[pftind]] <- pfts
    npfts <- dim(data)[pftind] # update
} # if length(mapping) > 0

# normalize
if (T) {
    if (varname == "pft_fract_box") {
        message("\nvarname is pft_fract_box --> normalize by total land points ...")
        nland <- atts$slm_nland
        data <- data/nland*100
    } else if (varname == "pft_area_box") {
        message("\nvarname is pft_area_box --> normalize by total land area ...")
        arealand <- atts$slm_arealand
        data <- data/arealand*100
    }
    ylab <- "PFT cover of global land [%]"
}

# plot
plotname <- getwd()
if (file.access(plotname, mode=2) == -1) stop("can not save plot to path ", plotname)
plotname <- paste0(plotname, "/", basename(fin), "_", npfts, "_PFTs")
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
message("ylim = ", ylim[1], ", ", ylim[2])
# pft_fract_box:
# historical2: ylim = 1.59224856728788, 17.4425790794838
# esm-hist: ylim = 1.81241902579601, 15.7980779086769
# pft_area_box 4 PFTs:
# historical: ylim = 2.25243802237673, 26.7298189933622
# esm-hist: ylim = 2.42445514286109, 23.452160737464
if (T) {
    message("\nspecial: historical2 and esm-hist ylims")
    #ylim <- c(1.59224856728788, 17.4425790794838) # pft_fract_box
    #ylim <- c(1.59224856728788, 17.4425790794838) # pft_area_box
    #ylim <- c(1.59224856728788, 26.7328782398961) # pft_fract_box and pft_area_box
    ylim <- c(2.25243802237673, 26.7298189933622) # pft_area_box historical2 esm-hist
    message("ylim = ", ylim[1], ", ", ylim[2])
}
plot(time, rep(NA, times=length(time)), 
     ylim=ylim, xlab=xlab, ylab=ylab,
     xaxt="n", yaxt="n")
if (F) {
    axis.POSIXct(1, at=pretty(time, n=10))
} else if (T) {
    axis.POSIXct(1, at=pretty(time, n=10), format="%Y")
}
axis(2, pretty(ylim, n=10), las=2)
for (pfti in seq_len(npfts)) {
    tmp <- abind::asub(data, idx=pfti, dims=pftind) # arbitrary subsetting
    lines(time, tmp, lty=ltys[pfti], col=cols[pfti])
    if (F) points(time, tmp, col=cols[pfti])
}
legend("topright", dimnames(data)[[pftind]], lty=ltys, col=cols,
       bty="n", x.intersp=0.2, cex=1, ncol=1)
invisible(dev.off())

message("\nfinished\n")


#!/usr/bin/env Rscript

rm(list=ls()); graphics.off()
options(warn=2) # stop on warnings    

plottype <- "png" # "png" "pdf"
exclude <- NULL
if (F) exclude <- c("glacier", "bare land (1-veg_ratio_max)")

#########################################

known_varnames <- c("pft_fract_box")

if (interactive()) { # test
    #args <- "/work/ba1103/a270073/post/jsbach/fldsum/pft_fract_box/awi-esm-1-1-lr_kh800_piControl_and_esm-piControl_jsbach_fldsum_pft_fract_box_global_annual_1950-3945.nc"
    args <- "/work/ba1103/a270073/post/jsbach/fldsum/pft_fract_box/awi-esm-1-1-lr_kh800_esm-piControl2_jsbach_fldsum_pft_fract_box_global_annual_1850-1872.nc"
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
if (F) {
    message("\nspecial: set new time ...")
    time <- as.POSIXlt(time)
    #time$year <- time$year - time$year[1] - 1900 # make first year zero
    time$year <- time$year - time$year[1] - 1900 + 1 # make first year 1
}

# exclude certain pfts
if (!is.null(exclude)) {
    message("\n`exclude` = ", paste(exclude, collapse=", "))
    inds <- na.omit(match(exclude, levs))
    if (!all(is.na(inds))) {
        message("remove ", length(inds), " levels: ", paste(paste0(inds, ":\"", levs[inds], "\""), collapse=", "))
        data <- abind::asub(data, idx=seq_along(levs)[-inds], dims=levind)
    } else {
        stop("those levels are not found in the data")
    }
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
if (plottype == "png") {
    #png(plotname, width=7, height=5.25, units="in", pointsize=14.8, res=300, family="Droid Sans")
    png(plotname, width=7, height=7, units="in", pointsize=14.8, res=300, family="Droid Sans")
} else if (plottype == "pdf") {
    #pdf(plotname, width=7, height=5.25, pointsize=14.8)
    pdf(plotname, width=7, height=7, pointsize=14.8)
} else {
    stop("plottype == ", plottype, " not defined")
}
ylim <- range(data, na.rm=T)
ltys <- rep(1:10, e=8, l=nlev)
cols <- rep(1:8, t=10, l=nlev)
plot(time, rep(NA, t=length(time)), 
     ylim=ylim, xlab="time", ylab=ylab,
     xaxt="n", yaxt="n")
axis.POSIXct(1, at=pretty(time, n=10))
axis(2, pretty(ylim, n=10), las=2)
for (i in seq_len(dim(data)[levind])) {
    tmp <- abind::asub(data, idx=i, dims=levind)
    lines(time, tmp, lty=ltys[i], col=cols[i])
}
legend("topright", dimnames(data)[[levind]], lty=ltys, col=cols,
       bty="n", x.intersp=0.5, cex=0.5, ncol=3)
invisible(dev.off())

message("\nfinished\n")


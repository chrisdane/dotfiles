#!/usr/bin/env Rscript

thkcello <- NULL
if (interactive()) {
    me <- "get_dz.r"
    if (T) { # mpi-esm lr
        args <- "/work/ik1017/CMIP6/data/CMIP6/CMIP/MPI-M/MPI-ESM1-2-LR/historical/r1i1p1f1/Ofx/thkcello/gn/v20190710/thkcello_Ofx_MPI-ESM1-2-LR_historical_r1i1p1f1_gn.nc"
        thkcello <- c(12, 10, 10, 10, 10, 10, 13, 15, 20, 25, 30, 35, 40, 45, 50, 55, 60, 70, 80, 90, 100, 110, 120, 130, 140, 150, 170, 180, 190, 200, 220, 250, 270, 300, 350, 400, 450, 500, 500, 600)
    } else if (F) { # awi-esm mr
        args <- "/work/ik1017/CMIP6/data/CMIP6/CMIP/AWI/AWI-CM-1-1-MR/historical/r1i1p1f1/Omon/thetao/gn/v20181218/thetao_Omon_AWI-CM-1-1-MR_historical_r1i1p1f1_gn_201101-201412.nc"
        thkcello <- c(5, 10, 10, 10, 10, 10, 10, 10, 10, 10, 12.5, 17.5, 22.5, 27.5, 35, 45, 55, 65, 75, 85, 95, 105, 115, 125, 135, 145, 160, 185, 210, 225, 240, 250, 250, 250, 250, 250, 250, 250, 250, 250, 250, 250, 250, 250, 250, 250)
    }
} else { # if not interactive
    args <- commandArgs(trailingOnly=F)
    me <- basename(sub("--file=", "", args[grep("--file=", args)]))
    args <- commandArgs(trailingOnly=T)
}

help <- paste0("\nUsage:\n $ ", me, " file.nc")

# stop if help
if (length(args) != 1) {
    message(help)
    if (interactive()) {
        stop("stop interactive call")
    } else {
        quit()
    }
}

fin <- args[1]
message("input file: ", fin)
if (!file.exists(fin)) stop("file does not exist")

cdo <- Sys.which("cdo")
if (cdo == "") stop("could not find cdo")

options(warn=2) # stop on warnings

cmd <- paste0(cdo, " -s showlevel ", fin)
message("run `", cmd, "` ...")
z <- system(cmd, intern=T)
z <- unlist(strsplit(trimws(z), "\\s+"))
z <- as.numeric(z)
nz <- length(z)
message("--> loaded ", nz, " z levels:"); print(z)

# dz =?
dz_diff <- c(NA, abs(diff(z)))
dz_default <- rep(NA, t=nz)
dz_default[1] <- (z[1] - z[2])/2
dz_default[nz] <- (z[nz - 1] - z[nz])/2
for (i in 2:(nz - 1)) dz_default[i] <- (z[i - 1] - z[i])/2 + (z[i] - z[i + 1])/2
dz_default <- abs(dz_default)

df <- data.frame(z=z, dz_diff=dz_diff, dz_default=dz_default)
if (!is.null(thkcello)) df <- cbind(df, thkcello=thkcello)
print(df)

message("sum(dz):")
print(apply(df[,2:ncol(df)], 2, sum, na.rm=T))

message("sum(dz) - z[nz]:")
print(apply(df[,2:ncol(df)], 2, sum, na.rm=T) - z[nz])


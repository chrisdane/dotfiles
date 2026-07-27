#!/usr/bin/env Rscript

# get dz = thkcello from `cdo showlevel`

if (interactive()) {
    me <- "get_dz.r"
    #args <- "/work/ik1017/CMIP6/data/CMIP6/CMIP/MPI-M/MPI-ESM1-2-LR/historical/r1i1p1f1/Ofx/thkcello/gn/v20190710/thkcello_Ofx_MPI-ESM1-2-LR_historical_r1i1p1f1_gn.nc"
    #args <- "/work/ik1017/CMIP6/data/CMIP6/CMIP/AWI/AWI-CM-1-1-MR/historical/r1i1p1f1/Omon/thetao/gn/v20181218/thetao_Omon_AWI-CM-1-1-MR_historical_r1i1p1f1_gn_201101-201412.nc"
    args <- "/work/bb1469/a270092/runtime/awiesm3-develop/Final_CMIP7_IO_Test_06/outdata/fesom/temp.fesom.1586.nc"

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

# calc dz = thkcello
message("\ndz:")

# wrong: simple diff
dz_diff <- c(NA, abs(diff(z)))

# wrong: my old version
dz_my <- rep(NA, times=nz)
dz_my[1] <- (z[1] - z[2])/2
dz_my[nz] <- (z[nz - 1] - z[nz])/2
for (i in 2:(nz-1)) dz_my[i] <- (z[i-1] - z[i])/2 + (z[i] - z[i+1])/2
dz_my <- base::abs(dz_my)

# correct (?):
# --> 1st and last thkcello need special treatment
# --> `cdo genlevelbounds` or `ncap2 -s 'thkcello=depth_bnds(:,1)-depth_bnds(:,0)' fin fout` not alaways correct, e.g.
# z = 2.5, 7.5, 15, ... --> bnds = (2.5, 5), (5, 11.25), --> upper zero missing
# --> do manually:
# bounds[1] = 0
# bounds[k+1] = 0.5 * (z[k] + z[k+1])
# bounds[n+1] = z[n] + 0.5 * (z[n] - z[n-1])
# thkcello[k] = bounds[k+1] - bounds[k]
bounds <- rep(NA, times=nz+1)
bounds[1] <- 0
bounds[2:nz] <- 0.5*(z[-nz] + z[-1])
bounds[nz+1] <- z[nz] + 0.5*(z[nz] - z[nz-1])
thkcello <- base::diff(bounds)

# print
df <- data.frame(z=c(z, NA), bounds=bounds, thkcello=c(thkcello, NA), diff=c(dz_diff, NA), my=c(dz_my, NA))
print(df)


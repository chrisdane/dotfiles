#!/usr/bin/env Rscript

# load convert_lon_360_to_180() from myfunctions.r
if (!file.exists("~/scripts/r/functions/myfunctions.r")) {
    stop("\"~/scripts/r/functions/myfunctions.r\" does not exist")
} else {
    source("~/scripts/r/functions/myfunctions.r") # loads convert_lon_360_to_180()
}

# get argument names of convert_lon_360_to_180()
allowed_args <- formalArgs(convert_lon_360_to_180)

args <- commandArgs(trailingOnly=F)
me <- basename(sub("--file=", "", args[grep("--file=", args)]))
args <- commandArgs(trailingOnly=T)
    
usage <- paste0("\nUsage:\n", me, " ",
                paste(paste0("--", allowed_args), collapse=" "), "\n")

if (length(args) == 0) {    
    message(usage)
    quit()
}

# all checks are done within the function itself
nc_file <- nc_out <- nc_varname <- NULL
if (any(grepl("--nc_file", args))) {
    nc_file <- sub("--nc_file=", "", args[grep("--nc_file=", args)])
}
if (any(grepl("--nc_out", args))) {
    nc_out <- sub("--nc_out=", "", args[grep("--nc_out=", args)])
}
if (any(grepl("--nc_varname", args))) {
    nc_varname <- sub("--nc_varname=", "", args[grep("--nc_varname=", args)])
}

# run actual function
convert_lon_360_to_180(nc_file=nc_file, nc_out=nc_out, nc_varname=nc_varname)


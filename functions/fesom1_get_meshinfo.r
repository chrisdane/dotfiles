#!/usr/bin/env Rscript

# just print stuff

if (interactive()) {
    me <- "fesom1_get_meshinfo.r"
    args <- c("/pool/data/AWICM/FESOM1/MESHES/core")
} else {
    args <- commandArgs(trailingOnly=F)
    me <- basename(sub("--file=", "", args[grep("--file=", args)]))
    args <- commandArgs(trailingOnly=T)
    #./fesom1_get_meshinfo.r /pool/data/AWICM/FESOM1/MESHES/core
}

usage <- paste0("\nUsage:\n",
                " $ ", me, " meshpath [--lib=/add/path/where/R/packages/are/installed,/separated/by/comma/if/more/than/one]\n",
                "\n",
                "e.g. ", me, " /pool/data/AWICM/FESOM1/MESHES/core\n",
                "     ", me, " /pool/data/AWICM/FESOM1/MESHES/core --lib=~/scripts/r/packages/bin/r_4.1\n") 

# check
if (length(args) < 1) {
    message(usage)
    quit()
}

meshpath <- args[1]
if (!dir.exists(meshpath)) stop("meshpath ", meshpath, " does not exist")

lib <- .libPaths() # default
if (any(grepl("--lib", args))) {
    lib <- sub("--lib=", "", args[grep("--lib=", args)])
    lib <- strsplit(lib, ",")[[1]]
}

message("\ncheck r package data.table ... ", appendLF=F)
data.table_check <- suppressWarnings(library(data.table, lib=lib, logical.return=T))
if (!data.table_check) {
    message("could not load data.table package from library path\n",
         paste(paste0("   ", lib), collapse="\n"), "\n",
         "rerun with\n  ", me, " ", meshpath, " --lib=/path/where/package/data.table/is/installed\n",
         "or install data.table package with\n   `install.packages(\"data.table\")`\n",
         "or\n   `install.packages(\"data.table\", lib=\"/path/where/the/package/should/get/installed\")`\n",
         "in an R session\n",
         "--> will continue using base::scan() which is slower")
} else {
    message("available (loaded from ", dirname(dirname(attr(packageDescription("data.table"), "file"))), ")")
}

message("\nget number of 2d nodes, 3d nodes and levels, depth levels ...")
n2 <- base::scan(paste0(meshpath, "/nod2d.out"), what=integer(), n=1, quiet=T)
n3 <- base::scan(paste0(meshpath, "/nod3d.out"), what=integer(), n=1, quiet=T)
nlev <- base::scan(paste0(meshpath, "/aux3d.out"), what=integer(), n=1, quiet=T)
aux3d <- NULL
if (data.table_check) {
    nod3d <- data.table::fread(paste0(meshpath, "/nod3d.out"), skip=1, showProgress=F)
    nod3d <- as.matrix(nod3d)
    if (F) {
        aux3d <- data.table::fread(paste0(meshpath, "/aux3d.out"), skip=1, nrows=n2*nlev, 
                                   na.strings="-999", showProgress=F)
        aux3d <- matrix(aux3d$V1, nrow=nlev, ncol=n2)
    }
} else { # use base::scan
    nod3d <- base::scan(paste0(meshpath, "/nod3d.out"), skip=1, quiet=T)
    nod3d <- matrix(nod3d, nrow=n3, byrow=T)
    if (F) {
        aux3d <- base::scan(paste0(meshpath, "/aux3d.out"), skip=1, nlines=n2*nlev, 
                            na.strings="-999", quiet=T)
        aux3d <- matrix(aux3d, nrow=nlev, ncol=n2)
    }
}
colnames(nod3d) <- c("no", "lon", "lat", "depth", "")
depth <- abs(unique(nod3d[,"depth"]))
message("--> n2 = ", n2, "\n",
        "--> n3 = ", n3, "\n",
        "--> nlev = ", nlev, "\n",
        "--> depths = ", paste(depth, collapse=", "))
if (!is.null(aux3d)) {
    n_non_NA <- rep(NA, t=nlev)
    for (di in seq_len(nlev)) {
        message("depth ", di, "/", nlev, " ", depth[di], ": ", appendLF=F)
        non_NA_inds <- which(aux3d[di,] > -999)
        if (length(non_NA_inds) > 0) {
            n_non_NA[di] <- length(non_NA_inds)
            message(n_non_NA[di], "/", n2, 
                    " non-NA nodes = ", round(n_non_NA[di]/n2*100), "% from ",
                    paste(aux3d[di,range(non_NA_inds)], collapse=" to "))
        } else {
            message()
        }
    }
    message("--> total non-NA nodes = ", sum(n_non_NA, na.rm=T))
}

message("\nfinished")


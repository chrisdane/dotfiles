#!/usr/bin/env Rscript

# DIC_remin = r*AOU
#   with   r = 0.688 C O-1
#            = carbon to oxygen ratio
#        AOU = O2sat - O2 (eq 5.2.2 in Sarmiento & Gruber 2006; Frenger et al. 2024 have typo)
#            = apparent oxygen utilization = remineralized oxygen component 
# - surface AOU can be <0 due to O2 supersaturation, especially in low lats
# - high AOU in thermocline due to large remineralization or slow circulation?

if (interactive()) {
    rm(list=ls())
    me <- "calc_DIC_remin.r"
    if (F) { # non-levelwise
        args <- c("thetao=/work/ba1103/a270073/out/awicm-1.0-recom/awi-esm-1-1-lr_kh800/historical2/outdata/post/fesom/monmean/thetao_fesom_20140101.nc",
                  "so=/work/ba1103/a270073/out/awicm-1.0-recom/awi-esm-1-1-lr_kh800/historical2/outdata/post/fesom/monmean/so_fesom_20140101.nc",
                  "bgc22=/work/ba1103/a270073/out/awicm-1.0-recom/awi-esm-1-1-lr_kh800/historical2/outdata/recom/bgc22_fesom_20140101.nc",
                  "nod3d.out=/pool/data/AWICM/FESOM1/MESHES/core/nod3d.out",
                  "unit_out=mmol m-3",
                  "fout=/work/ba1103/a270073/out/awicm-1.0-recom/awi-esm-1-1-lr_kh800/historical2/outdata/post/recom/dic_remin_2014.nc")
    } else if (T) { # levelwise
        args <- c("thetao=/work/ba1103/a270073/out/awicm-1.0-recom/awi-esm-1-1-lr_kh800/esm-hist/outdata/post/fesom/levelwise/thetao_fesom_20140101_levelwise_0-5900m_setgrid.nc",
                  "so=/work/ba1103/a270073/out/awicm-1.0-recom/awi-esm-1-1-lr_kh800/esm-hist/outdata/post/fesom/levelwise/so_fesom_20140101_levelwise_0-5900m_setgrid.nc",
                  "bgc22=/work/ba1103/a270073/out/awicm-1.0-recom/awi-esm-1-1-lr_kh800/esm-hist/outdata/post/recom/levelwise/bgc22_fesom_20140101_levelwise_0-5900m_setgrid.nc",
                  "unit_out=mmol m-3",
                  "fout=/work/ba1103/a270073/out/awicm-1.0-recom/awi-esm-1-1-lr_kh800/esm-hist/outdata/post/recom/levelwise/dic_remin_20140101_levelwise_0-5900m_setgrid.nc")
    }
} else {
    args <- commandArgs(trailingOnly=F)
    me <- basename(sub("--file=", "", args[grep("--file=", args)]))
    args <- commandArgs(trailingOnly=T)
}

usage <- paste0("\nUsage:\n",
                " $ ", me, " thetao_varname=/path/to/thetao so_varname=/path/to/so o2_varname=/path/to/o2 [nod3d.out=/path/to/nod3d.out/if/fesom1/non-levelwise] [unit_out=\"mmol m-3\"] fout=/path/to/fout\n",
                "\n",
                "e.g. nod3d.out=/pool/data/AWICM/FESOM1/MESHES/core/nod3d.out\n",
                "\n",
                "the o2 file (3rda rg) must have the same units as `unit_out`\n",
                "\n")

# check
if (length(args) < 3) {
    message(usage)
    if (interactive()) {
        stop("stop interactive call")
    } else {
        quit()
    }
}

if (!interactive()) {
    library(ncdf4)
    library(gsw)
}
cdo <- Sys.which("cdo")
if (cdo == "") stop("could not find cdo")
o2sat_script <- "~/bin/my_gsw_O2sol_SP_pt.r"
if (!file.exists(o2sat_script)) stop("o2sat script ", o2sat_script, " does not exist")

thetao_varname <- strsplit(args[1], "=")[[1]]
if (length(thetao_varname) != 2) stop("1st arg must be `thetao_varname=/path/to/file`")
thetao_file <- thetao_varname[2]
if (!file.exists(thetao_file)) {
    if (interactive()) {
        stop("thetao file", thetao_file, " does not exist")
    } else {
        message("thetao file", thetao_file, " does not exist")
        quit()
    }
}
thetao_varname <- thetao_varname[1]

so_varname <- strsplit(args[2], "=")[[1]]
if (length(so_varname) != 2) stop("2nd arg must be `so_varname=/path/to/file`")
so_file <- so_varname[2]
if (!file.exists(so_file)) {
    if (interactive()) {
        stop("so file", so_file, " does not exist")
    } else {
        message("so file", so_file, " does not exist")
        quit()
    }
}
so_varname <- so_varname[1]

o2_varname <- strsplit(args[3], "=")[[1]]
if (length(o2_varname) != 2) stop("3nd arg must be `o2_varname=/path/to/file`")
o2_file <- o2_varname[2]
if (!file.exists(o2_file)) {
    if (interactive()) {
        stop("o2 file", o2_file, " does not exist")
    } else {
        message("o2 file", o2_file, " does not exist")
        quit()
    }
}
o2_varname <- o2_varname[1]

if (any(grepl("^fout=", args))) {
    ind <- which(grepl("^fout=", args))
    if (length(ind) != 1) stop("found ", length(ind), " args that start with \"fout\". must be 1")
    fout <- sub("fout=", "", args[ind])
    args <- args[-ind]
} else {
    stop("provide `fout=/path/to/fout")
}
if (file.exists(fout)) {
    if (interactive()) {
        stop("fout ", fout, " already exists. skip") 
    } else {
        message("fout ", fout, " already exists. quit")
        quit()
    }
}
dir.create(dirname(fout), recursive=T, showWarnings=F)
if (!dir.exists(dirname(fout))) stop("could not create output dir ", dirname(fout))

known_units <- c("µmol kg-1", "mmol m-3")
if (any(grepl("^unit_out=", args))) {
    ind <- which(grepl("^unit_out=", args))
    if (length(ind) != 1) stop("found ", length(ind), " args that start with \"out_unit\". must be 1 if provided")
    unit_out <- sub("unit_out=", "", args[ind])
    if (!any(unit_out == known_units)) {
        stop("`unit_out` = ", unit_out, " must be one of ", paste(known_units, collapse=", "))
    }
    args <- args[-ind]
} else {
    unit_out <- "µmol kg-1" # gsw default
}

nod3d_file <- NULL
if (any(grepl("^nod3d.out=", args))) {
    ind <- which(grepl("^nod3d.out=", args))
    if (length(ind) != 1) stop("found ", length(ind), " args that start with \"nod3.out=\". must be 1 if provided")
    nod3d_file <- sub("nod3d.out=", "", args[ind])
    if (!file.exists(nod3d_file)) stop("provided nod3d.out file ", nod3d_file, " does not exist")
    args <- args[-ind]
}

#############################################################################

# get o2sat
message("\ncalc o2sat ...")
fout_o2sat <- paste0(fout, "_o2sat")
if (file.exists(fout_o2sat)) {
    message("tmp o2sat file ", fout_o2sat, " already exists. skip o2sat calculation")
} else {
    cmd <- paste0(o2sat_script, 
                  " ", thetao_varname, "=", thetao_file,
                  " ", so_varname, "=", so_file)
    if (!is.null(nod3d_file)) {
        cmd <- paste0(cmd, " nod3d.out=", nod3d_file)
    }
    cmd <- paste0(cmd,
                  " unit_out=\"", unit_out, "\"",
                  " fout=", fout_o2sat)
    message("run `", cmd, "` ...")
    check <- system(cmd)
    if (check != 0) stop("error")
}

# calc dic_remin
message("\ncalc dic_remin ...")
r <- 0.688
cmd <- paste0(cdo,
              " -expr,'dic_remin=", r, "*(o2sat - ", o2_varname, ")'", 
              " -merge",
              " [ -select,name=o2sat ", fout_o2sat, " ]",
              " [ -select,name=", o2_varname, " ", o2_file, " ]",
              " ", fout)
message("run `", cmd, "` ...")
check <- system(cmd)
if (check != 0) stop("error")

message("\nfinished\n")


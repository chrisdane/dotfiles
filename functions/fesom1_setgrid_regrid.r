#!/usr/bin/env Rscript

# run `cdo [remapycon,global_1] -setgrid,<fesom1_griddesh.nc> <cmd_before_regrid> <in_irreg_2D_or_levelwise> <out>`
# input must either be 2D or, if 3D, levelwise

rm(list=ls()); graphics.off()

if (interactive()) {
    me <- "fesom1_setgrid_regrid.r"
    args <- c("griddes"="/pool/data/AWICM/FESOM1/MESHES/core/griddes.nc",
              #"regrid"="-remapycon,global_1",
              #"cmd_before_regrid="-shifttime,-1day",
              "outdir=/work/ba1103/a270073/out/awicm-1.0-recom/awi-esm-1-1-lr_kh800/historical2/outdata",
              "/work/ba1103/a270073/out/awicm-1.0-recom/awi-esm-1-1-lr_kh800/historical2/levelwise/thetao_fesom_29360101.nc")
} else {
    args <- commandArgs(trailingOnly=F)
    me <- basename(sub("--file=", "", args[grep("--file=", args)]))
    args <- commandArgs(trailingOnly=T)
    #fesom1_setgrid_regrid.r griddes=/pool/data/AWICM/FESOM1/MESHES/core/griddes.nc outdir=/work/ba1103/a270073/out/awicm-1.0-recom/awi-esm-1-1-lr_kh800/ssp126/outdata/levelwise/wgrid /work/ba1103/a270073/out/awicm-1.0-recom/awi-esm-1-1-lr_kh800/ssp126/outdata/levelwise/*.nc > setgrid_ssp126.log 2>&1 &
}

usage <- paste0("\nUsage:\n $ ", me, 
                " griddes=/path/to/griddes.nc [regrid=-remapycon,global_1] [cmd_before_regrid=-shifttime,-1day] outdir=/path/to/save/result file1 [file2 filen]\n",
                "\n",
                " with e.g. griddes=/pool/data/AWICM/FESOM1/MESHES/core/griddes.nc\n",
                "           griddes=/work/ollie/pool/FESOM/meshes_default/core/griddes.nc\n",
                "\n",
                " runs `cdo [remapycon,global_1] -setgrid,<fesom1_griddes.nc> <cmd_before_regrid> <in_irreg_2D_or_levelwise> <out>`\n",
                " input must either be 2D or, if 3D, levelwise\n")

# check
if (length(args) < 3) { # griddes, outdir, file1
    message(usage)
    quit()
}
message()

if (any(grepl("^griddes=", args))) {
    ind <- which(grepl("^griddes=", args))
    if (length(ind) != 1) stop("found ", length(ind), " args that start with \"griddes=\". must be 1")
    griddes <- sub("griddes=", "", args[ind])
    args <- args[-ind]
} else {
    stop("provide `griddes=/path/to/griddes.nc")
}
if (file.access(griddes, mode=0) == -1) { # not existing
    stop("provided griddes = \"", griddes, "\" does not exist")
}
if (file.access(griddes, mode=4) == -1) { # not readable
    stop("provided griddes = \"", griddes, "\" not readable.")
}
griddes <- normalizePath(griddes)
message("griddes = ", griddes)

if (any(grepl("^outdir=", args))) {
    ind <- which(grepl("^outdir=", args))
    if (length(ind) != 1) stop("found ", length(ind), " args that start with \"outdir=\". must be 1")
    outdir <- sub("outdir=", "", args[ind])
    args <- args[-ind]
} else {
    stop("provide `outdir=/path/to/save/result")
}
if (file.access(outdir, mode=0) == -1) { # not existing
    message("outdir = \"", outdir, "\" does not exist. try to create ... ", appendLF=F)
    dir.create(outdir, recursive=T)
    if (!file.exists(outdir)) {
        stop("not successful. error msg:")
    } else {
        message("success")
    }
} else { # outdir exists
    if (file.access(outdir, mode=2) == -1) { # not writable
        stop("provided outdir = \"", outdir, "\" not writeable.")
    }
}
outdir <- normalizePath(outdir)
message("outdir = ", outdir)

regrid <- NULL # default
if (any(grepl("^regrid=", args))) {
    ind <- which(grepl("^regrid=", args))
    regrid <- sub("regrid=", "", args[ind])
    args <- args[-ind]
}
if (!is.null(regrid)) message("regrid = ", regrid)

cmd_before_regrid <- NULL # default
if (any(grepl("^cmd_before_regrid=", args))) {
    ind <- which(grepl("^cmd_before_regrid=", args))
    cmd_before_regrid <- sub("cmd_before_regrid=", "", args[ind])
    args <- args[-ind]
}
if (!is.null(cmd_before_regrid)) message("cmd_before_regrid = ", cmd_before_regrid)

if (length(args) == 0) {
    message(usage)
    quit()
}
files <- args
files <- normalizePath(files, mustWork=T) # full path 
message("\nprocess ", length(files), " files:")
options(width=1000)
print(data.frame(file=files))
options(width=80)
message()

################

elapsed <- rep(NA, t=length(files))
for (fi in seq_along(files)) {

    fin <- files[fi]
    fout <- paste0(outdir, "/", tools::file_path_sans_ext(basename(fin)), "_setgrid")

    cmd <- paste0("cdo")
    if (!is.null(regrid)) {
        cmd <- paste0(cmd, " -P ", system("nproc", intern=T), " ", regrid)
        fout <- paste0(fout, "_", gsub("[[:punct:]]", "_", regrid))
    }
    cmd <- paste0(cmd, " -setgrid,", griddes)
    if (!is.null(cmd_before_regrid)) {
        cmd <- paste0(cmd, " ", cmd_before_regrid)
        fout <- paste0(fout, "_", gsub("[[:punct:]]", "_", cmd_before_regrid))
    }
    fout <- paste0(fout, ifelse(tools::file_ext(fin) == "", "", "."), tools::file_ext(fin))
    fout <- gsub("\\_+", "_", fout) # replace repeated underscores by one underscore
    if (file.exists(fout)) stop("fout ", fout, " already exists")
    cmd <- paste0(cmd, " ", fin, " ", fout)
    message("\nfile ", fi, "/", length(files), ": run `", cmd, "` ...")
    elapsed[fi] <- system.time(system(cmd))[3]

} # for fi

message("\n--> took ", round(sum(elapsed)), " sec = ", round(sum(elapsed)/60), " min\n",
        "--> ", round(mean(elapsed)), " sec = ", round(mean(elapsed)/60), " min per file")

message("\nfinished\n")


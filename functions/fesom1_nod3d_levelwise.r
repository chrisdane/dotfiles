#!/usr/bin/env Rscript

# apply spheRlab::sl.grid.FESOM3Ddata1Dto2D to input files
# --> the ncdf4::nc_create() and ncdf4::ncvar_put() calls take a long time if files are large 
# --> use cdo splityearmon (and splitday) to reduce file sizes:
# --> must apply `shifttime` before if necessary

# runtime stats:
# prepost, core, daily,  12 x 28/29/30/31 timesteps: 49    min per year (subsequent mergetime ~1min)
# prepost, core, daily, 365 x           1 timestep : 45    min per year (subsequent mergetime ~2min)
#  shared, core, daily,  12 x 28/29/30/31 timesteps: 12-17 min per year
# --> check fesom1_nod3d_levelwise_loop.r

rm(list=ls()); graphics.off()

if (interactive()) {
    me <- "fesom1_nod3d_levelwise.r"
    args <- c("meshdir=/pool/data/AWICM/FESOM1/MESHES/core",
              "outdir=/work/ba1103/a270073/out/awicm-1.0-recom/awi-esm-1-1-lr_kh800/historical2/outdata",
              "shifttime=-1day",
              "debug=true",
              "/work/ba1103/a270073/out/awicm-1.0-recom/awi-esm-1-1-lr_kh800/historical2/outdata/fesom/thetao_fesom_20130101.nc")
} else {
    args <- commandArgs(trailingOnly=F)
    me <- basename(sub("--file=", "", args[grep("--file=", args)]))
    args <- commandArgs(trailingOnly=T)
    #./fesom1_nod3d_levelwise.r meshdir=/pool/data/AWICM/FESOM1/MESHES/core outdir=/work/ba1103/a270073/out/awicm-1.0-recom/awi-esm-1-1-lr_kh800/historical2/outdata shifttime=-1day /work/ba1103/a270073/out/awicm-1.0-recom/awi-esm-1-1-lr_kh800/historical2/outdata/fesom/thetao_fesom_20140101.nc > levelwise.log 2>&1 &
}

usage <- paste0("\nUsage:\n $ ", me, 
                " meshdir=/path/to/mesh outdir=/path/to/save/result [idepth=/path/to/idepth] [shifttime=shifttimearg] file1 [file2 filen]\n")

# check
if (length(args) < 3) { # meshdir, outdir, file1
    message(usage)
    quit()
}

if (any(grepl("^meshdir=", args))) {
    ind <- which(grepl("^meshdir=", args))
    if (length(ind) != 1) stop("found ", length(ind), " args that start with \"meshdir=\". must be 1")
    meshdir <- sub("meshdir=", "", args[ind])
    args <- args[-ind]
} else {
    stop("provide `meshdir=/path/to/mesh")
}
if (file.access(meshdir, mode=0) == -1) { # not existing
    stop("provided meshdir = \"", meshdir, "\" does not exist")
}
if (file.access(meshdir, mode=4) == -1) { # not readable
    stop("provided meshdir = \"", meshdir, "\" not readable.")
}
meshdir <- normalizePath(meshdir)
message("meshdir = ", meshdir)

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

idepth <- NULL # default
if (any(grepl("^idepth=", args))) {
    ind <- which(grepl("^idepth=", args))
    idepth <- sub("idepth=", "", args[ind])
    args <- args[-ind]
}
if (!is.null(idepth)) {
    message("idepth = ", idepth)
    if (!file.exists(idepth)) stop("provided idepth = \"", idepth, "\" does not exist")
    if (file.access(meshdir, mode=4) == -1) { # not readable
        stop("provided idepth = \"", idepth, "\" not readable.")
    }
}

shifttime <- NULL # default
if (any(grepl("^shifttime=", args))) {
    ind <- which(grepl("^shifttime=", args))
    shifttime <- sub("shifttime=", "", args[ind])
    args <- args[-ind]
}
if (!is.null(shifttime)) message("shifttime = ", shifttime)

debug <- F # default
if (any(grepl("^debug=", args))) {
    ind <- which(grepl("^debug=", args))
    debug <- T
    args <- args[-ind]
}

if (length(args) == 0) {
    message(usage)
    quit()
}
files <- args
message(length(files), " files:")
options(width=1000)
print(data.frame(file=files))
options(width=80)

################

message("\nload necessary packages ...")
# load ncdf4 package
check <- T
if (!any(search() == paste0("package:ncdf4"))) {
    message("load ncdf4 package ...")
    check <- suppressWarnings(library(ncdf4, logical.return=T))
}
if (!check) { # no success
    stop("could not load ncdf4 package from\n",
         paste(paste0("   ", .libPaths()), collapse="\n"), "\n",
         "install with\n   `install.packages(\"ncdf4\")\n",
         "or\n   `install.packages(\"ncdf4\", lib=\"/path/where/the/package/should/get/installed\")")
}

# load spheRlab package
#remotes::install_github("FESOM/spheRlab")
#remotes::install_github("FESOM/spheRlab", lib="/path/where/the/package/should/get/installed")
#library(spheRlab)
#library(spheRlab, lib="/path/where/the/package/should/get/installed")
if (debug) {
    message("debug: load my spheRlab ...")
    spheRlab_path <- "/home/a/a270073/fesom/spheRlab/R"
    for (f in list.files(spheRlab_path, pattern="*.R", full.names=T)) source(f)
} else {
    check <- T
    if (!any(search() == paste0("package:spherRlab"))) {
        message("load spherRlab package ...")
        check <- suppressWarnings(library(spheRlab, logical.return=T))
        if (!check) { # no success
            stop("could not load spherRlab package from\n",
                 paste(paste0("   ", .libPaths()), collapse="\n"), "\n",
                 "install with\n   `remotes::install_github(\"FESOM/spheRlab\")\n",
                 "or\n   `remotes::install_github(\"FESOM/spheRlab\", lib=\"/path/where/the/package/should/get/installed\")")
        }
    }
}

message("\nconvert ", length(files), " files from nod3d to levelwise ...\n")
elapsed <- c()
cnt <- 0
for (fi in seq_along(files)) {
    message("******************************************************\n",
            "file ", fi, "/", length(files), ": ", files[fi])
    if (!file.exists(files[fi])) {
        message("file does not exist. skip")
    } else {
        suffix <- tools::file_path_sans_ext(basename(files[fi]))
        file_ext <- tools::file_ext(files[fi])
        ofile <- paste0(outdir, "/", suffix, "_levelwise.", file_ext)
        if (file.exists(ofile)) {
            message("ofile ", ofile, " already exists. skip")
        } else {

            # cdo splityearmon
            message("\ncdo shifttime (if necessary) and splityearmon 1 file ...")
            outdir_splityearmon <- paste0(outdir, "/splityearmon_", suffix, "_", Sys.getpid())
            if (dir.exists(outdir_splityearmon)) {
                stop("outdir_splityearmon = ", outdir_splityearmon, " already exists. this should not happen")
            } else {
                dir.create(outdir_splityearmon)
            }
            cmd <- "cdo -splityearmon"
            if (!is.null(shifttime)) cmd <- paste0(cmd, " -shifttime,", shifttime)
            cmd <- paste0(cmd, " ", files[fi], " ", outdir_splityearmon, "/", suffix, "_")
            message("run `", cmd, "` ...")
            system(cmd)
            files_splityearmon <- list.files(outdir_splityearmon)

            # cdo splitday if necessary
            message("\ncdo splitday ", length(files_splityearmon), " files if necessary ...")
            outdir_splitday <- paste0(outdir_splityearmon, "/splitday_", suffix, "_", Sys.getpid())
            if (dir.exists(outdir_splitday)) {
                stop("outdir_splitday = ", outdir_splitday, " already exists. this should not happen")
            } else {
                dir.create(outdir_splitday)
            }
            for (fj in seq_along(files_splityearmon)) {
                ntime <- system(paste0("cdo ntime ", outdir_splityearmon, "/", files_splityearmon[fj]), intern=T)
                ntime <- as.integer(ntime)
                if (F && ntime > 1) { # splitday necessary
                    cmd <- paste0("cdo splitday ", 
                                  outdir_splityearmon, "/", files_splityearmon[fj], " ", 
                                  outdir_splitday, "/", tools::file_path_sans_ext(files_splityearmon[fj]), "_")
                    message("run `", cmd, "` ...")
                    system(cmd)
                    invisible(file.remove(outdir_splityearmon, "/", files_splityearmon[fj]))
                } else { # splitday not necessary
                    invisible(file.rename(from=paste0(outdir_splityearmon, "/", files_splityearmon[fj]), 
                                          to=paste0(outdir_splitday, "/", files_splityearmon[fj])))
                }
            }
            files_splitday <- list.files(outdir_splitday)

            # spheRlab::sl.grid.FESOM3Ddata1Dto2D
            message("\nspheRlab::sl.grid.FESOM3Ddata1Dto2D ", length(files_splitday), " files ...")
            ofiles <- rep(NA, t=length(files_splitday))
            if (debug) source(paste0(spheRlab_path, "/sl.grid.FESOM3Ddata1Dto2D.R")) # reload in case of changes
            for (fj in seq_along(files_splitday)) {
                ifile <- paste0(outdir_splitday, "/", files_splitday[fj])
                message("split ", fj, "/", length(files_splitday), ": ", ifile, " ...")
                ofilej <- paste0(outdir_splitday, "/", tools::file_path_sans_ext(files_splitday[fj]), "_levelwise.", file_ext)
                ofiles[fj] <- ofilej
                cnt <- cnt + 1
                elapsed[cnt] <- system.time({
                    if (debug) {
                        sl.grid.FESOM3Ddata1Dto2D(ifile=ifile, ofile=ofilej, 
                                                  meshdir=meshdir, idepth=idepth)
                    } else {
                        spheRlab::sl.grid.FESOM3Ddata1Dto2D(ifile=ifile, ofile=ofilej, 
                                                            meshdir=meshdir, idepth=idepth)
                    }
                })[3]
                message("took ", round(elapsed[cnt]), " sec = ", round(elapsed[cnt]/60, 2), " min")
                invisible(file.remove(ifile))
            } # for fj

            # cdo mergetime
            cmd <- paste0("cdo mergetime ", paste(ofiles, collapse=" "), " ", ofile)
            message("\nrun `", cmd, "` ...")
            system(cmd)

            # rm tmp dir
            message("\n`rm -r ", outdir_splityearmon, "` ...")
            base::unlink(outdir_splityearmon, recursive=T)

        } # if fout already exists or not
    } # if fin exists or not
} # for fi

if (cnt > 0) {
    message("\nconverted ", cnt, " files\n",
            "--> spheRlab::sl.grid.FESOM3Ddata1Dto2D took ", sum(elapsed, na.rm=T), " sec = ", 
            round(sum(elapsed, na.rm=T)/60, 2), " min in total for ", length(files), " files\n",
            "--> ", round(sum(elapsed, na.rm=T)/length(files)), " sec = ", 
            round(sum(elapsed, na.rm=T)/length(files)/60, 2), " min per file")
} else {
    message("\nconverted not a single file")
}
message("\nfinished\n")


#!/usr/bin/env Rscript

if (interactive()) {
    me <- "fesom1_shifttime_-1dt.r"
    args <- c("-1d",
              "/work/ba1103/a270073/out/awicm-1.0-recom/awi-esm-1-1-lr_kh800/piControl/outdata/fesom/shifttime",
              "/work/ba1103/a270073/out/awicm-1.0-recom/awi-esm-1-1-lr_kh800/piControl/outdata/fesom/tos_fesom_29900101.nc",
              "/work/ba1103/a270073/out/awicm-1.0-recom/awi-esm-1-1-lr_kh800/piControl/outdata/fesom/tos_fesom_29910101.nc")
} else {
    args <- commandArgs(trailingOnly=F)
    me <- basename(sub("--file=", "", args[grep("--file=", args)]))
    args <- commandArgs(trailingOnly=T)
    #./fesom1_shifttime_-1dt.r "-1d" /work/ba1103/a270073/out/awicm-1.0-recom/awi-esm-1-1-lr_kh800/piControl/outdata/fesom/shifttime /work/ba1103/a270073/out/awicm-1.0-recom/awi-esm-1-1-lr_kh800/piControl/outdata/fesom/tos_fesom_* > shifttime.log 2>&1 &
}

usage <- paste0("\nUsage:\n",
                " $ ", me, " -1d <outpath> <files>\n",
                " $ ", me, " -1mon <outpath> <files>\n",
                " $ ", me, " -1year <outpath> <files>\n")

# check
if (length(args) < 3) {
    message(usage)
    quit()
}
shiftcmd <- args[1]
pathout <- args[2]
if (file.access(pathout, mode=0) == -1) { # not existing
    message("pathout = \"", pathout, "\" does not exist. try to create ... ", appendLF=F)
    dir.create(pathout, recursive=T)
    if (!file.exists(pathout)) {
        stop("not successful. error msg:")
    } else {
        message("success")
    }
} else { # pathout exists
    if (file.access(pathout, mode=2) == -1) { # not writable
        stop("provided pathout = \"", pathout, "\" not writeable.")
    }
}
files <- args[3:length(args)]

# shift every file by -1dt
message("\nshift time of ", length(files), " files by ", shiftcmd, " ...")
for (fi in seq_along(files)) {
    if (!file.exists(files[fi])) {
        message("fin ", files[fi], " does not exist. skip")
    } else {
        fout <- paste0(pathout, "/", basename(files[fi]))
        if (file.exists(fout)) {
            message("fout ", fout, " already exists. skip")
        } else {
            cmd <- paste0("cdo shifttime,", shiftcmd, " ", files[fi], " ", fout)
            message("file ", fi, "/", length(files), ": run `", cmd, "` ...")
            system(cmd)
        }
    }
} # for fi

message("\nfinished")

